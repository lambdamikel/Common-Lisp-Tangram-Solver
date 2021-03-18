;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: PERSISTENCE-MANAGER; Base: 10 -*-

(in-package tangram-persistence-manager)

(defun print-persistence-manager-info (&optional (stream t))
  (format stream ";;; The store/restore facility is based on software developed~%~
                  ;;; by Michael Wessel.~2%"))

(cl:defstruct (big-vector (:constructor make-big-vector-internal))
  n-elements 
  n-subarrays
  (arrays nil))

(defconstant +max-n-of-array-elements+ (- array-total-size-limit 1))

(defun make-big-vector (dimension &key (initial-element nil))
  (multiple-value-bind (n-subarrays-minus-1 size-of-rest-array)
      (floor dimension +max-n-of-array-elements+)
    (let ((big-vector (make-big-vector-internal)))
      (setf (big-vector-n-subarrays big-vector) (+ n-subarrays-minus-1 1))
      (setf (big-vector-n-elements big-vector) dimension)
      (setf (big-vector-arrays big-vector)
	(coerce (append (loop repeat n-subarrays-minus-1 
			    collect (make-array +max-n-of-array-elements+
						:initial-element initial-element))
			(list (make-array size-of-rest-array)))
		'vector))
      big-vector)))

(declaim (inline bvref))

(defun bvref (big-array index)
  (multiple-value-bind (subarray-index index-of-rest-array)
      (floor index +max-n-of-array-elements+)
    (svref (svref (big-vector-arrays big-array) subarray-index)
           index-of-rest-array)))

(declaim (inline (setf bvref)))

(defun (setf bvref) (new-value big-array index)
  (multiple-value-bind (subarray-index index-of-rest-array)
      (floor index +max-n-of-array-elements+)
    (setf (svref (svref (big-vector-arrays big-array) subarray-index)
                 index-of-rest-array)
      new-value)))

;;;
;;;
;;;

(defconstant +persistence-version+ 3)

(defconstant +n-bytes-for-written-objects+ 16)
(defconstant +maximum-written-objects+ 
    (- (expt 10 +n-bytes-for-written-objects+) 1))

;;(defconstant +vector-size+ 1000000)
(defconstant +hash-table-size+ 10000)
(defconstant +rehash-size+ 40000)

(defvar *read-objects* nil)
;;  (make-array +vector-size+ :initial-element nil)

(defvar *written-objects* 
    (make-hash-table :test #'eql
		     :size +hash-table-size+
		     :rehash-size +rehash-size+))

;;;
;;;
;;;

(defvar *io-id-counter* 0)

(defvar *aux-counter* 0)

(defvar *ref-counter* 0)

(declaim (inline get-io-id-for create-io-id store retrieve 
                 write-coded-string1
                 write-coded-string
                 read-coded-string
                 write-coded-symbol
                 read-coded-symbol
                 write-coded-integer
                 read-coded-integer
                 write-coded-number
                 read-coded-numer
                 write-coded-marker
                 read-coded-marker
                 write-coded-list
                 read-coded-list
                 read-byte*
                 unread-byte*))

;;;
;;;
;;;

(defun write-coded-string (stream string)
  (write-coded-integer stream (length string))
  (loop as char across string do
        (write-byte (char-code char) stream)))

(defun write-coded-string-1 (stream string)
  (loop as char across string do
        (write-byte (char-code char) stream)))

(defun read-coded-string (stream)
  (let* ((n (read-coded-integer stream))
         (string (make-string n :initial-element #\space)))
    (loop as i from 1 to n do
          (setf (aref string (1- i))
	    (code-char (read-byte stream))))
    string))


(defun write-coded-symbol (stream symbol)
  (let* ((package (symbol-package symbol))
         (name (symbol-name symbol))
         (length (if (null package)
                     (+ 2 (length name))
                   (+ (length (package-name package)) 2 (length name)))))
    (write-coded-integer stream length)
    (if (null package)
        (write-coded-string-1 stream "#:")
      (progn 
        (write-coded-string-1 stream (package-name package))
        (write-coded-string-1 stream "::")))
    (write-coded-string-1 stream (symbol-name symbol))))

(defun read-coded-symbol (stream)
  (read-from-string (read-coded-string stream)))

(defun write-coded-integer (stream num)
  (unless (integerp num)
    (error "You have given ~A to write-coded-integer!" num))
  (let ((orig-num num)
        (num (abs num)))
    (loop 
      (multiple-value-bind (div mod)
	  (floor num 254)
	(write-byte mod stream)
	(setf num div)
	(when (= 0 div) 
	  (write-byte (if (minusp orig-num)
			  254 255)
		      stream)
	  (return-from write-coded-integer))))))


(defun read-coded-integer (stream)
  (let ((num 0)
        (index 1))
    (loop 
      (let ((byte (read-byte* stream)))
	(cond ((eq byte 'eof)
	       (return-from read-coded-integer 'eof))
	      ((= byte 255)
	       (return-from read-coded-integer num))
	      ((= byte 254)
	       (return-from read-coded-integer (- num)))
	      (t (incf num (* index byte))
		 (setf index (* index 254))))))))

(defun write-coded-number (stream num)
  (write-coded-string stream (write-to-string num)))

(defun read-coded-number (stream)
  (read-from-string (read-coded-string stream)))

(defun write-coded-marker (stream marker)
  (write-byte marker stream))

(defun read-coded-marker (stream)
  (read-byte* stream))




(defun write-coded-list (stream list)
  (write-coded-string stream (format nil "~S" list)))

(defun read-coded-list (stream)
  (read-from-string (read-coded-string stream)))


;;;
;;;
;;;

(defvar *last-byte-read* nil)
(defvar *unread-occured-p* nil)

(defun read-byte* (stream)
  (if (and *unread-occured-p* *last-byte-read*)
      (progn 
        (setf *unread-occured-p* nil)
        *last-byte-read*)
    (let ((byte (read-byte stream nil 'eof)))
      (if (eq byte 'eof)
          (setf *last-byte-read* nil)
        (setf *last-byte-read* byte))
      byte)))

(defun unread-byte* ()
  (setf *unread-occured-p* t))

;;;
;;;
;;;

(defun get-io-id-for (object)
  (or (gethash object *written-objects*)
      (error "Can't find object ~A!" object)))

(defun create-io-id ()
  (incf *io-id-counter*))

(defun store (io-id obj)
  (setf *io-id-counter* (max io-id *io-id-counter*)) 
  (setf (bvref *read-objects* io-id) obj)
  obj)

(defun retrieve (io-id)
  (or (bvref *read-objects* io-id)
      (error "Object ~A not found! Dangling pointer!" io-id)))

(defun reset ()
  (setf *io-id-counter* 0
        *aux-counter* 0
        *ref-counter* 0
        *unread-occured-p* nil)
  (clrhash *written-objects*))

(defun read-reset (n-of-objects-to-read)
  (setf *io-id-counter* 0
        *aux-counter* 0
        *ref-counter* 0
        *unread-occured-p* nil)
  (setf *read-objects* (make-big-vector (+ n-of-objects-to-read 3)
                                        :initial-element nil)))

;;;
;;;
;;;

(cl:defclass persistent-object ()
  ())

(defmethod write-object-constructor ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

(defmethod write-object-initializer ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

(defmethod fill-persistent-object ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

;;;
;;; verhindere, da"s initialize-instance fuer Subklassen aufgerufen wird!
;;;

(defmethod initialize-instance :around ((obj persistent-object) 
					&rest initargs
					&key (dont-initialize nil))
  (if (not dont-initialize)
      (progn
	(call-next-method))		; normale Initialisierung
    (apply #'shared-initialize obj t initargs)))

(defmethod initialize-loaded-persistent-object ((obj persistent-object))
  nil)

;;;
;;;
;;;

(cl:defstruct persistent-structure)

(defmethod write-object-constructor ((obj persistent-structure) stream)
  (declare (ignore stream))
  nil)

(defmethod write-object-initializer ((obj persistent-structure) stream)
  (declare (ignore stream))
  nil)

(defmethod fill-persistent-object ((obj persistent-structure) stream)
  (declare (ignore stream))
  nil)

(defmethod initialize-loaded-persistent-object ((obj persistent-structure))
  nil)

;;;
;;;
;;;

(defconstant +string-marker+ 10)
(defconstant +array-marker+ 20)
(defconstant +hashtable-marker+ 30)
(defconstant +cons-marker+ 40)
(defconstant +nil-marker+ 50)
(defconstant +integer-marker+ 60)
(defconstant +number-marker+ 70)
(defconstant +symbol-marker+ 80)
(defconstant +object-marker+ 90)
(defconstant +structure-marker+ 100)
(defconstant +unbound-marker+ 110)
(defconstant +section-marker+ 120)
(defconstant +user-responsibility-marker+ 130)

(defvar *secret-hashtable-size-key* (gensym "*#?@secret-"))

;;;
;;;
;;;

(defmacro with-only-once-constructed-object ((object marker stream) &body body)
  `(unless (gethash ,object *written-objects*)
     (let ((io-id (create-io-id)))
       (setf (gethash ,object *written-objects*) io-id)
       (write-coded-marker ,stream ,marker)
       (write-coded-integer ,stream io-id)
       ,@body)))

(defmethod write-constructor :after ((object t) stream)
  (declare (ignore stream))
  (incf *ref-counter*))

;;;
;;;
;;;

(defmethod user-write-constructor (stream (object t))
  (error "Please create a method \"USER-WRITE-CONSTRUCTOR (STREAM (OBJECT ~S))\"
and use subsequent calls to \"(USER-WRITE-COMPONENT-CONSTRUCTOR STREAM COMPONENT)\" to construct the
component objects of the object!" (type-of object)))

(defmethod user-write-component-constructor (stream (object t))
  (write-constructor object stream))

;;;
;;;
;;;


(defmethod write-constructor ((object t) stream)
  (with-only-once-constructed-object (object +user-responsibility-marker+ stream)
    (write-coded-symbol stream (type-of object))
    (user-write-constructor stream object))
  (values))

(defmethod write-constructor ((object null) stream)
  (declare (ignore stream))
  (values))

(defmethod write-constructor ((object number) stream)
  (declare (ignore stream))
  (values))

(defmethod write-constructor ((object cons) stream)
  (with-only-once-constructed-object (object +cons-marker+ stream)
    (write-constructor (car object) stream)
    (write-constructor (cdr object) stream))
  (values))


(defmethod write-constructor ((object string) stream)
  (with-only-once-constructed-object (object +string-marker+ stream)
    (write-coded-string stream object))
  (values))

(defmethod write-constructor ((object symbol) stream)
  (with-only-once-constructed-object (object +symbol-marker+ stream)
    (write-coded-symbol stream object))
  (values))

(defmethod write-constructor ((object array) stream)
  (with-only-once-constructed-object (object +array-marker+ stream)
    (write-coded-list stream (array-dimensions object))
    (dotimes (i (array-total-size object))
      (write-constructor (row-major-aref object i) stream)))
  (values))


(defmethod write-constructor ((object hash-table) stream)
  (with-only-once-constructed-object (object +hashtable-marker+ stream)
    (write-coded-integer stream (hash-table-count object))
    (write-coded-integer stream (hash-table-size object))
    (write-coded-integer stream (hash-table-rehash-size object))
    (write-coded-number stream (hash-table-rehash-threshold object))
    (write-coded-symbol stream (hash-table-test object))
    (maphash #'(lambda (key value)
                 (write-constructor key stream)
                 (write-constructor value stream))
             object))
  (values))

(defmethod write-constructor ((object persistent-object) stream)
  (write-constructor (type-of object) stream)
  (with-only-once-constructed-object (object +object-marker+ stream)
    (write-referencer (type-of object) stream)
    (write-object-constructor object stream))
  (values))

(defmethod write-constructor ((object persistent-structure) stream)
  (write-constructor (type-of object) stream)
  (with-only-once-constructed-object (object +structure-marker+ stream)
    (write-referencer (type-of object) stream)
    (write-object-constructor object stream))
  (values))

;;;
;;;
;;;

(defmacro with-complex-object-header ((stream object) &body body)
  `(progn 
     (write-coded-integer ,stream (get-io-id-for ,object))
     ,@body))

(defun write-referencer (object stream)
  (typecase object
    (null 
     (write-coded-marker stream +nil-marker+))
    ((or cons array hash-table string symbol persistent-object)
     (write-coded-marker stream +object-marker+)
     (write-coded-integer stream (get-io-id-for object)))
    (persistent-structure
     (write-coded-marker stream +structure-marker+)
     (write-coded-integer stream (get-io-id-for object)))
    (integer (write-coded-marker stream +integer-marker+)
             (write-coded-integer stream object))
    (number 
     (write-coded-marker stream +number-marker+)
     (write-coded-number stream object))
    (otherwise
     (write-coded-marker stream +user-responsibility-marker+)
     (write-coded-integer stream (get-io-id-for object)))))


;;;
;;;
;;;

(defmethod user-write-initializer (stream (object t))
  (error "Please create a method \"USER-WRITE-INITIALIZER (STREAM (OBJECT ~S))\"
and use subsequent calls to \"(USER-WRITE-COMPONENT-INITIALIZER STREAM COMPONENT)\" to initialize
the component objects of the object!" (type-of object)))

(defmethod user-write-component-initializer (stream (object t))
  (write-referencer object stream))

;;;
;;;
;;;

(defmethod write-initializer ((object t) stream)
  (with-complex-object-header (stream object)    
    (user-write-initializer stream object))
  (values))

(defmethod write-initializer ((object string) stream)
  (values))

(defmethod write-initializer ((object symbol) stream)
  (values))

(defmethod write-initializer ((object cons) stream)
  (with-complex-object-header (stream object)    
    (write-referencer (car object) stream)
    (write-referencer (cdr object) stream))
  (values))

(defmethod write-initializer ((object array) stream)
  (with-complex-object-header (stream object)
    (dotimes (i (array-total-size object))
      (write-referencer (row-major-aref object i)
			stream)))
  (values))

(defmethod write-initializer ((object hash-table) stream)
  (with-complex-object-header (stream object)
    (maphash #'(lambda (key value)
                 (write-referencer key stream)
                 (write-referencer value stream))
             object))
  (values))

(defmethod write-initializer ((object persistent-object) stream)
  (with-complex-object-header (stream object)
    (write-object-initializer object stream))
  (values))


(defmethod write-initializer ((object persistent-structure) stream)
  (with-complex-object-header (stream object)
    (write-object-initializer object stream))
  (values))

;;;
;;;
;;;

(defmethod fill-object ((object symbol) stream)  
  (declare (ignore stream))
  object)

(defmethod fill-object ((object string) stream)  
  (declare (ignore stream))
  object)

(defmethod fill-object ((object cons) stream)  
  (declare (ignore stream))
  (setf (car object)
    (read-value stream))
  (setf (cdr object)
    (read-value stream))
  object)

(defmethod fill-object ((object array) stream)
  (dotimes (i (array-total-size object))
    (setf (row-major-aref object i)
      (read-value stream)))
  object)

(defmethod fill-object ((object hash-table) stream)  
  (dotimes (i (gethash *secret-hashtable-size-key* object))
    (let ((key (read-value stream))
          (value (read-value stream)))
      (setf (gethash key object) value)))
  (remhash *secret-hashtable-size-key* object)
  object)

(defmethod fill-object ((object persistent-object) stream)
  (fill-persistent-object object stream)
  object)

(defmethod fill-object ((object persistent-structure) stream)
  (fill-persistent-object object stream)
  object)

(defmethod fill-object ((object t) stream)
  (user-fill-object stream object))

;;;
;;;
;;;

(defmethod user-fill-object (stream (object t))
  (error "Please create a method \"USER-FILL-OBJECT (STREAM (OBJECT ~S))\"
and use subsequent calls to \"(USER-READ-COMPONENT-OBJECT STREAM)\" to read-in 
and get the component objects of the object; assign these to the appropriate places
within the ~S and return the object!" (type-of object) (type-of object)))

(defmethod user-read-component-object (stream)
  (read-value stream))

;;;
;;;
;;;

(defun read-value (stream)
  (let ((marker (read-coded-marker stream)))
    (cond ((or (= marker +object-marker+)               
               (= marker +structure-marker+)
               (= marker +array-marker+)
               (= marker +hashtable-marker+)
               (= marker +user-responsibility-marker+))
	   (values 
	    (retrieve (read-coded-integer stream))))
          ((= marker +nil-marker+)
	   (values 
	    nil))
          ((= marker +integer-marker+)
	   (values 
	    (read-coded-integer stream)))	  
	  ((= marker +number-marker+)
	   (values 
	    (read-coded-number stream)))
	  ((= marker +unbound-marker+)
           (values 
            nil))
	  (t (error "Bad marker ~A in read-value!" marker)))))



(defun probe-bound-p (stream)
  (let ((marker (read-coded-marker stream)))
    (prog1 
        (not (= marker +unbound-marker+))
      (unread-byte*))))

(defmethod user-create-empty-object ((type t))
  (error "Please create a method \"USER-CREATE-EMPTY-OBJECT (TYPE (EQL '~S))\"
that creates an EMPTY container object of type ~S!" 
         type type))

(defun construct-object (stream)
  (loop 
    (let ((marker (read-coded-marker stream)))
      
      (if (or (= marker +section-marker+)
	      (eq marker 'eof))
	  (return)
	(let ((id (read-coded-integer stream)))
	  (store id
		 (cond ((= marker +string-marker+)
			(read-coded-string stream))
		       ((= marker +symbol-marker+)
			(read-coded-symbol stream))
		       ((= marker +cons-marker+)
			(cons (incf *aux-counter*) nil))
		       ((= marker +array-marker+)
			(make-array (read-coded-list stream)))
		       ((= marker +hashtable-marker+)
			(let* ((no-of-entries (read-coded-integer stream))
			       (size (read-coded-integer stream))
			       (rehash-size (read-coded-integer stream))
			       (rehash-threshold (read-coded-number stream))
			       (test (read-coded-symbol stream))
			       (table (make-hash-table 
				       :size size
				       :rehash-size rehash-size
				       :rehash-threshold rehash-threshold
				       :test test)))
			  (setf (gethash *secret-hashtable-size-key* table)
			    no-of-entries)
			  table))
		       ((= marker +object-marker+)
			(make-instance (read-value stream)
			  :allow-other-keys t 
			  :dont-initialize t))
		       ((= marker +structure-marker+)
			(funcall (structure-initializer (read-value stream))))
		       ((= marker +user-responsibility-marker+)
			(let ((type (read-coded-symbol stream)))                         
			  (user-create-empty-object type)))
		       (t (error "Unknown marker: ~A." marker)))))))))

(defun initialize-object (stream)
  (loop 
    (let ((io-id (read-coded-integer stream)))
      (if (eq io-id 'eof)
	  (return)
	(fill-object (retrieve io-id) stream)))))

;;;
;;;
;;;

(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let* ((name (first rest))
		   (superclasses (append (second rest) '(persistent-object)))
		   (body (third rest))
		   (slotnames (mapcar #'first body)))
	      (list 
	       `(cl:defclass ,name ,superclasses 
		  ,(loop for slotspec in body collect
			 (remove :not-persistent slotspec)))

	       `(defmethod write-object-constructor ((obj ,name) stream)
                  #+:allegro (declare (ignore-if-unused stream))
                  #+:lispworks (declare (ignore stream))
                  #+:mcl (declare (ccl:ignore-if-unused stream))
                  (with-slots ,slotnames obj
                    ,@(mapcan #'(lambda (slot)
                                  (let ((name (first slot)))
                                    (unless (member :not-persistent slot)
                                      `((when (slot-boundp obj ',name)
                                          (write-constructor ,name stream))))))
                              (reverse body)))
                  (call-next-method))

	       `(defmethod write-object-initializer ((obj ,name) stream)
                  #+:allegro (declare (ignore-if-unused stream))
                  #+:lispworks (declare (ignore stream))
                  #+:mcl (declare (ccl:ignore-if-unused stream))
                  (progn ;;with-slots ,slotnames obj
                    ,@(mapcan #'(lambda (slot)
                                  (let ((name (first slot)))
                                    (unless (member :not-persistent slot)
                                      `((if (slot-boundp obj ',name)
                                            (write-referencer (slot-value obj ',name) stream)
                                          (write-coded-marker stream +unbound-marker+))))))
                              (reverse body)))
                  (call-next-method))
	       
	       `(defmethod fill-persistent-object ((obj ,name) stream)
                  #+:allegro (declare (ignore-if-unused stream))
                  #+:lispworks (declare (ignore stream))
                  #+:mcl (declare (ccl:ignore-if-unused stream))
		  (let (val unbound)
		    #+:allegro (declare (ignore-if-unused val unbound))
                    #+:lispworks (declare (ignore val unbound))
		    #+:mcl (declare (ccl:ignore-if-unused val unbound))
		    (with-slots ,slotnames obj
		      ,@(mapcan #'(lambda (slot)
				    (let ((name (first slot)))
				      (unless (member :not-persistent slot)
					`((if (probe-bound-p stream)
                                              (setf ,name (read-value stream))
                                            (read-value stream))))))
				(reverse body)))
		    (call-next-method)))
               (list 'quote name)))))

(defmacro defclass (&rest forms)
  `(defpersistentclass .,forms))

;;;
;;;
;;;

(cl:defstruct structure-info
  name 
  options
  slot-specifications
  slotnames
  initializer)

(defvar *structures* (make-hash-table))

(defun find-structure (name &key (error-p t))
  (let ((result (gethash name *structures*)))
    (if (null result)
        (if error-p
            (error "Cannot find structure with name ~A." name)
          nil)
      result)))

(defun (setf find-structure) (new-value name)
  (if (null new-value) 
      (remhash name *structures*)
    (setf (gethash name *structures*) new-value)))


(defmacro with-structure-slots (structure-name slotnames obj &body forms)
  `(symbol-macrolet ,(mapcar (lambda (slotname)
                               (list slotname 
                                     (structure-slot-accessor structure-name 
                                                              slotname
                                                              obj)))
                             slotnames)
     .,forms))

(defun structure-slot-accessor (structure-name slotname obj)
  (let ((conc-name (or (second 
                        (find ':conc-name
                              (structure-info-options (find-structure structure-name))
                              :key #'first))
                       (concatenate 'string (symbol-name structure-name) "-"))))
    (if (symbolp conc-name)
        (setf conc-name (symbol-name conc-name)))
    (list 
     (intern (concatenate 'string 
	       conc-name
	       (symbol-name slotname))
             (symbol-package structure-name))
     obj)))

(defun set-structure-initializer (structure-entry)
  (let* ((user-defined-constructor 
          (find ':constructor
                (structure-info-options structure-entry)
                :key #'first))
         (constructor
          (if user-defined-constructor
              (let ((arguments (required-arguments (third user-defined-constructor))))
                (if arguments
                    #'(lambda ()
                        (apply (second user-defined-constructor)
                               (loop repeat (length arguments) collect nil)))
                  (second user-defined-constructor)))
            (intern 
             (concatenate 'string "MAKE-"
                          (symbol-name (structure-info-name structure-entry)))
             (symbol-package (structure-info-name structure-entry))))))
    (setf (structure-info-initializer structure-entry) constructor)))

(defun required-arguments (arguments)
  (if (null arguments)
      nil
    (if (member (first arguments) '(&rest &key &optional &allow-other-keys))
        nil
      (cons (first arguments)
            (required-arguments (rest arguments))))))

(declaim (inline structure-initializer))

(defun structure-initializer (structure-name)
  (structure-info-initializer (find-structure structure-name)))

(defun all-structure-slots (structure-entry)
  (let ((included-structure-name 
         (structure-info-included-structure structure-entry)))
    (if (null included-structure-name)
        (structure-info-slotnames structure-entry)
      (append (structure-info-slotnames structure-entry)
              (all-structure-slots (find-structure included-structure-name))))))

(defun structure-info-included-structure (structure-entry)
  (second (find ':include (structure-info-options structure-entry)
                :key #'first)))

(defun real-structure-p (options)
  (not (find ':type options :key #'first)))

(defmacro defpersistentstruct (name-or-name-and-specifications &rest doc-and-slots)
  (let* ((name (if (consp name-or-name-and-specifications)
                   (first name-or-name-and-specifications)
                 name-or-name-and-specifications))
         (options (if (consp name-or-name-and-specifications)
                      (rest name-or-name-and-specifications)
                    nil))
         (include (second (find ':include options :key #'first)))         
         (documentation (if (stringp (first doc-and-slots)) 
                            (list (first doc-and-slots))))
         (slots (if documentation 
                    (rest doc-and-slots)
                  doc-and-slots))
         (slot-specifications (mapcar (lambda (slot-spec)
                                        (cond ((symbolp slot-spec) (list slot-spec nil))
                                              ((consp slot-spec) slot-spec)
                                              (t (error "Illegal slot specification: ~A."
                                                        slot-spec))))
                                      slots))
         (slotnames (mapcar #'first slot-specifications))
         all-slotnames 
         (structure-entry-var (gensym))
         (structure-entry (find-structure name :error-p nil)))
    (if structure-entry 
        (setf (structure-info-options structure-entry) options
              (structure-info-slot-specifications structure-entry) slots
              (structure-info-slotnames structure-entry) slotnames)
      (setf structure-entry
	(make-structure-info :name name
			     :options options
			     :slot-specifications slot-specifications
			     :slotnames slotnames)))
    (set-structure-initializer structure-entry)
    (setf (find-structure name) structure-entry)
    (setf all-slotnames (all-structure-slots structure-entry))
    (if (real-structure-p options)
        `(progn 
           (let ((,structure-entry-var (find-structure ',name :error-p nil)))
             (if ,structure-entry-var 
                 (setf (structure-info-options ,structure-entry-var) ',options
                       (structure-info-slot-specifications ,structure-entry-var) ',slots
                       (structure-info-slotnames ,structure-entry-var) ',slotnames)
               (setf ,structure-entry-var
		 (make-structure-info :name ',name
				      :options ',options
				      :slot-specifications ',slot-specifications
				      :slotnames ',slotnames)))
             (set-structure-initializer ,structure-entry-var)
             (setf (find-structure ',name) ,structure-entry-var))
           (cl:defstruct (,name .,(if (null include)
                                      (cons (list :include 'persistent-structure)
                                            options)
                                    options))
             ,@documentation
             .,slot-specifications)
	   
           (defmethod write-object-constructor ((obj ,name) stream)
             #+:allegro (declare (ignore-if-unused stream))
             #+:lispworks (declare (ignore stream))
             #+:mcl (declare (ccl:ignore-if-unused stream))
             (with-structure-slots ,name ,all-slotnames obj
				   ,@(mapcan #'(lambda (slotname)
						 `((write-constructor ,slotname stream)))
					     all-slotnames)))
	   
           (defmethod write-object-initializer ((obj ,name) stream)
             #+:allegro (declare (ignore-if-unused stream))
             #+:lispworks (declare (ignore stream))
             #+:mcl (declare (ccl:ignore-if-unused stream))
             (with-structure-slots ,name ,all-slotnames obj
				   ,@(mapcan #'(lambda (slotname)
						 `((write-referencer ,slotname stream)))
					     all-slotnames)))
	   
           (defmethod fill-persistent-object ((obj ,name) stream)
             #+:allegro (declare (ignore-if-unused stream))
             #+:lispworks (declare (ignore stream))
             #+:mcl (declare (ccl:ignore-if-unused stream))
             (with-structure-slots ,name ,all-slotnames obj
				   ,@(mapcan #'(lambda (slotname)
						 `((setf ,slotname 
						     (read-value stream))))
					     all-slotnames)))
           ',name)
      `(cl:defstruct (,name .,options)
         ,@documentation
         .,slot-specifications))))

(defmacro defstruct (&rest forms)
  `(defpersistentstruct .,forms))


;;;
;;;
;;;

(defconstant +file-type-marker+ 66647963)

(defun write-section-separator (stream)
  (write-coded-marker stream +section-marker+))

(defun generate-temp-filename (filename)
  (let* ((pathname (pathname filename))
         (name (pathname-name pathname))
         new-name)
    (loop for i from 1 to 100 do
          (when (not (probe-file (setf new-name 
				   (merge-pathnames (concatenate 'string
						      name 
						      "_"
						      (format nil "~D" i))
						    pathname))))
            (return-from generate-temp-filename new-name)))
    (error "Cannot generate name for temp file.")))


(defun make-object-persistent (obj fn &optional (package *package*))
  (let ((filename (generate-temp-filename fn)))
    (handler-case 
        (progn 
          (with-open-file (stream filename
			   :element-type 'unsigned-byte
			   :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
            (let ((*package* package))
              (reset)
              (write-coded-integer stream +file-type-marker+)
              (write-coded-string stream 
                                  (make-string +n-bytes-for-written-objects+ 
                                               :initial-element #\space))
              (write-coded-string stream (package-name package))
              (write-coded-integer stream +persistence-version+)
              (write-constructor (list obj) stream)
              (write-section-separator stream)
              (maphash #'(lambda (key value)
                           (declare (ignore value))
                           (write-initializer key stream))
	               *written-objects*)))
          (when (> *io-id-counter* +maximum-written-objects+)
            (error "Maximum number of objects per file (~D) exceeded."
                   +maximum-written-objects+))
          (with-open-file (stream filename 
			   :element-type 'unsigned-byte                          
			   :direction :output :if-exists :overwrite
			   :if-does-not-exist :create)

            (write-coded-integer stream +file-type-marker+)
            (write-coded-string stream 
                                (format nil
                                        (concatenate 'string
					  "~"
					  (format nil "~D" +n-bytes-for-written-objects+)
					  ",d")
                                        *io-id-counter*)))
	  (when (probe-file fn)
	    (delete-file fn ))
          (rename-file filename fn)
          (format nil "~A objects and ~A references have been written to ~A." *io-id-counter* *ref-counter* fn))
      (error (c)
	(format *error-output* "~A" c)
	(when (probe-file filename)
	  (delete-file filename))
	nil))))


(defun load-persistent-object (fn &key (initialization-protocol t))
  (with-open-file (stream fn :direction :input :element-type 'unsigned-byte)
    (let ((file-type-marker (read-coded-integer stream)))
      (unless (= file-type-marker +file-type-marker+)
        (error "File ~S is not a Racer dump file." fn))

      (let* ((n-of-objects-to-read (read-from-string (read-coded-string stream)))
             (package-name (read-coded-string stream))
             (package (find-package package-name))
             (*package* (or package
                            (error "Cannot restore object: Package ~S not found." package-name)))
             (version (read-coded-integer stream)))
        (unless (= version +persistence-version+)
          (error "Dump file format of ~A not compatible with current version of Racer." fn))
        (read-reset n-of-objects-to-read)
        (construct-object stream)
        (initialize-object stream)
        (when initialization-protocol
          (dotimes  (i (+ 2 *io-id-counter*))
            (let ((obj (bvref *read-objects* i)))
              (when (or (typep obj 'persistent-object)
                        (typep obj 'persistent-structure))
                (initialize-loaded-persistent-object obj)))))
        (let ((result (first (retrieve 1))))
          (setf *read-objects* nil)
          (values result
                  (format nil "Loaded ~A objects from ~A." (+ 2 *io-id-counter*) fn)
                  #|(loop as i from 1 to *io-id-counter* when (typep (retrieve i) 'persistent-object) 
		  collect (retrieve i))|#))))))


;;;
;;; How to enable the framework to store and restore CLIM's RGB-COLOR objects! 
;;;

(defmethod user-write-constructor (stream (object clim-utils:rgb-color))
  ;; write appropriate component constructors for the object 
  ;; (the frame work doesn't know the structure of the object!)
  (multiple-value-bind (r g b)
      (clim-utils::color-rgb object)
    (user-write-component-constructor stream r) ;; not strictly necessary if the component objects are 
    (user-write-component-constructor stream g) ;; atomic values (e.g., symbols or numbers), but
    (user-write-component-constructor stream b) ;; doesn't harm the frame work either
    (values)))

(defmethod user-write-initializer (stream (object clim-utils:rgb-color))  
  (multiple-value-bind (r g b)
      (clim-utils::color-rgb object)
    (user-write-component-initializer stream r) ;; ensure to write-out the component objects
    (user-write-component-initializer stream g) ;; (numbers in this case)
    (user-write-component-initializer stream b)))

(defmethod user-create-empty-object ((type (eql 'clim-utils::rgb-color)))
  ;; create an "empty container" object of appropriate type, return it
  ;; in this case, ensure that CLIM really creates an RGB-COLOR object
  ;; (and not just a GRAY-COLOR object!) 
  (clim::make-rgb-color 0.1 0.2 0.3)) 

(defmethod user-fill-object (stream (object clim-utils::rgb-color))
  ;; read in the component values and assign them to appropriate places within object
  ;; don't forget to return the object! 
  (with-slots (clim-utils::red clim-utils::green clim-utils::blue) object
    (setf clim-utils::red   (user-read-component-object stream)
          clim-utils::green (user-read-component-object stream)
          clim-utils::blue  (user-read-component-object stream)))
  object)

;;;
;;; Same Thing for Gray Colors
;;;

;;;
;;; aus irgendeinem Grund gibt der "make-gray-color"-Konstruktor
;;; u.U.  ein bereits konstruiertes (falsches!) Farb-Objekt zurück!
;;; Funktioniert also nicht richtig... 
;;;

(defmethod user-write-constructor (stream (object clim-utils:gray-color))
  (with-slots (clim-utils::luminosity) object    
    (user-write-component-constructor stream clim-utils::luminosity)
    (values)))

(defmethod user-write-initializer (stream (object clim-utils:gray-color))  
  (with-slots (clim-utils::luminosity) object
    (user-write-component-initializer stream clim-utils::luminosity)))

(defmethod user-create-empty-object ((type (eql 'clim-utils::gray-color)))
  (clim::make-gray-color 0.3))

(defmethod user-fill-object (stream (object clim-utils::gray-color))
  (setf (slot-value object 'clim-utils::luminosity)
    (user-read-component-object stream))
  object)
