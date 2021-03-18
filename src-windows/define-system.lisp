;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(cl:in-package :cl-user)

;;; ----------------------------------------------------------------------
;;;
;;; Portable system declaration expanding into vendor-specific versions.
;;;
;;; DEFINE System = DEFsystem Is Now Expanded System
;;;
;;; ----------------------------------------------------------------------

;;; Make load-system and compile-system available in cl-user.
;;; Both systems accept a system-name (symbol or string) as a parameter.
;;; Please notice that in some native defsystems, symbols used for
;;; system names might be package-sensitive.
;;; Therefore it is recommended that cl-user is used for defsystem definitions.

#+:Genera
(import '(scl:load-system scl:compile-system) 'fcl-user)

#+:MCL
(if (or (fboundp 'load-system) (fboundp 'compile-system))
    (warn "The function definitions of load-system and/or compile-system will be overwritten.")
    (when (y-or-n-p "Do you want to do this? ") 
      (setf (symbol-function 'load-system) #'load-unit)
      (setf (symbol-function 'compile-system) #'compile-unit)))


(defun load-system-definition (system-name)
  (let ((name (if (symbolp system-name)
		  (string-downcase (symbol-name system-name))
		(string-downcase system-name))))
    (load-logical-pathname-translations name)
    (load (concatenate 'string
	    name
	    ":system-declarations;"
	    name
	    "-sysdcl.lisp"))))


(defmacro define-system (name
			 (&key
			   (pretty-name (symbol-name name))
			   default-pathname
			   (subsystem nil))
			 components)
  #+(or :Allegro :mcl :Lispworks)
  (declare (ignore subsystem #+(or :mcl :Lispworks) pretty-name))
  (labels ((host-substring (logical-pathname)
	     (let ((position (position #\: logical-pathname)))
	       (if position 
		   (subseq logical-pathname 0 position)
		 nil)))
	   #+:Allegro
	   (system-loading-code (components)
	     (let ((systems (remove-if #'stringp 
				       (flatten-serial-parallel-descriptions
					components))))
	       (mapcar #'(lambda (system-name)
			   `(unless (find-system ',system-name)
			      (load-system-definition ',system-name)))
		       systems)))
	   #+(or :Allegro :Genera :mcl :Lispworks)
	   (flatten-serial-parallel-descriptions
	     (description)
	     (if (consp description)
		 (mapcan #'flatten-serial-parallel-descriptions
			 (rest description))
		 (list description))))
    
    (unless default-pathname
      (error "A default pathname must be supplied in a system definition."))
    
    (let ((logical-host (host-substring default-pathname))
	  #+(or :Genera :mcl)
	  (systems-depending-on
	    (remove-if-not #'symbolp
			   (flatten-serial-parallel-descriptions
			     components))))
      
      (unless logical-host
	(error "Systems must be given a logical pathname as default pathname."))
      
      `(progn
	 #+:Allegro
	 ,@(system-loading-code components)
	 (load-logical-pathname-translations ,logical-host)
	 #+:Allegro
	 (excl:defsystem ,name
			 (:default-pathname ,default-pathname
			  :pretty-name ,pretty-name)
			 ,components)

	 #+:Lispworks
	 (lw:defsystem ,name
		       (:default-pathname ,(string-upcase default-pathname))
		       :members
		       ,(mapcar #'(lambda (component)
				    (if (symbolp component)
					`(,component :type :system)
					component))
				(flatten-serial-parallel-descriptions
				  components))
		       :rules
		       ((:in-order-to :compile :all
				      (:requires (:load :previous)))))

	 #+:Genera
	 (,(if subsystem
	       'sct:defsubsystem
	       'sct:defsystem)
	  ,name
	  (:default-pathname ,default-pathname
	   :pretty-name ,pretty-name)
	  ,@(mapcar #'(lambda (system)
			`(:module ,system (,system) (:type :system)))
		    systems-depending-on)
	  ,components)

	 #+:mcl
	 (cc:defunit ,name
		     (:depends-on . ,systems-depending-on)
		     (:source-pathname ,default-pathname)
		     (:binary-pathname ,default-pathname)	; not default
		     (:components . ,(remove-if
				       #'symbolp
				       (flatten-serial-parallel-descriptions
					 components))))))))




#|


(define-system :test
  (:default-pathname "test:default;"
   :pretty-name "Test"
   :subsystem t)
  (:serial (:parallel :bar :bvyy) "test1" "test2"))

|#
