;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: PERSISTENCE-MANAGER; Base: 10 -*-

(in-package persistence-manager)

;;;
;;;
;;;


(defpersistentclass test ()
  ((a :accessor a :initarg :a)
   (b :accessor b :initarg :b)))

(defpersistentclass test2 (test)
  ((c :accessor c :initarg :c)))


(defun run-test ()
  (setf table (let ((table (make-hash-table :test #'equal
                                            :size 100
                                            :rehash-size 100)))
                (loop as i from 1 to 100 do
                      (setf (gethash (list i i i) table)
                            (loop as j from 1 to (+ i 10) collect 
                                  (list (* j j )
                                        (- (* j j ))))))
                table))
  (setf x (make-instance 'test
	                 :a table))
  (setf y (make-instance 'test :a x
		         :b
		         (make-array '(10))))
  (setf z (make-instance 'test2 :c (list x y
				         (make-array '(3)
						     :initial-contents (list x y x)))))
  (setf orig (vector x y z (list x table (vector x z y) x z)))
  (make-object-persistent orig "test")
  (setf copy (load-persistent-object "test")))


(defpersistentstruct stest 
                     (a)
                     (b))

(defpersistentstruct (stest2 (:include stest))
                     (c))


(defun run-stest ()
  (setf table (let ((table (make-hash-table :test #'equal
                                            :size 100
                                            :rehash-size 100)))
                (loop as i from 1 to 100 do
                      (setf (gethash (list i i i) table)
                            (loop as j from 1 to (+ i 10) collect (* j j ))))
                table))
  (setf x (make-stest
           :a table))
  (setf y (make-stest :a x
                      :b
                      (make-array '(10))))
  (setf z (make-stest2 :c (list x y
                                (make-array '(3)
                                            :initial-contents (list x y x)))))
  (setf orig (vector x y z (list x table (vector x z y) x z)))
  (make-object-persistent orig "test")
  (setf copy (load-persistent-object "test")))



(defun test ()
  (with-open-file (stream "pertest" 
                          :element-type 'unsigned-byte
                          :direction :output :if-exists :supersede
		          :if-does-not-exist :create)
    (write-coded-string stream "das ist ein test!")
    (write-coded-integer stream 123)
    (write-coded-symbol stream 'clim::test)
    (write-coded-number stream 123.345)
    (write-coded-number stream 4/3)
    (write-coded-list stream (list 1 2 3 4))   
    (make-instance 'test
                   :a table)
    )
  (with-open-file (stream "pertest" 
                          :element-type 'unsigned-byte
                          :direction :input)
    (list      
     (read-coded-string stream)
     (read-coded-integer stream)
     (read-coded-symbol stream)
     (read-coded-number stream)
     (read-coded-number stream)
     (read-coded-list stream))))
  


(defun test ()
  (make-object-persistent (list 'symbol 123 'clim::test "abcdef" 123.3 4/3 (list 'a 1 2 'b "xyz"))
                          "pertest")
  (load-persistent-object "pertest"))
