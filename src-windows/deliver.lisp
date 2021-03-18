;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-

(in-package :cl-user)

(load-all-patches)

#+win32 
(progn 
  (defconstant +tangram+ "c:/mwessel/tangram/")
  (load (format nil "~A~A" +home+ "define-system.lisp")))

#-win32
(progn 
  (defconstant +tangram+ "~mwessel/lispworks/work/tangram/"))
  
(defun csys (system &rest args)
  (apply #'compile-system system args)
  (apply #'load-system system args))
  

(progn 
  (setf (logical-pathname-translations "tangram")
        (list `("**;*.*" ,(format nil "~A~A" +tangram+ "**/*.*"))))
  (load "tangram:tangram-sysdcl.lisp"))

(csys "tangram" :force-p t)

(deliver 'tangram::tangram 
         #+:linux "~/temp/tangram.exe"
         4
         :console t
         :interface :capi)


(quit)