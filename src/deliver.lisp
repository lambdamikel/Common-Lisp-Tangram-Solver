;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-

(in-package :cl-user)

(load-all-patches)

#+:mswindows
(setf (logical-pathname-translations "tangram")
      '(("**;*.*" "C:\\Users\\micha\\Desktop\\GIT\\Common-Lisp-Tangram-Solver\\src\\**\\*.*")))

#-:mswindows
(setf (logical-pathname-translations "tangram")
      '(("**;*.*" "/home/mwessel/tangram/src/**/*.*")))

;;;
;;;
;;;

(require "clim") 

(load "tangram:define-system.lisp")
(load "tangram:tangram-sysdcl.lisp")

  
(defun csys (system &rest args)
  (apply #'compile-system system args)
  (apply #'load-system system args))
  
(csys "tangram" :force-p t)

(deliver 'tangram::tangram 
         #-:mswindows
         "/home/mwessel/temp/tangram"
         #+:mswindows         
         "c:\\temp\\tangram.exe"
         4
	:packages-to-keep 
	'(tangram-geometry tangram-persistence-manager cl-user tangram)
	:interface :capi 
	:multiprocessing t
	)

(quit)

