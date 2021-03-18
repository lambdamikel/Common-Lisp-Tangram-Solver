;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
 
(in-package :cl-user)

(setf (logical-pathname-translations "tangram")
      '(("**;*.*" "C:\\Users\\Michael\\Desktop\\Tangram\\src\\**\\*.*")))

;;;
;;;
;;;

(require "clim") 

(load "tangram:define-system.lisp")

;;;
;;;
;;;

(define-system tangram-aux 
  (:default-pathname "tangram:tools;")
  (:serial 
   "aux8"))

(define-system tangram-persistence 
  (:default-pathname "tangram:persistence;")
  (:serial 
   "persistence2"))

(define-system tangram-geometry
  (:default-pathname "tangram:geometry;")
  (:serial 
   "geometry"
   "box-relations"
   "predicates"
   "rcc-predicates"   
   "relations"))


;;;
;;;
;;;

(define-system tangram-main
  (:default-pathname "tangram:main;")
  (:serial tangram-aux tangram-persistence "covering3"))

(define-system tangram-gui
  (:default-pathname "tangram:gui;")
  (:serial tangram-aux tangram-persistence "gui-aux" "gui3"))

(define-system tangram
    (:default-pathname "tangram:")
  (:serial "tangram-packages"
   ;"tangram-templates"
   tangram-aux tangram-persistence tangram-geometry
   tangram-main tangram-gui))

;;;
;;;
;;;

(compile-system "tangram")

(load-system "tangram")

(tangram::tangram)



