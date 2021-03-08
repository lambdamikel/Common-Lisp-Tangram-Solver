;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)

;;;
;;; Define Packages
;;;

(export '(transform-xy-list

          psgraph
	  
          tree-reverse
	  tree-remove
          tree-collect-if
          tree-find
	  tree-find-if
          tflatten
          
          my-round
	  =>
	  <=>
          
          kill
          run

          blank-line-p

          with-standard-output-to-file

          concept-name

	  intervall-intersects-p
	  circle-intervall-intersects-p
	  lies-in-circle-intervall-p	  
	  circle-intervall-length
	  
	  rad-to-deg
	  deg-to-rad
	  
	  +2pi+
	  +pi/2+
	  
          recode-german-characters
	  
	  zerop-eps
	  =-eps
	  <=-eps
	  >=-eps

          one-of
          
          xor
          
          defun-memo
          defmethod-memo
          reset-memo
	  
	  yes
	  no

          ensure-list	  
	  circle-subseq
	  mynconc
	  my-format
	  pushend
          make-cyclic
          
	  get-lambda-args
	  
          set-equal
	  set-disjoint	  

          fac
          n-over-k
          k-out-of-n
          compute-all-subsets
          power
          prod 
          perm
          permutations
          
	  forcecdr
	  
          stringsubst-char-with-string
          string-substitute
	  
          build-elem-pairs-in-list
	  vector-orientation
	  splice-in
         
          reorder
          
          loop-over-avl-tree
          insert-into-avl-tree
          delete-from-avl-tree
          find-in-avl-tree 
          print-avl-tree))

(defpackage tangram-persistence-manager
  (:use common-lisp)
  (:shadow #:defclass #:defstruct)
  (:export 
   #:defpersistentclass 
   #:defpersistentstruct 
   #:initialize-loaded-persistent-object
   #:make-object-persistent
   #:load-persistent-object
   #:print-persistence-manager-info

   #:user-write-constructor
   #:user-write-component-constructor
   #:user-write-initializer
   #:user-write-component-initializer
   #:user-fill-object 
   #:user-read-component-object
   #:user-create-empty-object))


(defpackage tangram-geometry 
  (:use common-lisp-user 
   common-lisp
   tangram-persistence-manager)
  (:export 

   infinit
	   	   
   geom-thing
   geom-point
   geom-line
   geom-chain-or-polygon
   geom-chain	   
   geom-polygon
   geom-aggregate
   bounding-box
   bounding-box-mixin
	   
   delete-object
	   
   polygon
   chain

   make-circle
   make-arc

   make-point
   p
   make-line
   l
   make-polygon
   poly
   make-chain
   chain
   make-aggregate
   agg
   make-bounding-box
   bb

   poly-from-xy-list
   chain-from-xy-list
   get-xy-list
   bounding-box-p
	   
   bound-to
	   
   ;id
   mark-value
   mark-object
   x
   y
   det
   det-0-vecs
   bb-width
   bb-height
	   
   p1 
   p2
   centroid
	   
   p1-of
   p2-of
   centroid-of

	   

   pmin				; BB 
   pmax				; BB

   radius			; BB
	   
   segments	  
   point-list
   has-parts			; nur fuer Aggregate !
   part-of
	   
   point-=-p
   point->=-p
   point-<=-p
   line-=-p
	   
   joins-p
   parallel-p
   upright-p
	   
   primary-p			; Primaerobjekt <=> keine Master
   component-p 
   common-root-p
   get-all-masters		; fuer alle Objekte !
   get-all-components 
   get-direct-components	; fuer alle Objekte !
   get-already-present-direct-master ; fuer alle Objekte !
   get-topmost-master
   get-topmost-common-master
   get-direct-common-master
   get-direct-common-master-from-list
	   
	   
   for-each-master-holds-p
   for-each-component-holds-p
	   
   for-some-master-holds-p
   for-some-component-holds-p

   calculate-bounding-box
   invalidate-bounding-box
	   
   calculate-bounding-box-for-complex-object
   calculate-centroid
	   
   calculate-intersection-point
   calculate-intersection-line
	   
   angle-difference
	   
   distance-between*
   distance-between-point-and-line
   distance-between
   distance-between-xy

   length-of-line 
	   
   point-truly-inside-box-p
   point-truly-inside-box-p*

   point-inside-box-p
   point-inside-box-p*
	   
   point-truly-outside-box-p
   point-truly-outside-box-p*

   point-outside-box-p
   point-outside-box-p*
	   
   box-overlaps-box-p
   box-overlaps-box-p*
	   
   enlarged-box-overlaps-box-p
	   
   box-truly-overlaps-box-p
   box-truly-overlaps-box-p*
	   
   box-touches-box-p
   box-touches-box-p*

   box-inside-box-p
   box-inside-box-p*
	   
   box-truly-inside-box-p
   box-truly-inside-box-p*
	   
   line-intersects-box-border-p
   line-intersects-box-border-p*
	   
   normalize
   angle-between
   angle-between*		   
   distance-and-orientation
   distance-and-orientation*
   global-orientation
	   
	   
   make-matrix
   reset
   translate
   scale
   rotate
   compose
   with-matrix	   
   with-translation
   with-scaling
   with-rotation
   with-no-matrix-at-all
	   
   proportional-2-point
   fixed-sy-2-point	   

   lies-on-p
   lies-on-p*	   
   inside-p  
   inside-p*
   outside-p
   outside-p*
   intersects-p
   crosses-p
   crosses-p*
   crossed-by-p   
   touches-p
   covers-p
   covered-by-p
   overlaps-p
   congruent-p
   0d-touches-p
   1d-touches-p
   1d-intersects-p
   equal-p
   inside-epsilon-p
   inside-epsilon-p*

   calculate-relation
   inverse

   get-epsilon-enclosure-relevant-points
   get-epsilon-enclosure-relevant-points*

   segment-list-ok-p
	   
   ;; Symbole (werden von calculate-relation geliefert)
   borders
   bordered-by
   disjoint
   lies-on
   crosses   
   crossed-by
   touches
   0d-intersects	       
   1d-intersects		
   intersects
   inside
   contains
   covers
   covered-by
   overlaps
   congruent

   calculate-relation
   calculate-rcc-relation
   inverse-rcc-relation
   
   orientation
   decode-ccw
   orientate-counterclockwise
   orientate-clockwise
   affected-by-matrix-p
   insert-new-border-points
   
   ))

(defpackage tangram 
  (:use common-lisp-user 
   common-lisp
   clim
   clim-lisp   
   tangram-persistence-manager   
   tangram-geometry)
  (:SHADOW
   CLIM:COLOR)
  (:shadowing-import-from tangram-geometry 
   POLYGON 
   MAKE-POINT 
   MAKE-LINE 
   MAKE-POLYGON
   WITH-TRANSLATION
   WITH-SCALING
   WITH-ROTATION)
  (:shadowing-import-from clim
			  PATHNAME TRUENAME
			  BOOLEAN 
			  INTERACTIVE-STREAM-P))




   




