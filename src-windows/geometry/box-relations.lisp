;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: TANGRAM-GEOMETRY; Base: 10 -*-

(in-package tangram-geometry)

(defgeneric point-truly-inside-box-p (point box)
  (:documentation "Determine if point is contained by box. If point lies on the border of the box,
returns NIL."))


(defgeneric point-inside-box-p (point box)
  (:documentation "Determine if point is contained by box. If point lies on the border of the box,
returns T."))

(defgeneric point-truly-outside-box-p (point box)
  (:documentation "Determine if point is outside box. If point lies on the border of the box,
returns NIL."))


(defgeneric point-outside-outside-box-p (point box)
  (:documentation "Determine if point is outside box. If point lies on the border of the box,
returns T."))

(defgeneric box-inside-box-p (box1 box2)
  (:documentation "Determine whether box1 is inside box2. If box1 touches box2 from the inside,
returns T."))

(defgeneric box-truly-inside-box-p (box1 box2)
  (:documentation "Determine whether box1 is inside box2. If box1 touches box2 from the inside, 
returns NIL."))

(defgeneric box-overlaps-box-p (box1 box2)
  (:documentation "Determine whether box1 overlaps box2. If only the borders intersect, but not their
interiors, returns T."))

(defgeneric box-truly-overlaps-box-p (box1 box2)
  (:documentation "Determine whether box1 overlaps box2. If only the borders intersect, but not their
interiors, returns NIL."))

(defgeneric box-touches-box-p (box1 box2)
  (:documentation "Determine whether box1 touches box2. This is the case, if
only the borders of the boxes intersect, but not their interiors."))

(defgeneric line-intersects-box-border-p (line box)
  (:documentation "Determine whether line intersects the border of box. Intersections between the
interior and the line will be ignorerd."))


(defgeneric enlarged-box-overlaps-box-p (obj box r)
  (:documentation "Enlarge box in all directions by R and determine wheter box and the bounding box of
obj are overlapping."))


;;;
;;; BOX-Relationen	
;;;


(defun point-inside-box-p* (x y box)
  (and (<= (x (pmin box)) x (x (pmax box)))
       (<= (y (pmin box)) y (y (pmax box)))))


(defmethod point-inside-box-p ((point geom-point) (box bounding-box-mixin))
  (point-inside-box-p* (x point) (y point) box))



(defun point-truly-inside-box-p* (x y box)
  (and (< (x (pmin box)) x (x (pmax box)))
       (< (y (pmin box)) y (y (pmax box)))))


(defmethod point-truly-inside-box-p ((point geom-point) (box bounding-box-mixin))
  (point-truly-inside-box-p* (x point) (y point) box))





(defun point-outside-box-p* (x y box)
  (or (>= x (x (pmax box)))
      (<= x (x (pmin box)))
      (>= y (y (pmax box)))
      (<= y (y (pmin box)))))

(defmethod point-outside-box-p ((point geom-point) (box bounding-box-mixin))
  (point-outside-box-p* (x point) (y point) box))


(defun point-truly-outside-box-p* (x y box)
  (or (> x (x (pmax box)))
      (< x (x (pmin box)))
      (> y (y (pmax box)))
      (< y (y (pmin box)))))

(defmethod point-truly-outside-box-p ((point geom-point) (box bounding-box-mixin))
  (point-truly-outside-box-p* (x point) (y point) box))




;;;
;;;
;;;

(defun box-inside-box-p* (xmin1 ymin1 xmax1 ymax1
			  xmin2 ymin2 xmax2 ymax2)
  (and (>= xmin1 xmin2)
       (>= ymin1 ymin2)
       
       (<= xmax1 xmax2)
       (<= ymax1 ymax2)))


(defmethod box-inside-box-p ((box1 bounding-box-mixin) (box2 bounding-box-mixin))    
  (box-inside-box-p* (x (pmin box1))
		     (y (pmin box1))
		     (x (pmax box1))
		     (y (pmax box1))
		     
		     (x (pmin box2))
		     (y (pmin box2))
		     (x (pmax box2))
		     (y (pmax box2))))

;;;
;;;
;;;

(defun box-truly-inside-box-p* (xmin1 ymin1 xmax1 ymax1
				xmin2 ymin2 xmax2 ymax2)
  (and (> xmin1 xmin2)
       (> ymin1 ymin2)
       
       (< xmax1 xmax2)
       (< ymax1 ymax2)))


(defmethod box-truly-inside-box-p ((box1 bounding-box-mixin) (box2 bounding-box-mixin))    
  (box-truly-inside-box-p* (x (pmin box1))
			   (y (pmin box1))
			   (x (pmax box1))
			   (y (pmax box1))
			   
			   (x (pmin box2))
			   (y (pmin box2))
			   (x (pmax box2))
			   (y (pmax box2))))

;;;
;;;
;;;

(defun box-overlaps-box-p* (xmin1 ymin1 xmax1 ymax1
			    xmin2 ymin2 xmax2 ymax2)
  (and (<= xmin2 xmax1)
       (<= ymin2 ymax1)
       (>= xmax2 xmin1)
       (>= ymax2 ymin1)))

(defmethod box-overlaps-box-p ((box1 bounding-box-mixin) (box2 bounding-box-mixin))
  (box-overlaps-box-p* (x (pmin box1))
		       (y (pmin box1))
		       (x (pmax box1))
		       (y (pmax box1))
		       
		       (x (pmin box2))
		       (y (pmin box2))
		       (x (pmax box2))
		       (y (pmax box2))))

;;;
;;;
;;;

(defun box-truly-overlaps-box-p* (xmin1 ymin1 xmax1 ymax1
				  xmin2 ymin2 xmax2 ymax2)
  (and (< xmin2 xmax1)
       (< ymin2 ymax1)
       (> xmax2 xmin1)
       (> ymax2 ymin1)))

(defmethod box-truly-overlaps-box-p ((box1 bounding-box-mixin) (box2 bounding-box-mixin))
  (box-truly-overlaps-box-p* (x (pmin box1))
			     (y (pmin box1))
			     (x (pmax box1))
			     (y (pmax box1))
			     
			     (x (pmin box2))
			     (y (pmin box2))
			     (x (pmax box2))
			     (y (pmax box2))))


;;;
;;;
;;;


(defun box-touches-box-p* (xmin1 ymin1 xmax1 ymax1
			   xmin2 ymin2 xmax2 ymax2)
  (and (not (box-truly-overlaps-box-p* xmin1 ymin1 xmax1 ymax1
				       xmin2 ymin2 xmax2 ymax2))
       (box-overlaps-box-p* xmin1 ymin1 xmax1 ymax1
			    xmin2 ymin2 xmax2 ymax2)))


(defmethod box-touches-box-p ((box1 bounding-box-mixin) (box2 bounding-box-mixin))
  (box-touches-box-p* (x (pmin box1))
		      (y (pmin box1))
		      (x (pmax box1))
		      (y (pmax box1))
		      
		      (x (pmin box2))
		      (y (pmin box2))
		      (x (pmax box2))
		      (y (pmax box2))))

;;;
;;;
;;;

(defun line-intersects-box-border-p* (x1 y1 x2 y2 b)  
  (let ((xmin (x (pmin b)))
	(ymin (y (pmin b)))
	(xmax (x (pmax b)))
	(ymax (y (pmax b))))
    (labels ((code (x y)
	       (let ((bit1 (< x xmin))
		     (bit2 (> x xmax))
		     (bit3 (< y ymin))
		     (bit4 (> y ymax)))
		 (values bit1 bit2 bit3 bit4))))
      (multiple-value-bind (c1 c2 c3 c4)
	  (code x1 y1)
	(multiple-value-bind (ca cb cc cd)
	    (code x2 y2)
	  
	  (and (not 
		(or (and c1 ca)		; ein gemeinsames Bit ? (OR ...) => VOLLSTAENDIG UNSICHTBAR
		    (and c2 cb)
		    (and c3 cc)
		    (and c4 cd)))
	       (=> (not (or c1 c2 c3 c4 ca cb cc cd)) ; vollst. innen 
		   (or (= x1 xmin)	; liegt auf Rand ?
		       (= x1 xmax)
		       (= x2 xmin)
		       (= x2 xmax)
		       
		       (= y1 ymin)
		       (= y1 ymax)
		       (= y2 ymin)
		       (= y2 ymax)))))))))


(defmethod line-intersects-box-border-p ((line geom-line) (box bounding-box-mixin))
  (and
   (=> (and (bounding-box-p line)
	    (bounding-box-p box))
       (box-overlaps-box-p line box))
   (line-intersects-box-border-p* (x (p1 line))
				  (y (p1 line))
				  (x (p2 line))
				  (y (p2 line))
				  box)))

;;;
;;;
;;;


(defmethod enlarged-box-overlaps-box-p ((box1 bounding-box-mixin) 
					(box2 bounding-box-mixin) r)
  (box-overlaps-box-p* (- (x (pmin box1)) r) (- (y (pmin box1)) r)
		       (+ (x (pmax box1)) r) (+ (y (pmax box1)) r)
		       (x (pmin box2)) (y (pmin box2))
		       (x (pmax box2)) (y (pmax box2))))


