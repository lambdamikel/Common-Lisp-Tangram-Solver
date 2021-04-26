;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: TANGRAM-GEOMETRY; Base: 10 -*-

(in-package tangram-geometry)

;;;
;;; Basis-Prädikate
;;;

(defun intersects-p* (xs1 ys1 xe1 ye1
		          xs2 ys2 xe2 ye2)
  (let* ((a  (ccw* xs1 ys1 
		   xe1 ye1
		   xs2 ys2))
	 (b  (ccw* xs1 ys1
		   xe1 ye1
		   xe2 ye2))
	 (c  (ccw* xs2 ys2
		   xe2 ye2
		   xs1 ys1))
	 (d  (ccw* xs2 ys2
		   xe2 ye2
		   xe1 ye1)))
    (and
     (<= (* a b) 0.0) (<= (* c d) 0.0))))

(defun crosses-p* (xs1 ys1 xe1 ye1
		       xs2 ys2 xe2 ye2)
  (let* ((a  (ccw* xs1 ys1 
		   xe1 ye1
		   xs2 ys2))
	 (b  (ccw* xs1 ys1
		   xe1 ye1
		   xe2 ye2))
	 (c  (ccw* xs2 ys2
		   xe2 ye2
		   xs1 ys1))
	 (d  (ccw* xs2 ys2
		   xe2 ye2
		   xe1 ye1)))
    (when (and (<= (* a b) 0.0) (<= (* c d) 0.0)) ; Schnitt liegt vor
      (not (or (zerop a) 
	       (zerop b)
	       (zerop c)
	       (zerop d)
	       
	       (zerop (ccw* xe1 ye1
			    xs1 ys1 
			    xs2 ys2))
	       (zerop (ccw* xe1 ye1
			    xs1 ys1
			    xe2 ye2))
	       (zerop (ccw* xe2 ye2
			    xs2 ys2
			    xs1 ys1))
	       (zerop (ccw* xe2 ye2
			    xs2 ys2
			    xe1 ye1)))))))

(defmethod lies-on-p* (x y (line geom-line))
  (or
   (zerop (ccw* (x (p1 line))
		(y (p1 line))
		(x (p2 line))
		(y (p2 line))
		x y))
   (zerop (ccw* (x (p2 line))
		(y (p2 line))
		(x (p1 line))
		(y (p1 line))
		x y))))

(defmethod-memo lies-on-p* (x y (poly-or-chain geom-chain-or-polygon))
    ((x y poly-or-chain (trafo-id *matrix*)))
  (some #'(lambda (segment)
	    (lies-on-p* x y segment))
	(segments poly-or-chain)))

;;;
;;; LIES-ON 
;;;


(defmethod lies-on-p ((point1 geom-point) (point2 geom-point))
  (point-=-p point1 point2))

(defmethod lies-on-p ((point geom-point) (line geom-line))
  (or (zerop (ccw (p1 line) (p2 line) point))
      (zerop (ccw (p2 line) (p1 line) point))))

(defmethod-memo lies-on-p ((point geom-point) (poly-or-chain geom-chain-or-polygon)) 
    ((point poly-or-chain (trafo-id *matrix*)))
  (some #'(lambda (segment)
	    (lies-on-p point segment))
	(segments poly-or-chain)))

(defmethod lies-on-p ((l1 geom-line) (l2 geom-line))
  (and (lies-on-p (p1 l1) l2)
       (lies-on-p (p2 l1) l2)))
       

#|
(defmethod-memo lies-on-p ((line geom-line) (poly geom-polygon)) 
    ((line poly-or-chain))
  (or (some #'(lambda (segment) 
                (equal-p line segment))
            (segments poly))
      (and (intersects-p line poly)
           (not (one-part-is-inside-p line poly))
           (not (one-part-is-outside-p line poly)))))
|#
         
(defmethod-memo lies-on-p ((line geom-line) (poly geom-polygon)) 
    ((line poly (trafo-id *matrix*)))
   (or (some #'(lambda (segment) 
                 (lies-on-p line segment))
             (segments poly))
       (and (lies-on-p (p1 line) poly)
            (lies-on-p (p2 line) poly)
            (or (every #'(lambda (point)
                           (lies-on-p point line))
                       (get-points-between poly (p1 line) (p2 line)))                
                (every #'(lambda (point)
                           (lies-on-p point line))
                       (get-points-between poly (p2 line) (p1 line)))))))
                

(defmethod-memo lies-on-p ((line geom-line) (chain geom-chain)) 
    ((line chain (trafo-id *matrix*)))
   (or (some #'(lambda (segment) 
                 (lies-on-p line segment))
             (segments chain))
       (and (lies-on-p (p1 line) chain)
            (lies-on-p (p2 line) chain)
            (every #'(lambda (point)
                       (lies-on-p point line))
                   (get-points-between chain (p1 line) (p2 line))))))


(defmethod-memo lies-on-p ((poly-or-chain1 geom-chain-or-polygon) (poly-or-chain2 geom-chain-or-polygon))
    ((poly-or-chain1 poly-or-chain2 (trafo-id *matrix*)))
  (every #'(lambda (segment) 
             (lies-on-p segment poly-or-chain2))
         (segments poly-or-chain1)))

;;;
;;; INTERSECTS
;;;


(defmethod intersects-p ((point1 geom-point) (point2 geom-point))
  (point-=-p point1 point2))

(defmethod intersects-p ((point geom-point) (line geom-line))
  (lies-on-p point line))

(defmethod intersects-p ((line geom-line) (point geom-point))
  (lies-on-p point line))

(defmethod intersects-p ((point geom-point) (poly-or-chain geom-chain-or-polygon))
  (lies-on-p point poly-or-chain))

(defmethod intersects-p ((poly-or-chain geom-chain-or-polygon) (point geom-point))
  (lies-on-p point poly-or-chain))

(defmethod intersects-p ((l1 geom-line) (l2 geom-line)) 
  (and (=> (and (bounding-box-p l1)
		(bounding-box-p l2))
	   (box-overlaps-box-p l1 l2))
       (let* ((l1p1 (p1 l1))
	      (l2p1 (p1 l2))
	      (l1p2 (p2 l1))
	      (l2p2 (p2 l2))
	      
	      (a  (ccw l1p1 l1p2 l2p1))
	      (b  (ccw l1p1 l1p2 l2p2))
	      (c  (ccw l2p1 l2p2 l1p1))
	      (d  (ccw l2p1 l2p2 l1p2)))
	 (and
	  (<= (* a b) 0.0) (<= (* c d) 0.0)))))

(defmethod-memo intersects-p ((line geom-line) (poly-or-chain geom-chain-or-polygon))
    ((line poly-or-chain (trafo-id *matrix*)))
  (and (=> (and (bounding-box-p line)
		(bounding-box-p poly-or-chain))
	   (box-overlaps-box-p line poly-or-chain))
       (some #'(lambda (segment) 
		 (intersects-p line segment))	     
	     (segments poly-or-chain))))

(defmethod intersects-p ((poly-or-chain geom-chain-or-polygon) (line geom-line))
  (intersects-p line poly-or-chain))

(defmethod-memo intersects-p ((poly-or-chain1 geom-chain-or-polygon) (poly-or-chain2 geom-chain-or-polygon))
    ((poly-or-chain1 poly-or-chain2 (trafo-id *matrix*)))
  (some #'(lambda (segment)
            (intersects-p segment poly-or-chain2))
        (segments poly-or-chain1)))

;;;
;;; CROSSES
;;;

(defmethod crosses-p ((l1 geom-line) (l2 geom-line)) 
  ;; "X" oder "+" 
  (and (=> (and (bounding-box-p l1)
                (bounding-box-p l2))
           (box-truly-overlaps-box-p l1 l2))
       (let* ((l1p1 (p1 l1))
              (l2p1 (p1 l2))
              (l1p2 (p2 l1))
              (l2p2 (p2 l2))
	      
              (a  (ccw l1p1 l1p2 l2p1))
              (a1 (ccw l1p2 l1p1 l2p1))
              (b  (ccw l1p1 l1p2 l2p2))
              (b1 (ccw l1p2 l1p1 l2p2))
              (c  (ccw l2p1 l2p2 l1p1))
              (c1 (ccw l2p2 l2p1 l1p1))
              (d  (ccw l2p1 l2p2 l1p2))
              (d1 (ccw l2p2 l2p1 l1p2)))
	 
         (and (not (or (zerop a)
                       (zerop a1)
                       (zerop b)
                       (zerop b1)
                       (zerop c)
                       (zerop c1)
                       (zerop d)
                       (zerop d1)))
              (<= (* a b) 0) (<= (* c d) 0)))))

(defmethod crossed-by-p ((l1 geom-line) (l2 geom-line))
  (crosses-p l2 l1))

;;;
;;; TOUCHES
;;;


(defmethod touches-p ((l1 geom-line) (l2 geom-line)) 
  ;; z.B. meets ("--"), aber auch "T" oder "\/"
  (and (=> (and (bounding-box-p l1)
                (bounding-box-p l2))
           (box-overlaps-box-p l1 l2))
       (let ((l1p1-l2 (lies-on-p (p1 l1) l2))
             (l1p2-l2 (lies-on-p (p2 l1) l2))
             (l2p1-l1 (lies-on-p (p1 l2) l1))
             (l2p2-l1 (lies-on-p (p2 l2) l1)))

         (or ;; "T"-Faelle
                (and l1p1-l2 (not l1p2-l2) (not l2p1-l1) (not l2p2-l1)) 
                (and (not l1p1-l2) l1p2-l2 (not l2p1-l1) (not l2p2-l1)) 
                (and (not l1p1-l2) (not l1p2-l2) l2p1-l1 (not l2p2-l1))  
                (and (not l1p1-l2) (not l1p2-l2) (not l2p1-l1) l2p2-l1)
	        
                ;; "--" und "\/"-Faelle 
                (and (joins-p l1 l2)
                     ;; 1d-Schnitte ausschliessen
                     (not (or (and l1p1-l2 l1p2-l2) 
                              (and l2p1-l1 l2p2-l1))))))))

;;;
;;; 1D-INTERSECTS
;;;


(defmethod 1d-intersects-p ((l1 geom-line) (l2 geom-line)) 
  ;; overlaps, contains, covers, equal + Inverse
  (and (=> (and (bounding-box-p l1)
		(bounding-box-p l2))
	   (box-overlaps-box-p l1 l2))
       (let ((l1p1-l2 (lies-on-p (p1 l1) l2))
	     (l1p2-l2 (lies-on-p (p2 l1) l2))
	     (l2p1-l1 (lies-on-p (p1 l2) l1))
	     (l2p2-l1 (lies-on-p (p2 l2) l1)))
	 
	 
	 (or (and l1p1-l2       (not l1p2-l2) l2p1-l1       (not l2p2-l1) ; overlaps-faelle
		  (not (point-=-p (p1 l1) (p1 l2)))) ; touches ausschliessen
	     (and l1p1-l2       (not l1p2-l2) (not l2p1-l1) l2p2-l1
		  (not (point-=-p (p1 l1) (p2 l2))))
	     (and (not l1p1-l2) l1p2-l2       l2p1-l1       (not l2p2-l1)
		  (not (point-=-p (p2 l1) (p1 l2))))
	     (and (not l1p1-l2) l1p2-l2       (not l2p1-l1) l2p2-l1
		  (not (point-=-p (p2 l1) (p2 l2))))
	     
	     (and      l1p1-l2       l1p2-l2  (not l2p1-l1) (not l2p2-l1)) ; l1 contains l2
	     (and (not l1p1-l2) (not l1p2-l2)      l2p1-l1       l2p2-l1)
	     
	     (and l1p1-l2 l1p2-l2 l2p1-l1 l2p2-l1) ; equal
	     
	     (and l1p1-l2       (not l1p2-l2) l2p1-l1 l2p2-l1) ; l1 covers l2
	     (and (not l1p1-l2)      l1p2-l2  l2p1-l1 l2p2-l1)
	     
	     (and l1p1-l2 l1p2-l2      l2p1-l1  (not l2p2-l1)) ; l2 covers l1
	     (and l1p1-l2 l1p2-l2 (not l2p1-l1)      l2p2-l1)))))

;;;
;;; Bestimme, ob bzgl. der von "line" erzeugten Halbebenen alle 
;;; gegebenen Strecken auf einer Seite (bzw. in einer der Halbebenenen)
;;; liegen. 
;;; 


(defmethod-memo one-one-side-of-p ((segments list) (line geom-line))  
    ((segments line (trafo-id *matrix*)))
  (let ((ccws
         (mapcar #'(lambda (x) 
                     (ccw (p1 line) (p2 line) x))
                 (apply #'append
                        (mapcar #'point-list segments)))))
    (or (every #'(lambda (x) (not (minusp x)))
               ccws)
        (every #'(lambda (x) (not (plusp x)))
               ccws))))

;;;
;;; Inside-Relation
;;;

(defparameter *cur-line*		; willkuerlich gewaehlt
  (l
   (p 0 0 :affected-by-matrix-p nil)  ;;; WICHTIG!!!
   (p 1 1 :affected-by-matrix-p nil)
   :affected-by-matrix-p nil
   :bounding-box-p nil))

(defparameter *test-beam*		; willkuerlich gewaehlt
  (l
   (p 0 0 :affected-by-matrix-p nil)
   (p 1 1 :affected-by-matrix-p nil)
   :affected-by-matrix-p nil 
   :bounding-box-p nil))


#| 
int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
{
  int i, j, c = 0;
  for (i = 0, j = nvert-1; i < nvert; j = i++) {
    if ( ((verty[i]>testy) != (verty[j]>testy)) &&
     (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
       c = !c;
  }
  return c;
}
|# 
#| 

(defun-memo count-intersections* (x y polygon)
    ((x y polygon (trafo-id *matrix*)))
  (let* ((points (t-list polygon))
         (count 0))
    (dolist (point points) 
      (if (not (eq (> ( y point)  y)


((verty[i]>testy) != (verty[j]>testy)) &&
     (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
       c = !c;


|# 


(defun-memo count-intersections* (x y polygon)
    ((x y polygon (trafo-id *matrix*)))
  (let* ((points (point-list polygon))
	 (count 0)
	 (from-point nil)
	 (cur-point nil)	 
	 (mark-point '?)
	 (flag nil))

    (setf 	  
     (x (p1 *test-beam*)) x
     (y (p1 *test-beam*)) y
	
     (x (p2 *test-beam*)) +big-int+
     (y (p2 *test-beam*)) +big-int2+)
      
    (loop until (eq mark-point 'stop)
		
	  do				; einmal ganz rum	  


	  (setf cur-point (first points)
		points (rest points))	  
	  (when (null points) 
	    (setf points (point-list polygon)))
	  
	  (when (eq cur-point mark-point)
	    (setf mark-point 'stop))

	  
	  (cond ((and (= y (y cur-point)) ; Zeit sparen...
		      (<= x (x cur-point))
		      (lies-on-p cur-point *test-beam*))

		 (setf flag t))
		 
		(t
		 (if (not from-point)	; noch kein erster Pkt. gefunden
		     (progn
		       (setf mark-point cur-point) ; merken => einmal rum!
		       (setf flag nil)
		       (setf from-point cur-point))
		   (progn
		     (cond (flag	; es lagen Punkte zwischen from-point und cur-point a.d. Teststrahl
			    (when (or (<= (y from-point) y (y cur-point))
				      (>= (y from-point) y (y cur-point)))
			      (incf count)))
			   (t		; es lagen keine Pkt. a.d. Teststrahl
			    (setf (p1 *cur-line*) from-point)
			    (setf (p2 *cur-line*) cur-point)
			    (let ((p1c (p1 *cur-line*))
				  (p2c (p2 *cur-line*))
				  (p1t (p1 *test-beam*))
				  (p2t (p2 *test-beam*)))				
			      (when (not (and (= (x p1c) (x p2c) x)))
				(when (intersects-p* (x p1c) (y p1c) (x p2c) (y p2c)
						     (slot-value p1t 'x) (slot-value p1t 'y)
						     (slot-value p2t 'x) (slot-value p2t 'y))
				  (incf count))))))
						     
		     
		     (setf flag nil)	     
		     (setf from-point cur-point))))))
    count))

(defun count-intersections (point polygon)
  (count-intersections* (x point) (y point) polygon))


;;;
;;; ACHTUNG: ALLE INSIDE/OUTSIDE-RELATIONEN SIND "TRULY"-INSIDE => RAND GEHOERT NICHT DAZU !!!
;;;

(defmethod-memo inside-p* (x y (polygon geom-polygon))
    ((x y polygon (trafo-id *matrix*)))
  (and
   (=> (bounding-box-p polygon) 
       (point-truly-inside-box-p* x y polygon))
   (not (lies-on-p* x y polygon))
   (oddp (count-intersections* x y polygon))))

(defmethod inside-p ((point geom-point) (polygon geom-polygon))
  (inside-p* (x point) (y point) polygon))

(defmethod contains-p ((polygon geom-polygon) (point geom-point) )
  (inside-p point polygon))

(defmethod-memo outside-p* (x y (polygon geom-polygon))
    ((x y polygon (trafo-id *matrix*)))
  (or 
   (and (bounding-box-p polygon)
	(point-truly-outside-box-p* x y polygon))
   (and 
    (not (lies-on-p* x y polygon))
    (evenp (count-intersections* x y polygon)))))
  
(defmethod outside-p ((point geom-point) (polygon geom-polygon))
  (outside-p* (x point) (y point) polygon))

;;;
;;;
;;;


(defmethod-memo inside-p ((line geom-line) (poly geom-polygon))
    ((line poly (trafo-id *matrix*)))
  (and (=> (and (bounding-box-p line)
		(bounding-box-p poly))
	   (box-truly-inside-box-p line poly))
       (not (intersects-p line poly))
       (inside-p (p1 line) poly)
       (inside-p (p2 line) poly)))

(defmethod contains-p ((poly geom-polygon) (line geom-line))
  (inside-p line poly))

(defmethod-memo outside-p ((line geom-line) (poly geom-polygon)) 
    ((line poly (trafo-id *matrix*)))
  (or (and (bounding-box-p line)
	   (bounding-box-p poly)
	   (not (box-overlaps-box-p line poly)))
      
      (and (not (intersects-p line poly))
	   (outside-p (p1 line) poly)
	   (outside-p (p2 line) poly))))

;;;
;;;
;;;

(defmethod-memo inside-p ((poly-or-chain geom-chain-or-polygon) (poly geom-polygon))
    ((poly-or-chain poly (trafo-id *matrix*)))
  (and (=> (and (bounding-box-p poly-or-chain)
		(bounding-box-p poly))
	   (box-truly-inside-box-p poly-or-chain poly))
       (every #'(lambda (segment)
		  (inside-p segment poly))
	      (segments poly-or-chain))))

(defmethod contains-p ((poly geom-polygon) (poly-or-chain geom-chain-or-polygon))
  (inside-p poly-or-chain poly))

(defmethod-memo outside-p ((poly-or-chain geom-chain-or-polygon) (poly geom-polygon))
    ((poly-or-chain poly (trafo-id *matrix*)))
  (or (and (bounding-box-p poly-or-chain)
	   (bounding-box-p poly)
	   (not (box-overlaps-box-p poly-or-chain poly)))
      
      (every #'(lambda (segment) 
		 (outside-p segment poly))
	     (segments poly-or-chain))))


;;;
;;; Approximation: ist ein Teil einer Linie innerhalb eines Polygons enthalten? 
;;; Bzw. gibt es einen echten Schnitt mit dem Inneren des Polygon?
;;; Gibt es hierfür einen effizienten Algorithmus?
;;; Hier zählt der Rand vom Polygon *NICHT* als Inneres!!!
;;;

(defconstant +granularity+ 20)

(defmethod-memo one-part-is-inside-p ((line geom-line) (poly geom-polygon))
    ((line poly (trafo-id *matrix*)))
  (or (inside-p (p1 line) poly)
      (inside-p (p2 line) poly)      
      (one-part-is-inside-p* (x (p1 line)) (y (p1 line)) 
                             (x (p2 line)) (y (p2 line))
                             poly
                             (/ (+ (expt (- (x (p1 line)) (x (p2 line))) 2)
                                   (expt (- (y (p1 line)) (y (p2 line))) 2))
                                +granularity+))))

(defun-memo one-part-is-inside-p* (x1 y1 x2 y2 poly delta)
    ((x1 y1 x2 y2 poly (trafo-id *matrix*)))
  (let ((xm (float (/ (+ x1 x2) 2)))
        (ym (float (/ (+ y1 y2) 2))))
    ;; BUG IN LISPWORKS!!!!! WITH RATIOS IT DOESN'T WORK - WRONG VALUE ON STACK!!!!
    (or (inside-p* xm ym poly)
        (unless (< (+ (expt (- x1 x2) 2)
                      (expt (- y1 y2) 2))
                   delta)
          (or (one-part-is-inside-p* x1 y1 xm ym poly delta)
              (one-part-is-inside-p* xm ym x2 y2 poly delta))))))

;;;
;;; Hier zählt der Rand des Polygons *NICHT* als Äußeres!!! 
;;;

(defmethod-memo one-part-is-outside-p ((line geom-line) (poly geom-polygon))
    ((line poly (trafo-id *matrix*)))
  (or (and (not (lies-on-p (p1 line) poly))
           (not (inside-p (p1 line) poly)))
      (and (not (lies-on-p (p2 line) poly))
           (not (inside-p (p2 line) poly)))
      (one-part-is-outside-p* (x (p1 line)) (y (p1 line)) 
                              (x (p2 line)) (y (p2 line))
                              poly
                              (/ (+ (expt (- (x (p1 line)) (x (p2 line))) 2)
                                    (expt (- (y (p1 line)) (y (p2 line))) 2))
                                 +granularity+))))

(defun-memo one-part-is-outside-p* (x1 y1 x2 y2 poly delta)
    ((x1 y1 x2 y2 poly (trafo-id *matrix*)))
  (let ((xm (/ (+ x1 x2) 2))
        (ym (/ (+ y1 y2) 2)))
    (or (and (not (inside-p* xm ym poly))
             (not (lies-on-p* xm ym poly)))
        (unless (< (+ (expt (- x1 x2) 2)
                      (expt (- y1 y2) 2))
                   delta)
          (or (one-part-is-outside-p* x1 y1 xm ym poly delta)
              (one-part-is-outside-p* xm ym x2 y2 poly delta))))))


;;;
;;; Equal-Relation
;;;


(defmethod equal-p ((obj1 geom-thing) (obj2 geom-thing))
  nil)

(defmethod equal-p ((obj1 geom-point) (obj2 geom-point))
  (point-=-p obj1 obj2))

(defmethod equal-p ((obj1 geom-line) (obj2 geom-line))
  (line-=-p obj1 obj2))

(defmethod-memo equal-for-complex-objects-p ((obj1 geom-thing) (obj2 geom-thing) (access-fn function))    
    ((obj1 obj2 (trafo-id *matrix*)))
  (and (= (length (funcall access-fn obj1))
	  (length (funcall access-fn obj2)))
       (every #'(lambda (i)
		  (= 1 
		     (count-if #'(lambda (j)
				   (equal-p i j))
			       (funcall access-fn obj2))))
	      (funcall access-fn obj1))))


(defmethod-memo equal-p ((obj1 geom-chain-or-polygon) (obj2 geom-chain-or-polygon))
    ((obj1 obj2 (trafo-id *matrix*)))
  (equal-for-complex-objects-p obj1 obj2 #'segments))


(defmethod-memo equal-p ((obj1 geom-aggregate) (obj2 geom-aggregate))
    ((obj1 obj2 (trafo-id *matrix*)))
  (equal-for-complex-objects-p obj1 obj2 #'has-parts))


;;;
;;;
;;;

(defmethod get-segments-between ((chain-or-poly geom-chain-or-polygon) (s1 geom-line) (s2 geom-line))
  ;;; bei einer Kette gibt's nur eine Richtung 
  ;;; beim Polygon gibt es zwei!!! 
  ;;; in dem Fall wird von s1 nach s2 gegangen   
  (labels ((do-it (chain-or-poly s1 s2)
             (let ((current s1)
                   (list (list s1)))
               (if (line-=-p s1 s2)
                   list
                 (loop 
                  (let ((next 
                         (find-if #'(lambda (x) 
                                      (and 
                                       (joins-p current x)
                                       (not (member x list :test #'line-=-p))))
                                  (segments chain-or-poly))))
                    (unless next
                      (return nil))
                    (push next list)
                    (setf current next)
                    (when (line-=-p next s2) 
                      (return list))))))))
    (or (do-it chain-or-poly s1 s2)
        (do-it chain-or-poly s2 s1))))



(defmethod get-points-between ((chain-or-poly geom-chain-or-polygon) (p1 geom-point) (p2 geom-point))
  (let* ((s1 (find-if #'(lambda (segment)
                          (lies-on-p p1 segment))
                      (segments chain-or-poly)))         
         (s2 (find-if #'(lambda (segment)
                          (lies-on-p p2 segment))
                      (segments chain-or-poly))))
    (when (and s1 s2)
      (let* ((segments (get-segments-between chain-or-poly s1 s2))
             (point-list (mapcar #'point-list segments))
             (points (apply #'append 
                            (mapcar #'(lambda (x y)
                                        (intersection x y :test #'point-=-p))
                                    point-list (cdr point-list)))))
        (remove-if #'(lambda (x) 
                       (or (point-=-p x p1)
                           (point-=-p x p2)))
                   points)))))

                         
        
                     
