;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TANGRAM -*-

(in-package :tangram)

(defun compactness (poly)
  (with-no-matrix-at-all 
    (/ 
     (loop as s in (segments poly) 
           sum (length-of-line s))
     (sqrt (tangram-geometry::calculate-area poly)))))

(defun polygon-difference (x y)	;;; "x minus y"
  (if (not (member (calculate-rcc-relation x y) '(:tppi :ec)))
      (error "Bad polygons!")
    
    (let* ((points-of-y-on-x 
            (remove-if-not #'(lambda (point-of-y)
                               (lies-on-p point-of-y x))
                           (point-list y)))
           (points-of-x-on-y
            (remove-if-not #'(lambda (point-of-x)
                               (lies-on-p point-of-x y))
                           (point-list x)))
	   
           (x 
            (insert-new-border-points x points-of-y-on-x))
           
           (y
            (insert-new-border-points y points-of-x-on-y))

           
           (good-y-segments 
            (remove-if #'(lambda (y-seg) 
                           (some #'(lambda (x-seg)
                                     (line-=-p x-seg y-seg))
                                 (segments x)))
                       (segments y)))
           
           (good-x-segments 
            (remove-if #'(lambda (y-seg) 
                           (some #'(lambda (x-seg)
                                     (line-=-p x-seg y-seg))
                                 (segments y)))
                       (segments x)))

           (segments (append good-x-segments good-y-segments)))

      
      (let ((ox (decode-ccw (orientation x)))
            (oy (decode-ccw (orientation y))))

        (when (eq ox :clockwise)
          (orientate-counterclockwise x))
	
        (when (eq oy :counterclockwise)
          (orientate-clockwise y))
        
        (prog1
            (mapcar #'(lambda (component)
                        (poly-from-xy-list
                         (mapcar #'get-xy-list
                                 (append (mapcar #'p1 component)
                                         (list (p2 (first (last component))))))
			 :normalize-p nil
                         :affected-by-matrix-p nil))
                    (find-components segments))
          
          (when (eq ox :clockwise)
            (orientate-clockwise x))
          
          (when (eq oy :counterclockwise)
            (orientate-counterclockwise y)))))))




(defun polygon-union (x y) ;;; "x vereinigt y"
  (if (not (or (eq (calculate-rcc-relation x y) :ec)
               (eq (calculate-rcc-relation x y) :dc)
               (eq (calculate-rcc-relation x y) :po)))
      (error "Bad polygons!")

    
    (let* ((points-of-y-on-x 
            (remove-if-not #'(lambda (point-of-y)
                               (lies-on-p point-of-y x))
                           (point-list y)))
           (points-of-x-on-y
            (remove-if-not #'(lambda (point-of-x)
                               (lies-on-p point-of-x y))
                           (point-list x)))

           (new-intersection-points
            (let ((res nil))
              (dolist (sa (segments x))
                (dolist (sb (segments y))
                  (unless (joins-p sa sb)
                    (multiple-value-bind (xc yc)
                        (calculate-intersection-point sa sb)
                      (when xc
                        (push (list xc yc) res))))))
              res))
	   
           (x 
            (insert-new-border-points x (nconc points-of-y-on-x new-intersection-points)))
           
           (y
            (insert-new-border-points y (nconc points-of-x-on-y new-intersection-points)))
           
           (good-y-segments 
            (remove-if #'(lambda (y-seg) 
                           (some #'(lambda (x-seg)
                                     (line-=-p x-seg y-seg))
                                 (segments x)))
                       (segments y)))
           
           (good-x-segments 
            (remove-if #'(lambda (y-seg) 
                           (some #'(lambda (x-seg)
                                     (line-=-p x-seg y-seg))
                                 (segments y)))
                       (segments x)))

           (segments (append good-x-segments good-y-segments)))

      (let ((ox (decode-ccw (orientation x)))
            (oy (decode-ccw (orientation y))))

        (when (eq ox :clockwise)
          (orientate-counterclockwise x))
	
        (when (eq oy :clockwise)
          (orientate-counterclockwise y))
        
        (prog1
            (mapcar #'(lambda (component)
                        (poly-from-xy-list
                         (mapcar #'get-xy-list
                                 (append (mapcar #'p1 component)
                                         (list (p2 (first (last component))))))
			 :normalize-p nil
                         :affected-by-matrix-p nil))
                    (let ((res (find-components segments)))
                      (if res 
                          res
                        (list (segments x) (segments y)))))
	  
          
          (when (eq ox :clockwise)
            (orientate-clockwise x))
          
          (when (eq oy :clockwise)
            (orientate-clockwise y)))))))


(defun polygon-set-union (set-of-polygons)
  (let ((change t)
        (union set-of-polygons))
    (loop while change do
          (setf change nil)
          (let ((pair nil))
            (find-if #'(lambda (a)
                         (find-if #'(lambda (b) 
                                      (when (and 
                                             (not (eq a b))
                                             (some #'(lambda (a)
                                                       (some #'(lambda (b)
                                                                 (1d-intersects-p a b))
                                                             (segments b)))
                                                   (segments a))
                                             (member (calculate-rcc-relation a b) '(:ec)))
					
                                        (setf pair (list a b))))
                                  union))
                     union)
            (when pair
              (setf change t)
              (setf union
                    (delete (first pair) union))
              (setf union 
                    (delete (second pair) union))
              (setf union
                    (append union 
                            (polygon-union (first pair) 
                                           (second pair)))))))

    union))   


(defun find-components (segments)
  (let ((components nil))
    (labels ((do-it (cur-component segments)             
               
               (let* ((current-segment (first cur-component))
                      (succ-candidates
                       (remove-if-not #'(lambda (cand)
                                          (point-=-p (p1 cand)
                                                     (p2 current-segment)))
                                      segments)))
                 
                 (when (and                         
                        (segment-list-ok-p cur-component t t)
                        (some #'(lambda (x) 
                                  (not (equal (part-of x)
                                              (part-of current-segment))))
                              cur-component))
                   
                   (push (reverse cur-component) components)
                   
                   (when segments
                     (do-it (list (first segments)) (rest segments))))

                 (dolist (succ-candidate succ-candidates) ; jeder Gabelung folgen!
                   (do-it (cons succ-candidate cur-component)
                          (remove succ-candidate segments))))))
      
      (do-it (list (first segments))
             (rest segments))

      components)))


(defun angle-between (sa sb)
  (let* ((x1 (x (p1 sa)))
         (y1 (y (p1 sa)))
         (x2 (x (p2 sa)))
         (y2 (y (p2 sa)))
         (phi-a (angle-between* x1 y1 x2 y2)))
    (let* ((x1 (x (p1 sb)))
           (y1 (y (p1 sb)))
           (x2 (x (p2 sb)))
           (y2 (y (p2 sb)))
           (phi-b (angle-between* x1 y1 x2 y2)))

      
      (mod (- phi-b phi-a) (* 2 pi)))))

;;;
;;;
;;;

(defparameter *remove-congruent-configurations-p* t)

(defparameter *debug-p* nil)

(defvar *x* nil)

;;;
;;;
;;;

(defun find-possible-alignments (a b &key  how-many frame color score-p rem-tile-types)
  ;;; versuche "b" in "a" unterzubringen! 

  (let ((res nil)
        (abma (affected-by-matrix-p a))
        (abmb (affected-by-matrix-p b)))

    (orientate-counterclockwise a)
    (orientate-counterclockwise b)

    (block enumerate
      
      (dolist (sa (segments a))	
        (dolist (sb (segments b))
	  
          (let (phi 
                xf yf
                xt yt)
	    
            (setf phi (angle-between sb sa)
		  
                  xf (x (p1 sb))
                  yf (y (p1 sb))
		  
                  xt (x (p1 sa))
                  yt (y (p1 sa)))
	    
            ;;; Drehwinkel fuer "b"
            ;;; Drehpunkt: "p1"

            ;;; Die Transformation bezieht sich nur auf b! 

            (setf (affected-by-matrix-p a) nil)
            (setf (affected-by-matrix-p b) t)
	    
            (with-translation ( (- xf) (- yf) )                    	      
              (with-rotation ( phi )                       		
                (with-translation ( xt yt )

		  (when (and frame (show-thinking-p frame))
		    (show-polygons (list b)
				   :clear-p nil
				   :inks (list +flipping-ink+)
				   :filled-p nil))

                  (let* ((rel (calculate-rcc-relation a b)))
                
                    (when *debug-p* 
                                                          
                      (let ((aa (poly-from-xy-list 
                                        (append (mapcar #'get-xy-list 
                                                        (mapcar #'p1 (segments a)))
                                                (list (get-xy-list (p2 (first (last (segments a)))))))
                                        :affected-by-matrix-p nil))
			    
                            (bb (poly-from-xy-list 
                                        (append (mapcar #'get-xy-list 
                                                        (mapcar #'p1 (segments b)))
                                                (list (get-xy-list (p2 (first (last (segments b)))))))
                                        :affected-by-matrix-p nil)))
                        
                        (setf *x* (list rel aa bb color))))

                    (case rel

                      ((:tppi)

                       (let* ((succ-conf (polygon-difference a b))
                              (aligned-poly (poly-from-xy-list 
                                             (append (mapcar #'get-xy-list 
                                                             (mapcar #'p1 (segments b)))
                                                     (list (get-xy-list (p2 (first (last (segments b)))))))
                                             :affected-by-matrix-p nil))
                              (h (if score-p 
                                     (alignment-result-heuristic-for-polygon 
                                      aligned-poly 
                                      succ-conf
                                      a
                                      rem-tile-types)
                                   0)))

                         (unless (eq h :bad)
                           (push (list h
                                       aligned-poly 
                                       succ-conf
                                       a
                                       rem-tile-types)
                                 res))
                       
                         (when *debug-p* 
                           (let ((x
                                  (count-if #'(lambda (p) 
                                                (inside-p p a))
                                            (point-list b))))                             
                             (when (zerop x)
                               (show-polygons (append succ-conf (list aligned-poly))
                                              :clear-p t
                                              :inks (append (loop as x in succ-conf collect +blue+) (list +yellow+))
                                              :filled-p t)
                               (sleep 0.3)))))
                                                  
                       (when (and frame (show-thinking-p frame))
                         (show-polygons (list b)
					:clear-p nil
					:inks (list +flipping-ink+)
					:filled-p nil))
                       
                       (when (and how-many (> (length res) how-many))
                         (return-from enumerate)))

                      (:eq 
                       
                       (let* ((succ-conf :match)
                              (aligned-poly (poly-from-xy-list 
                                             (append (mapcar #'get-xy-list 
                                                             (mapcar #'p1 (segments b)))
                                                     (list (get-xy-list (p2 (first (last (segments b)))))))
                                             :affected-by-matrix-p nil))
                              (h (if score-p 
                                     (alignment-result-heuristic-for-polygon 
                                      aligned-poly 
                                      succ-conf
                                      a
                                      rem-tile-types)
                                   0)))

		       (when (and frame (show-thinking-p frame))
			 (show-polygons (list b)
					:clear-p nil
					:inks (list +flipping-ink+)
					:filled-p nil))

		       (return-from find-possible-alignments
                         (prog1 
                             (list 
                              (list h 
                                    aligned-poly 
                                    succ-conf
                                    a
                                    rem-tile-types))
                           (setf (affected-by-matrix-p a) abma)
                           (setf (affected-by-matrix-p b) abmb)))))
                                            
		      (otherwise 
		       (when (and frame (show-thinking-p frame))
			 (show-polygons (list b)
					:clear-p nil
					:inks (list +flipping-ink+)
					:filled-p nil))))))))

            (setf (affected-by-matrix-p a) abma)
            (setf (affected-by-matrix-p b) abmb)))))

    (if *remove-congruent-configurations-p*         
        (remove-duplicates res
                           :key #'third
                           :test #'(lambda (x y)
                                     (set-equal x y 
                                                :test #'congruent-p)))
      
      res)))


;;;
;;;
;;;

(defun history (n)
  (show-polygons (nth n *x*) :inks (list (first +gray-colors+)
					 (third (nth n *x*)))))

;;;
;;;
;;;

(defun tuple-> (a b) 
  (when (and a b)
    (cond ((> (first a) (first b))
           t)
          ((= (first a) (first b))
           (tuple-> (rest a) (rest b)))
          (t nil))))

(defun find-covering (polygon-set tile-types &key (remove-used-tiles-p t)
                                  frame 
                                  all-solutions-p                                  
                                  allow-less-tiles-p                      
                                  show-thinking-fn)

  (let ((solutions nil))

    (labels ((goal-p (polygons tile-types) 
               (and (not polygons)
                    (or allow-less-tiles-p
                        (not tile-types))))

             (do-it (polygons tile-types history)
                 
               (cond ((goal-p polygons tile-types) 

                      (push (list polygons tile-types history) solutions)

                      (when show-thinking-fn (funcall show-thinking-fn polygons tile-types history t))

                      (unless all-solutions-p 
                        (return-from do-it nil)))
		       
                     (t 
			
                      (when show-thinking-fn (funcall show-thinking-fn polygons tile-types history nil))

                      #+:ignore 
                      (sleep 1) 

                      (let* ((alignments nil)
                             (perfect-alignments nil))
			                          
                        (dolist (polygon polygons)
                          (dolist (tile-type tile-types) ; tile-type is the set of "mirrored" versions of that tile! defined by means of the "mirror values" for the tile 
                            (let ((rem-tile-types 
                                   (if remove-used-tiles-p 
                                       (remove tile-type tile-types)
                                     tile-types)))
                              (dolist (tile tile-type)
                                (dolist (succ-conf 
                                         (find-possible-alignments polygon (shape tile)
                                                                       :frame frame 
                                                                       :color (color tile)
                                                                       :score-p t
                                                                       :rem-tile-types rem-tile-types))
                                  ;; succ-conf = (list h aligned-poly succ-conf orig-poly rem-tile-types)
                                  (if (eq (third succ-conf) :match)
                                      (push (list succ-conf tile ) perfect-alignments)
                                    (push (list succ-conf tile ) alignments)))))))
                        
                        (let ((sorted (append (sort perfect-alignments #'tuple-> :key #'caar)
                                              (subseq (sort alignments #'tuple-> :key #'caar) 
                                                      ;; best n only ? 
                                                      0 
                                                      ;; (min 4 (length alignments))
                                                      (length alignments)
                                                      ))))
                          
                          (loop as ((h aligned-poly succ-conf orig-poly rem-tile-types) tile) in sorted do        
                                (let* ((rem-polygons (remove orig-poly polygons)))
                                  (do-it (if (eq succ-conf :match)
                                             rem-polygons 
                                           (append succ-conf rem-polygons   ))
                                         rem-tile-types
                                         (cons (list aligned-poly tile) history))))))))))
      
      (do-it polygon-set tile-types nil)
      
      solutions)))


;;;
;;;
;;;

(defun alignment-result-heuristic-for-polygon (aligned-polygon conf orig-polygon rem-tile-types)
  (cond ((symbolp conf) ; :match! 
        
         (list (tangram-geometry::calculate-area aligned-polygon) 0))

        ((some #'(lambda (poly) 
                   (every #'(lambda (tile-type)                                 
                              (let ((tile (first tile-type))) ; use first reflected variant 
                                (< (+ (tangram-geometry::calculate-area poly) 2) ;; account for inaccuracies... hack 
                                   (tangram-geometry::calculate-area (shape tile)))))
                          rem-tile-types))
               conf)
         :bad )

        (t 

         ;; score list for lexicographic tuple->-p sorting 

         (list              
          (tangram-geometry::calculate-area aligned-polygon)                
          (- (count-if #'(lambda (p) 
                           (inside-p p orig-polygon))
                       (point-list aligned-polygon)))
          (apply #'min (mapcar #'(lambda (x) (- (compactness x))) conf))
          (- (reduce #'+ (mapcar #'length (mapcar #'segments conf))))))))