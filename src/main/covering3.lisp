;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TANGRAM -*-

(in-package :tangram)

(defun compactness (poly)
  (with-no-matrix-at-all 
    (/ 
     (loop as s in (segments poly) 
           sum (length-of-line s))
     (sqrt (tangram-geometry::calculate-area poly)))))

(defun polygon-difference (x y)	;;; "x minus y"
  (if (not (eq (calculate-rcc-relation x y) :tppi))
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

(defun find-possible-alignments (a b &key how-many frame color)
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
				   :filled-p nil)
                    #+:ignore
                    (sleep 1) )
		  
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

                        (push (list aa bb color) *x*)))	

                    (case rel
                      (:tppi 
                       (push (list (polygon-difference a b)
                                   (poly-from-xy-list 
                                    (append (mapcar #'get-xy-list 
                                                    (mapcar #'p1 (segments b)))
                                            (list (get-xy-list (p2 (first (last (segments b)))))))
                                    :affected-by-matrix-p nil))
			     
                             res)
		       (when (and frame (show-thinking-p frame))
			 (show-polygons (list b)
					:clear-p nil
					:inks (list +flipping-ink+)
					:filled-p nil))
                       (when (and how-many (> (length res) how-many))
                         (return-from enumerate)))

                      (:eq 
		       (when (and frame (show-thinking-p frame))
			 (show-polygons (list b)
					:clear-p nil
					:inks (list +flipping-ink+)
					:filled-p nil))

		       (return-from find-possible-alignments
                         (prog1 
                             (list (list :match 
                                         (poly-from-xy-list 
                                          (append
                                           (mapcar #'get-xy-list 
                                                   (mapcar #'p1 (segments b)))
                                           (list (get-xy-list (p2 (first (last (segments b)))))))
                                          :affected-by-matrix-p nil)))

                           (setf (affected-by-matrix-p a) abma)
                           (setf (affected-by-matrix-p b) abmb))))
                                            
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
                           :key #'first 
                           :test #'(lambda (x y)
                                     (set-equal x y 
                                                :test #'congruent-p)))
      
      res)))


#|
(defun find-covering (polygon-set tile-types &key (remove-used-tiles-p t)
                                  frame 
                                  all-solutions-p                                  
                                  allow-less-tiles-p                      
                                  show-thinking-fn)

  ;; these are lists of equivalent tiles! 
  #+:ignore
  (when (some #'cdr tile-types)
    (error "Bad tiles!"))
      

  (let* ((tile-types (apply #'append tile-types))
         (solutions nil)
         (orig-tile-types (copy-list tile-types)))

    (labels ((goal-p (polygons rem-tiles) 
               (and (not polygons)
                    (or allow-less-tiles-p
                        (not rem-tiles))))

             (do-it (polygons rem-tiles history)
                 
               (cond ((goal-p polygons rem-tiles) 

                      (push (list polygons rem-tiles history) solutions)

                      (when show-thinking-fn (funcall show-thinking-fn polygons rem-tiles history t))

                      (unless all-solutions-p 
                        (return-from do-it nil)))

                     (t 
			
                      (when show-thinking-fn (funcall show-thinking-fn polygons rem-tiles history nil))

                      (let* (;; first, determine next polygon per heuristics: 
                             (polygons (sort polygons
                                             #'>
                                             :key #'(lambda (polygon)
                                                      (polygon-selection-heuristic polygon))))
                             (res nil))
			  
                        (dolist (polygon polygons)

                          ;; next, determine order of sequence in which tiles from the type (= set of tiles) are tried 
                          (let* ((tiles
                                  (sort rem-tiles 
                                        #'>
                                        :key #'(lambda (tile)
                                                 (tile-selection-heuristic-for-polygons tile polygon))))
                                 
                                 (res nil))
                            
                            (dolist (tile tiles)
                                
                              (when (<= (tangram-geometry::calculate-area (shape tile))
                                        (tangram-geometry::calculate-area polygon))
                                
                                (let* ((possible-alignments
                                        (find-possible-alignments polygon (shape tile)
                                                                  :frame frame :color (color tile))))
                                  
                                  (push (list possible-alignments polygon tile) res))))))
                        
                        
                        (setf res (sort res #'> :key #'alignment-score))
                        
                        (loop as (possible-alignments polygon tile) in res do

                              (let ((sorted
                                     ;; next, determine which of the possible alignments to pursue next
                                     (sort possible-alignments 
                                           #'> 
                                           :key #'(lambda (conf)
                                                    (alignment-result-heuristic-for-polygon conf polygon))))
                                    (rem-polygons (remove polygon polygons)))

                                (dolist (possible-alignment sorted)
                                  (let ((polygon-set (first possible-alignment))
                                        (aligned-polygon (second possible-alignment)))
                                              
                                    (when polygon-set                                      
                                      (do-it (if (eq polygon-set :match)
                                                 rem-polygons 
                                               (append rem-polygons polygon-set))
                                             (if remove-used-tiles-p 
                                                 (remove tile rem-tiles)
                                               rem-tiles)
                                             (cons (list aligned-polygon tile) history))))))))))))
      
      (do-it polygon-set tile-types nil)
      
      solutions)))

;;;
;;;
;;;

(defun polygon-selection-heuristic (polygon)
  ;; give precedence to bigger polygons 
  (tangram-geometry::calculate-area polygon))


(defun alignment-score (alignment)
  ;; determine which successor conf to determine next
  (let ((align (first alignment))
        (rem-polygons (second alignment))
        (tile (third alignment)))
    (let ((polygon-set (first align))
          (aligned-polygon (second align)))
      (if (eq polygon-set :match)
          1000
        (- 999 (length rem-polygons))))))


(defun alignment-result-heuristic-for-polygon (conf orig-polygon)
  ;; determine which successor conf to determine next
  (let* ((remaining-polygon-set (first conf))         
         (aligned-polygon (second conf))
         (rem-area (reduce #'+ (mapcar #'tangram-geometry::calculate-area remaining-polygon-set))))
    #+:ignore
    (apply #'+
           (mapcar #'compactness
                   remaining-polygon-set))
    rem-area))


(defun tile-selection-heuristic-for-polygon (tile polygon)
  ;; biggest tiles first 
  (- 10 (position (name tile)
                  '(a b c d e f g))))

|# 

#|
(defun tile-selection-heuristic-for-polygon (tile polygon)
  ;; biggest tiles first 
  (- (tangram-geometry::calculate-area polygon)
     (tangram-geometry::calculate-area (shape tile))))
|# 

;;;
;;;
;;;


(defun history (n)
  (show-polygons (nth n *x*) :inks (list (first +gray-colors+)
					 (third (nth n *x*)))))

;;;
;;;
;;;


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

             (bad-configuration-p (polygons tile-types)
               nil)

             (do-it (polygons tile-types history)
                 
               (cond ((goal-p polygons tile-types) 

                      (push (list polygons tile-types history) solutions)

                      (when show-thinking-fn (funcall show-thinking-fn polygons tile-types history t))

                      (unless all-solutions-p 
                        (return-from do-it nil)))

                     ((bad-configuration-p polygons tile-types)
                      nil)
		       
                     (t 
			
                      (when show-thinking-fn (funcall show-thinking-fn polygons tile-types history nil))

                      #+:ignore 
                      (sleep 1) 

                      (let* ((orig-types (copy-list tile-types))

                             ;; first, determine next polygon per heuristics: 
                             #+:ignore
                             (polygons (sort polygons
                                             #'<
                                             :key #'(lambda (polygon)
                                                      (polygon-selection-heuristic polygon))))
                             ;; determine tile type (list of list of congruent tiles -> (first tile) 
                             #+:ignore
                             (tile-types 
                              (sort tile-types 
                                    #'>
                                    :key #'(lambda (tile)
                                             (tile-selection-heuristic-for-polygons (first tile) polygons))))

                             )
			  
                        ;;(princ tile-types) (break "~S" tile-types)
                        (loop while tile-types do
                              (dolist (polygon polygons)
                                (let* ((tile-type (first tile-types))) ; only try one congruent tile from each class (list of list of congruent tiles) 
                                  (dolist (tile tile-type)

                                    (when t
                                      ;; this DOES not work, then certain problem cannot be found!
                                      ;; for example, 45 degree rotated yellow 
                                      #+:ignore
                                      (<=  (tangram-geometry::calculate-area (shape tile))
                                           (tangram-geometry::calculate-area polygon))

                                      (let* ((possible-alignments
                                              (find-possible-alignments polygon (shape tile)
                                                                        :frame frame :color (color tile)))
                                             (rem-polygons (remove polygon polygons))
                                             (sorted possible-alignments)
                                             
                                             #+:ignore
                                             (sorted
                                              ;; next, determine which of the possible alignments to pursue next
                                              (sort possible-alignments 
                                                    #'> 
                                                    :key #'(lambda (conf)
                                                             (alignment-result-heuristic-for-polygon conf polygon)))))
                                          
                                        (dolist (possible-alignment sorted)

                                          (let ((polygon-set (first possible-alignment))
                                                (aligned-polygon (second possible-alignment)))
                                              
                                            (when polygon-set                                      
                                              (do-it (if (eq polygon-set :match)
                                                         rem-polygons 
                                                       (append rem-polygons polygon-set))
                                                     (if remove-used-tiles-p 
                                                         (remove tile-type orig-types)
                                                       orig-types)
                                                     (cons (list aligned-polygon tile) history))))))))))
                                  
                              (setf tile-types (rest tile-types))))))))
      
      (do-it polygon-set tile-types nil)
      
      solutions)))


;;;
;;;
;;;

(defun polygon-selection-heuristic (polygon)
  ;; give precedence to bigger polygons 
  (- (tangram-geometry::calculate-area polygon)))


(defun alignment-result-heuristic-for-polygon (conf orig-polygon)
  ;; determine which successor conf to determine next
  (let* ((remaining-polygon-set (first conf))         
         (aligned-polygon (second conf))
         (rem-area (reduce #'+ (mapcar #'tangram-geometry::calculate-area remaining-polygon-set))))
    (- (apply #'+
           (mapcar #'compactness
                   remaining-polygon-set)))
    ; rem-area
    ))


(defun tile-selection-heuristic-for-polygons (tile polygons)
  ;; biggest tiles first 
  (- 10 (position (name tile)
                  '(a b c d e f g))))


#|
(defun tile-selection-heuristic-for-polygon (tile polygon)
  ;; biggest tiles first 
  (- (tangram-geometry::calculate-area polygon)
     (tangram-geometry::calculate-area (shape tile))))
|# 
