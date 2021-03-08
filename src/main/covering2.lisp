;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TANGRAM -*-

(in-package :tangram)

(defun polygon-difference (x y) ;;; "x minus y"
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

(defparameter *remove-congruent-configurations-p* nil)

(defparameter *debug-p* nil)

(defvar *x* nil)

;;;
;;;
;;;

(defun find-possible-alignments (a b &key how-many)
  ;;; versuche "b" in "a" unterzubringen! 
  
  (let ((res nil))

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
                  
                    
            (with-translation ( (- xf) (- yf) )                    
        
              (with-rotation ( phi )                       
                        
                (with-translation ( xt yt )

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


                        (setf +gray-colors+ (list +green+ +blue+))
                        (show-polygons (list a b) :filled-p nil)
                              
                              
                        (setf *x* (list aa bb))
                              
                        (notify-user nil 
                                     (format nil "A-B: ~A Congruent? ~A Intersects? ~A Covered? ~A ~A"
                                                   
                                             rel
                                             (congruent-p a b)
                                             (intersects-p a b)
                                             (covered-by-p a b)
                                             (covers-p a b)
                                                   
                                             ))
                              
                        (notify-user nil 
                                     (format nil "AA-BB: ~A Congruent? ~A Intersects? ~A Covered? ~A ~A"
                                                   
                                             (with-no-matrix-at-all
                                               (calculate-rcc-relation aa bb))
                                             (with-no-matrix-at-all
                                               (congruent-p aa bb))
                                             (with-no-matrix-at-all
                                               (intersects-p aa bb))
                                             (with-no-matrix-at-all
                                               (covered-by-p aa bb))
                                             (with-no-matrix-at-all
                                               (covers-p aa bb))
                                                   
                                             ))))


                    (case rel
                      (:tppi 
                       (push (list (polygon-difference a b)
                                   (poly-from-xy-list 
                                    (append (mapcar #'get-xy-list 
                                                    (mapcar #'p1 (segments b)))
                                            (list (get-xy-list (p2 (first (last (segments b)))))))))
                               
                             res)
                       (when (and how-many (> (length res) how-many))
                         (return-from enumerate)))
                      (:eq (return-from find-possible-alignments
                             (list (list :match 
                                         (poly-from-xy-list 
                                          (append
                                           (mapcar #'get-xy-list 
                                                   (mapcar #'p1 (segments b)))
                                           (list (get-xy-list (p2 (first (last (segments b)))))))))))))))))))))
    
    (if *remove-congruent-configurations-p* 
        (remove-duplicates res
                           :key #'first 
                           :test #'(lambda (x y)
                                     (set-equal x y 
                                                :test #'congruent-p)))
      res)))
  



(defun find-covering (polygon-set tile-types &key (remove-used-tiles-p t)
                                  all-solutions-p                                  
                                  allow-less-tiles-p                      
                                  show-thinking-fn)
  (let ((solutions nil))
    (labels ((do-it (configurations)
               (when configurations 
                 (let ((goals
                        (remove-if-not #'(lambda (x) 
                                           (goal-p x :allow-less-tiles-p allow-less-tiles-p))
                                       configurations)))
                   
                   (if goals
                       (progn
                         (push goals solutions)
                         (dolist (goal goals)
                           (when show-thinking-fn (funcall show-thinking-fn goal t)))
                         (if all-solutions-p 
                             (do-it (set-difference configurations goals))
                           (return-from do-it nil)))))
                 
                 (let* ((configuration (select-configuration configurations))
                        
                        (succs (create-successor-configurations
                                configuration 
                                :remove-used-tiles-p remove-used-tiles-p))
                        
                        (configurations 
                         (remove-if #'bad-configuration-p 
                                    (combine-configurations (remove configuration 
                                                                    configurations)
                                                            succs))))
                   
                   (when show-thinking-fn (funcall show-thinking-fn configuration nil))
                    
                   (do-it configurations)))))
    
      (dolist (tile-type tile-types)
        (dolist (tile tile-type)
          (setf (affected-by-matrix-p (shape tile)) t)))
      (dolist (polygon polygon-set)
        (setf (affected-by-matrix-p polygon) nil))

      (do-it (list (list polygon-set tile-types nil)))

      solutions)))

 
(defun goal-p (conf &key allow-less-tiles-p)
  (let ((polygons (first conf))
        (tiles (second conf)))
    (and (not polygons)
         (=> (not allow-less-tiles-p)
             (not tiles)))))

 
;;;
;;;
;;;

(defun create-successor-configurations (conf &key (remove-used-tiles-p t))
  (let ((polygons (first conf))
        (tile-types (second conf))
        (history (third conf))

        (new-confs nil))
    (dolist (tile-type tile-types) ; mirrored instances! 
      (dolist (tile tile-type)
        (let ((rem-tiles (remove tile-type tile-types)))
          (dolist (polygon polygons)
            
            (let* ((possible-alignments
                    (find-possible-alignments polygon (shape tile)))
                   (rem-polygons (remove polygon polygons)))
            
              (dolist (possible-alignment possible-alignments)
                (let ((polygon-set (first possible-alignment))
                      (aligned-polygon (second possible-alignment)))
                
                  (when polygon-set

                    (if (eq polygon-set :match)
                        (push (list rem-polygons 
                                    (if remove-used-tiles-p 
                                        rem-tiles
                                      tile-types)
                                    (cons (list aligned-polygon tile) history))
                              new-confs)
                      (push (list (append rem-polygons polygon-set) 
                                  (if remove-used-tiles-p 
                                      rem-tiles
                                    tile-types)
                                  (cons (list aligned-polygon tile) history))
                            new-confs))))))))))

    new-confs))


;;;
;;;
;;;

#|

(defun bad-configuration-p (conf)
  (let ((polygons (first conf))
        (tiles (second conf))
        (shortest nil)
        (longest nil))

    (dolist (tile tiles)
      (dolist (s (segments (shape tile)))
        (let ((l (length-of-line s)))
          (when (or (not shortest)
                    (< l shortest))
            (setf shortest l))
          (when (or (not longest)
                    (> l longest))
            (setf longest l)))))
    
    (some #'(lambda (poly) 
              (some #'(lambda (s)
                        (< (length-of-line s) shortest))
                    (segments poly)))
          polygons)))


(defun combine-configurations (old new-succs)
  (append old new-succs))


(defun select-configuration (confs)
  (let ((min nil)
        (found nil))
    (loop as conf in confs do
          (let* ((polys (first conf))
                 (rem-tiles (second conf))
                 (n (length rem-tiles))
                 (sum (apply #'+ (mapcar #'geometry::calculate-area 
                                         (mapcar #'shape rem-tiles))))
                 (size 
                  (* sum 
                     (apply #'+ 
                            (mapcar #'(lambda (x)
                                        (/ 
                                         (loop as s in (segments x) 
                                               sum (length-of-line s))
                                         (sqrt (geometry::calculate-area x))))
                                    polys)))))
            (when (or (not min)
                      (< size min))
              (setf min size
                    found conf))))
    found))

|# 



(defun bad-configuration-p (conf)
  nil)

(defun combine-configurations (old new-succs)
  (append new-succs old))

(defun select-configuration (confs)
  (first confs))
