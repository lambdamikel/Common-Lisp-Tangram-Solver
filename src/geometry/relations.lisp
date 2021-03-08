;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: TANGRAM-GEOMETRY; Base: 10 -*-

(in-package tangram-geometry)


(defgeneric calculate-relation (obj1 obj2 &rest rest)
  (:documentation "Recognizes various spatial relationships between obj1 and obj2."))

;;;
;;;
;;;

(defun inverse (rel)
  (list 'inv rel))

;;;
;;;


(defmethod calculate-relation ((p1 geom-point) (p2 geom-point) &key (lod 0)) 
  ;; LOD = "leve of detail" of the description
  (cond ((and (plusp lod) (eq p1 p2))
         `(intersects 
           ,@(when (plusp lod)
               `(0d 
                 ,@(when (plusp (1- lod))
                     `(equal 
                       ,@(when (plusp (- lod 2))
                           '(eq))))))))
        ((point-=-p p1 p2)
         `(intersects 
           ,@(when (plusp lod)
               `(0d
                 ,@(when (plusp (1- lod))
                     `(equal))))))
        (t
         '(disjoint))))

(defmethod calculate-relation ((p geom-point) (l geom-line) &key (lod 0))
  (cond ((lies-on-p p l)
         `(intersects 
           ,@(when (plusp lod)
               `(0d 
                 ,@(when (plusp (1- lod))
                     `(lies-on inside 
                       ,@(when (plusp (- lod 2))
                           (cond ((point-=-p (p1 l) p)
                                  `(is-endpoint
                                    ,@(when (plusp (- lod 3))
                                        `((point-p1-relation ,(calculate-relation p (p1 l)
                                                                                  :lod (- lod 4)))))))
                                ((point-=-p (p2 l) p)
                                 `(is-endpoint 
                                   ,@(when (plusp (- lod 3))
                                       `((point-p2-relation ,(calculate-relation p (p2 l)
                                                                  :lod (- lod 4)))))))))))))))
        (t 
         '(disjoint))))

(defmethod calculate-relation ((l geom-line) (p geom-point) &rest rest)
  (inverse
   (apply #'calculate-relation p l rest)))

(defmethod calculate-relation ((p geom-point) (poly geom-polygon) &key (lod 0))
  (cond ((lies-on-p p poly)
         `(intersects 
           ,@(when (plusp lod)
               `(0d 
                 ,@(when (plusp (1- lod))
                     `(lies-on lies-on-border
                       ,@(when (plusp (- lod 2))
                           (let ((res (remove-if-not #'(lambda (segment) 
                                                         (lies-on-p p segment))
                                                     (segments poly))))
                             `(,@(when (some #'(lambda (x)
                                                 (point-=-p x p))
                                             (point-list poly))
                                   '(is-vertex-of))
                               ,@(when (and res (plusp (- lod 3)))
                                   `(point-segment-relations
                                     ,@(mapcar #'(lambda (segment) 
                                                   (calculate-relation p segment 
                                                                       :lod (- lod 4)))
                                               res))))))))))))
        ((inside-p p poly)
         `(intersects 
           ,@(when (plusp lod)
               `(0d
                 ,@(when (plusp (1- lod))    
                     `(inside))))))
        (t '(disjoint outside))))

(defmethod calculate-relation ((poly geom-polygon) (p geom-point) &rest rest)
  (inverse
   (apply #'calculate-relation p poly rest)))


(defmethod calculate-relation ((p geom-point) (c geom-chain) &key (lod 0))
  (cond ((lies-on-p p c)
         `(intersects 
           ,@(when (plusp lod)
               `(0d 
                 ,@(when (plusp (1- lod))
                     `(lies-on 
                       ,@(when (plusp (- lod 2))
                           (when (some #'(lambda (x)
                                           (point-=-p x p))
                                       (point-list c))
                             '(is-vertex-of)))
                           
                       ,@(when (plusp (- lod 3))
                           (cond ((point-=-p (p1 c) p)
                                  `(is-endpoint 
                                    ,@(when (plusp (- lod 4))
                                        `((point-p1-relation ,(calculate-relation p (p1 c)
                                                                   :lod (- lod 5)))))))
                                ((point-=-p (p2 c) p)
                                 `(is-endpoint 
                                   ,@(when (plusp (- lod 3))
                                       `((point-p2-relation ,(calculate-relation p (p2 c)
                                                                  :lod (- lod 5)))))))))
                       
                       ,@(let ((res (remove-if-not #'(lambda (segment) 
                                                       (lies-on-p p segment))
                                                   (segments c))))
                              
                           (when (and res (plusp (- lod 4)))
                             `((point-segment-relations
                                ,@(mapcar #'(lambda (segment) 
                                              (calculate-relation p segment 
                                                                  :lod (- lod 5)))
                                          res)))))))))))
        (t '(disjoint))))

  

(defmethod calculate-relation ((l geom-chain) (p geom-point) &rest rest)
  (inverse
   (apply #'calculate-relation p l rest)))


;;;
;;;
;;;


(defmethod calculate-relation ((l1 geom-line) (l2 geom-line) &key (lod 0))
  (if (and (bounding-box-p l1)
	   (bounding-box-p l2)
	   (not (box-overlaps-box-p l1 l2)))
      '(disjoint)
    (let* ((l1p1 (p1 l1))
	   (l2p1 (p1 l2))
	   (l1p2 (p2 l1))
	   (l2p2 (p2 l2))
	   
	   (a  (ccw l1p1 l1p2 l2p1))
	   (b  (ccw l1p1 l1p2 l2p2))
	   (c  (ccw l2p1 l2p2 l1p1))
	   (d  (ccw l2p1 l2p2 l1p2)))
      
      (cond ((and (<= (* a b) 0.0) (<= (* c d) 0.0)) ; Schnitt liegt vor

             `(intersects
               ,@(when (plusp lod)             
	           (let* ((l1p1-l2 (lies-on-p (p1 l1) l2))
		          (l1p2-l2 (lies-on-p (p2 l1) l2))
		          (l2p1-l1 (lies-on-p (p1 l2) l1))
		          (l2p2-l1 (lies-on-p (p2 l2) l1))
		          (n (+ (if l1p1-l2 1 0)
			        (if l1p2-l2 1 0)
			        (if l2p1-l1 1 0)
			        (if l2p2-l1 1 0))))
                     
	             (let ((res
		            (ecase n
			      
                              (0 `(0d 
                                   ,@(when (plusp (1- lod))
                                       '(crosses))))	; X
                              
			      (1 `(0d 
                                   ,@(when (plusp (1- lod))
                                       '(touches))))	; T
			      
                              (3 `(1d
                                   ,@(when (plusp (1- lod))
                                       (if (and l1p1-l2 l1p2-l2)
				           '(covered-by)
                                         '(covers)))))

		              (4 `(1d 
                                   ,@(when (plusp (1- lod))
                                       `(equal
                                         ,@(when (and (eq l1 l2)
                                                      (plusp (- lod 2)))
                                             '(eq))))))
                        
			      (2 (cond ((joins-p l1 l2) 
                                        `(0d ,@(when (plusp (1- lod))
                                                 '(touches))))	; --, \/                                   
				       ((and l1p1-l2 l1p2-l2)
				        `(1d ,@(when (plusp (1- lod))
                                                 '(inside))))
				       ((and l2p1-l1 l2p2-l1)
				        `(1d ,@(when (plusp (1- lod))
                                                 '(contains))))
                                       (t 
                                        `(1d ,@(when (plusp (1- lod))
				                 '(overlaps)))))))))
                 
                       (if (plusp (- lod 2))
                           `(,@res (endpoint-line-relations ,(calculate-relation (p1 l1) l2 :lod (- lod 3))
                                                            ,(calculate-relation (p2 l1) l2 :lod (- lod 3))
                                                            ,(calculate-relation (p1 l2) l1 :lod (- lod 3))
                                                            ,(calculate-relation (p2 l2) l1 :lod (- lod 3))))
              
                         res))))))
            (t '(disjoint))))))



(defmethod calculate-relation ((l geom-line) (c geom-chain) &key (lod 0))
  (cond ((intersects-p l c)
         
         `(intersects 
           
           ,@(when (plusp lod)
               `( ,@(if (some #'(lambda (x) 
                                  (1d-intersects-p x l))
                              (segments c))
                        `(1d
                     
                          ,@(when (and (plusp (1- lod)))
                              
                              `( ,@(when (some #'(lambda (x) 
                                                   (line-=-p x l))
                                               (segments c))
                                     '(segment-of))
                                 
                                 ,@(when (lies-on-p l c)
                                     '(lies-on)))))
                      `(0d))))
           
                     
           ,@(when (plusp (- lod 2))
               `((endpoint-chain-relations ,(calculate-relation (p1 l) c :lod (- lod 3))
                                           ,(calculate-relation (p2 l) c :lod (- lod 3)))))

           ,@(let ((res (remove-if-not #'(lambda (segment) 
                                           (intersects-p l segment))
                                       (segments c))))
                              
               (when (and res (plusp (- lod 3)))
                 (list (cons 'line-segment-relations
                             (mapcar #'(lambda (segment) 
                                         (calculate-relation l segment 
                                                             :lod (- lod 4)))
                                     res)))))))
        (t '(disjoint))))

  

(defmethod calculate-relation ((c geom-chain) (l geom-line) &rest rest)
  (inverse
   (apply #'calculate-relation l c rest)))


(defmethod calculate-relation ((l geom-line) (p geom-polygon) &key (lod 0))
  (declare (ignore rest))
  (if (intersects-p l p)
      `(intersects         
        ,@(when (plusp lod)
            (cond ((inside-p l p)
                   `(inside 
                     ,@(when (plusp (1- lod))
                         `(1d))))
                  ((lies-on-p l p)
                   `(lies-on lies-on-border 
                     ,@(when (plusp (1- lod))
                         `(1d))))
                  ((covered-by-p l p)
                   `(covered-by
                     ,@(when (plusp (1- lod))
                         `(1d))))
                          
                  ((crosses-p l p)
                   `(crosses
                     ,@(when (plusp (1- lod))
                         `(1d))))
                  (t 
                   `(touches
                     ,@(when (plusp (1- lod))
                         `( ,@(if (some #'(lambda (segment)
                                            (1d-intersects-p l segment))
                                        (segments p))
                                  '(1d)
                                '(0d))))))))

        ,@(when (plusp (- lod 2))            
            `((endpoint-polygon-relations
               ,(calculate-relation (p1 l) p :lod (- lod 3))
               ,(calculate-relation (p2 l) p :lod (- lod 3)))))
        
        ,@(let ((res (remove-if-not #'(lambda (segment) 
                                        (intersects-p l segment))
                                    (segments p))))
            
            (when (and res (plusp (- lod 3)))
              `((line-segment-relations
                 ,@(mapcar #'(lambda (segment) 
                               (calculate-relation l segment 
                                                   :lod (- lod 4)))
                           res))))))
    '(disjoint)))


(defmethod calculate-relation ((p geom-polygon) (l geom-line) &rest rest)
  (inverse
   (apply #'calculate-relation l p rest)))

;;;
;;;
;;; 

(defmethod calculate-relation ((c1 geom-chain) (c2 geom-chain) &key (lod 0))
  (cond ((intersects-p c1 c2)         
         `(intersects            
           ,@(when (plusp lod)
               `( ,@(if (some #'(lambda (x) 
                                  (some #'(lambda (y)                                   
                                            (1d-intersects-p x y))
                                        (segments c2)))
                              (segments c1))
                        `(1d                     
                          ,@(when (and (plusp (1- lod)))
                              
                              `( ,@(when (lies-on-p c1 c2)
                                     '(lies-on))))                          
                          ,@(when (and (plusp (- lod 2)))
                              
                              `( ,@(when (some #'(lambda (x)
                                                   (some #'(lambda (y)
                                                             (line-=-p x y))
                                                         (segments c2)))
                                               (segments c1))
                                                   
                                     '(common-segment)))))
                      `(0d))))                     
           ,@(when (plusp (- lod 2))
               `((endpoint-chain-relations ,(calculate-relation (p1 c1) c2 :lod (- lod 3))
                                           ,(calculate-relation (p2 c1) c2 :lod (- lod 3))
                                           ,(calculate-relation (p1 c2) c1 :lod (- lod 3))
                                           ,(calculate-relation (p2 c2) c1 :lod (- lod 3)))))
           ,@(let ((res (remove-if-not #'(lambda (segment) 
                                           (intersects-p segment c2))
                                       (segments c1))))
                              
               (when (and res (plusp (- lod 3)))
                 (list (cons 'segment-chain-relations
                             (mapcar #'(lambda (segment) 
                                         (calculate-relation segment c2
                                                             :lod (- lod 4)))
                                     res)))))))
        (t '(disjoint))))


(defmethod calculate-relation ((c geom-chain) (p geom-polygon) &key (lod 0))
 (declare (ignore rest))
 (if (intersects-p c p)
     `(intersects         
       ,@(when (plusp lod)
           (cond ((inside-p c p)
                  `(inside 
                     ,@(when (plusp (1- lod))
                         `(1d))))
                 ((lies-on-p c p)
                  `(lies-on lies-on-border 
                    ,@(when (plusp (1- lod))
                        `(1d))))
                 ((covered-by-p c p)
                  `(covered-by
                    ,@(when (plusp (1- lod))
                        `(1d))))
                          
                 ((crosses-p c p)
                   `(crosses
                     ,@(when (plusp (1- lod))
                         `(1d))))
                 (t 
                  `(touches
                    ,@(when (plusp (1- lod))
                        `( ,@(if (some #'(lambda (i)
                                           (some #'(lambda (j)
                                                     (1d-intersects-p i j))
                                                 (segments p)))
                                       (segments c))
                                 '(1d)
                               '(0d))))))))

       ,@(when (plusp (- lod 2))            
           `((endpoint-polygon-relations
              ,(calculate-relation (p1 c) p :lod (- lod 3))
              ,(calculate-relation (p2 c) p :lod (- lod 3)))))
        
        ,@(let ((res (remove-if-not #'(lambda (segment) 
                                        (intersects-p c segment))
                                    (segments p))))
            
            (when (and res (plusp (- lod 3)))
              `((chain-segment-relations
                 ,@(mapcar #'(lambda (segment) 
                               (calculate-relation c segment 
                                                   :lod (- lod 4)))
                           res))))))
   '(disjoint)))


  
(defmethod calculate-relation ((p geom-polygon) (c geom-chain) &rest rest)
  (inverse
   (apply #'calculate-relation c p rest)))

;;;
;;;
;;;

(defmethod calculate-relation ((p1 geom-polygon) (p2 geom-polygon) &key (lod 0))
  (declare (ignore lod))
  (if (intersects-p p1 p2)
      `(intersects
        ,@(when (plusp lod)
            (cond ((inside-p p1 p2)
                   `(inside
                     ,@(when (plusp (1- lod))
                         `(2d))))
                  ((inside-p p2 p1)
                   `(contains
                     ,@(when (plusp (1- lod))
                         `(2d))))
                  ((covered-by-p p1 p2)
                  `(covered-by
                    ,@(when (plusp (1- lod))
                        `(2d))))
                  ((covered-by-p p2 p1)
                   `(covers-p
                    ,@(when (plusp (1- lod))
                        `(2d))))               
                  ((overlaps-p p1 p2)
                   `(overlaps
                     ,@(when (plusp (1- lod))
                         `(2d))))
                 (t
                  `(touches
                    ,@(when (plusp (1- lod))
                        `( ,@(if (some #'(lambda (i)
                                           (some #'(lambda (j)
                                                     (1d-intersects-p i j))
                                                 (segments p2)))
                                       (segments p1))
                                 '(1d)
                               '(0d))))))))

        ,@(when (plusp (- lod 2))
            `((segment-poly-relations
               ,@(mapcar #'(lambda (segment) 
                             (calculate-relation segment p2
                                                 :lod (- lod 3)))
                         (segments p1))))))
    'disjoint))

;;;
;;;
;;;
  
(defun inverse-rcc-relation (rel)
  (labels ((lookup (rel)
             (ecase rel
               (:pp :ppi)
               (:ppi :pp)
               (:tpp :tppi)
               (:ntpp :ntppi)
               (:tppi :tpp)
               (:ntppi :ntpp)
               (:borders :bordered-by)
               (:bordered-by :borders)
               (:ec :ec)
               (:po :po)
               (:dc :dc)
               (:dr :dr)
               (:eq :eq))))
    (if (consp rel)
        (mapcar #'lookup rel)
      (lookup rel))))
  

(defun-memo calculate-rcc-relation (a b)
            ((a b (trafo-id *matrix*)))
  ;;; zusätzlich erweitert um "borders/bordered-by"
  ;;; für lineale Features   
  (typecase a 
    
    (geom-point 
     (typecase b
       (geom-point
        (cond ((point-=-p a b)
               :eq)
              (t :dc)))
       (geom-line
        (cond ((lies-on-p a b)
               :bordered-by)
              (t :dc)))
       (geom-chain 
        (cond ((lies-on-p a b)
               :bordered-by)
              (t :dc)))       
       (geom-polygon 
        (cond ((inside-p a b)
               :ntpp)
              ((lies-on-p a b)
               :bordered-by)
              (t 
               :dc)))))
    
    (geom-line   
     (typecase b
       (geom-point
        (cond ((lies-on-p b a)
               :borders)
              (t :dc)))
       (geom-line
        (cond ((line-=-p a b)
               :eq)
              ((intersects-p a b)
               :po)
              (t :dc)))
       (geom-chain 
        (cond ((lies-on-p a b)
               :bordered-by)
              ((intersects-p a b)
               :po)
              (t :dc)))
       (geom-polygon 
        (cond ((intersects-p  a b)
               (cond ((lies-on-p  a b)
                      :bordered-by)
                     ((covered-by-p a b)
                      :tpp)
                     ((touches-p a b)
                      :ec)
                     (t :po)))
              ((inside-p a b)
               :ntpp)
              (t :dc)))))
    

    (geom-chain   
     (typecase b
       (geom-point
        (cond ((lies-on-p b a)
               :borders)
              (t :dc)))
       (geom-line
        (cond ((lies-on-p b a)
               :borders)
              ((intersects-p a b)
               :po)
              (t :dc)))
       (geom-chain 
        (cond ((congruent-p a b)
               :eq)
              ((lies-on-p a b)
               :bordered-by)
              ((lies-on-p b a)
               :borders)
              ((intersects-p a b)
               :po)
              (t :dc)))
       (geom-polygon 
        (cond ((intersects-p  a b)
               (cond ((lies-on-p  a b)
                      :bordered-by)
                     ((covered-by-p a b)
                      :tpp)
                     ((touches-p a b)
                      :ec)
                     (t :po)))
              ((inside-p a b)
               :ntpp)
              (t :dc)))))
    
    (geom-polygon   
     (typecase b
       (geom-point
        (cond ((lies-on-p b a)
               :borders)
              ((inside-p b a)
               :ntppi)              
              (t :dc)))
       (geom-line
        (cond ((intersects-p a b)
               (cond ((lies-on-p b a)
                      :borders)
                     ((covered-by-p b a)
                      :tppi)
                     ((touches-p a b)
                      :ec)
                     (t :po)))
              ((inside-p b a)
               :ntppi)
              (t :dc)))
       (geom-chain
        (cond ((intersects-p a b)
               (cond ((lies-on-p b a)
                      :borders)
                     ((covered-by-p b a)
                      :tppi)
                     ((touches-p a b)
                      :ec)
                     (t :po)))
              ((inside-p b a)
               :ntppi)
              (t :dc)))
       (geom-polygon 
        (cond ((congruent-p a b)
               :eq)
              ((intersects-p a b)
               (cond ((covered-by-p b a)
                      :tppi)
                     ((covered-by-p a b)
                      :tpp)                                          
                     ((touches-p a b)
                      :ec)
                     (t :po)))
              ((inside-p b a)
               :ntppi)
              ((inside-p a b)
               :ntpp)
              (t :dc))))))) 



    
