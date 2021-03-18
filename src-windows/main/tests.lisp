;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TANGRAM -*-

(in-package :tangram)


(progn 
  (setf a1 (poly-from-xy-list '((0 0) (80 0) (40 40) (0 0)))
        b1 (poly-from-xy-list '((0 0) (0 80) (40 40) (0 0)))
        c1 (poly-from-xy-list '((80 80) (80 40) (40 80) (80 80)))
        d1 (poly-from-xy-list '((80 0) (80 40) (60 20) (80 0)))
        e1 (poly-from-xy-list '((20 60) (60 60) (40 40) (20 60)))
        f1 (poly-from-xy-list '((60 20) (80 40) (60 60) (40 40) (60 20)))
        g1 (poly-from-xy-list '((0 80) (40 80) (60 60) (20 60) (0 80))))

  (setf a2 (poly-from-xy-list '((0 0) (80 0) (40 40) (0 0)))
        b2 (poly-from-xy-list '((0 0) (0 80) (40 40) (0 0)))
        c2 (poly-from-xy-list '((80 80) (80 40) (40 80) (80 80)))
        d2 (poly-from-xy-list '((80 0) (80 40) (60 20) (80 0)))
        e2 (poly-from-xy-list '((20 60) (60 60) (40 40) (20 60)))
        f2 (poly-from-xy-list '((60 20) (80 40) (60 60) (40 40) (60 20)))
        g2 (poly-from-xy-list '((0 80) (40 80) (60 60) (20 60) (0 80))))

  (setf x 
        (with-translation (10 10)
          (let* ((poly c1)
                 (xy-list 
                  (append (mapcar #'get-xy-list 
                                  (mapcar #'p1 (segments poly)))
                          (list (get-xy-list 
                                 (p2 (first (last (segments poly)))))))))
            (princ xy-list)            
            (poly-from-xy-list xy-list))))

  (setf y
        (with-translation (-10 -10)
          (let* ((poly x)
                 (xy-list 
                  (append (mapcar #'get-xy-list 
                                  (mapcar #'p1 (segments poly)))
                          (list (get-xy-list 
                                 (p2 (first (last (segments poly)))))))))
            (princ xy-list)
            (poly-from-xy-list xy-list))))

  (setf *x* nil)
  (let ((*debug-p* t))
    (princ (congruent-p c1 y))
    (terpri)
    (terpri)
    (princ `(:allignemnts ,(find-possible-alignments c1 x)))
    (pprint *x*)))

  
  
  

 
(progn
  (setf x (poly-from-xy-list '((0 0) (10 0) (10 10) (0 10) (0 0))))
  
  (setf y (poly-from-xy-list '((5 0) (10 5) (10 10) (5 10) (0 5) (5 0))))
  
  (setf z (poly-from-xy-list '((5 0) (10 5) (5 10) (0 5) (5 0))))
  
  (setf u (poly-from-xy-list '((0 5) (2 0) (3 3) (5 0) (6 0) (10 3) (5 5) (10 7) (10 10) (0 10) (0 5))))

  (princ (mapcan #'(lambda (x)
                     (format t "Component: ~A~%" (get-xy-list x)))
                 (polygon-difference x y)))
  (terpri) (terpri)
  (princ (mapcan #'(lambda (x)
                     (format t "Component: ~A~%" (get-xy-list x)))
                 (polygon-difference x z)))
  (terpri) (terpri)
  (princ (mapcan #'(lambda (x)
                     (format t "Component: ~A~%" (get-xy-list x)))
                 (polygon-difference y z)))
  
  (terpri) (terpri)
  (princ (mapcan #'(lambda (x)
                     (format t "Component: ~A~%" (get-xy-list x)))
                 (polygon-difference x u))))

(progn
  (setf x (poly-from-xy-list '((0 0) (10 0) (10 10) (0 10) (0 0))))
  
  (setf y (poly-from-xy-list '((5 5) (15 5) (15 15) (5 15) (5 5))))

  (terpri) (terpri)
  (princ (remove-duplicates (polygon-union x y) :test #'congruent-p)))
  
(progn
  (setf x (poly-from-xy-list '((0 0) (5 5) (10 0) (10 10) (0 10) (0 0))))
  
  (setf y (poly-from-xy-list '((0 707/100) (707/100 707/100) (0 0) (0 707/100))))
  
  (princ (mapcan #'(lambda (x)
                     (format t "Component: ~A~%" (get-xy-list x)))
                 (polygon-difference x y)))
  (terpri) (terpri))
  

(progn
  (setf a (poly-from-xy-list '((0 0) (30 0) (30 30) (0 30) (0 0))))
  
  (setf b (poly-from-xy-list '((30 30) (50 30) (40 40) (30 30))))

  (princ (find-possible-alignments a b)))


(progn
  (setf x (poly-from-xy-list '((0 0) (10 0) (10 10) (0 0))))
  
  (setf y (poly-from-xy-list '((12 0) (20 0) (20 20) (12 0))))

  (terpri) (terpri)
  (princ (remove-duplicates (polygon-union x y) :test #'congruent-p)))
  

(princ
 (length
  (find-covering (list (poly-from-xy-list '((0 0) (10 10) (0 10) (0 0))))
                 (list (list (make-tile 'a '((0 0) (5 5) (0 10) (0 0)) +black+)))
                 :remove-used-tiles-p nil
                 :allow-less-tiles-p t
                 :all-solutions-p t)))

(princ
 (length
  (find-covering
   (list (poly-from-xy-list '((822/5 411/5) (1233/5 411/5) (2877/10 1233/10)
                              (411/2 1233/10) (822/5 411/5))))
   (list 
    (list
     (make-tile 'a 
                '((822/5 411/5) (411/2 1233/10) (2877/10 1233/10) (1233/5 411/5) (822/5 411/5))
                +black+))))))





(princ 
 (prog2
   (setf *x* nil)
   (find-covering 
    (list (let ((poly (poly-from-xy-list '((0 80) (40 80) (60 60) (20 60) (0 80)))))
            (with-rotation (pi)
              (with-translation (0 0)
                (let ((xy-list 
                       (append (mapcar #'get-xy-list 
                                       (mapcar #'p1 (segments poly)))
                               (list (get-xy-list 
                                      (p2 (first (last (segments poly)))))))))

                  (poly-from-xy-list xy-list))))))

  (list ; a SET of set of tiles 
   (list (make-tile 'a '((0 80) (40 80) (60 60) (20 60) (0 80))
                    (make-rgb-color 1 0.3 0.9)))))
  (pprint *x*)))




(princ 
 (find-covering 

  
  (list (poly-from-xy-list '((0 80) (40 80) (60 60) (20 60) (0 80))))        
  (list ; a SET of set of tiles 
   (list (make-tile 'a '((0 80) (40 80) (60 60) (20 60) (0 80))
                    (make-rgb-color 1 0.3 0.9))))))



(progn

  ;;c1 (poly-from-xy-list '((80 80) (80 40) (40 80) (80 80)))

  (setf x (poly-from-xy-list '((0 0) (10 0) (10 10) (0 10) (0 0))))
  
  (setf y (poly-from-xy-list '((5 5) (15 5) (15 15) (5 15) (5 5))))

  (setf x1 
        (let ((poly (poly-from-xy-list '((0 0) (10 0) (10 10) (0 10) (0 0)))))
            (with-rotation (pi)
              (with-translation (-10 -10)
                (let ((xy-list 
                       (append (mapcar #'get-xy-list 
                                       (mapcar #'p1 (segments poly)))
                               (list (get-xy-list 
                                      (p2 (first (last (segments poly)))))))))
                  (print xy-list)

                  (poly-from-xy-list xy-list))))))

  (princ (congruent-p x x))

  (princ (congruent-p x y))

  (princ (congruent-p x x1)))
