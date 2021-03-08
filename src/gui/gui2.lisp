;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TANGRAM -*-

(in-package :tangram)


(define-gesture-name :align :pointer-button (:middle :shift))

(define-gesture-name :rotate :pointer-button (:left :shift))

(define-gesture-name :mirror :pointer-button (:right :shift))

(define-gesture-name :delete :pointer-button (:middle))

;;;
;;;
;;;

(defpersistentclass tangram-tile ()
  ((shape :accessor shape :initarg :shape)
   (color :accessor color :initarg :color)
   (name :accessor name :initarg :name)

   (x-off :accessor x-off :initform 100)
   (y-off :accessor y-off :initform 100)
   (orientation :accessor orientation :initform 0)
   (mirror :accessor mirror :initform 0)
   
   (on-board-p :accessor on-board-p :initform nil)

   (ref-point :accessor ref-point :initarg :ref-point)))

(defun make-tile (name xy-shape color)
  (let* ((shape (poly-from-xy-list xy-shape :affected-by-matrix-p t))
         (ref (first (last (point-list shape)))))
    (make-instance 'tangram-tile
      :name name
      :shape shape
      :color color
      :ref-point ref)))

(defmethod set-reference-point ((tile tangram-tile) (point geom-point))
  (setf (ref-point tile)
    point))


(defmethod put-on-board ((tile tangram-tile))
  (with-application-frame (frame) 
    (home tile)
    (unless (current-tile frame)
      (align-grid-to-tile tile frame))
    (setf (on-board-p tile) t)))

(defmethod remove-from-board ((tile tangram-tile))
  (with-application-frame (frame)
    (setf (on-board-p tile) nil)
    (when (eq (current-tile frame)
              tile)
      (setf (current-tile frame) nil)
      (loop as tile in (tiles (current-set frame)) when
            (on-board-p tile) do
            (align-grid-to-tile tile frame)))))

(defpersistentclass tangram-set ()
  ((tiles :accessor tiles :initarg :tiles)))


(defun make-tangram-set (&rest tiles)
  (make-instance 'tangram-set :tiles tiles))


(defmethod get-max-width-and-height ((set tangram-set))
  (get-max-width-and-height (tiles set)))

(defmethod get-max-width-and-height ((set list))
  (values 
   (loop as tile in set maximize
         (bb-width (shape tile)))
   (loop as tile in set maximize
         (bb-height (shape tile)))))

(defmethod get-min-width-and-height ((set tangram-set))
  (values 
   (loop as tile in (tiles set) minimize
         (bb-width (shape tile)))
   (loop as tile in (tiles set) minimize
         (bb-height (shape tile)))))


(defpersistentclass tangram-problem ()
  ((polygons :accessor polygons :initarg :polygons)
   (tiles :accessor tiles :initarg :tiles)
   (aggregate :accessor aggregate :initarg :aggregate)

   (known-solutions :accessor known-solutions :initform nil)))

(defmethod register-solution ((problem tangram-problem) solution)
  (with-application-frame (frame)
    (push solution 
          (known-solutions problem))
    (setf (current-solution frame) solution)))

(defun make-problem (polygons tiles)
  (let* ((polygons (mapcar #'(lambda (x)
                               (if (typep x 'geom-polygon)
                                   x
                                 (poly-from-xy-list polygons)))
                           (ensure-list polygons)))
         (agg (make-aggregate polygons)))
    (make-instance 'tangram-problem 
      :polygons polygons
      :tiles tiles
      :aggregate agg)))

;;;
;;;
;;;

(define-application-frame tangram-frame ()
  ((current-set :initform nil :accessor current-set)
   (current-tile :initform nil :accessor current-tile)
   (current-problem :initform nil :accessor current-problem)
   (current-solution :initform nil :accessor current-solution)
   (show-problem-p :initform nil :accessor show-problem-p)
   (show-thinking-p :initform t :accessor show-thinking-p)
   (game-type :initform 'all :accessor game-type)
   (all-solutions-p :initform t :accessor all-solutions-p)
   (display-view :initform 'construction :accessor display-view)

   (grid-xmin :accessor grid-xmin :initform 0)
   (grid-xmax :accessor grid-xmax :initform 0)
   (grid-ymin :accessor grid-ymin :initform 0)
   (grid-ymax :accessor grid-ymax :initform 0)
   
   (all-sets :initform (list (make-tangram-set 
                              (make-tile 'a '((0 0) (80 0) (40 40) (0 0)) +red+)
                              (make-tile 'b '((0 0) (0 80) (40 40) (0 0)) +green+)
                              (make-tile 'c '((80 0) (80 40) (60 20) (80 0)) +blue+)
                              (make-tile 'd '((80 80) (80 40) (40 80) (80 80)) +yellow+)
                              (make-tile 'e '((20 60) (60 60) (40 40) (20 60)) +black+)   
                              (make-tile 'f '((60 20) (80 40) (60 60) (40 40) (60 20)) +cyan+)
                              (make-tile 'g '((0 80) (40 80) (60 60) (20 60) (0 80)) (make-rgb-color 1 0.3 0.9))))
             :accessor all-sets)
   (problems :initform nil :accessor problems))   

  (:panes   
   (new-button (make-pane 'push-button
                          :label "New"
                          :activate-callback #'(lambda (&rest args)
                                                 (declare (ignore args)) 
                                                 (with-application-frame (frame)
                                                   (dolist (tile (tiles (current-set frame)))
                                                     (home tile))
                                                   (setf (current-tile frame) nil)
                                                   (setf (display-view frame) 'construction)
                                                   (refresh 'display)))))
   
   (save-button (make-pane 'push-button
                           :label "Save"
                           :activate-callback #'(lambda (&rest args)
                                                  (declare (ignore args))
                                                  (with-application-frame (frame)
                                                    (with-slots (current-set 
                                                                 current-tile 
                                                                 current-problem
                                                                 current-solution
                                                                 show-problem-p
                                                                 show-thinking-p 
                                                                 game-type
                                                                 all-solutions-p
                                                                 display-view
                                                                 grid-xmin
                                                                 grid-xmax
                                                                 grid-ymin
                                                                 grid-ymax
                                                                 all-sets
                                                                 problems) frame
                                                      (let ((file (file-selector "Save" "tangram:" "tan" :save t))) 
                                                        (when file 
                                                          (make-object-persistent 
                                                           (list current-set 
                                                                 current-tile 
                                                                 current-problem
                                                                 current-solution
                                                                 show-problem-p
                                                                 show-thinking-p 
                                                                 game-type
                                                                 all-solutions-p
                                                                 display-view
                                                                 grid-xmin
                                                                 grid-xmax
                                                                 grid-ymin
                                                                 grid-ymax
                                                                 all-sets
                                                                 problems)
                                                           (format nil "~A" file)))))))))

   (load-button (make-pane 'push-button
                           :label "Load"
                           :activate-callback #'(lambda (&rest args)
                                                  (declare (ignore args)) 
                                                  (with-application-frame (frame)
                                                    (with-slots (current-set 
                                                                 current-tile 
                                                                 current-problem
                                                                 current-solution
                                                                 show-problem-p
                                                                 show-thinking-p 
                                                                 game-type
                                                                 all-solutions-p
                                                                 display-view
                                                                 grid-xmin
                                                                 grid-xmax
                                                                 grid-ymin
                                                                 grid-ymax
                                                                 all-sets
                                                                 problems) frame
                                                      (let ((file (file-selector "Load" "tangram:" "tan" :save nil)))
                                                        (when file 
                                                          (let ((obj (load-persistent-object file)))
                                                            (setf current-set (pop obj)
                                                                  current-tile (pop obj)
                                                                  current-problem (pop obj)
                                                                  current-solution (pop obj)
                                                                  show-problem-p (pop obj)
                                                                  show-thinking-p (pop obj)
                                                                  game-type (pop obj)
                                                                  all-solutions-p (pop obj)
                                                                  display-view (pop obj)
                                                                  grid-xmin (pop obj)
                                                                  grid-xmax (pop obj) 
                                                                  grid-ymin (pop obj) 
                                                                  grid-ymax (pop obj) 
                                                                  all-sets (pop obj) 
                                                                  problems (pop obj)))
                                                          (refresh 'all))))))))
   
   (left-button (make-pane 'push-button
                           :label "<"
                           :activate-callback #'(lambda (&rest args)
                                                  (declare (ignore args)) 
                                                  (with-application-frame (frame)
                                                    (multiple-value-bind (dx)
                                                        (get-grid-cell-dimension frame)
                                                      (dolist (tile (tiles (current-set frame)))
                                                        (when (on-board-p tile)
                                                          (decf (x-off tile) dx)))
                                                      (when (current-tile frame)
                                                        (align-grid-to-tile (current-tile frame) frame))
                                                      (setf (display-view frame) 'construction)
						      (refresh 'display)
					;(redisplay-frame-panes frame :force-p t)
						      )))))
   (right-button (make-pane 'push-button
                            :label ">"
                            :activate-callback #'(lambda (&rest args)
                                                   (declare (ignore args)) 
                                                   (with-application-frame (frame)
                                                     (multiple-value-bind (dx)
                                                         (get-grid-cell-dimension frame)
                                                       (dolist (tile (tiles (current-set frame)))
                                                         (when (on-board-p tile)
                                                           (incf (x-off tile) dx)))
                                                       (when (current-tile frame)
                                                         (align-grid-to-tile (current-tile frame) frame))
                                                       (setf (display-view frame) 'construction)
                                                       (refresh 'display
								))))))
   (up-button (make-pane 'push-button
                         :label "/\\"
                         :activate-callback #'(lambda (&rest args)
                                                (declare (ignore args)) 
                                                (with-application-frame (frame)
                                                  (multiple-value-bind (dx dy)
                                                      (get-grid-cell-dimension frame)
                                                    (declare (ignore dx))

                                                    (dolist (tile (tiles (current-set frame)))
                                                      (when (on-board-p tile)
                                                        (decf (y-off tile) dy)))
                                                    (when (current-tile frame)
                                                      (align-grid-to-tile (current-tile frame) frame))
                                                    (setf (display-view frame) 'construction)
                                                    (refresh 'display))))))
   (down-button (make-pane 'push-button
                           :label "\\/"
                           :activate-callback #'(lambda (&rest args)
                                                  (declare (ignore args)) 
                                                  (with-application-frame (frame)
                                                    (multiple-value-bind (dx dy)
                                                        (get-grid-cell-dimension frame)
                                                      (declare (ignore dx))
                                                      (dolist (tile (tiles (current-set frame)))
                                                        (when (on-board-p tile)
                                                          (incf (y-off tile) dy)))
                                                      (when (current-tile frame)
                                                        (align-grid-to-tile (current-tile frame) frame))
                                                      (setf (display-view frame) 'construction)
                                                      (refresh 'display))))))

   (refresh-button (make-pane 'push-button
                              :label "Refresh"
                              :activate-callback #'(lambda (&rest args)
                                                     (declare (ignore args)) 
                                                     (with-application-frame (frame)
                                                       (setf (display-view frame) 'construction)
                                                       (refresh 'all)))))
   
   (stop-button (make-pane 'push-button
                           :label "Stop"
                           :activate-callback #'(lambda (&rest args)
                                                  (declare (ignore args))
                                                  (with-application-frame (frame)
                                                    (when *search-process*
						      #+:lispworks
						      (kill "Tangram Solver")
						      #+:allegro
						      (mp:process-kill *search-process*))
						    
                                                    (setf (display-view frame) 'construction)
                                                    (refresh 'all)
                                                    (Beep)))))

   (quit-button (make-pane 'push-button
                           :label "Quit"
                           :activate-callback #'(lambda (&rest args)
                                                  (declare (ignore args)) 
                                                  (with-application-frame (frame)
                                                    (when *process*
						      #+:allegro (mp:process-kill *process*)
						      #+:lispwork (kill "Tangram")
						      (frame-exit frame))))))
   
   (problem-button (make-pane 'push-button
                              :label "Problem"
                              :activate-callback #'(lambda (&rest args)
                                                     (declare (ignore args)) 
                                                     (with-application-frame (frame)
                                                       (create-problem (current-set frame))
                                                       (refresh 'problems)))))
   (solve-button (make-pane 'push-button
                            :label "Solve!"
                            :activate-callback #'(lambda (&rest args)
                                                   (declare (ignore args)) 
                                                   (with-application-frame (frame)
                                                     (setf *search-process*
						       (mp:process-run-function
							   "Tangram Solver"
							 #+:lispworks '(:size 1400000) ;;; set Stack Size! 
							 #'(lambda ()
							     (setf *application-frame* frame)
							     (solve-problem frame))))))))
   
   (display :application
            :end-of-line-action :allow
            :end-of-page-action :allow
            :display-function #'draw-display
	    :display-after-commands t
	    :initial-cursor-visibility nil
	    :incremental-redisplay nil
            :scroll-bars nil)
   
   (tiles :application
          :display-function #'draw-tiles
	  :initial-cursor-visibility nil
	  :incremental-redisplay nil            
          :scroll-bars nil)

   (tile-sets :application
              :label "Available Tile Sets"
              :display-function #'show-tile-sets
              :initial-cursor-visibility nil
	      :incremental-redisplay nil
	      :end-of-line-action :allow
              :scroll-bars :vertical)

   (problems :application
             :label "Available Problems"
             :display-function #'show-problems
             :initial-cursor-visibility nil
	     :incremental-redisplay nil
	     :end-of-line-action :allow
             :scroll-bars :vertical)
   
   (solutions :application
              :label "Known Solutions"
              :incremental-redisplay nil
	      :display-function #'show-solutions
              :end-of-line-action :allow
              :scroll-bars :vertical)

   (info :application :incremental-redisplay nil
            )

   (options :accept-values
	    :scroll-bars nil
 	    :min-height :compute :height :compute :max-height :compute 
	    :display-function
	    `(accept-values-pane-displayer
	      :displayer ,#'(lambda (frame stream)
                              (formatting-table (stream :x-spacing '(2 :character))      
                                (formatting-column (stream)
				  
	                          (formatting-cell (stream :align-x :center)
	                            (let ((view
                                           (accept 'completion
		                                   :query-identifier 'display
		                                   :prompt nil
		                                   :prompt-mode :raw
		                                   :stream stream 
		                                   :default (display-view frame)
		                                   :view `(option-pane-view :items
                                                                            (Construction
                                                                             Problem 
                                                                             Solution)))))
                                      (setf (display-view frame) view)))
				  

	                          (formatting-cell (stream :align-x :center)
	                            (let ((type
                                           (accept 'completion
		                                   :query-identifier 'type
		                                   :prompt nil
		                                   :prompt-mode :raw
		                                   :stream stream 
		                                   :default (game-type frame)
		                                   :view `(option-pane-view :items
                                                                            (all
                                                                             less
                                                                             copies)))))

                                      (setf (game-type frame) type)))

                                  (formatting-cell (stream :align-x :center)
                                    (let ((bool
                                           (accept 'boolean
                                                   :prompt "Show Thinking"
                                                   :stream stream 
                                                   :default (show-thinking-p frame)
                                                   :query-identifier 'show-thinking)))
                                      (setf (show-thinking-p frame) bool)))
                                  

                                  (formatting-cell (stream :align-x :center)
                                    (let ((bool
                                           (accept 'boolean
                                                   :prompt "All Solutions"
                                                   :stream stream 
                                                   :default (all-solutions-p frame)
                                                   :query-identifier 'all-solutions)))
                                      (setf (all-solutions-p frame) bool)))))))))
  
  (:layouts
   (:default
       (vertically ()
	 (1/10       
	  (horizontally ()
	    (1/8 quit-button)
	    (1/8 load-button)
	    (1/8 save-button)         
	    (1/8 stop-button)
	    (1/8 refresh-button)
	    (1/8 new-button)
	    (1/8 problem-button)
	    (1/8 solve-button)))
	 (6/10
	  (horizontally ()
	    (3/4 
	     (vertically ()
	       (1/20 up-button)
	       (18/20 
		(horizontally ()
		  (1/20 left-button)
		  (18/20 (outlining () display))
		  (1/20 right-button)))
	       (1/20 down-button)))
	    (1/4 (outlining () 
		   (vertically ()
		     (1/4 options)
		     (3/4 tiles))))))
	 (3/10 
	  (horizontally ()
	    (1/3 (outlining () tile-sets))
	    (1/3 (outlining () problems))
	    (1/3 (outlining () solutions))))))))



(defmethod refresh ((pane (eql 'all)))
  (refresh-pane 'display)
  (refresh-pane 'problems)
  (refresh-pane 'solutions))


(defmethod refresh ((pane (eql 'display)))
  (refresh-pane pane))

(defmethod refresh ((pane (eql 'problems)))
  (refresh-pane pane))

(defmethod refresh ((pane (eql 'solutions)))
  (refresh-pane pane))

(defmethod refresh ((pane (eql 'options)))
  (refresh-pane pane))


(defun refresh-pane (pane)
  (with-application-frame (frame)
    (let ((stream (get-frame-pane frame pane)))
      (window-clear stream)
      (draw-pane pane frame stream ))))

(defmethod draw-pane ((pane (eql 'display)) (frame tangram-frame) stream)
  (draw-display frame stream))

(defmethod draw-pane ((pane (eql 'solutions)) (frame tangram-frame) stream)
  (show-solutions frame stream))

(defmethod draw-pane ((pane (eql 'problems)) (frame tangram-frame) stream)
  (show-problems frame stream))


;;;
;;;
;;;


(defmethod get-window-dimension ((frame tangram-frame))
  (let ((stream (get-frame-pane frame 'display)))
    (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))))

(defconstant +grid-resolution+ 20)  

(defmethod get-grid-cell-dimension ((frame tangram-frame))
  (let ((stream (get-frame-pane frame 'display)))
    (multiple-value-bind (window-width window-height)
        (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
      (let* ((dx (/ window-width +grid-resolution+))
             (dy (/ window-height +grid-resolution+))
             (s (min dx dy)))
        (values s s)))))

(defmethod get-scaling-factors-for-tile ((tile tangram-tile) (frame tangram-frame))
  (multiple-value-bind (min-tile-width min-tile-height)
      (get-min-width-and-height (current-set frame))
    (multiple-value-bind (dx dy)
        (get-window-dimension frame)
      (let ((dx (/ dx 10))
            (dy (/ dy 10)))            
        (let* ((tile-width (bb-width (shape tile)))
               (tile-height (bb-height (shape tile)))

               (s (min 
                   (/ (* dx (/ tile-width min-tile-width))
                      tile-width)
                   (/ (* dy (/ tile-height min-tile-height))
                      tile-height))))
          (values s s))))))

;;;
;;;
;;;

(defmethod home ((tile tangram-tile))
  (with-application-frame (frame)
    (multiple-value-bind (widht height)
        (get-window-dimension frame)      
      (setf (on-board-p tile) nil
            (x-off tile) (/ widht 2)
            (y-off tile) (/ height 2)
            (orientation tile) 0)
      (set-reference-point tile
                           (first (point-list (shape tile)))))))





(defmethod align-grid-to-tile ((tile tangram-tile) (frame tangram-frame))  
  (with-application-frame (frame) 
    (setf (current-tile frame) tile)
    (with-slots (grid-xmin grid-ymin grid-xmax grid-ymax) frame
      (multiple-value-bind (sx sy)
          (get-scaling-factors-for-tile tile frame)
        (let* ((xc (x (ref-point tile)))
               (yc (y (ref-point tile)))
               (phi (* (/ +2pi+ 8) 
                       (mod (orientation tile) 8)))
               (m (mod (mirror tile) 4))
               (mx (ecase m
                     ((0 1) 1)
                     ((2 3) -1)))
               (my (ecase m
                     ((0 2) 1)
                     ((1 3) -1))))
	  
          ;;;
          ;;; Achtung! "meine" Transformationsoperationen
          ;;; sind genau entgegengesetzt zu den CLIM-Transformationen
          ;;; geschachtelt!
          ;;;
	  
          (with-translation ((- xc) (- yc))
            (with-rotation (phi)
              (with-scaling ((* mx sx) (* my sy))
                (with-translation ((x-off tile)
                                   (y-off tile))

                  (setf grid-xmin (x (pmin (shape tile)))
                        grid-ymin (y (pmin (shape tile)))
                        grid-xmax (x (pmax (shape tile)))
                        grid-ymax (y (pmax (shape tile)))))))))))))

;;;
;;;
;;;


(defclass handle () ())



#|
(define-presentation-method highlight-presentation ((type tangram-tile) record stream state) 
  (with-application-frame (frame)
    (let ((tile (presentation-object record)))
      (when (on-board-p tile)
        (multiple-value-bind (sx sy)
            (get-scaling-factors-for-tile tile frame)
          (let* ((xc (x (ref-point tile)))
                 (yc (y (ref-point tile)))
                 (phi (* (/ +2pi+ 8) 
                         (mod (orientation tile) 8)))
                 (m (mod (mirror tile) 4))
                 (mx (case m
                       ((0 1) 1)
                       ((2 3) -1)))
                 (my (case m
                       ((0 2) 1)
                       ((1 3) -1))))                       
	    
            (clim:with-translation (stream (x-off tile)
                                           (y-off tile))
              (clim:with-scaling (stream (* mx sx) (* sy my))
                (clim:with-rotation (stream phi)
                  (clim:with-translation (stream (- xc) (- yc))
                    (draw tile stream :highlight-p t)))))))))))

|#

(defmethod draw-display ((frame tangram-frame) stream &key (output-recording-p t))
  (case (display-view frame)
    (problem
     (show-current-problem frame))
    (solution 
     (show-solution frame))
    (construction
     (multiple-value-bind (window-width window-height)
         (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
       
       (let ((tile (current-tile frame)))
         (when tile
           (with-slots (grid-xmin grid-ymin grid-xmax grid-ymax) frame
             (draw-line* stream
                         0 grid-ymin
                         window-width grid-ymin :ink +yellow+)
	     
             (draw-line* stream
                         0 grid-ymax
                         window-width grid-ymax :ink +yellow+)
	     
	     
             (draw-line* stream
                         grid-xmin 0
                         grid-xmin window-height :ink +yellow+)
	     
             (draw-line* stream
                         grid-xmax 0
                         grid-xmax window-height :ink +yellow+))))
       
       
       (let ((tiles (tiles (current-set frame))))
         (dolist (tile tiles)
           (when (on-board-p tile)
             (multiple-value-bind (sx sy)
                 (get-scaling-factors-for-tile tile frame)
               (let* ((xc (x (ref-point tile)))
                      (yc (y (ref-point tile)))
                      (phi (* (/ +2pi+ 8) 
                              (mod (orientation tile) 8)))
                      (m (mod (mirror tile) 4))
                      (mx (case m
                            ((0 1) 1)
                            ((2 3) -1)))
                      (my (case m
                            ((0 2) 1)
                            ((1 3) -1))))                       
		 
                 (clim:with-translation (stream (x-off tile)
                                                (y-off tile))
                   (clim:with-scaling (stream (* mx sx) (* sy my))
                     (clim:with-rotation (stream phi)
                       (clim:with-translation (stream (- xc) (- yc))
			 
                         (if output-recording-p
			     (with-output-as-presentation (stream tile (type-of tile)
								  :allow-sensitive-inferiors t
								  :single-box nil)
			       (draw tile stream)
			       (dolist (point (point-list (shape tile)))
				 (with-output-as-presentation (stream (list point tile) 'handle :single-box t)
				   (draw-circle* stream (x point) (y point) 4 
						 :filled t
						 :ink +blue+))
				 (draw-circle* stream (x (ref-point tile)) (y (ref-point tile)) 4
					       :filled t
					       :ink +red+)))
			   (progn
			     (with-output-recording-options (stream :record nil :draw t)			
			       (draw tile stream)
			       (dolist (point (point-list (shape tile)))
				   (draw-circle* stream (x point) (y point) 4 
						 :filled t
						 :ink +blue+))
			       (draw-circle* stream (x (ref-point tile)) (y (ref-point tile)) 4
					     :filled t
					     :ink +red+)))))))))))))))))

(defmethod draw-tiles ((frame tangram-frame) stream)
  (case (display-view frame)
    (construction (draw-given-tiles (tiles (current-set frame))))
    (problem (when (current-problem frame)
               (draw-given-tiles (tiles (current-problem frame)))))
    (solution (when (current-problem frame)
                (draw-given-tiles (tiles (current-problem frame)))))))

(defclass tangram-tile-not-on-board () ())

(defun draw-given-tiles (tiles)
  (when tiles
    (dolist (tile tiles)
      (invalidate-bounding-box (shape tile)))    
    (with-application-frame (frame)
      (let ((stream (get-frame-pane frame 'tiles)))
        (window-clear stream)
        (multiple-value-bind (tile-width tile-height)
            (get-max-width-and-height tiles)
          (multiple-value-bind (window-width window-height)
              (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
            (let* ((y-inc (/ window-height (1+ (length tiles))))
                   (y 0)
                   (xs (/ (- window-width 30) tile-width))
                   (ys (/ window-height (* (1+ (length tiles)) tile-height)))
                   (s (min xs ys)))
	      
              (dolist (tile tiles)
                (when (or (eq (display-view frame) 'problem)
                          (and 
                           (not (eq (display-view frame) 'problem))
                           (not (on-board-p tile))))
                  (let* ((xc (x (pcenter (shape tile))))
                         (yc (y (pcenter (shape tile)))))

                    (incf y y-inc)

		      
			  (clim:with-translation (stream (/ window-width 2) y)
			    
			    (clim:with-scaling (stream s s)
			      
			      (clim:with-translation (stream (- xc) (- yc))   				    
				(with-output-as-presentation (stream tile 'tangram-tile-not-on-board :single-box t)
													      
				  (draw tile stream)))))))))))))))


(defmethod draw ((tile tangram-tile) stream &key highlight-p)
  (draw-polygon* stream (apply #'append 
                               (mapcar #'get-xy-list (point-list (shape tile))))
                 :ink (if highlight-p 
                          +flipping-ink+ 
                        (color tile)))
  (draw-polygon* stream (apply #'append 
                               (mapcar #'get-xy-list (point-list (shape tile))))
                 :ink (if highlight-p 
                          +flipping-ink+ 
                        +black+) :filled nil))

(defclass solution nil nil)

(defmethod show-solutions ((frame tangram-frame) stream)
  (when (current-problem frame)
    (let ((problem (current-problem frame)))
      (multiple-value-bind (window-width window-height)
          (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
        (let ((y 0))
          (dolist (solution (known-solutions (current-problem frame)))
            (with-output-as-presentation (stream solution 'solution :single-box t)
              (let ((width (bb-width (aggregate problem)))
                    (height (bb-height (aggregate problem))))

                (let* ((xs (/ window-width width))
                       (ys (/ (/ window-height 3) height))
                       (s (min xs ys))
                       (xc (x (pcenter (aggregate problem))))
                       (yc (y (pcenter (aggregate problem)))))

                  (clim:with-translation (stream (/ window-width 2) 
                                                 (+ y (/ (/ window-height 2) 2)))
                    (clim:with-scaling (stream s s)    
                      (clim:with-translation (stream (- xc) (- yc))    
                        (dolist (entry solution)
                          (let* ((polygon (first entry))
                                 (tile (second entry))
                                 (list (apply #'append (mapcar #'get-xy-list (mapcar #'p1 (segments polygon))))))
                            (draw-polygon* stream
                                           list
                                           :filled t
                                           :ink (color tile))
                            (draw-polygon* stream
                                           list
                                           :filled nil
                                           :ink +black+))))))
		  
                  (incf y (+ 15 (* s height))))))))))))


(defmethod show-solution ((frame tangram-frame))
  (when (current-solution frame)
    (let ((stream (get-frame-pane frame 'display)))
      (window-clear stream)
      (dolist (entry (current-solution frame))
        (let ((polygon (first entry))
              (tile (second entry)))
          (draw-polygon* stream
                         (apply #'append (mapcar #'get-xy-list (mapcar #'p1 (segments polygon))))
                         :filled t
                         :ink (color tile))
          (draw-polygon* stream
                         (apply #'append (mapcar #'get-xy-list (mapcar #'p1 (segments polygon))))
                         :filled nil
                         :ink +black+))))))

(defmethod show-tile-sets ((frame tangram-frame) stream)
  (multiple-value-bind (window-width window-height)
      (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
    (let ((y 0))

      (dolist (set (all-sets frame))
	
        (with-output-as-presentation (stream set (type-of set) :single-box t)   
	  
          (let ((tiles (tiles set)))
            (multiple-value-bind (tile-width tile-height)
                (get-max-width-and-height set)
              (let* ((x-inc (/ window-width (1+ (length tiles))))
                     (x 0)               
                     (xs (/ window-width (* (1+ (length tiles)) tile-height)))
                     (ys (/ (/ window-height 3) tile-width)))

                (incf y (+ 10 (/ window-height 3)))

                (dolist (tile tiles)
                  (let ((xc (x (pcenter (shape tile))))
                        (yc (y (pcenter (shape tile)))))

                    (incf x x-inc)
		    
                    (clim:with-translation (stream x y)

                      (clim:with-scaling (stream xs ys)
			
                        (clim:with-translation (stream (- xc) (- yc))   

                          (draw tile stream))))))))))))))



(defmethod show-problems ((frame tangram-frame) stream)
  (multiple-value-bind (window-width window-height)
      (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
    (let ((y 0))
      (dolist (problem (problems frame))
        (with-output-as-presentation (stream problem (type-of problem) :single-box t)                         
          (let ((polygons (polygons problem))
                (width (bb-width (aggregate problem)))
                (height (bb-height (aggregate problem))))

            (let* ((xs (/ window-width width))
                   (ys (/ (/ window-height 3) height))
                   (s (min xs ys))
                   (xc (x (pcenter (aggregate problem))))
                   (yc (y (pcenter (aggregate problem)))))

              (clim:with-translation (stream (/ window-width 2) 
                                             (+ y (/ (/ window-height 2) 2)))
                (clim:with-scaling (stream s s)    
                  (clim:with-translation (stream (- xc) (- yc))                
                    (dolist (polygon polygons)
                      (draw-polygon* stream
                                     (apply #'append (mapcar #'get-xy-list (mapcar #'p1 (segments polygon))))
                                     :filled t
                                     :ink (if (eq problem 
                                                  (current-problem frame))
                                              +yellow+
                                            (make-rgb-color 0.6 0.6 0.6))))
                    (loop as polygon in polygons 
			as ink in (make-cyclic (list +red+ +green+ +blue+ +black+ +yellow+))
			do
                          (dolist (s (segments polygon))
                            (draw-line* stream (x (p1 s)) (y (p1 s)) (x (p2 s)) (y (p2 s))
                                        :line-thickness 3
                                        :ink ink))))))

              (incf y (+ 15 (* s height))))))))))



;;;
;;;
;;;


(define-tangram-frame-command (com-select-set)
    ((set 'tangram-set))
  (with-application-frame (frame)
    (setf (current-set frame) set)
    (setf (display-view frame) 'construction)))


(define-tangram-frame-command (com-select-solution)
    ((solution 'solution))
  (with-application-frame (frame)
    (setf (display-view frame) 'solution)
    (setf (current-solution frame) 
      solution)))


(define-tangram-frame-command (com-delete-solution)
    ((solution 'tangram-problem))
  (with-application-frame (frame)
    (setf (known-solutions 
           (current-problem frame))
      (delete solution 
	      (known-solutions 
	       (current-problem frame))))
    (when (eq (current-solution frame) solution)
      (setf (current-solution frame) 
	(first (known-solutions (current-problem frame)))))))


(define-tangram-frame-command (com-select-problem)
    ((problem 'tangram-problem))
  (with-application-frame (frame)
    (setf (display-view frame) 'problem)
    (setf (current-problem frame) problem)))



(define-tangram-frame-command (com-delete-problem)
    ((problem 'tangram-problem))
  (with-application-frame (frame)
    (setf (problems frame) (delete problem
                                   (problems frame)))
    (when (eq problem (current-problem frame))
      (setf (current-problem frame) (first (problems frame))))))


(define-tangram-frame-command (com-put-tile-on-board)
    ((object 'tangram-tile-not-on-board))
  (put-on-board object))


(define-tangram-frame-command (com-align-grid-to-tile)
    ((tile 'tangram-tile) (frame 'tangram-frame))
  (align-grid-to-tile tile frame))

(define-tangram-frame-command (com-remove-tile-from-board)
    ((object 'tangram-tile))
  (remove-from-board object))


(define-tangram-frame-command (com-move-tile)
    ((tile 'tangram-tile) (frame 'tangram-frame))
  
  (let ((stream (get-frame-pane frame 'display))
        (first-click t)
        (ox)
        (oy))

    (if (eq tile (current-tile frame))
          (tracking-pointer (stream)
            (:pointer-motion (x y)
			     (when first-click
			       (setf ox (x-off tile)
				     oy (y-off tile))
			       (setf first-click nil))

			     (setf (x-off tile) x
				   (y-off tile) y)

			     (align-grid-to-tile tile frame)

			     
			     (with-output-recording-options (stream :draw t :record nil)
			       (window-clear stream)    
			       (draw-display frame (get-frame-pane frame 'display)
					     :output-recording-p nil)))
            (:pointer-button-press (event)
				   (when (= (pointer-event-button event)
					    +pointer-right-button+)
				     (setf (x-off tile) 
				       ox
				       (y-off tile)
				       oy)
				     (align-grid-to-tile tile frame))

				   (return)))
      
      (multiple-value-bind (sx sy)
          (get-scaling-factors-for-tile tile frame)
        (let* ((xc (x (ref-point tile)))
               (yc (y (ref-point tile)))
               (phi (* (/ +2pi+ 8) 
                       (mod (orientation tile) 8)))
               (m (mod (mirror tile) 4))
               (mx (case m
                     ((0 1) 1)
                     ((2 3) -1)))
               (my (case m
                     ((0 2) 1)
                     ((1 3) -1))))
	  
          ;;;
          ;;; Achtung! "meine" Transformationsoperationen
          ;;; sind genau entgegengesetzt zu den CLIM-Transformationen
          ;;; geschachtelt!
          ;;;
	  
          (let ((coords nil)
                (point nil)
                (last-point nil)
                (min-dist nil))

            (with-translation ((- xc) (- yc))
              (with-rotation (phi)
                (with-scaling ((* mx sx) (* my sy))
                  (with-translation ((x-off tile)
                                     (y-off tile))

                    (let* ((tile-xmin (x (pmin (shape tile))))
                           (tile-ymin (y (pmin (shape tile))))
                           (tile-xmax (x (pmax (shape tile))))
                           (tile-ymax (y (pmax (shape tile))))
			   
                           (tile-width (- tile-xmax tile-xmin))
                           (tile-height (- tile-ymax tile-ymin)))

                      (with-slots (grid-xmin grid-ymin grid-xmax grid-ymax) frame
                        (multiple-value-bind (window-width window-height)
                            (get-window-dimension frame)

                          (let ((x-coords (list 0
                                                (- grid-xmin (* 2 tile-width))
                                                (- grid-xmin tile-width) 
                                                grid-xmin
                                                (+ grid-xmin tile-width) 
                                                (+ grid-xmin (* 2 tile-width))
                                                (- grid-xmax (* 2 tile-width))
                                                (- grid-xmax tile-width) 
                                                grid-xmax
                                                (+ grid-xmax tile-width)
                                                (+ grid-xmax (* 2 tile-width))
                                                window-width))
                                (y-coords (list 0
                                                (- grid-ymin (* 2 tile-height))
                                                (- grid-ymin tile-height) 
                                                grid-ymin
                                                (+ grid-ymin tile-height) 
                                                (+ grid-ymin (* 2 tile-height))
                                                (- grid-ymax (* 2 tile-height))
                                                (- grid-ymax tile-height) 
                                                grid-ymax
                                                (+ grid-ymax tile-height)
                                                (+ grid-ymax (* 2 tile-height))
                                                window-height)))
			    
			    
                            (dolist (x x-coords)
                              (dolist (y y-coords)
                                (push (list x y) coords)))))))))))
	    
              (tracking-pointer (stream)
                (:pointer-motion (x y)
				 (when first-click
				   (setf ox x
					 oy y)
				   (setf first-click nil))                               
				 
				 (setf min-dist nil)
				 
				 (loop as coord in coords do
				       (let* ((xt (first coord))
					      (yt (second coord))
					      (dist (distance-between* xt yt x y)))
					 (when (or (not min-dist) (< dist min-dist))
					   (setf min-dist dist
						 point coord))))

				 (unless (equal last-point point)
				   (setf (x-off tile) (first point)
					 (y-off tile) (second point))
				    
				   (with-output-recording-options (stream :draw t :record nil)
				     (window-clear stream)
				     (draw-display frame (get-frame-pane frame 'display) 
						   :output-recording-p nil)
				     (dolist (coord coords)
				       (draw-circle* stream (first coord) (second coord) 8 :filled nil :ink +red+)))
				   (setf last-point point)
				   ))

		
                (:pointer-button-press (event)
				       (when (= (pointer-event-button event)
						+pointer-right-button+)
					 (setf (x-off tile) 
					   ox
					   (y-off tile)
					   oy)
					 (align-grid-to-tile (current-tile frame) frame))
				       
				       (return)))))))))



(define-tangram-frame-command (com-make-reference-point)
    ((point-and-tile 'handle))
  
  (let* ((point (first point-and-tile))
         (tile (second point-and-tile)))
    
    (set-reference-point tile point)))



(define-tangram-frame-command (com-rotate-tile)
    ((tile 'tangram-tile))
  (incf (orientation tile))
  (with-application-frame (frame)
    (when (eq (current-tile frame) tile)
      (align-grid-to-tile tile frame))))


(define-tangram-frame-command (com-mirror-tile)
    ((tile 'tangram-tile))
  (incf (mirror tile))
  (with-application-frame (frame)
    (when (eq (current-tile frame) tile)
      (align-grid-to-tile tile frame))))

;;;
;;;
;;;

(define-presentation-to-command-translator select-set
    (tangram-set com-select-set tangram-frame
                 :gesture :select)
  (object)
  (list object))


(define-presentation-to-command-translator select-problem
    (tangram-problem com-select-problem tangram-frame
                     :gesture :select)
  (object)
  (list object))


(define-presentation-to-command-translator delete-problem
    (tangram-problem com-delete-problem tangram-frame
                     :gesture :delete)
  (object)
  (list object))


(define-presentation-to-command-translator select-solution
    (solution com-select-solution tangram-frame
              :gesture :select)
  (object)
  (list object))

(define-presentation-to-command-translator delete-solution
    (solution com-delete-solution tangram-frame
              :gesture :delete)
  (object)
  (list object))


(define-presentation-to-command-translator put-tile-on-board
    (tangram-tile-not-on-board com-put-tile-on-board tangram-frame
                               :gesture :select
                               :tester ((frame)
                                        (eq (display-view frame) 'construction)))

  (object)
  (list object))


(define-presentation-to-command-translator move-tile
    (tangram-tile com-move-tile tangram-frame
                  :gesture :select)
  (object frame)
  (list object frame))


(define-presentation-to-command-translator align-grid-to-tile
    (tangram-tile com-align-grid-to-tile tangram-frame
                  :gesture :align)
  (object frame)
  (list object frame))


(define-presentation-to-command-translator make-reference-point
    (handle com-make-reference-point tangram-frame
            :gesture :select)
  (object) 
  (list object))




(define-presentation-to-command-translator rotate-tile
    (tangram-tile com-rotate-tile tangram-frame
                  :gesture :rotate)
  (object)
  (list object))


(define-presentation-to-command-translator mirror-tile
    (tangram-tile com-mirror-tile tangram-frame
                  :gesture :mirror)
  (object)
  (list object))

(define-presentation-to-command-translator remove-tile-from-board
    (tangram-tile com-remove-tile-from-board tangram-frame
                  :gesture :describe)
  (object)
  (list object))


;;;
;;;
;;;


(defmethod solve-problem ((frame tangram-frame))
  (let ((stream (get-frame-pane frame 'display)))
    (refresh 'all)
    (when (current-problem frame)
      (find-covering (polygons (current-problem frame))
                     (tiles (current-problem frame))
                     :all-solutions-p (all-solutions-p frame)
                     :remove-used-tiles-p (member (game-type frame) '(less all))
                     :allow-less-tiles-p (member (game-type frame) '(copies less))
                     :show-thinking-fn 
                     #'(lambda (configuration solution-p)
                         (let ((polygons (first configuration))
                               (tiles (second configuration))
                               (history (third configuration)))
                           (declare (ignore tiles poylgons))
                           (when (or (show-thinking-p frame) solution-p)
                             (setf (display-view frame) 'problem)
                             (unless solution-p 
                               (window-clear stream)                                                                    
                               (show-polygons polygons))
                             (when history
                               (dolist (entry history)
                                 (let* ((poly (first entry))
                                        (tile (second entry))
                                        (list (apply #'append 
                                                     (mapcar #'get-xy-list 
                                                             (mapcar #'p1 (segments poly))))))
                                   (draw-polygon* stream
                                                  list
                                                  :filled t
                                                  :ink (color tile))
                                   (draw-polygon* stream
                                                  list
                                                  :filled nil
                                                  :ink +black+))))
                             (draw-given-tiles tiles))
			   
                           (when solution-p 
                             (setf (display-view frame) 'solution)
                             (register-solution (current-problem frame) history)
                             (refresh 'solutions)
                             )))))))


;;;
;;;
;;;

(defmethod create-problem ((set tangram-set))  
  (let ((polygons nil)
        (tiles nil))
    (with-application-frame (frame) 
      (let ((cl-user::*use-store* nil))    
        
        (dolist (tile (tiles set))
          (setf (affected-by-matrix-p (shape tile)) t)
	  
          (multiple-value-bind (sx sy)
              (get-scaling-factors-for-tile tile frame)
            (let* ((xc (x (ref-point tile)))
                   (yc (y (ref-point tile)))
                   (phi (* (/ +2pi+ 8) 
                           (mod (orientation tile) 8)))
                   (m (mod (mirror tile) 4))
                   (mx (case m
                         ((0 1) 1)
                         ((2 3) -1)))
                   (my (case m
                         ((0 2) 1)
                         ((1 3) -1))))                      

              ;;;
              ;;; Achtung! "meine" Transformationsoperationen
              ;;; sind genau entgegengesetzt zu den CLIM-Transformationen
              ;;; geschachtelt!
              ;;;
	      
              (with-translation ((- xc) (- yc))
                (with-rotation (phi)
                  (with-scaling ((* mx sx) (* my sy))
                    (with-translation ((x-off tile)
                                       (y-off tile))
		      
                      (let ((xy-list 
                             (append (mapcar #'get-xy-list 
                                             (mapcar #'p1 (segments (shape tile))))
                                     (list (get-xy-list 
                                            (p2 (first (last (segments (shape tile))))))))))
                        (when (on-board-p tile)
                          (push (poly-from-xy-list xy-list                                   
                                                   :affected-by-matrix-p nil)
                                polygons))

                        (push (make-tile
                               (name tile)                               
                               xy-list
                               (color tile))
                              tiles)))))))))

        (when (some #'(lambda (a)
                        (some #'(lambda (b)
                                  (and (not (eq a b))
                                       (not (member (calculate-rcc-relation a b) 
                                                    '(:dc :ec)))))
                              polygons))
                    polygons)
          (beep)
          (return-from create-problem nil))

        (let ((union 
               (polygon-set-union polygons)))
          (push (make-problem union tiles)
                (problems frame))
          (setf (current-problem frame) 
	    (first (problems frame))))))))


(defmethod show-current-problem ((frame tangram-frame))
  (when (current-problem frame)
    (show-polygons (polygons (current-problem frame)))))

(defparameter +gray-colors+
    (make-cyclic (list 
		  (make-gray-color 0.3)
		  (make-gray-color 0.4)                
		  (make-gray-color 0.5)
		  (make-gray-color 0.6)
		  (make-gray-color 0.7)
		  (make-gray-color 0.8)
		  (make-gray-color 0.9))))



(defun show-polygons (polygons)
  (with-application-frame (frame)
    (let ((stream (get-frame-pane frame 'display)))
      (window-clear stream)
      (loop as p in polygons
	  as ink in +gray-colors+ do
            (draw-polygon* stream
                           (apply #'append (mapcar #'get-xy-list (mapcar #'p1 (segments p))))
                           :filled t
                           :ink ink)))))

;;;
;;;
;;;

(defvar *tangram-frame*)

(defvar *process* nil)

(defvar *search-process* nil)

(defun tangram ()
  (setf *tangram-frame*
    (make-application-frame
     'tangram-frame          
     :left 40 
     :top 40 
     :width 900 
     :height 950))
  (setf (current-set *tangram-frame*)
    (first (all-sets *tangram-frame*)))

  (setf *process*
    #+mcl
    (ccl:process-run-function
	"Tangram"
      #'(lambda ()
	  (run-frame-top-level *tangram-frame*)))
    
    (mp:process-run-function
	"Tangram"
      #+lispworks '(:size 1400000)	;;; set Stack Size! 
      #'(lambda ()
	  (run-frame-top-level *tangram-frame*)))))


