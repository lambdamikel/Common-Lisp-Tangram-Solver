;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: TANGRAM; Base: 10 -*-

(in-package tangram)

#+:mcl
(defun file-selector (title directory pattern &key save)
  (declare (ignore title))
  (loop
    (let ((pathname 
           (ccl:catch-cancel
             (if (not save)
               (ccl:choose-file-dialog :directory directory)
               (ccl:choose-new-file-dialog :directory directory)))))
      (cond ((eq pathname :cancel)
             (return nil))
            ((string-equal (pathname-type pathname) pattern)
             (return pathname))
            (t (ccl:message-dialog 
                (format nil "Required File Extension: ~A" pattern)))))))

#+:allegro
(defun file-selector (title directory pattern &key (save nil))
  (with-application-frame (frame)
    (loop
      (let ((file (select-file frame
			       :pattern (concatenate 'string "*." pattern)
			       :title title
			       :directory (namestring 
					   (truename directory)))))
	(if file	    
          (when (and file (not (string= "" file)))
            (cond ((char= (elt file
                               (1- (length file)))
                          #\/)
                   (notify-user frame
                                "You must select a file, not a directory!"
                                :style :error))
                  ((not (string= (subseq file (- (length file) (length pattern)))
                                 pattern))
                   (notify-user frame
                                (format nil "Required File Extension: ~A" pattern)
                                :style :error))
                  (save
                   (when (cl-user:=> (probe-file file)
                                     (notify-user frame
                                                  (format nil "Overwrite ~A?" file) 
                                                  :style :warning))
                     (return file)))
                  ((and (not save)
                        (probe-file file))
                   (return file))
                  (t
                   (notify-user frame
                                (format nil "File ~A does not exist" file)
                                :style :error))))
          (return nil))))))

#+:lispworks
(defun file-selector (title directory pattern &key (save nil))
  (with-application-frame (frame)
    (loop
      (let ((file (select-file frame
			       :pattern (concatenate 'string "*." pattern)
			       :title title
			       :directory (directory-namestring 
                                           (translate-logical-pathname directory)))))
	(if file	    
            (let ((file (namestring file)))
              (when (and file (not (string= "" file)))
                (cond ((char= (elt file
                                   (1- (length file)))
                              #\/)
                       (notify-user frame
                                    "You must select a file, not a directory!"
                                    :style :error))
                      ((not (string= (subseq file (- (length file) (length pattern)))
                                     pattern))
                       (notify-user frame
                                    (format nil "Required File Extension: ~A" pattern)
                                    :style :error))
                      (save
                       (when (cl-user:=> (probe-file file)
                                         (notify-user frame
                                                      (format nil "Overwrite ~A?" file) 
                                                      :style :warning))
                         (return file)))
                      ((and (not save)
                            (probe-file file))
                       (return file))
                      (t
                       (notify-user frame
                                    (format nil "File ~A does not exist" file)
                                    :style :error)))))
          (return nil))))))
  

#+:mcl
(defmacro mcl-pane (&body body)
  `(outlining ()
     ,@body))

(defun inverse-highlighter (type record stream state)
  (declare (ignore state type))
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates 
	stream (output-record-parent record))
    (with-bounding-rectangle* (left top right bottom) record
      (draw-rectangle* stream
		       (+ left xoff) (+ top yoff)
		       (+ right xoff) (+ bottom yoff)
		       :ink +flipping-ink+))))


(defmacro with-centering ((stream) &body body)
  (let ((x (gensym)))
    `(multiple-value-bind (,x)
	 (window-inside-size ,stream)       
       (formatting-table (stream)
	 (formatting-row (stream)
	   (formatting-cell (stream :align-x :center
				    :align-y :center
				    :min-width ,x)
	     ,@body)))
       (terpri stream))))


(defmacro with-border ((stream &key (offset 0)) &body body)
  (let ((or (gensym)))
    `(let ((,or
	    (with-output-to-output-record (,stream)
	      ,@body)))
       (draw-rectangle* ,stream
			(- (bounding-rectangle-min-x ,or) ,offset)
			(- (bounding-rectangle-min-y ,or) ,offset)
			(+ (bounding-rectangle-max-x ,or) ,offset)
			(+ (bounding-rectangle-max-y ,or) ,offset)
			:line-dashes '(1 1)
			:filled nil)
       ,@body)))

(defmacro draw-marker* (stream x y radius &rest args)
  `(let ((x ,x)
	 (y ,y)
	 (r ,radius))
     (clim:draw-rectangle* ,stream
			   (- x r)
			   (- y r)
			   (+ x r)
			   (+ y r) 
			   :line-thickness 1 
			   :filled nil ,@args)
     (clim:draw-line* stream 
		      (- x r)
		      (- y r)
		      (+ x r)
		      (+ y r) 
		      :line-thickness 1
		      ,@args)
     (clim:draw-line* stream 
		      (- x r)
		      (+ y r)
		      (+ x r)
		      (- y r)
		      :line-thickness 1 
		      ,@args)))

(defun get-size-of-graphic (stream fn)
  (let ((or
	 (with-output-to-output-record (stream)
	   (funcall fn))))
    (values
     (bounding-rectangle-width or)
     (bounding-rectangle-height or))))



#+:lispworks
(defmacro with-correct-clipping ((stream) &body body)
  `(multiple-value-bind (window-width window-height)    
       (bounding-rectangle-size (bounding-rectangle (window-viewport ,stream)))
    
     (with-drawing-options (,stream :clipping-region 
                                    (make-bounding-rectangle 0 0 window-width window-height))
       ,@body)))

#-:lispworks
(defmacro with-correct-clipping ((stream) &body body)
  body)
