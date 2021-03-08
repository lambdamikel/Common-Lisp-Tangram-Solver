;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package cl-user)

(defconstant +pi/2+ (/ pi 2))
(defconstant +2pi+ (* 2 pi))

;;;
;;;
;;;

(defun yes (&rest args)
  (declare (ignore args))
  t)

(defun no (&rest args)
  (declare (ignore args))
  nil)

;;;
;;;
;;;

(defparameter *round-p* t) ;; when t -> Koordinaten werden auf "Round precision"-Stellen Genauigkeit reduziert

(defparameter *rounding-precision* 10)

(defmacro my-round (num)
  `(if *round-p* 
       (/ (floor (+ (* ,num *rounding-precision*) 1/2))
          *rounding-precision*)
     ,num))

;;;
;;;
;;;

(defun forcecdr (stream &key num (fn #'identity))
  (let ((res nil)
        (count 0))
    (loop while (and stream (if (numberp num) (< count num) t)) do
          (incf count)     
          (push (funcall fn (first stream)) res)
          (setf stream (funcall (second stream))))
    (reverse res)))


#+(or allegro mcl)
(defun get-lambda-args (fn)
  (nreverse
   (mapcar #'(lambda (arg)
	       (if (consp arg)
		   (first arg)
		 arg))
	   (set-difference
            #+:allegro
	    (excl:arglist fn)
            #+:mcl
	    (arglist fn)
	    '(&rest &optional &key)))))


(defun set-disjoint (a b)
  (and (not (some #'(lambda (i)
		      (member i b))
		  a))
       (not (some #'(lambda (i)
		      (member i a))
		  b))))       

(defun set-equal (a b &rest args)
  (and (apply #'subsetp a b args)
       (apply #'subsetp b a args)))


(defun  make-cyclic (x)
  (when x
    (setf (cdr (last x)) x)))

;;;
;;;
;;;

#|

(defconstant +epsilon+ 1e-6)

(defmacro zerop-eps (a)
  `(=-eps 0 ,a))

(defmacro =-eps (a b &optional (epsilon +epsilon+))
  `(<= (abs (- ,a ,b)) ,epsilon))

(defmacro <=-eps (a b &optional (epsilon +epsilon+))
  `(<=  ,a (+ ,b ,epsilon)))

(defmacro >=-eps (a b &optional (epsilon +epsilon+))
  `(>=  ,a (- ,b ,epsilon)))

|#


;;;
;;;
;;;


;(defun => (a b) 
;  (or (not a) b))


(defmacro => (a b)
  `(or (not ,a) ,b))

(defmacro <=> (a b)
  `(and (=> ,a ,b)
	(=> ,b ,a)))

(defmacro xor (&rest args)
  `(or ,@(mapcar #'(lambda (arg)
		     `(and ,arg ,@(mapcar #'(lambda (arg)
					      `(not ,arg))
					  (remove arg args :count 1))))
		 args)))

(defun ensure-list (arg)
  (if (listp arg)
      arg
    (list arg)))

;;;
;;;
;;;

(defmacro rotate-to (list n)
  `(rotate-by ,list (position ,n ,list)))

(defmacro rotate-by (list n)
  `(progn 
     (when (and ,n ,list)
       (let ((n (mod ,n (length ,list))))
         (unless (zerop n)
           (setf ,list 
                 (nconc (subseq ,list n)
                        (subseq ,list 0 n))))))
     ,list))

;;;
;;;
;;;

(defun tree-reverse (liste)
  (labels ((do-it (liste akku)
	     (cond ((null liste) akku)
		   (t (let ((first (first liste)))
			(do-it (rest liste)
			       (cons
			        (if (listp first)
			            (do-it first nil) first)
			        akku)))))))
    (do-it liste nil)))

(defun tree-remove (item tree)
  (cond ((not (consp tree))
	 (if (eq item tree) nil tree))
	(t
	 (let ((car (tree-remove item (car tree)))
	       (cdr (tree-remove item (cdr tree))))
	   (if car
	       (cons car cdr)
	     cdr)))))

(defun tree-collect-if (fn tree)
  (let ((items nil))
    (labels ((do-it (tree)
               (if (and tree (not (consp tree)))
                   (when (funcall fn tree) 
                     (push tree items))
                 (when tree
                   (do-it (car tree))                   
                   (do-it (cdr tree))))))
      (do-it tree)
      items)))


(defun tree-find (tree x &rest args)
  (or (apply #'member x tree args)
      (some #'(lambda (sub)
		(and (consp sub)
		     (apply #'tree-find sub x args)))
	    tree)))

(defun tree-find-if (fn tree &rest args)
  (some #'(lambda (sub)
	    (cond ((consp sub)
		   (apply #'tree-find-if fn sub args))
		  (t (apply fn sub args))))
	tree))

(defun tflatten (tree)
  (if (consp tree)
      (append (tflatten (car tree))
	      (tflatten (cdr tree)))
    (when tree (list tree))))

;;;
;;;
;;;

(defun one-of (sequence)
  (elt sequence (random (length sequence))))

;;;
;;;
;;;


(defun subseq-member (seqa sequence)
  (let ((n (length seqa)))
    (some #'(lambda (item)
	      (loop as seq on item
		    when (equal (subseq seq 0 (min n (length seq)))
			        seqa)
		    return t))
	  sequence)))

(defmacro enqueue (elem queue)
  `(setf ,queue 
         (nconc ,queue (list ,elem))))

(defmacro dequeue (queue)
  `(prog1 
       (first (last ,queue))
     (setf ,queue (butlast ,queue))))


(defun reorder (sequence)  
  (let ((res nil)
        (sequence (copy-list sequence))
        (length (length sequence)))
    (loop
     (when (null sequence)
       (return res))
     (let ((elem (elt sequence (random length))))
       (push elem res)
       (setf sequence (delete elem sequence :count 1)
             length (1- length))))))

;;;
;;;
;;;

(defun splice-in (sym list)
  (append (mapcan #'(lambda (x)
		      (list x sym))
		  (butlast list))
	  (last list)))


(defun build-elem-pairs-in-list (list)
  (let ((res nil)
	(pre nil)
	(succ (copy-list list)))
    (loop
     (unless (cdr succ) (return res))
     (push (append pre 
                   (cons (list (first succ)
                               (second succ))
                         (cddr succ)))
           res)
     (setf pre (append pre 
                       (list (first succ))))      
     (setf succ (cdr succ)))))

(defun stringsubst-char-with-string (orgstring char string)
  (let ((pos (position char orgstring)))
    (if pos
	(concatenate 'string
	             (subseq orgstring 0 pos)
	             string
	             (stringsubst-char-with-string (subseq orgstring (1+ pos))
					           char string))
      orgstring)))
				

(defparameter *replacements*
  `(("--" "-" t)
    ("  " " " t)))

(defun blank-line-p (line)
  (or (eq line 'newline)
      (and (typep line 'string)
	   (not (position-if-not #'(lambda (i) (char= i #\space)) line)))))

	  
(defun string-substitute (string &optional (rules *replacements*)
			         &key add-spaces)
  (labels ((do-it (string akku)
             (cond ((blank-line-p string) akku)
                   (t 
                    (let ((min-pos nil)
                          (min-from-to))
                      (dolist (from-to rules)
                        (let* ((from (first from-to))
                               (pos (search from string)))
                          (when pos
                            (if (or (not min-pos) 
                                    (< pos min-pos))
                                (setf min-from-to from-to
                                      min-pos pos)))))
                      (let ((from (first min-from-to))
                            (to (second min-from-to))
                            (replaced-as-new-input-p (third min-from-to)))
                        (if min-pos
                            (if replaced-as-new-input-p
                                (do-it 
                                 (concatenate 'string 
					      to
					      (subseq string (+ min-pos (length from))))
                                 (append akku 
                                         (list (subseq string 0 min-pos))))
                              (do-it 
                               (subseq string (+ min-pos (length from)))
                               (append akku 
                                       (list (subseq string 0 min-pos))
                                       (list to))))
                          (append akku (list string)))))))))
      
    (let ((res (do-it (if add-spaces 
                          (concatenate 'string " " string " ")
                        string)
		      nil)))
      (if res 	  
          (reduce #'(lambda (x y)
                      (concatenate 'string x y))
                  res)
        ""))))


(defun transform-xy-list (xylist)
  (loop for x in xylist by #'cddr 
        for y in (cdr xylist) by #'cddr 
        collect (list x y)))


(defmacro pushend (obj list)
  `(setf ,list (nconc ,list (list ,obj))))

(defmacro mynconc (lista listb)
  `(setf ,lista 
         (nconc ,lista ,listb)))

(defmacro my-format (stream indent-level string &rest args)
  `(progn 
     (dotimes (i (* 7 ,indent-level))
       (princ " " ,stream))
     (format ,stream ,string ,@args)))

(defmacro my-read (stream)
  #+:allegro
  `(read ,stream)
  #+(or mcl lispworks)
  `(read-from-string (read-line ,stream)))

#+(or allegro lispworks)
(defun circle-subseq (list from to)
  (let* ((copy (copy-list list)))
    (setf (rest (last copy)) copy)
    (subseq copy from to)))

#+:mcl
(defun circle-subseq (list from to)
  (let* ((copy (copy-list list)))
    (setf (rest (last copy)) copy)
    (let ((start copy))
      (loop repeat from do
            (setf start (cdr start)))
      (loop repeat (- to from)
	    collect (car start) do
            (setf start (cdr start))))))

#+:allegro
(defun recode-german-characters (string)

  (nsubstitute (character 228) (character 204) string) ; dos aeh -> unix aeh
  (nsubstitute (character 246) (character 224) string) ; dos oeh -> unix oeh
  (nsubstitute (character 252) (character 201) string) ; dos ueh -> unix ueh
  
  (nsubstitute (character 196) (character 216) string) ; dos AEH -> unix AEH
  (nsubstitute (character 214) (character 231) string) ; dos OEH -> unix OEH
  (nsubstitute (character 220) (character 232) string) ; dos UEH -> unix UEH
  
  (nsubstitute (character 223) #\· string) ; dos sz -> unix sz
  
  (nsubstitute (character 228) (character 132) string) ; siemens aeh -> unix aeh ; dxf
  (nsubstitute (character 246) (character 148) string) ; siemens oeh -> unix oeh ; dxf
  (nsubstitute (character 252) (character 129) string) ; siemens ueh -> unix ueh ; dxf
  
  (nsubstitute (character 214) (character 153) string) ; siemens OEH -> unix OEH ; andere Code unbekannt! ; dxf
  
  (nsubstitute (character 228) (character #xbf) string) ; siemens aeh -> unix aeh ; sqd
  (nsubstitute (character 246) (character #xd0) string) ; siemens oeh -> unix oeh ; sqd
  (nsubstitute (character 252) (character #xdd) string) ; siemens ueh -> unix ueh ; sqd
  
  (nsubstitute (character 214) (character #xf0) string) ; siemens OEH -> unix OEH ; andere Codes sind unbekannt! ; dxf

  (nsubstitute (character 223) (character #xc5) string) ; dos sz -> unix sz ; dxf

  string)

#+:lispworks
(defun recode-german-characters (string)
  (dolist (subs '((#\Ö #\U+0095)
                  (#\Ö #\U+0099)
                  (#\ß #\U+0089)
                  (#\ß #\á)
                  (#\ä #\ø)
                  (#\ä #\U+0084)
                  (#\ü #\ð)
                  (#\ü #\U+0081)
                  (#\ö #\Soft-Hyphen)
                  (#\ö #\U+0094)))
    (nsubstitute (first subs) (second subs) string))
  string)

#+:mcl
(defun recode-german-characters (string)
  string)

;;;
;;;
;;;

(defun compute-all-subsets (set &optional (akku '(nil)))
  (let ((newakku akku))
    (mapl #'(lambda (lset)
	      (let ((item (first lset)))		    
		(unless (some #'(lambda (set) (member item set)) newakku)
		  (dolist (expand akku)
		    (let ((new (cons item expand)))
		      (push new newakku)))
		  (setf newakku (compute-all-subsets (rest lset) newakku)))))
	  set)
    newakku))



(defun prod-pow-n (xs n)
  (if (= n 1)
      (mapcar #'list xs)
    (prodnx
     (loop as i from 1 to n collect xs))))


(defun prod (xs ys)
  (reduce 
   #'nconc
   (mapcar #'(lambda (x)
	       (mapcar #'(lambda (y)
			   (list x y))
		       ys))
	   xs)))

(defun prodn (&rest args)
  (reduce #'prod args))

(defun prodnx (arguments)
  (reduce #'prod arguments))

(defun newprod (list-of-args)
  (if (not (cdr list-of-args))
      list-of-args
    (let ((list (first list-of-args))
	  (res (newprod (rest list-of-args))))
      (if (not (cddr list-of-args))
	  (apply #'append
		 (mapcar #'(lambda (elem1)
			     (mapcar #'(lambda (elem2)
					 (list elem1 elem2))
				     (first list-of-args)))
			 (second list-of-args)))
	(apply #'append 
	       (mapcar #'(lambda (list2)
			   (mapcar #'(lambda (elem1)
				       (cons elem1 list2))
				   list))
		       res))))))

(defun power (xs n)
  (mapcar #'tflatten (prod-pow-n xs n)))

#|
(defun perm (list)
  (if (cdr list)
      (remove-duplicates 
       (mapcan #'(lambda (a)
		   (let ((rest (remove a list :count 1)))
		     (mapcar #'(lambda (rest)
				 (cons a rest))
			     (perm rest))))
	       list)
       :test #'equal)
    (list list)))
|#

(defun permutations (list &optional (n (length list)) (i 0)) 
  (perm list n i))

(defun perm (list &optional (n (length list)) (i 0)) ;;; choose "k" out of "n" 
                                                         (if (= i n) 
                                                             '(nil)
                                                           (remove-duplicates 
                                                            (mapcan #'(lambda (a)
                                                                        (let ((rest (remove a list :count 1)))
                                                                          (mapcar #'(lambda (rest)
                                                                                      (cons a rest))
                                                                                  (perm rest n (1+ i)))))
                                                                    list)
                                                            :test #'equal)))

;;;
;;;
;;;

(defun fac (n)
  (if (zerop n) 
      1
    (* n (fac (1- n)))))

(defun n-over-k (n k)
  (/ (fac n) (* (fac (- n k)) (fac k))))

(defun k-out-of-n (k n)
  (/ (fac n) (fac (- n k))))

;;;
;;;
;;;

(defun vector-orientation (fromx fromy tox toy)
  (let ((dx (let ((d (- tox fromx)))
              (if (< (abs d) 0.0001)
                  0 d)))
        (dy (let ((d (- toy fromy)))
              (if (< (abs d) 0.0001)
                  0 d))))
    (if (and (zerop dx) (zerop dy))
        (error "~A ~A. Degenerate point vector - no orientation!" dx dy)      
      (if (zerop dy)
          (if (plusp dx)
              0
            pi)
        (if (zerop dx)
            (if (plusp dy)
                (/ pi 2)
              (* 3/2 pi))
          (if (plusp dx)
              (if (plusp dy)
                  (atan (/ dy dx))
                (- (* 2 pi)
                   (atan (/ (abs dy) dx))))
            (if (plusp dy) ;;; (minusp dx)
                (- pi (atan (/ dy (abs dx))))
              (+ pi (atan (/ (abs dy) (abs dx)))))))))))
        



(defun intervall-intersects-p (a b c d)
  (not (or (< b c) 
	   (< d a))))

(defun lies-in-circle-intervall-p (x a b)
  (if (= a b) 
      t
    (if (<= a b)
	(<= a x b)
      (or (>= b x) (>= x a)))))

(defun circle-intervall-intersects-p (a b c d)
  (or (lies-in-circle-intervall-p b c d)
      (lies-in-circle-intervall-p d a b)))

(defun circle-intervall-length (a b &optional (modulo +2pi+))
  (if (<= a b)
      (- b a)
    (+ (- modulo a) b)))

;;;
;;;
;;;

(defun rad-to-deg (phi)
  (* 180 (/ phi pi)))

(defun deg-to-rad (phi)
  (* pi (/ phi 180)))

;;;
;;;
;;;

(defparameter *use-store* t)

(defvar *all-tables* nil)

(defun reset-memo ()
  (mapc #'clrhash *all-tables*))


(defmacro defun-memo (function-name
                      lambda-list
                      (params-to-memoize
                       &rest hash-args)
                      &body body)
  (let ((hash (apply #'make-hash-table
                     (append '(:test equal) hash-args))))
    (push hash *all-tables*)
    `(defun ,function-name ,lambda-list
       (if *use-store*
           (multiple-value-bind (ret found-p)
               (gethash (list ,@params-to-memoize)
                        ,hash)
             (if found-p
                 (apply #'values ret)
               (apply #'values
                      (setf (gethash (list ,@params-to-memoize)
                                     ,hash)
                            (multiple-value-list
                             (progn ,@body))))))
         (progn ,@body)))))

(defmacro defmethod-memo (method-name
                          lambda-list
                          (params-to-memoize
                           &rest hash-args)
                          &body body)
  (let ((hash (apply #'make-hash-table
                     (append '(:test equal) hash-args))))
    (push hash *all-tables*)    
    `(defmethod ,method-name ,lambda-list
       (if *use-store*
           (multiple-value-bind (ret found-p)
               (gethash (list ,@params-to-memoize)
                        ,hash)
             (if found-p
                 (apply #'values ret)
               (apply #'values
                      (setf (gethash (list ,@params-to-memoize)
                                     ,hash)
                            (multiple-value-list
                             (progn ,@body))))))
         (progn ,@body)))))

;;;
;;;
;;;

(defun concept-name (x &optional package)
  (let ((x (format nil "~A" x)))
    (intern 
     (nstring-upcase  
      (string-substitute  
       (string-substitute x '(("  " " " t) (" " "-" t) ("." "-" t) (";" "-" t) ("," "-" t) ("/" "-" t) ("\\" "-" t) ("!" "" t)
                              ("," "-" t) ("(" "-" t) (")" "" t)  ("--" "-" t)))
       '(("--" "-" t))))
     (or package *package*))))



#+:lispworks
(defun run (fn)
  (mp:process-run-function
   "Process"
   '(:size 1400000) ;;; set Stack Size! 
   fn))

#+:lispworks 
(defun kill (&optional name)
  (let ((process (mp:find-process-from-name 
                  (or name "Process"))))
    (when process 
      (mp:process-kill process))))



;;;
;;;
;;;

(defmacro with-standard-output-to-file ((file) &rest body)
  `(with-open-file (stream ,file :direction :output :if-exists :supersede)
     (let ((*standard-output* stream))
       ,@body)))


;;;
;;; AVL-Trees laut Wirth
;;;

(declaim (inline balance-r balance-l))


(defun find-in-avl-tree (item tree &key
                              (key #'identity) (compare-fn #'<) (match-fn #'=)
                              (apply-key-to-item-argument-p t))
  (macrolet ((item (x)
               `(first ,x))
             (bal (x)
               `(second ,x))
             (left (x)
               `(third ,x))
             (right (x)
               `(fourth ,x)))

    (let ((x (if apply-key-to-item-argument-p 
                 (funcall key item)
               item)))
      
      (labels ((search (p) 
                 (when p
                   (if (funcall match-fn (funcall key (item p)) x)
                       (item p)
                     (if (funcall compare-fn x (funcall key (item p)))
                         (search (left p))
                       (search (right p)))))))
        
        (search tree)))))

                  

(defun insert-into-avl-tree* (item tree &key (key #'identity) (compare-fn #'<) (match-fn #'=))
  (macrolet ((item (x)
               `(first ,x))
             (bal (x)
               `(second ,x))
             (left (x)
               `(third ,x))
             (right (x)
               `(fourth ,x)))
    
    (let ((x (funcall key item)))
  
      (labels ((search (p)

                 (if (not p)
                     (values (list item 0 nil nil) t)

                   (if (funcall match-fn (funcall key (item p)) x)
                       (error "Already present!")

                     (if (funcall compare-fn x (funcall key (item p)))

                         (multiple-value-bind (subtree h)
                             (search (left p))
                           (setf (left p) subtree)
                           (when h
                             (case (bal p)
                               (1 (setf (bal p) 0
                                        h nil))
                               (0 (setf (bal p) -1))
                               (-1 
                                ;; rebalance
                                (let ((p1 (left p)))
                                  (if (= (bal p1) -1)
                                      (setf (left p) (right p1)
                                            (right p1) p
                                            (bal p) 0
                                            p p1)
                                    (let ((p2 (right p1)))
                                      (setf (right p1) (left p2)
                                            (left p2) p1
                                            (left p) (right p2)
                                            (right p2) p)
                                      (if (= (bal p2) -1) 
                                          (setf (bal p) 1)
                                        (setf (bal p) 0))
                                      (if (= (bal p2) 1)
                                          (setf (bal p1) -1)
                                        (setf (bal p1) 0))
                                      (setf p p2))))
                                (setf (bal p) 0
                                      h nil))))
                           (values p h))

                       ;; (> x (item p))
                     
                       (multiple-value-bind (subtree h)
                           (search (right p))
                         (setf (right p) subtree)                         
                         (when h
                           (case (bal p)
                             (-1 (setf (bal p) 0
                                       h nil))
                             (0 (setf (bal p) 1))
                             (1 
                              ;; rebalance
                              (let ((p1 (right p)))
                                (if (= (bal p1) 1)
                                    (setf (right p) (left p1)
                                          (left p1) p
                                          (bal p) 0
                                          p p1)
                                  (let ((p2 (left p1)))
                                    (setf (left p1) (right p2)
                                          (right p2) p1
                                          (right p) (left p2)
                                          (left p2) p)
                                    (if (= (bal p2) 1) 
                                        (setf (bal p) -1)
                                      (setf (bal p) 0))
                                    (if (= (bal p2) -1)
                                        (setf (bal p1) 1)
                                      (setf (bal p1) 0))
                                    (setf p p2))))
                              (setf (bal p) 0
                                    h nil))))
                         
                         (values p h)))))))

        (search tree)))))


(defun delete-from-avl-tree* (item tree &key (key #'identity) (compare-fn #'<) (match-fn #'=))
  (macrolet ((item (x)
               `(first ,x))
             (bal (x)
               `(second ,x))
             (left (x)
               `(third ,x))
             (right (x)
               `(fourth ,x)))

    (let ((x (funcall key item)))

      (labels ((get-subtree-and-rightmost-item (r)
                 (if (right r) 
                     (multiple-value-bind (subtree item h)
                         (get-subtree-and-rightmost-item (right r))
                       (setf (right r) subtree)
                       (when h
                         (multiple-value-bind (nr nh)
                             (balance-r r h)
                           (setf r nr
                                 h nh)))
                       (values r item h))
                   (progn
                     (values (left r) (item r) t))))
               
               (delete-it (p h)
                 (when p                   
                   (if (funcall match-fn x (funcall key (item p)))
                       (cond ((not (right p))
                              (setf p (left p)
                                    h t))
                             ((not (left p))
                              (setf p (right p)
                                    h t))
                             (t
                              (multiple-value-bind (subtree item nh)
                                  (get-subtree-and-rightmost-item (left p))
                                    
                                (setf (item p) item)
                                (setf (left p) subtree)                                    
                                (setf h nh)
                                    
                                (when h
                                  (multiple-value-bind (np nh)
                                      (balance-l p h)
                                    (setf p np
                                          h nh))))))
                       
                     (if (funcall compare-fn x (funcall key (item p)))
                         (multiple-value-bind (subtree nh)
                             (delete-it (left p) h)
                           (setf (left p) subtree)
                           (setf h nh)
                           (when h 
                             (multiple-value-bind (np nh)                                  
                                 (balance-l p h)
                               (setf p np
                                     h nh))))

                       (multiple-value-bind (subtree nh)
                           (delete-it (right p) h)
                         (setf (right p) subtree)
                         (setf h nh)
                         (when h 
                           (multiple-value-bind (np nh)
                               (balance-r p h)
                             (setf p np
                                   h nh))))))

                   (values p h))))

        (delete-it tree nil)))))
  

(defun balance-l (p h)
  (macrolet ((item (x)
               `(first ,x))
             (bal (x)
               `(second ,x))
             (left (x)
               `(third ,x))
             (right (x)
               `(fourth ,x)))
    
    (case (bal p)
        
      (-1 (setf (bal p) 0))
        
      (0 (setf (bal p) 1
               h nil))

      (1 (let* ((p1 (right p))
                (b1 (bal p1)))

           (if (>= b1 0)
               (progn 
                 (setf (right p) (left p1)
                       (left p1) p)
                 (if (= 0 b1)
                     (setf (bal p) 1
                           (bal p1) -1
                           h nil)
                   (setf (bal p) 0
                         (bal p1) 0))
                 (setf p p1))
             (let* ((p2 (left p1))
                    (b2 (bal p2)))
                 
               (setf (left p1) (right p2)
                     (right p2) p1
                     (right p) (left p2)
                     (left p2) p)

               (if (= 1 b2) 
                   (setf (bal p) -1)
                 (setf (bal p) 0))
               (if (= -1 b2)
                   (setf (bal p1) 1)
                 (setf (bal p1) 0))
               (setf p p2
                     (bal p2) 0))))))

    (values p h)))
                 
                      


(defun balance-r (p h)
  (macrolet ((item (x)
               `(first ,x))
             (bal (x)
               `(second ,x))
             (left (x)
               `(third ,x))
             (right (x)
               `(fourth ,x)))
    
    (case (bal p)
        
      (1 (setf (bal p) 0))
        
      (0 (setf (bal p) -1
               h nil))

      (-1 (let* ((p1 (left p))
                 (b1 (bal p1)))

            (if (<= b1 0)
                (progn 
                  (setf (left p) (right p1)
                        (right p1) p)
                  (if (= 0 b1)
                      (setf (bal p) -1
                            (bal p1) 1
                            h nil)
                    (setf (bal p) 0
                          (bal p1) 0))
                  (setf p p1))
              (let* ((p2 (right p1))
                     (b2 (bal p2)))
                 
                (setf (right p1) (left p2)
                      (left p2) p1
                      (left p) (right p2)
                      (right p2) p)

                (if (= -1 b2) 
                    (setf (bal p) 1)
                  (setf (bal p) 0))
                (if (= 1 b2)
                    (setf (bal p1) -1)
                  (setf (bal p1) 0))
                (setf p p2
                      (bal p2) 0))))))

    (values p h)))


(defun get-items-from-avl-tree (tree)
  (macrolet ((item (x)
               `(first ,x))
             (bal (x)
               `(second ,x))
             (left (x)
               `(third ,x))
             (right (x)
               `(fourth ,x)))
    
    (when tree
      (cons (item tree)
            (append (get-items-from-avl-tree (left tree))
                    (get-items-from-avl-tree (right tree)))))))
                   

(defmacro loop-over-avl-tree ((var tree) &rest body)
  (let ((fn-sym (gensym))
        (node-sym (gensym)))
  `(progn
     (labels ((,fn-sym (,node-sym)
                (when ,node-sym
                  (let ((,var (first ,node-sym)))
                    ,@body)
                  (,fn-sym (third  ,node-sym))
                  (,fn-sym (fourth ,node-sym)))))

       (,fn-sym ,tree)))))


(defmacro insert-into-avl-tree (item tree &rest args)
  `(setf ,tree (insert-into-avl-tree* ,item ,tree ,@args)))

(defmacro delete-from-avl-tree (item tree &rest args)
  `(setf ,tree (delete-from-avl-tree* ,item ,tree ,@args)))

(defun print-avl-tree (tree stream)
  (macrolet ((item (x)
               `(first ,x))
             (bal (x)
               `(second ,x))
             (left (x)
               `(third ,x))
             (right (x)
               `(fourth ,x)))
    
    (labels ((do-it (tree &optional (level 0) next)	     
               (let ((node (item tree))
                     (children (remove nil
                                       (list (left tree)
                                             (right tree)))))
	         (format stream ";;;")
	         (if (= level 0)
		     (format stream "   ~A~%" node)
	           (progn 
		     (dolist (item (butlast next))
		       (if item 
		           (format stream "   |")
		         (format   stream "    ")))
		     (let ((last (first (last next))))
		       (if last
		           (format stream "   |---~A~%" node)
                         (format stream   "   \\___~A~%" node)))))

                 (let ((last (first (last children))))
	           (dolist (child children)
		     (do-it child (1+ level) (append next (list (not (eq child last))))))))))
    
      (do-it tree))))

#|

(defun test-avl-trees (n)
  (loop 
   (let* ((keys (reorder (loop as i from 1 to n collect i)))        
          (tree nil))

     (princ "*") 

     (dolist (key keys)
       (setf tree (insert-into-avl-tree key tree)))
     
     (dolist (key (reorder keys))
       (unless tree (error "!"))
       (setf tree (delete-from-avl-tree key tree)))

     (when tree (error "!")))))

|#

