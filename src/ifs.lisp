(in-package :overmind-intuition)

(def membership
    "Returns the grade of membership associated to `x` in `mf`"
  :sig ((number ifs-params) number)
  :tests nil
  :body
  ((x ifs-params)
   (let* ((params (-> ifs-params))
	  (ab-m (access params :ab-m))
	  (ab-b (access params :ab-b))
	  (bc-m (access params :bc-m))
	  (bc-b (access params :bc-b))
	  (ab-y (+ (* ab-m x) ab-b))
	  (bc-y (+ (* bc-m x) bc-b))
	  )
     (if (or (> ab-y 1) (< ab-y 0))
	 (if (or (> bc-y 1) (< bc-y 0))
	     0
	     bc-y)
	 ab-y))))

(def if-membership
    "Returns the intuitionistic fuzzy membership of `x` in the intuitionistic fuzzy set `ifs`."
  :sig ((number ifs) number)
  :tests nil
  :body
  ((x ifs)
   (let ((mem (membership x (-> ifs :mf :parameters)))
	 (non-mem (membership x (-> ifs :nmf :parameters))))
     (* (+ mem (- 1 non-mem))
	mem))))

(def alpha-cut
    "Returns a list of points that represents the alpha-cut of the
`ifs` by the intuitionistic fuzzy membership `ifm`."
  :sig ((number ifs) ifs-polygon)
  :tests nil
  :body
  ((ifm ifs)
   (let* ((mf-params (-> ifs :mf :parameters :=))
	  (nmf-params (-> ifs :nmf :parameters :=))
	  
	  (mf-ab-m (access mf-params :ab-m))
	  (mf-ab-b (access mf-params :ab-b))
	  (mf-bc-m (access mf-params :bc-m))
	  (mf-bc-b (access mf-params :bc-b))

	  (nmf-ab-m (access nmf-params :ab-m))
	  (nmf-ab-b (access nmf-params :ab-b))
	  (nmf-bc-m (access nmf-params :bc-m))
	  (nmf-bc-b (access nmf-params :bc-b))

	  (mf-ab (make-line :x1 (access mf-params :a)
			    :y1 0
			    :x2 (/ (- ifm mf-ab-b) mf-ab-m)
			    :y2 ifm
			    :m mf-ab-m
			    :b mf-ab-b))
	  (mf-cut (make-line :x1 (/ (- ifm mf-ab-b) mf-ab-m)
			     :y1 ifm
			     :x2 (/ (- ifm mf-bc-b) mf-bc-m)
			     :y2 ifm
			     :m 0
			     :b ifm))
	  (mf-bc (make-line :x1 (/ (- ifm mf-bc-b) mf-bc-m)
			    :y1 ifm
			    :x2 (access mf-params :c)
			    :y2 0
			    :m mf-bc-m
			    :b mf-bc-b))

	  (nmf-ab (make-line :x1 (access nmf-params :a)
			     :y1 0
			     :x2 (/ (- ifm nmf-ab-b) nmf-ab-m)
			     :y2 ifm
			     :m nmf-ab-m
			     :b nmf-ab-b))
	  (nmf-cut (make-line :x1 (/ (- ifm nmf-ab-b) nmf-ab-m)
			      :y1 ifm
			      :x2 (/ (- ifm nmf-bc-b) nmf-bc-m)
			      :y2 ifm
			      :m 0
			      :b ifm))
	  (nmf-bc (make-line :x1 (/ (- ifm nmf-bc-b) nmf-bc-m)
			     :y1 ifm
			     :x2 (access nmf-params :c)
			     :y2 0
			     :m nmf-bc-m
			     :b nmf-bc-b)))
     (make-ifs-polygon :mf (list mf-ab mf-cut mf-bc)
		       :nmf (list nmf-ab nmf-cut nmf-bc))
     )))

(def line-intersection-winners
    "Checks if there's an intersection between `line1` and `line2`.
If there is one, the two lines that are above the other two are returned."
  :sig ((line line) list)
  :tests nil
  :body
  ((line1 line2)
   (let ((m1 (-> line1 :m))
	 (m2 (-> line2 :m)))
     (unless (= m1 m2)
       (let* ((x11 (-> line1 :x1))
	      (x12 (-> line1 :x2))
	      (x21 (-> line2 :x1))
	      (x22 (-> line2 :x2))

	      (y11 (-> line1 :y1))
	      (y12 (-> line1 :y2))
	      (y21 (-> line2 :y1))
	      (y22 (-> line2 :y2))
	      
	      (b1 (-> line1 :b))
	      (b2 (-> line2 :b))
	      (x (/ (- b2 b1)
		    (- m1 m2)))
	      (y (+ (* m1 x) b1)))
	 (unless (or ;; (> y (max y11 y12))
		     ;; (> y (max y21 y22))
		     (< x (min x11 x12))
		     (< x (min x21 x22))
		     (> x (max x11 x12))
		     (> x (max x21 x22))
		     (< y 0))
	   (cl:cond
	     ;; Both going up. Left side of plot.
	     ((and (> m1 0) (> m2 0))
	      (if (< m1 m2)
		  (list (make-line :x1 x11 :y1 y11 :x2 x :y2 y :m m1 :b b1)
			(make-line :x1 x :y1 y :x2 x22 :y2 y22 :m m2 :b b2))
		  (list (make-line :x1 x21 :y1 y21 :x2 x :y2 y :m m2 :b b2)
			(make-line :x1 x :y1 y :x2 x12 :y2 y12 :m m1 :b b1))))
	     ;; Both going down. Right side of plot.
	     ((and (< m1 0) (< m2 0))
	      (if (> m1 m2)
		  (list (make-line :x1 x21 :y1 y21 :x2 x :y2 y :m m2 :b b2)
			(make-line :x1 x :y1 y :x2 x12 :y2 y12 :m m1 :b b1))
		  (list (make-line :x1 x11 :y1 y11 :x2 x :y2 y :m m1 :b b1)
			(make-line :x1 x :y1 y :x2 x22 :y2 y22 :m m2 :b b2))))
	     ((= m1 0)
	      (if (> m2 m1)
		  (list (make-line :x1 x11 :y1 y11 :x2 x :y2 y :m m1 :b b1)
			(make-line :x1 x :y1 y :x2 x22 :y2 y22 :m m2 :b b2))
		  (list (make-line :x1 x21 :y1 y21 :x2 x :y2 y :m m2 :b b2)
			(make-line :x1 x :y1 y :x2 x12 :y2 y12 :m m1 :b b1))))
	     ((= m2 0)
	      (if (> m1 m2)
		  (list (make-line :x1 x21 :y1 y21 :x2 x :y2 y :m m2 :b b2)
			(make-line :x1 x :y1 y :x2 x12 :y2 y12 :m m1 :b b1))
		  (list (make-line :x1 x11 :y1 y11 :x2 x :y2 y :m m1 :b b1)
			(make-line :x1 x :y1 y :x2 x22 :y2 y22 :m m2 :b b2))))
	     (t
	      (if (< m1 m2)
		  (list (make-line :x1 x11 :y1 y11 :x2 x :y2 y :m m1 :b b1)
			(make-line :x1 x :y1 y :x2 x22 :y2 y22 :m m2 :b b2))
		  (list (make-line :x1 x21 :y1 y21 :x2 x :y2 y :m m2 :b b2)
			(make-line :x1 x :y1 y :x2 x12 :y2 y12 :m m1 :b b1))))
	     )
	   )
	 ))
     )))

;; (is-top-line #S(LINE :X1 1.1056376 :Y1 0.3 :X2 1.1064587 :Y2 0.3 :M 0 :B 0.3)
;; 	     #S(LINE :X1 1.1058104 :Y1 0 :X2 1.1068906 :Y2 0.5 :M 462.84528 :B -511.81912))

(def is-top-line
    "Checks if `line1` is above `line2`, if their domain overlaps."
  :sig ((line line) boolean)
  :tests nil
  :body
  ((line1 line2)
   (let ((l1x1 (-> line1 :x1))
	 (l1x2 (-> line1 :x2))
	 (l1y1 (-> line1 :y1))
	 (l1y2 (-> line1 :y2))

	 (l2x1 (-> line2 :x1))
	 (l2x2 (-> line2 :x2))
	 (l2y1 (-> line2 :y1))
	 (l2y2 (-> line2 :y2))

	 (l1m (-> line1 :m))
	 (l2m (-> line2 :m))
	 (l1b (-> line1 :b))
	 (l2b (-> line2 :b)))
     ;; (or (and (> l1x2 l2x1)
     ;; 	      (< l1x2 l2x2))
     ;; 	 (and (> l1x1 l2x1)
     ;; 	      (< l1x1 l2x2)))
     (when (or (and (<= l1x1 l2x1)
		  (<= l1x2 l2x1))
	     (and (>= l1x1 l2x2)
		  (>= l1x2 l2x2))
	     (or (and (<= (+ (* l1x1 l2m) l2b) l1y1)
		      (<= (+ (* l1x2 l2m) l2b) l1y2))
		 (and (<= l2y1 (+ (* l2x1 l1m) l1b))
		      (<= l2y2 (+ (* l2x2 l1m) l1b))))
	     (cl:equalp line1 line2)
	     (and (= l1m 0)
		  (<= l2y1 l1b)
		  (<= l2y2 l1b))
	     )
	 t))))

;; TODO: Refactorize all this.
(def ifunion
    "Creates an intuitionistic fuzzy set represented by a
`ifs-polygon` which is the intuitionistic fuzzy union of `ifs1` and
`ifs2`, which is also a `ifs-polygon`."
  :sig ((ifs-polygon ifs-polygon) ifs-polygon)
  :tests nil
  :body
  ((ifs1 ifs2)
   (let* ((mf1 (-> ifs1 :mf))
	  (mf2 (-> ifs2 :mf))
	  (nmf1 (-> ifs1 :nmf))
	  (nmf2 (-> ifs2 :nmf))
	  (mf-lines (sort (remove-duplicates
			   (alexandria:flatten
			    (loop
			       for line1 in mf1
			       with intersected
			       collect (loop for line2 in mf2
					  collect (let ((inter (line-intersection-winners line1 line2)))
						    (if inter
							(progn
							  (push line1 intersected)
							  (push line2 intersected)
							  inter)
							(list line1 line2))))
			       into lines
			       finally (return (remove-if (lambda (line)
							    (cl:position line intersected :test #'cl:equalp))
							  lines))))
			   :test #'cl:equalp)
			  #'<
			  :key (lambda (elt) (-> elt :x1))))
	  ;; (nmf-lines (sort (remove-duplicates
	  ;; 		    (alexandria:flatten
	  ;; 		     (loop
	  ;; 			for line1 in nmf1
	  ;; 			with intersected
	  ;; 			collect (loop for line2 in nmf2
	  ;; 				   collect (let ((inter (line-intersection-winners line1 line2)))
	  ;; 					     (if inter
	  ;; 						 (progn
	  ;; 						   (push line1 intersected)
	  ;; 						   (push line2 intersected)
	  ;; 						   inter)
	  ;; 						 (list line1 line2))))
	  ;; 			into lines
	  ;; 			finally (return (remove-if (lambda (line)
	  ;; 						     (cl:position line intersected :test #'cl:equalp))
	  ;; 						   lines))))
	  ;; 		    :test #'cl:equalp)
	  ;; 		   #'<
	  ;; 		   :key (lambda (elt) (-> elt :x1))))
	  )
     (let ((meow (remove nil
			 (loop for line1 in mf-lines
			    collect (when (cl:every #'cl:identity
						    (loop for line2 in mf-lines
						       collect (is-top-line line1 line2)))
				      line1)))))
       (make-ifs-polygon
      :mf meow
      :nmf meow))
     )))

(def non-membership-polygon
    "Returns `y` in `ifs-polygon`'s nmf associated to `x`."
  :sig ((number ifs-polygon) number)
  :tests nil
  :body
  ((x ifs)
   (let ((result 0))
     (loop for line in (-> ifs :nmf)
	do (when (and (>= x (-> line :x1))
		      (<= x (-> line :x2)))
	     (cl:setf result (* (+ (* (-> line :m) x) (-> line :b))))
	     (return)))
     result)))

(def if-coa
    "Returns the intuitionistic center of area of `ifs`."
  :sig ((ifs-polygon) number)
  :tests nil
  :body
  ((ifs)
   (let ((imf (alexandria:copy-sequence 'list (-> ifs :mf))))
     (loop for line in imf
	;; Affecting memberships with non-membership, i.e.
	;; obtaining intuitionistic memberhip.
	do (progn
	     (cl:setf (-> line :y1) (* (+ (-> line :y1)
					  (- 1 (non-membership-polygon (-> line :x1) ifs)))
				       (-> line :y1)))
	     (cl:setf (-> line :y2) (* (+ (-> line :y2)
					  (- 1 (non-membership-polygon (-> line :x2) ifs)))
				       (-> line :y2)))
	     ))
     (let* ((first-line (first imf))
	    (last-line (first (last imf)))
	    (closing-line (list (make-line :x1 (-> last-line :x2) :y1 (-> last-line :y2)
					   :x2 (-> first-line :x1) :y2 (-> first-line :y1)
					   :b 0d0 :m 0d0)))
	    (full-imf (append imf closing-line))
	    ;; Obtaining area.
	    (A (/ (loop for line in full-imf
		     summing (let ((x1 (-> line :x1))
				   (x2 (-> line :x2))
				   (y1 (-> line :y1))
				   (y2 (-> line :y2)))
			       (- (* x1 y2)
				  (* x2 y1))))
		  2))
	    (Cx (if (= A 0)
		    0
		    (/ (loop for line in full-imf
			  summing (let ((x1 (-> line :x1))
					(x2 (-> line :x2))
					(y1 (-> line :y1))
					(y2 (-> line :y2)))
				    (* (+ x1 x2)
					      (- (* x1 y2)
						 (* x2 y1)))))
		       (* 6 A))))
	    ;; Eh, I didn't need this, but I'll leave it here. Just in case.
	    ;; (Cy (/ (loop for line in full-imf
	    ;; 	    summing (* (+ y1 y2)
	    ;; 		       (- (* x1 y2)
	    ;; 			  (* x2 y1))))
	    ;; 	 (* 6 A)))
	    )
       ;; (make-coa :a A :cx Cx)
       (if (< Cx 0)
	   (* (abs (* A Cx)) -1)
	   (abs (* A Cx)))
       ))))

;; (cl:time (membership 0 (ifs (gaussian-mf 0 10 -10000 10000 0 1)
;; 			    (gaussian-nmf 0 10 -10000 10000 0 1))))

;; (for-mf-what-nmf 0.5 (ifs (gaussian-mf 0 10 0 1)
;; 			  (gaussian-nmf 0 10 0 1)))

;; (cl:declaim (optimize (debug 3)))

;; (cl:describe (new 'ifs))
;; (cl:slot-value {ifs '(1 2 3 4)} 'overmind-intuition::value)
;; (-> {ifs '(1 2 3 4)})

;; (def for-mf-what-nmf
;;     "Returns the grade of non-membership associated to a grade of membership `mf` in an intuitionistic fuzzy set `ifs`."
;;   :sig ((number ifs) number)
;;   :tests nil
;;   ;; ((capture (fn 0.5 (ifs (gaussian-mf 0 10 0 1)
;;   ;; 			 (gaussian-nmf 0 10 0 1)))
;;   ;; 	    (is (equal ret 0.52649546)))
;;   ;;  (capture (fn 0.7 (ifs (gaussian-mf 0 10 0 1)
;;   ;; 			 (gaussian-nmf 0 10 0 1)))
;;   ;; 	    (is (equal ret 0.36604637))))
;;   :body
;;   ((mf ifs)
;;    ;; (cl:break (cl:format nil "~a" (-> (first ifs))))
;;    ;; (cl:break (cl:format nil "meh ~a ~a~%$" (-> ifs) (cl:mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (cl:class-of (cl:make-instance 'ifs))))))
;;    ;; (cl:break (cl:format nil "meh~a~%" (cl:mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (cl:class-of (cl:make-instance 'ifs))))))
;;    (let* ((gifs (copy-tree (-> ifs)))
;; 	  (lifs (copy-tree (-> ifs)))
;; 	  (gsorted-ifs (sort gifs #'> :key (lambda (elt)
;; 					     (second elt))))
;; 	  (lsorted-ifs (sort lifs #'< :key (lambda (elt)
;; 					     (second elt)))))
;;      (if (>= mf (second (first gsorted-ifs)))
;; 	 (nth 2 (first gsorted-ifs))
;; 	 (if (<= mf 0)
;; 	     (nth 2 (first lsorted-ifs))
;; 	     (let* ((index (let ((try1 (remove-if-not (lambda (elt)
;; 							(> (nth 1 elt) mf))
;; 						      (-> ifs)))
;; 				 (try2 (remove-if-not (lambda (elt)
;; 							(< (nth 1 elt) mf))
;; 						      (-> ifs))))
;; 			     (if (= (first (first try1)) (first (first (-> ifs))))
;; 			     	 (first (first try2))
;; 			     	 (first (first try1)))
;; 			     ))
;; 		    ;; (fst (access:access (-> ifs) (- index 1)))
;; 		    ;; (lst (access:access (-> ifs) index))
;; 		    fst
;; 		    lst
;; 		    )
;; 	       ;; Searching for closest left element
;; 	       (cl:dolist (elt (cl:reverse (-> ifs)))
;; 	       	 (if (< (first elt) index)
;; 	       	     (cl:progn
;; 	       	       (cl:setf fst (cl:rest elt))
;; 	       	       (cl:return))
;; 	       	     (cl:setf lst (cl:rest elt))))
;; 	       ;; (cl:break "index: ~a, fst: ~a, lst: ~a" index fst lst)
;; 	       ;; (+ (nth 2 lst) (* (abs (- (nth 2 lst) (nth 2 fst)))
;; 	       ;; 			 (/ (- mf (nth 1 lst))
;; 	       ;; 			    (- (nth 1 fst)
;; 	       ;; 			       (nth 1 lst)))))
;; 	       ;; (+ (nth 1 lst) (* (abs (- (nth 1 lst) (nth 1 fst)))
;; 	       ;; 			 (/ (- mf (nth 0 lst))
;; 	       ;; 			    (- (nth 0 fst)
;; 	       ;; 			       (nth 0 lst)))))
;; 	       (+ (nth 1 fst) (abs (* (- (nth 1 lst) (nth 1 fst))
;; 				      (/ (- mf (nth 0 fst))
;; 					 (- (nth 0 fst)
;; 					    (nth 0 lst))))))
;; 	       ))))))

;; (for-mf-what-nmf 0.3 (ifs (gaussian-mf 0 10 0 100 0 1)
;; 			  (gaussian-nmf 0 10 0 100 0 1)))
;; (for-mf-what-nmf 0.5 (ifs (gaussian-mf 0 10 -100 100 0 1)
;; 			  (gaussian-nmf 0 10 -100 100 0 1)))

;; (-> (ifs (gaussian-mf 0 10 0 100 0 1)
;; 	 (gaussian-nmf 0 10 0 100 0 1)))
;; (-> (ifs (gaussian-mf 0 10 -100 100 0 1)
;; 	 (gaussian-nmf 0 10 -100 100 0 1)))

;; (def if-membership
;;     "Returns the intuitionistic fuzzy membership of `x` in the intuitionistic fuzzy set `ifs`."
;;   :sig ((number ifs) number)
;;   :tests nil
;;   ;; ((capture (fn 50 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			       (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 		   (is (equal ret 3.7266532e-6)))
;;   ;; 	  (capture (fn 20 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			       (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 		   (is (equal ret 0.13533528))))
;;   :body
;;   ((x ifs)
;;    (let ((m (membership x ifs)))
;;      (* (+ (nth 1 m) (nth 2 m))
;; 	(nth 1 m)))))

;; (def if-membership
;;     "Returns the intuitionistic fuzzy membership of `x` in the intuitionistic fuzzy set `ifs`."
;;   :sig ((number ifs) number)
;;   :tests nil
;;   :body
;;   ((x ifs)))

;; ;; (def if-membership
;; ;;     "Returns the intuitionistic fuzzy membership of `x` in the intuitionistic fuzzy set `ifs`."
;; ;;   :sig ((number ifs) number)
;; ;;   :tests nil
;; ;;   :body
;; ;;   ((x ifs)
;; ;;    (let ((m (membership x ifs)))
;; ;;      (- (nth 1 m)
;; ;;         (* (+ (nth 2 m) (nth 1 m))
;; ;; 	   (nth 2 m))))))

;; ;; (if-membership 45 (ifs (gaussian-mf 50 7 0 100 0 0.5)
;; ;; 		       (gaussian-nmf 60 7 0 100 0 1.0)))
;; ;; 0.387 0.1125

;; (def clip
;;     "Performs an alpha-cut at grade of membership `x` on the intuitionistic fuzzy set `ifs`."
;;   :sig ((number ifs) ifs)
;;   :tests nil
;;   ;; ((capture (fn 0.1 (ifs (gaussian-mf 50 10 0 100 0 0.5)
;;   ;; 				(gaussian-nmf 50 10 0 100 0 0.5)))
;;   ;; 		   (is (equal (nth 0 (-> ret)) '(0 0.0 0.5)))
;;   ;; 		   (is (equal (nth 1 (-> ret)) '(1 1.1934616e-6 0.4999988)))
;;   ;; 		   (is (equal (nth 99 (-> ret)) '(99 1.1934616e-6 0.4999988)))
;;   ;; 		   (is (equal (nth 100 (-> ret)) '(100 0.0 0.5)))))
;;   :body
;;   ((x ifs)
;;    {ifs (let ((nm-max (for-mf-what-nmf x ifs)))
;; 	  (if nm-max
;; 	      (mapcar #'list
;; 		      (mapcar #'first (-> ifs))
;; 		      (mapcar (lambda (elt)
;; 				(min x (nth 1 elt)))
;; 			      (-> ifs))
;; 		      (mapcar (lambda (elt)
;; 				(max nm-max (nth 2 elt)))
;; 			      (-> ifs)))))}))

;; ;; (-> (clip 0.1 (ifs (gaussian-mf 50 10 0 0.5)
;; ;; 		   (gaussian-nmf 50 10 0 0.5))))

;; (def rule
;;     "Returns an intuitionistic fuzzy set that represents the alpha-cut of the
;; `consequent` based on the activation of `predicate` by the input `x`."
;;   :sig ((number ifs ifs) ifs)
;;   :tests nil
;;   ;; ((capture (fn 10 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			       (gaussian-nmf 0 10 0 100 0 1))
;;   ;; 		       (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			    (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 		   (is (equal (nth 0 (-> ret)) '(0 0.60653067 0.5143819)))
;;   ;; 		   (is (equal (nth 1 (-> ret)) '(1 0.60653067 0.5143819)))
;;   ;; 		   (is (equal (nth 99 (-> ret)) '(99 3.2879808e-22 1.0)))
;;   ;; 		   (is (equal (nth 100 (-> ret)) '(100 0.0 1.0)))))
;;   :body
;;   ((x predicate consequent)
;;    (clip (if-membership x predicate)
;; 	 consequent)))

;; ;; (-> (rule 10 (ifs (gaussian-mf 0 10 0 1)
;; ;; 		  (gaussian-nmf 0 10 0 1))
;; ;; 	  (ifs (gaussian-mf 0 10 0 1)
;; ;; 	       (gaussian-nmf 0 10 0 1))))

;; (def coa
;;     "Returns the centroid of an intuitionistic fuzzy set, which should be the
;; aggregation of different alpha-cut consequents. This function does not consider
;; the non-membership function, i.e. it represents a traditional center of area of
;; a fuzzy set."
;;   :sig ((ifs) number)
;;   :tests nil
;;   ;; ((capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
;;   ;; 			    (gaussian-nmf 70 20 0 100 0 1)))
;;   ;; 		   (is (equal ret 49.99998)))
;;   ;; 	  (capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
;;   ;; 			    (gaussian-nmf 40 50 0 100 0 1)))
;;   ;; 		   (is (equal ret 49.99998)))
;;   ;; 	  (capture (fn (ifs (gaussian-mf 30 50 0 100 0 1)
;;   ;; 			    (gaussian-nmf 70 20 0 100 0 1)))
;;   ;; 		   (is (equal ret 39.011227))))
;;   :body
;;   ((ifs)
;;    (% (cl:reduce #'+ (mapcar (lambda (elt)
;; 			       (* (nth 1 elt)
;; 				  (nth 0 elt)))
;; 			     (-> ifs)))
;;       (cl:reduce #'+ (mapcar (lambda (elt)
;; 			       (nth 1 elt))
;; 			     (-> ifs))))))

;; ;; (coa (ifs (gaussian-mf 30 50 0 1)
;; ;; 	  (gaussian-nmf 40 20 0 1)))

;; (def if-coa
;;     "Returns the centroid of an intuitionistic fuzzy set, which should be the
;; aggregation of different alpha-cut consequents. This function considers the
;; non-membership function of the intuitionistic fuzzy set."
;;   :sig ((ifs) number)
;;   :tests nil
;;   ;; ((capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
;;   ;; 			    (gaussian-nmf 70 20 0 100 0 1)))
;;   ;; 		   (is (equal ret 45.462933)))
;;   ;; 	  (capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
;;   ;; 			    (gaussian-nmf 20 50 0 100 0 1)))
;;   ;; 		   (is (equal ret 53.854233)))
;;   ;; 	  (capture (fn (ifs (gaussian-mf 30 50 0 100 0 1)
;;   ;; 			    (gaussian-nmf 20 20 0 100 0 1)))
;;   ;; 		   (is (equal ret 42.296585))))
;;   :body
;;   ((ifs)
;;    (% (cl:reduce #'+ (mapcar (lambda (elt)
;; 			       (* (+ (nth 2 elt)
;; 				     (nth 1 elt))
;; 				  (nth 1 elt)
;; 				  (nth 0 elt)))
;; 			     (-> ifs)))
;;       (cl:reduce #'+ (mapcar (lambda (elt)
;; 			       (* (+ (nth 2 elt)
;; 				     (nth 1 elt))
;; 				  (nth 1 elt)))
;; 			     (-> ifs))))))

;; (if-coa (ifs (gaussian-mf 50 20 0 1)
;; 	     (gaussian-nmf 20 50 0 1)))





;; ;; (defn gaussian [mean spread from to]
;; ;;   (ifs (gaussian-mf mean spread from to)
;; ;;        (gaussian-nmf mean spread from to)))

;; (usables '((number number number number) ifs))

;; ;; (defn triangular-mf [a b c from to]
;; ;;   (map #(vector (double %1) %2)
;; ;;        (range 0 101)
;; ;;        (scale (map (fn [x]
;; ;;                      (double (if (<= x a)
;; ;;                                0
;; ;;                                (if (and (<= a x) (<= x b))
;; ;;                                  (/ (- x a)
;; ;;                                     (- b a))
;; ;;                                  (if (and (<= b x) (<= x c))
;; ;;                                    (/ (- c x)
;; ;;                                       (- c b))
;; ;;                                    0)))))
;; ;;                    (range 0 101)) from to)))

;; ;; (defn triangular-nmf [a b c from to]
;; ;;   (map #(vector (double %1) %2)
;; ;;        (range 0 101)
;; ;;        (scale
;; ;;         (map #(- 1 %)
;; ;;              (map (fn [x]
;; ;;                     (double (if (<= x a)
;; ;;                               0
;; ;;                               (if (and (<= a x) (<= x b))
;; ;;                                 (/ (- x a)
;; ;;                                    (- b a))
;; ;;                                 (if (and (<= b x) (<= x c))
;; ;;                                   (/ (- c x)
;; ;;                                      (- c b))
;; ;;                                   0)))))
;; ;;                   (range 0 101))) from to)))

;; ;; (defn triangular [a b c from to]
;; ;;   (ifs (triangular-mf a b c from to)
;; ;;        (triangular-nmf a b c from to)))

;; ;; (defn trapezoidal-mf [a b c d from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale
;; ;;         (map (fn [x]
;; ;;                (max (min (/ (- x a) (- b a)) 1 (/ (- d x) (- d c))) 0))
;; ;;              (range 0 101))
;; ;;         from to)))

;; ;; (defn trapezoidal-nmf [a b c d from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale
;; ;;         (map #(- 1 %)
;; ;;              (map (fn [x]
;; ;;                     (max (min (/ (- x a) (- b a)) 1 (/ (- d x) (- d c))) 0))
;; ;;                   (range 0 101)))
;; ;;         from to)))

;; ;; (defn trapezoidal [a b c d from to]
;; ;;   (ifs (trapezoidal-mf a b c d from to)
;; ;;        (trapezoidal-nmf a b c d from to)))

;; ;; (defn gbell-mf [a b c from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale (map (fn [x]
;; ;;                      (/ 1 (+ 1 (Math/pow (abs (/ (- x c) a)) (* 2 b)))))
;; ;;                    (range 0 101))
;; ;;               from to)))

;; ;; (defn gbell-nmf [a b c from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale (map #(- 1 %)
;; ;;                    (map (fn [x]
;; ;;                           (/ 1 (+ 1 (Math/pow (abs (/ (- x c) a)) (* 2 b)))))
;; ;;                         (range 0 101)))
;; ;;               from to)))

;; ;; (defn gbell [a b c from to]
;; ;;   (ifs (gbell-mf a b c from to)
;; ;;        (gbell-nmf a b c from to)))

;; ;; (defn sigmoidal-mf [a b from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale (map (fn [x]
;; ;;                      (/ 1 (+ 1 (exp (* (- a) (- x b)))))
;; ;;                      )
;; ;;                    (range 0 101))
;; ;;               from to)))

;; ;; (defn sigmoidal-nmf [a b from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale (map #(- 1 %)
;; ;;                    (map (fn [x]
;; ;;                           (/ 1 (+ 1 (exp (* (- a) (- x b)))))
;; ;;                           )
;; ;;                         (range 0 101)))
;; ;;               from to)))

;; ;; (defn sigmoidal [a b from to]
;; ;;   (ifs (sigmoidal-mf a b from to)
;; ;;        (sigmoidal-nmf a b from to)))

;; ;; (defn left-right-mf [a b c from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale (map (fn [x]
;; ;;                      (if (<= x c)
;; ;;                        (let [v (sqrt (- 1 (Math/pow (/ (- c x) a) 2)))]
;; ;;                               (if (Double/isNaN v)
;; ;;                                 0
;; ;;                                 v))
;; ;;                        (exp (- (Math/pow (abs (/ (- x c) b)) 3)))
;; ;;                        ))
;; ;;                    (range 0 101))
;; ;;               from to)))

;; ;; (defn left-right-nmf [a b c from to]
;; ;;   (map #(vector (double %1) (double %2))
;; ;;        (range 0 101)
;; ;;        (scale (map #(- 1 %)
;; ;;                    (map (fn [x]
;; ;;                           (if (<= x c)
;; ;;                             (let [v (sqrt (- 1 (Math/pow (/ (- c x) a) 2)))]
;; ;;                               (if (Double/isNaN v)
;; ;;                                 0
;; ;;                                 v))
;; ;;                             (exp (- (Math/pow (abs (/ (- x c) b)) 3)))
;; ;;                             ))
;; ;;                         (range 0 101)))
;; ;;               from to)))

;; ;; (defn left-right [a b c from to]
;; ;;   (ifs (left-right-mf a b c from to)
;; ;;        (left-right-nmf a b c from to)))
