(defpackage overmind-intuition
  (:use :overmind-code :overmind-intuition.classes)
  (:export :%
	   :scale
	   :gaussian-mf
	   :gaussian-nmf
	   :ifs
	   :ifunion
	   :ifintersection
	   :membership
	   :if-membership
	   :for-mf-what-nmf
	   :clip
	   :rule
	   :coa
	   :if-coa))
(in-package :overmind-intuition)

(def %
    "Performs a \"safe\" division, where (/ `num` `den`) == `num` if `den` == 0."
  :sig ((number number) number)
  :tests
  ((capture (fn 50 0) (is (= ret 50)))
   (capture (fn 30 0) (is (= ret 30)))
   (capture (fn 30 2) (is (= ret 15))))
  :body
  ((num den)
   (if (= den 0)
       num
       (/ num den))))

(def scale
    "Scales `lst` from a new minimum `from` to a new maximum `to`."
  :sig ((list number number) list)
  :tests nil
  ;; ((capture (fn (list 1 2 3 5)
  ;; 		5 10)
  ;; 	    (is (= (first ret) 5))))
  :body
  ((lst from to)
   (let ((mx (apply #'max lst))
	 (mn (apply #'min lst)))
     (mapcar (lambda (x)
	       (if (= (- mx mn) 0)
		   0
		   (+ (* from (- 1 (/ (- x mn) (- mx mn))))
		      (* to (/ (- x mn) (- mx mn)))))
	       )
	     lst))))

;; (scale (range 100) 50 70)

;; membership functions

(def gaussian-mf
    "Creates a Gaussian membership function with mean `mean`, a standard
deviation `stdev`, where grades of membership start at `from` and has a core at
`to`."
  :sig ((number number number number number number) mf)
  :tests ((is (= 5 5)))
  ;; ((capture (fn 50 10 0 100 0 0.5)
  ;; 	    (is (equal (first (-> ret)) '(0 0.0)))
  ;; 	    (is (equal (second (-> ret)) '(1 1.1934616e-6)))
  ;; 	    (is (= (length (-> ret)) 101))))
  :body
  ((mean stdev from-x to-x from-y to-y)
   {mf (mapcar (lambda (x gm)
		 (list x gm))
	       (range (ceiling (+ 1 to-x)) :min (floor from-x))
	       (scale (mapcar (lambda (gm)
				(exp (* -0.5 (expt (/ (- gm mean) stdev) 2))))
			      (range (ceiling (+ 1 to-x)) :min (floor from-x) :step 10))
		      from-y to-y))}))

(def gaussian-mf
    "Creates a Gaussian membership function with mean `mean`, a standard
deviation `stdev`, where grades of membership start at `from` and has a core at
`to`."
  :sig ((number number number number number number) mf)
  :tests ((is (= 5 5)))
  ;; ((capture (fn 50 10 0 100 0 0.5)
  ;; 	    (is (equal (first (-> ret)) '(0 0.0)))
  ;; 	    (is (equal (second (-> ret)) '(1 1.1934616e-6)))
  ;; 	    (is (= (length (-> ret)) 101))))
  :body
  ((mean stdev from-x to-x from-y to-y)
   {mf (mapcar (lambda (x gm)
		 (list x gm))
	       (alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100))
	       (scale (mapcar (lambda (gm)
				(exp (* -0.5 (expt (/ (- gm mean) stdev) 2))))
			      (alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100)))
		      from-y to-y))}))

;; (usables '(mf))

(def gaussian-nmf
    "Creates a Gaussian non-membership function with mean `mean`, a standard
deviation `stdev`, where grades of membership start at `from` and has a core at
`to`."
  :sig ((number number number number number number) nmf)
  :tests ((is (= 5 5)))
  ;; ((capture (fn 50 10 0 100 0 0.5)
  ;; 	    (is (equal (first (-> ret)) '(0 0.5)))
  ;; 	    (is (equal (second (-> ret)) '(1 0.4999988)))
  ;; 	    (is (= (length (-> ret)) 101))))
  :body
  ((mean stdev from-x to-x from-y to-y)
   {nmf (mapcar (lambda (x gm)
		  (list x gm))
		;; (range (ceiling (+ 1 to-x)) :min (floor from-x))
		(alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100))
		(scale (mapcar (lambda (x)
				 (- 1 x))
			       (mapcar (lambda (gm)
					 (exp (* -0.5 (expt (/ (- gm mean) stdev) 2))))
				       ;; (range (ceiling (+ 1 to-x)) :min (floor from-x) :step 1)
				       (alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100))
				       ))
		       from-y to-y))}))

;; (-> (gaussian-mf 50 10 0 100 0 0.5))
;; (-> (gaussian-nmf 50 10 -100 100 0 0.5))

(def ifs
    "Creates an intuitionistif fuzzy set using a membership function `mf` and a
non-membership function `nmf`."
  :sig ((mf nmf) ifs)
  :tests nil
  ;; ((capture (fn (gaussian-mf 50 10 0 100 0 0.5)
  ;; 		(gaussian-nmf 50 10 0 100 0 0.5))
  ;; 	    (is (equal (first (-> ret)) '(0 0.0 0.5)))
  ;; 	    (is (equal (second (-> ret)) '(1 1.1934616e-6 0.4999988)))))
  :body
  ((memf nmemf)
   {ifs (mapcar (lambda (m n)
		  (list (first m) (second m) (second n)))
		(-> memf) (-> nmemf))}))

;; (-> (ifs (gaussian-mf 50 10 0 0.5)
;; 	 (gaussian-nmf 50 10 0 0.5)))

;; operators

(def ifunion
    "Creates an intuitionistif fuzzy set which is the intuitionistic fuzzy union of `ifs1` and `ifs2`."
  :sig ((ifs ifs) ifs)
  :tests nil
  ;; ((capture (fn (ifs (gaussian-mf 50 10 0 100 0 0.5)
  ;; 		     (gaussian-nmf 50 10 0 100 0 0.5))
  ;; 		(ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 		     (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal (nth 0 (-> ret)) '(0 1.0 0.0)))
  ;; 	    (is (equal (nth 1 (-> ret)) '(1 0.99501246 0.004987538)))
  ;; 	    (is (equal (nth 99 (-> ret)) '(99 1.1934616e-6 0.4999988)))
  ;; 	    (is (equal (nth 100 (-> ret)) '(100 0.0 0.5)))))
  :body
  ((ifs1 ifs2)
   {ifs (mapcar (lambda (elt1 elt2)
		  (list (nth 0 elt1)
		  	(max (nth 1 elt1) (nth 1 elt2))
		  	(min (nth 2 elt1) (nth 2 elt2))))
		(-> ifs1)
		(-> ifs2))}))

;; (-> (ifunion (ifs (gaussian-mf 50 10 0 100 0 0.5)
;; 		(gaussian-nmf 50 10 0 100 0 0.5))
;; 	   (ifs (gaussian-mf 0 10 0 100 0 1)
;; 		(gaussian-nmf 0 10 0 100 0 1))))

(def ifintersection
    "Creates an intuitionistif fuzzy set which is the intuitionistic fuzzy intersection of `ifs1` and
`ifs2`."
  :sig ((ifs ifs) ifs)
  :tests nil
  ;; ((capture (fn (ifs (gaussian-mf 50 10 0 100 0 0.5)
  ;; 		     (gaussian-nmf 50 10 0 100 0 0.5))
  ;; 		(ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 		     (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal (nth 0 (-> ret)) '(0 0.0 0.5)))
  ;; 	    (is (equal (nth 1 (-> ret)) '(1 1.1934616e-6 0.4999988)))
  ;; 	    (is (equal (nth 99 (-> ret)) '(99 3.2879808e-22 1.0)))
  ;; 	    (is (equal (nth 100 (-> ret)) '(100 0.0 1.0)))))
  :body
  ((ifs1 ifs2)
   {ifs (mapcar (lambda (elt1 elt2)
		  (list (nth 0 elt1)
			(min (nth 1 elt1) (nth 1 elt2))
		  	(max (nth 2 elt1) (nth 2 elt2))))
		(-> ifs1)
		(-> ifs2))}))

;; (-> (ifintersection (ifs (gaussian-mf 50 10 0 0.5)
;; 			     (gaussian-nmf 50 10 0 0.5))
;; 			(ifs (gaussian-mf 0 10 0 1)
;; 			     (gaussian-nmf 0 10 0 1))))

(def membership
    "Returns the grade of membership associated to `x` in `ifs`"
  :sig ((number ifs) number)
  :tests nil
  ;; ((capture (fn 0 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(0 1.0 0.0))))
  ;;  (capture (fn 1 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(1 0.99501246 0.004987538))))
  ;;  (capture (fn 99 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			(gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(99 3.2879808e-22 1.0))))
  ;;  (capture (fn 100 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			 (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(100 0.0 1.0)))))
  :body
  ((x ifs)
   (if (>= x ;; (- (length (-> ifs)) 1)
	   (first (first (last (-> ifs))))
	   )
       (first (last (-> ifs)))
       (if (< x (first (first (-> ifs))))
	   (first (-> ifs))
	   (let* ((index (floor x))
		  (fraction (- x index)))
	     (mapcar #'+ ;; (nth index (-> ifs))
		     (cl:append (list index) (access:access (-> ifs) index))
		     (mapcar (lambda (diff)
			       (* fraction diff))
			     (mapcar #'- ;; (nth (+ index 1) (-> ifs))
				     (cl:append (list (+ index 1)) (access:access (-> ifs) (+ index 1)))
				     (cl:append (list index) (access:access (-> ifs) index))
				     ;; (nth index (-> ifs))
				     ))))))))

(def membership
    "Returns the grade of membership associated to `x` in `ifs`"
  :sig ((number ifs) number)
  :tests nil
  ;; ((capture (fn 0 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(0 1.0 0.0))))
  ;;  (capture (fn 1 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(1 0.99501246 0.004987538))))
  ;;  (capture (fn 99 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			(gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(99 3.2879808e-22 1.0))))
  ;;  (capture (fn 100 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			 (gaussian-nmf 0 10 0 100 0 1)))
  ;; 	    (is (equal ret '(100 0.0 1.0)))))
  :body
  ((x ifs)
   (if (>= x ;; (- (length (-> ifs)) 1)
	   (first (first (last (-> ifs))))
	   )
       (first (last (-> ifs)))
       (if (< x (first (first (-> ifs))))
	   (first (-> ifs))
	   (let* ((index x)
		  (fraction (- x index))
		  lelt
		  relt)
	     ;; Searching for closest left element
	     (cl:dolist (elt (-> ifs))
	       (if (> (first elt) index)
		   (cl:progn
		    (cl:setf relt elt)
		    (cl:return))
		   (cl:setf lelt elt)))
             (cl:append (list x)
			(mapcar #'+
				(cl:rest lelt)
				(cl:rest
				 (mapcar (lambda (diff)
					   (* fraction diff))
					 (mapcar #'- relt lelt)))))
	     )))))

;; (cl:time (membership 0 (ifs (gaussian-mf 0 10 -10000 10000 0 1)
;; 			    (gaussian-nmf 0 10 -10000 10000 0 1))))

;; (for-mf-what-nmf 0.5 (ifs (gaussian-mf 0 10 0 1)
;; 			  (gaussian-nmf 0 10 0 1)))

;; (cl:declaim (optimize (debug 3)))

;; (cl:describe (new 'ifs))
;; (cl:slot-value {ifs '(1 2 3 4)} 'overmind-intuition::value)
;; (-> {ifs '(1 2 3 4)})

(def for-mf-what-nmf
    "Returns the grade of non-membership associated to a grade of membership `mf` in an intuitionistic fuzzy set `ifs`."
  :sig ((number ifs) number)
  :tests nil
  ;; ((capture (fn 0.5 (ifs (gaussian-mf 0 10 0 1)
  ;; 			 (gaussian-nmf 0 10 0 1)))
  ;; 	    (is (equal ret 0.52649546)))
  ;;  (capture (fn 0.7 (ifs (gaussian-mf 0 10 0 1)
  ;; 			 (gaussian-nmf 0 10 0 1)))
  ;; 	    (is (equal ret 0.36604637))))
  :body
  ((mf ifs)
   ;; (cl:break (cl:format nil "~a" (-> (first ifs))))
   ;; (cl:break (cl:format nil "meh ~a ~a~%$" (-> ifs) (cl:mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (cl:class-of (cl:make-instance 'ifs))))))
   ;; (cl:break (cl:format nil "meh~a~%" (cl:mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (cl:class-of (cl:make-instance 'ifs))))))
   (let* ((gifs (copy-tree (-> ifs)))
	  (lifs (copy-tree (-> ifs)))
	  (gsorted-ifs (sort gifs #'> :key (lambda (elt)
					     (second elt))))
	  (lsorted-ifs (sort lifs #'< :key (lambda (elt)
					     (second elt)))))
     (if (>= mf (second (first gsorted-ifs)))
	 (nth 2 (first gsorted-ifs))
	 (if (<= mf 0)
	     (nth 2 (first lsorted-ifs))
	     (let* ((index (let ((try1 (remove-if-not (lambda (elt)
							(> (nth 1 elt) mf))
						      (-> ifs)))
				 (try2 (remove-if-not (lambda (elt)
							(< (nth 1 elt) mf))
						      (-> ifs))))
			     (if (= (first (first try1)) (first (first (-> ifs))))
			     	 (first (first try2))
			     	 (first (first try1)))
			     ))
		    ;; (fst (access:access (-> ifs) (- index 1)))
		    ;; (lst (access:access (-> ifs) index))
		    fst
		    lst
		    )
	       ;; Searching for closest left element
	       (cl:dolist (elt (cl:reverse (-> ifs)))
	       	 (if (< (first elt) index)
	       	     (cl:progn
	       	       (cl:setf fst (cl:rest elt))
	       	       (cl:return))
	       	     (cl:setf lst (cl:rest elt))))
	       ;; (cl:break "index: ~a, fst: ~a, lst: ~a" index fst lst)
	       ;; (+ (nth 2 lst) (* (abs (- (nth 2 lst) (nth 2 fst)))
	       ;; 			 (/ (- mf (nth 1 lst))
	       ;; 			    (- (nth 1 fst)
	       ;; 			       (nth 1 lst)))))
	       ;; (+ (nth 1 lst) (* (abs (- (nth 1 lst) (nth 1 fst)))
	       ;; 			 (/ (- mf (nth 0 lst))
	       ;; 			    (- (nth 0 fst)
	       ;; 			       (nth 0 lst)))))
	       (+ (nth 1 fst) (abs (* (- (nth 1 lst) (nth 1 fst))
				      (/ (- mf (nth 0 fst))
					 (- (nth 0 fst)
					    (nth 0 lst))))))
	       ))))))

;; (for-mf-what-nmf 0.3 (ifs (gaussian-mf 0 10 0 100 0 1)
;; 			  (gaussian-nmf 0 10 0 100 0 1)))
;; (for-mf-what-nmf 0.5 (ifs (gaussian-mf 0 10 -100 100 0 1)
;; 			  (gaussian-nmf 0 10 -100 100 0 1)))

;; (-> (ifs (gaussian-mf 0 10 0 100 0 1)
;; 	 (gaussian-nmf 0 10 0 100 0 1)))
;; (-> (ifs (gaussian-mf 0 10 -100 100 0 1)
;; 	 (gaussian-nmf 0 10 -100 100 0 1)))

(def if-membership
    "Returns the intuitionistic fuzzy membership of `x` in the intuitionistic fuzzy set `ifs`."
  :sig ((number ifs) number)
  :tests nil
  ;; ((capture (fn 50 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			       (gaussian-nmf 0 10 0 100 0 1)))
  ;; 		   (is (equal ret 3.7266532e-6)))
  ;; 	  (capture (fn 20 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			       (gaussian-nmf 0 10 0 100 0 1)))
  ;; 		   (is (equal ret 0.13533528))))
  :body
  ((x ifs)
   (let ((m (membership x ifs)))
     (* (+ (nth 1 m) (nth 2 m))
	(nth 1 m)))))

;; (def if-membership
;;     "Returns the intuitionistic fuzzy membership of `x` in the intuitionistic fuzzy set `ifs`."
;;   :sig ((number ifs) number)
;;   :tests nil
;;   :body
;;   ((x ifs)
;;    (let ((m (membership x ifs)))
;;      (- (nth 1 m)
;;         (* (+ (nth 2 m) (nth 1 m))
;; 	   (nth 2 m))))))

;; (if-membership 45 (ifs (gaussian-mf 50 7 0 100 0 0.5)
;; 		       (gaussian-nmf 60 7 0 100 0 1.0)))
;; 0.387 0.1125

(def clip
    "Performs an alpha-cut at grade of membership `x` on the intuitionistic fuzzy set `ifs`."
  :sig ((number ifs) ifs)
  :tests nil
  ;; ((capture (fn 0.1 (ifs (gaussian-mf 50 10 0 100 0 0.5)
  ;; 				(gaussian-nmf 50 10 0 100 0 0.5)))
  ;; 		   (is (equal (nth 0 (-> ret)) '(0 0.0 0.5)))
  ;; 		   (is (equal (nth 1 (-> ret)) '(1 1.1934616e-6 0.4999988)))
  ;; 		   (is (equal (nth 99 (-> ret)) '(99 1.1934616e-6 0.4999988)))
  ;; 		   (is (equal (nth 100 (-> ret)) '(100 0.0 0.5)))))
  :body
  ((x ifs)
   {ifs (let ((nm-max (for-mf-what-nmf x ifs)))
	  (if nm-max
	      (mapcar #'list
		      (mapcar #'first (-> ifs))
		      (mapcar (lambda (elt)
				(min x (nth 1 elt)))
			      (-> ifs))
		      (mapcar (lambda (elt)
				(max nm-max (nth 2 elt)))
			      (-> ifs)))))}))

;; (-> (clip 0.1 (ifs (gaussian-mf 50 10 0 0.5)
;; 		   (gaussian-nmf 50 10 0 0.5))))

(def rule
    "Returns an intuitionistic fuzzy set that represents the alpha-cut of the
`consequent` based on the activation of `predicate` by the input `x`."
  :sig ((number ifs ifs) ifs)
  :tests nil
  ;; ((capture (fn 10 (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			       (gaussian-nmf 0 10 0 100 0 1))
  ;; 		       (ifs (gaussian-mf 0 10 0 100 0 1)
  ;; 			    (gaussian-nmf 0 10 0 100 0 1)))
  ;; 		   (is (equal (nth 0 (-> ret)) '(0 0.60653067 0.5143819)))
  ;; 		   (is (equal (nth 1 (-> ret)) '(1 0.60653067 0.5143819)))
  ;; 		   (is (equal (nth 99 (-> ret)) '(99 3.2879808e-22 1.0)))
  ;; 		   (is (equal (nth 100 (-> ret)) '(100 0.0 1.0)))))
  :body
  ((x predicate consequent)
   (clip (if-membership x predicate)
	 consequent)))

;; (-> (rule 10 (ifs (gaussian-mf 0 10 0 1)
;; 		  (gaussian-nmf 0 10 0 1))
;; 	  (ifs (gaussian-mf 0 10 0 1)
;; 	       (gaussian-nmf 0 10 0 1))))

(def coa
    "Returns the centroid of an intuitionistic fuzzy set, which should be the
aggregation of different alpha-cut consequents. This function does not consider
the non-membership function, i.e. it represents a traditional center of area of
a fuzzy set."
  :sig ((ifs) number)
  :tests nil
  ;; ((capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
  ;; 			    (gaussian-nmf 70 20 0 100 0 1)))
  ;; 		   (is (equal ret 49.99998)))
  ;; 	  (capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
  ;; 			    (gaussian-nmf 40 50 0 100 0 1)))
  ;; 		   (is (equal ret 49.99998)))
  ;; 	  (capture (fn (ifs (gaussian-mf 30 50 0 100 0 1)
  ;; 			    (gaussian-nmf 70 20 0 100 0 1)))
  ;; 		   (is (equal ret 39.011227))))
  :body
  ((ifs)
   (% (cl:reduce #'+ (mapcar (lambda (elt)
			       (* (nth 1 elt)
				  (nth 0 elt)))
			     (-> ifs)))
      (cl:reduce #'+ (mapcar (lambda (elt)
			       (nth 1 elt))
			     (-> ifs))))))

;; (coa (ifs (gaussian-mf 30 50 0 1)
;; 	  (gaussian-nmf 40 20 0 1)))

(def if-coa
    "Returns the centroid of an intuitionistic fuzzy set, which should be the
aggregation of different alpha-cut consequents. This function considers the
non-membership function of the intuitionistic fuzzy set."
  :sig ((ifs) number)
  :tests nil
  ;; ((capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
  ;; 			    (gaussian-nmf 70 20 0 100 0 1)))
  ;; 		   (is (equal ret 45.462933)))
  ;; 	  (capture (fn (ifs (gaussian-mf 50 20 0 100 0 1)
  ;; 			    (gaussian-nmf 20 50 0 100 0 1)))
  ;; 		   (is (equal ret 53.854233)))
  ;; 	  (capture (fn (ifs (gaussian-mf 30 50 0 100 0 1)
  ;; 			    (gaussian-nmf 20 20 0 100 0 1)))
  ;; 		   (is (equal ret 42.296585))))
  :body
  ((ifs)
   (% (cl:reduce #'+ (mapcar (lambda (elt)
			       (* (+ (nth 2 elt)
				     (nth 1 elt))
				  (nth 1 elt)
				  (nth 0 elt)))
			     (-> ifs)))
      (cl:reduce #'+ (mapcar (lambda (elt)
			       (* (+ (nth 2 elt)
				     (nth 1 elt))
				  (nth 1 elt)))
			     (-> ifs))))))

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
