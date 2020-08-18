;; (ql:quickload :overmind-intuition)
(defpackage overmind-intuition
  (:use :overmind-code)
  (:export :%
	   :scale
	   :gaussian-mf
	   :gaussian-nmf
	   :ifs
	   :ifunion
	   :ifintersection
	   :membership
	   :non-membership-polygon
	   :if-membership
	   :for-mf-what-nmf
	   :clip
	   :fire-rule
	   :alpha-cut
	   :coa
	   :if-coa))
(in-package :overmind-intuition)

(cl:setf cl:*read-default-float-format* 'cl:double-float)

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

;; (def gaussian-mf
;;     "Creates a Gaussian membership function with mean `mean`, a standard
;; deviation `stdev`, where grades of membership start at `from` and has a core at
;; `to`."
;;   :sig ((number number number number number number) mf)
;;   :tests ((is (= 5 5)))
;;   ;; ((capture (fn 50 10 0 100 0 0.5)
;;   ;; 	    (is (equal (first (-> ret)) '(0 0.0)))
;;   ;; 	    (is (equal (second (-> ret)) '(1 1.1934616e-6)))
;;   ;; 	    (is (= (length (-> ret)) 101))))
;;   :body
;;   ((mean stdev from-x to-x from-y to-y)
;;    {mf (mapcar (lambda (x gm)
;; 		 (list x gm))
;; 	       (range (ceiling (+ 1 to-x)) :min (floor from-x))
;; 	       (scale (mapcar (lambda (gm)
;; 				(exp (* -0.5 (expt (/ (- gm mean) stdev) 2))))
;; 			      (range (ceiling (+ 1 to-x)) :min (floor from-x) :step 10))
;; 		      from-y to-y))}))

;; (def gaussian-mf
;;     "Creates a Gaussian membership function with mean `mean`, a standard
;; deviation `stdev`, where grades of membership start at `from` and has a core at
;; `to`."
;;   :sig ((number number number number number number) mf)
;;   :tests ((is (= 5 5)))
;;   ;; ((capture (fn 50 10 0 100 0 0.5)
;;   ;; 	    (is (equal (first (-> ret)) '(0 0.0)))
;;   ;; 	    (is (equal (second (-> ret)) '(1 1.1934616e-6)))
;;   ;; 	    (is (= (length (-> ret)) 101))))
;;   :body
;;   ((mean stdev from-x to-x from-y to-y)
;;    {mf (mapcar (lambda (x gm)
;; 		 (list x gm))
;; 	       (alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100))
;; 	       (scale (mapcar (lambda (gm)
;; 				(exp (* -0.5 (expt (/ (- gm mean) stdev) 2))))
;; 			      (alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100)))
;; 		      from-y to-y))}))

;; ;; (usables '(mf))

;; (def gaussian-nmf
;;     "Creates a Gaussian non-membership function with mean `mean`, a standard
;; deviation `stdev`, where grades of membership start at `from` and has a core at
;; `to`."
;;   :sig ((number number number number number number) nmf)
;;   :tests ((is (= 5 5)))
;;   ;; ((capture (fn 50 10 0 100 0 0.5)
;;   ;; 	    (is (equal (first (-> ret)) '(0 0.5)))
;;   ;; 	    (is (equal (second (-> ret)) '(1 0.4999988)))
;;   ;; 	    (is (= (length (-> ret)) 101))))
;;   :body
;;   ((mean stdev from-x to-x from-y to-y)
;;    {nmf (mapcar (lambda (x gm)
;; 		  (list x gm))
;; 		;; (range (ceiling (+ 1 to-x)) :min (floor from-x))
;; 		(alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100))
;; 		(scale (mapcar (lambda (x)
;; 				 (- 1 x))
;; 			       (mapcar (lambda (gm)
;; 					 (exp (* -0.5 (expt (/ (- gm mean) stdev) 2))))
;; 				       ;; (range (ceiling (+ 1 to-x)) :min (floor from-x) :step 1)
;; 				       (alexandria:iota 101 :start from-x :step (/ (abs (- from-x to-x)) 100))
;; 				       ))
;; 		       from-y to-y))}))

;; (-> (gaussian-mf 50 10 0 100 0 0.5))
;; (-> (gaussian-nmf 50 10 -100 100 0 0.5))

;; (def ifs
;;     "Creates an intuitionistif fuzzy set using a membership function `mf` and a
;; non-membership function `nmf`."
;;   :sig ((mf nmf) ifs)
;;   :tests nil
;;   ;; ((capture (fn (gaussian-mf 50 10 0 100 0 0.5)
;;   ;; 		(gaussian-nmf 50 10 0 100 0 0.5))
;;   ;; 	    (is (equal (first (-> ret)) '(0 0.0 0.5)))
;;   ;; 	    (is (equal (second (-> ret)) '(1 1.1934616e-6 0.4999988)))))
;;   :body
;;   ((memf nmemf)
;;    {ifs (mapcar (lambda (m n)
;; 		  (list (first m) (second m) (second n)))
;; 		(-> memf) (-> nmemf))}))

;; (-> (ifs (gaussian-mf 50 10 0 0.5)
;; 	 (gaussian-nmf 50 10 0 0.5)))

;; operators

;; (def ifunion
;;     "Creates an intuitionistif fuzzy set which is the intuitionistic fuzzy union of `ifs1` and `ifs2`."
;;   :sig ((ifs ifs) ifs)
;;   :tests nil
;;   ;; ((capture (fn (ifs (gaussian-mf 50 10 0 100 0 0.5)
;;   ;; 		     (gaussian-nmf 50 10 0 100 0 0.5))
;;   ;; 		(ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 		     (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal (nth 0 (-> ret)) '(0 1.0 0.0)))
;;   ;; 	    (is (equal (nth 1 (-> ret)) '(1 0.99501246 0.004987538)))
;;   ;; 	    (is (equal (nth 99 (-> ret)) '(99 1.1934616e-6 0.4999988)))
;;   ;; 	    (is (equal (nth 100 (-> ret)) '(100 0.0 0.5)))))
;;   :body
;;   ((ifs1 ifs2)
;;    {ifs (mapcar (lambda (elt1 elt2)
;; 		  (list (nth 0 elt1)
;; 		  	(max (nth 1 elt1) (nth 1 elt2))
;; 		  	(min (nth 2 elt1) (nth 2 elt2))))
;; 		(-> ifs1)
;; 		(-> ifs2))}))

;; ;; (-> (ifunion (ifs (gaussian-mf 50 10 0 100 0 0.5)
;; ;; 		(gaussian-nmf 50 10 0 100 0 0.5))
;; ;; 	   (ifs (gaussian-mf 0 10 0 100 0 1)
;; ;; 		(gaussian-nmf 0 10 0 100 0 1))))

;; (def ifintersection
;;     "Creates an intuitionistif fuzzy set which is the intuitionistic fuzzy intersection of `ifs1` and
;; `ifs2`."
;;   :sig ((ifs ifs) ifs)
;;   :tests nil
;;   ;; ((capture (fn (ifs (gaussian-mf 50 10 0 100 0 0.5)
;;   ;; 		     (gaussian-nmf 50 10 0 100 0 0.5))
;;   ;; 		(ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 		     (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal (nth 0 (-> ret)) '(0 0.0 0.5)))
;;   ;; 	    (is (equal (nth 1 (-> ret)) '(1 1.1934616e-6 0.4999988)))
;;   ;; 	    (is (equal (nth 99 (-> ret)) '(99 3.2879808e-22 1.0)))
;;   ;; 	    (is (equal (nth 100 (-> ret)) '(100 0.0 1.0)))))
;;   :body
;;   ((ifs1 ifs2)
;;    {ifs (mapcar (lambda (elt1 elt2)
;; 		  (list (nth 0 elt1)
;; 			(min (nth 1 elt1) (nth 1 elt2))
;; 		  	(max (nth 2 elt1) (nth 2 elt2))))
;; 		(-> ifs1)
;; 		(-> ifs2))}))

;; (-> (ifintersection (ifs (gaussian-mf 50 10 0 0.5)
;; 			     (gaussian-nmf 50 10 0 0.5))
;; 			(ifs (gaussian-mf 0 10 0 1)
;; 			     (gaussian-nmf 0 10 0 1))))

;; (def membership
;;     "Returns the grade of membership associated to `x` in `ifs`"
;;   :sig ((number ifs) number)
;;   :tests nil
;;   ;; ((capture (fn 0 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(0 1.0 0.0))))
;;   ;;  (capture (fn 1 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(1 0.99501246 0.004987538))))
;;   ;;  (capture (fn 99 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			(gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(99 3.2879808e-22 1.0))))
;;   ;;  (capture (fn 100 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			 (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(100 0.0 1.0)))))
;;   :body
;;   ((x ifs)
;;    (if (>= x ;; (- (length (-> ifs)) 1)
;; 	   (first (first (last (-> ifs))))
;; 	   )
;;        (first (last (-> ifs)))
;;        (if (< x (first (first (-> ifs))))
;; 	   (first (-> ifs))
;; 	   (let* ((index (floor x))
;; 		  (fraction (- x index)))
;; 	     (mapcar #'+ ;; (nth index (-> ifs))
;; 		     (cl:append (list index) (access:access (-> ifs) index))
;; 		     (mapcar (lambda (diff)
;; 			       (* fraction diff))
;; 			     (mapcar #'- ;; (nth (+ index 1) (-> ifs))
;; 				     (cl:append (list (+ index 1)) (access:access (-> ifs) (+ index 1)))
;; 				     (cl:append (list index) (access:access (-> ifs) index))
;; 				     ;; (nth index (-> ifs))
;; 				     ))))))))

;; (def membership
;;     "Returns the grade of membership associated to `x` in `ifs`"
;;   :sig ((number ifs) number)
;;   :tests nil
;;   ;; ((capture (fn 0 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(0 1.0 0.0))))
;;   ;;  (capture (fn 1 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 		       (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(1 0.99501246 0.004987538))))
;;   ;;  (capture (fn 99 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			(gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(99 3.2879808e-22 1.0))))
;;   ;;  (capture (fn 100 (ifs (gaussian-mf 0 10 0 100 0 1)
;;   ;; 			 (gaussian-nmf 0 10 0 100 0 1)))
;;   ;; 	    (is (equal ret '(100 0.0 1.0)))))
;;   :body
;;   ((x ifs)
;;    (if (>= x ;; (- (length (-> ifs)) 1)
;; 	   (first (first (last (-> ifs))))
;; 	   )
;;        (first (last (-> ifs)))
;;        (if (< x (first (first (-> ifs))))
;; 	   (first (-> ifs))
;; 	   (let* ((index x)
;; 		  (fraction (- x index))
;; 		  lelt
;; 		  relt)
;; 	     ;; Searching for closest left element
;; 	     (cl:dolist (elt (-> ifs))
;; 	       (if (> (first elt) index)
;; 		   (cl:progn
;; 		    (cl:setf relt elt)
;; 		    (cl:return))
;; 		   (cl:setf lelt elt)))
;;              (cl:append (list x)
;; 			(mapcar #'+
;; 				(cl:rest lelt)
;; 				(cl:rest
;; 				 (mapcar (lambda (diff)
;; 					   (* fraction diff))
;; 					 (mapcar #'- relt lelt)))))
;; 	     )))))
