;; (ql:quickload :hermes-intuition)
(defpackage hermes-intuition
  (:use #:cl #:alexandria)
  (:import-from #:hscom.utils
                #:random-float)
  (:import-from #:hsinp.rates
                #:get-tp-sl
                #:from-pips)
  (:export #:con
           #:ant
           #:eval-ifis-gen
           #:eval-ifis-idx)
  (:nicknames #:hsint))
(in-package :hermes-intuition)

(cl:setf cl:*read-default-float-format* 'cl:double-float)

(defun con (y x0 x1)
  (let* ((m (/ 1
               (- x1 x0)))
         (b (+ (* m (- x0)) 0)))
    (/ (- y b) m)))
;; (con 0.0 -5 0)
;; (con 0.0 0 -5)

(defun ant (x x0 x1)
  (if (= (- x1 x0) 0)
      0
      (let* ((m (/ 1 (- x1 x0)))
             (b (+ (* m (- x0)) 0)))
        (+ (* m x) b))))
;; (ant 3 0 5)
;; (ant 3 5 0)

;; (defun make-antecedent (mean spread)
;;   (lambda (x) (exp (* -0.5 (expt (/ (- x mean) spread) 2)))))

(defun eval-ifis-gen (inputs antecedents consequents perceptions-count rules-count)
  (let ((tp 0)
        (sl 0)
        (activation 0))
    (loop for input in inputs
          for perception-idx from 0
          do (let ((winner-gm 0)
                   (winner-idx 0))
               (loop named winner-search
                     for i from (* perception-idx 4 rules-count) by 2
                     for idx from 0
                     ;; Each rule is represented by `4` points, so we divide by 4.
                     while (< i (* (1+ perception-idx) 4 rules-count))
                     do (let ((ant (subseq antecedents i (+ i 2))))
                          (let ((gm (ant input (aref ant 0) (aref ant 1))))
                            ;; Antecedents are ordered (ascending), so if `gm` < `winner-gm`,
                            ;; it means it's not necessary to check any further rules; we found the winner.
                            (when (and (<= gm 1)
                                       (>= gm 0)
                                       (< gm winner-gm))
                              (return-from winner-search))
                            ;; Antecedents will always work with OR (max).
                            (when (and (<= gm 1)
                                       (>= gm 0)
                                       (>= gm winner-gm))
                              (setf winner-idx idx)
                              (setf winner-gm gm)))))
               ;; Averaging outputs (tp and sl).
               (unless (= winner-gm 0)
                 (incf activation winner-gm)
                 ;; We divide `winner-idx` by 2 because each rule has 2 sides of a triangle.
                 ;; Each consequent has 2 points and 2 outputs (tp and sl), so 2*2 = `4`.
                 (let ((winner-offset (* (floor (/ winner-idx 2)) 4)))
                   (incf tp (con winner-gm
                                 (aref consequents (+ winner-offset 0))
                                 (aref consequents (+ winner-offset 1))))
                   (incf sl (con winner-gm
                                 (aref consequents (+ winner-offset 2))
                                 (aref consequents (+ winner-offset 3))))))))
    (values (/ tp perceptions-count)
            (/ sl perceptions-count)
            (/ activation perceptions-count))))

(defun eval-ifis-idx (inputs input-antecedents input-consequents)
  (let ((tp 0)
        (sl 0)
        (len (length inputs))
        (winner-gm 0)
        (winner-idx 0)
        (num-rules (length (aref input-antecedents 0))))
    ;; Calculating most activated antecedents.
    (loop
      for idx from 0 below num-rules
      do (let ((gm 0))
           (loop for antecedents across input-antecedents
                 for input in inputs
                 do (let* ((antecedent (aref antecedents idx))
                           (act (ant input (aref antecedent 0) (aref antecedent 1))))
                      (when (and (<= act 1)
                                 (>= act 0))
                        (incf gm act))))
           (when (>= gm winner-gm)
             (setf winner-idx idx)
             (setf winner-gm gm))))
    ;; Calculating outputs (TP & SL).
    (let ((activation (/ winner-gm len)))
      (unless (= winner-gm 0)
        (loop
          for consequents across input-consequents
          do (progn
               (incf tp (con activation
                             (aref (aref (aref consequents winner-idx) 0) 0)
                             (aref (aref (aref consequents winner-idx) 0) 1)))
               (incf sl (con activation
                             (aref (aref (aref consequents winner-idx) 1) 0)
                             (aref (aref (aref consequents winner-idx) 1) 1))))))
      (values (/ tp len)
              (/ sl len)
              activation))))

;; Keeping for historical reasons.
;; (defun ifis (i antecedents consequents)
;;   (let ((winner-gm 0)
;; 	(winner-idx 0))
;;     (loop
;;        for idx from 0
;;        for ant across antecedents
;;        do (let ((gm (funcall ant i)))
;; 	    (when (and (<= gm 1)
;; 		       (>= gm 0)
;; 		       (>= gm winner-gm))
;; 	      (setf winner-idx idx)
;; 	      (setf winner-gm gm))))
;;     (funcall (aref consequents winner-idx) winner-gm)))



;; (defparameter *rates* (fracdiff (get-rates-random-count-big :AUD_USD hscom.hsage:*train-tf* 10000)))
;; (make-ifis (gen-agent 3 :EUR_USD *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 10) 3 :EUR_USD *rates*)
;; (time
;;  (evaluate-agent (let ((beliefs (gen-random-beliefs hscom.hsage:*number-of-agent-inputs*)))
;; 		   (gen-agent hscom.hsage:*number-of-agent-rules*
;; 			      :AUD_USD
;; 			      *rates*
;; 			      (assoccess beliefs :perception-fns)
;; 			      (assoccess beliefs :lookahead-count)
;; 			      (assoccess beliefs :lookbehind-count)))
;; 		 *rates* :return-fitnesses-p t))

;; (slot-value (gen-agent 2 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 10) 'perception-fns)
;; (insert-agent (gen-agent 2 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 16) :EUR_USD hscom.hsage:*train-tf* '(:BULLISH))
