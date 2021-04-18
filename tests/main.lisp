(defpackage hermes-intuition/tests/main
  (:use :cl
        :hermes-intuition
        :rove))
(in-package :hermes-intuition/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hermes-intuition)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
