(defpackage overmind-intuition/tests/main
  (:use :cl
        :overmind-intuition
        :rove))
(in-package :overmind-intuition/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :overmind-intuition)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
