(defpackage overmind-intuition.classes
  (:use :overmind-code)
  (:export :ts :mf :nmf :ifs))
(in-package :overmind-intuition.classes)

(defc ts "A time series."
  :super (list number))
(defc mf "A membership function."
  :super (list number))
(defc nmf "A non-membership function."
  :super (list number))
(defc ifs "An intuitionistic fuzzy set."
  :super (list number))
