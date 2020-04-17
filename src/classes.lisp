(in-package :overmind-intuition)

(defc ifs-params "A list of parameters that define a fuzzy set."
  :super '(list number))
(defc mf "A membership function."
  :super '(list number)
  :slots '((:type keyword)
	   (:parameters ifs-params)))
(defc nmf "A non-membership function."
  :super '(list number)
  :slots '((:type keyword)
	   (:parameters ifs-params)))

(defc point
    "Represents a point (`x`, `y`) in a plane."
  :super '(number)
  :slots '((:x number)
	   (:y number)))

(defc coa
    "Represents the center of area of a shape, where `A` is the area
of the shape and `Cx` is the x coordinate of the centroid."
  :super '(number)
  :slots '((:a number)
	   (:cx number)))

(defc line
    "Represents a line defined by two points (`x1`,`y1`) and
(`x2`,`y2`), which has a slope `m` and constant `b`."
  :super '(number list)
  :slots '((:x1 number)
	   (:y1 number)
	   (:x2 number)
	   (:y2 number)
	   (:m number)
	   (:b number)))
(defc ifs-polygon "Represents an ifs by using a list of (x,y) points."
  :super '(line list number ifs)
  :slots '((:mf list)
	   (:nmf list)))
(defc ifs "An intuitionistic fuzzy set."
  :super '(list number ifs-params)
  :slots '((:mf mf)
	   (:nmf nmf)))

(defc domains "Domains (interval of values for x) for an `ifs`."
  :super '(number)
  :slots '((:antecedents-min number)
	   (:antecedents-max number)
	   (:consequents-min number)	   
	   (:consequents-max number)))
(defc rule "A pair of IFS, where one is an antecedent and the other a consequent."
  :super '(list number)
  :slots '((:antecedent ifs)
	   (:consequent ifs)))

(defc if-system "An intuitionistic fuzzy system."
  :super '(list number)
  :slots '((:domains domains)
	   (:rules list)))
