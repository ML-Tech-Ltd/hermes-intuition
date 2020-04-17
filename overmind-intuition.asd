(defsystem "overmind-intuition"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:overmind-code :alexandria)
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "classes")
		 (:file "ifs" :depends-on ("main" "classes")))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "overmind-intuition/tests"))))

(defsystem "overmind-intuition/tests"
  :author ""
  :license ""
  :depends-on ("overmind-intuition"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for overmind-intuition"

  :perform (test-op (op c) (symbol-call :rove :run c)))
