(defsystem "hermes-intuition"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (#:alexandria
	       #:hermes-common
	       #:hermes-input)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "hermes-intuition/tests"))))

(defsystem "hermes-intuition/tests"
  :author ""
  :license ""
  :depends-on ("hermes-intuition"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hermes-intuition"

  :perform (test-op (op c) (symbol-call :rove :run c)))
