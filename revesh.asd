(defsystem "revesh"
  :version "0.1.0"
  :author "Roland Everaert"
  :license ""
  :depends-on (:inferior-shell)
  :components ((:module "src"
                        :components
                        ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "revesh/tests"))))

(defsystem "revesh/tests"
  :author "Roland Everaert"
  :license ""
  :depends-on ("revesh"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for revesh"
  :perform (test-op (op c) (symbol-call :rove :run c)))
