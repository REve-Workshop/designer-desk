(defsystem "reve-workshop"
  :version "0.1.0"
  :author "Roland Everaert"
  :license ""
  :depends-on (:inferior-shell :str)
  :components ((:module "src"
                :components
                ((:file "git")
                 (:file "tools"))))
  :description "REVE Workshop environment."
  :in-order-to ((test-op (test-op "revesh/tests"))))

(defsystem "reve-workshop/tests"
  :author "Roland Everaert"
  :license ""
  :depends-on ("reve-workshop"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for reve-workshop"
  :perform (test-op (op c) (symbol-call :rove :run c)))
