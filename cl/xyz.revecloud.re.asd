(asdf:defsystem "xyz.revecloud.re"
  :version "0.1.0"
  :author "Roland Everaert"
  :license ""
  :depends-on (:xyz.revecloud.re.tools
               :xyz.revecloud.re.logging
               :xyz.revecloud.re.is)
  ;; :components ((:module "src"
  ;;                       :components
  ;;                       ((:file "git")
  ;;                        (:file "tools"))))
  :description "REVE Workshop environment."
  :in-order-to ((test-op (test-op "xyz.revecloud.re/tests"))))

(asdf:defsystem "xyz.revecloud.re/tests"
    :author "Roland Everaert"
    :license ""
    :depends-on ("xyz.revecloud.re"
                 "rove")
    :components ((:module "tests"
                          :components
                          ((:file "main"))))
    :description "Test system for reve-workshop"
    :perform (test-op (op c) (symbol-call :rove :run c)))
