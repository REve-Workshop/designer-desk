(asdf:defsystem "xyz.revecloud.re.logging/tests"
    :author "Roland Everaert"
    :license ""
    :depends-on ("xyz.revecloud.re.logging"
                 "rove")
    :components ((:module "tests"
                          :components
                          ((:file "main"))))
    :description "Test system for reve-workshop lisp logging system."
    :perform (test-op (op c) (symbol-call :rove :run c)))
