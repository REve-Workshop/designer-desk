(asdf:defsystem "xyz.revecloud.re.is/tests"
    :author "Roland Everaert"
    :license ""
    :depends-on ("xyz.revecloud.re.is"
                 "rove")
    :components ((:module "tests"
                          :components
                          ((:file "main"))))
    :description "Test system for reve-workshop information system."
    :perform (test-op (op c) (symbol-call :rove :run c)))
