(asdf:defsystem "xyz.revecloud.re.tools/tests"
    :author "Roland Everaert"
    :license ""
    :depends-on ("xyz.revecloud.re.tools"
                 "rove")
    :components ((:module "tests"
                          :components
                          ((:file "main"))))
    :description "Test system for reve-workshop tools."
    :perform (test-op (op c) (symbol-call :rove :run c)))
