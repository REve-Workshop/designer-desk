(asdf:defsystem "xyz.revecloud.re.tools"
    :version "0.1.0"
    :author "Roland Everaert"
    :license ""
    ;; :depends-on (:xyz.revecloud.re.tools.tools)
    :components ((:file "misc")
                 (:file "cabs"))
    :description "REVE Workshop tools."
    :in-order-to ((test-op (test-op "xyz.revecloud.re.tools/tests"))))
