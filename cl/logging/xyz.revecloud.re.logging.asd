(asdf:defsystem "xyz.revecloud.re.logging"
    :version "0.1.0"
    :author "Roland Everaert"
    :license ""
    ;; :depends-on (:str)
    :components ((:file "builder"))
    :description "REVE Workshop lisp logging system."
    :in-order-to ((test-op (test-op "xyz.revecloud.re.logging/tests"))))
