(asdf:defsystem "xyz.revecloud.re.is"
    :version "0.1.0"
    :author "Roland Everaert"
    :license ""
    ;; :depends-on (:fact-base :str)
    :components ((:file "define"))
    :description "REVE Workshop information system."
    :in-order-to ((test-op (test-op "xyz.revecloud.re.is/tests"))))
