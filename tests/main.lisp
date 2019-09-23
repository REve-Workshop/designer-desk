(defpackage revesh/tests/main
  (:use :cl
        :revesh
        :rove))
(in-package :revesh/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :revesh)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
