(defpackage xyz.revecloud.re.tools/tests/main
  (:use :cl
        :xyz.revecloud.re.tools
        :rove))
(in-package :xyz.revecloud.re.tools/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :xyz.revecloud.re.tools)' in your Lisp.

(deftest test-target-1
    (testing "should (= 1 1) to be true"
             (ok (= 1 1))))
