(defpackage reve-workshop.tools
  (:use :cl :str)
  (:export #:pp-hash-table
           #:merge-pathnames-to-string))

(in-package :reve-workshop.tools)

(defmacro merge-pathnames-to-string (root sub)
  "Return a merge ROOT and SUB to form a pathname as a string."
  `(namestring (merge-pathnames ,sub ,root)))

(defun pp-hash-table (table)
  "Print the content of the hash table TABLE."
  (maphash (lambda (k v) (print (format nil "~a : ~a" k v))) table))
