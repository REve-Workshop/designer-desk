(defpackage xyz.revecloud.re.tools.misc
  (:use :cl)
  (:export "pp-hash-table"
           "merge-pathnames-to-string"
           "escape-double-quote"))

(in-package :xyz.revecloud.re.tools.misc)

(defmacro merge-pathnames-to-string (root sub)
  "Return a merge ROOT and SUB to form a pathname as a string."
  `(namestring (merge-pathnames ,sub ,root)))

(defun pp-hash-table (table)
  "Print the content of the hash table TABLE."
  (maphash (lambda (k v) (print (format nil "~a : ~a" k v))) table))

(defun escape-double-quote (text)
  "Returns TEXT surrounded by \"."
  (format nil "\"~a\"" text))
