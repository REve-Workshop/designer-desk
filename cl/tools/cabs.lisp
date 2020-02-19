;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cabs.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;
;;;; File originally generated with Emacs package
;;;; 'file-create.re'. This package can be found at
;;;; 'https://github.com/montaropdf/reve-workshop'.
;;;;
;;;; The header comment template is inspired by the one in
;;;; common-lisp source files at
;;;; https://github.com/informatimago/lisp
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <RE> Roland Everaert <computing.re@revecloud.xyz>
;;;;MODIFICATIONS
;;;;    YYYY-MM-DD <RE> Some comment describing a modification.
;;;;BUGS
;;;;
;;;;LEGAL
;;;;
;;;; Insert your legalese here.
;;;;****************************************************************************
(defpackage xyz.revecloud.re.tools.cabs
(:use :cl)
;;; Export functions
;;
;; (:export "awesome-function-of-doom" "terrifying-macro-of-courtesy")
;;
;;; Import functions
;;
;; (:import-from "that.great.package.extra" "another-awesome-function" "that-great-function-i-like-to-use-in-every-file")
(:documentation
"This package provides tools to help compile common-lisp systems."))

(in-package :xyz.revecloud.re.tools.cabs)

;;; Begin to write your code here

(defun collect-asd-directories (path)
  "Collect all directories containing an asd file."
  (let ((asd-directories (directory path/**/*.asd))
        (directory-with-asd-file '()))
    (dolist (asd-file asd-directories directory-with-asd-file)
      (push (directory-namestring asd-file) directory-with-asd-file))))
