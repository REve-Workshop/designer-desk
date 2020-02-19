;;; file-create.re.el --- Generate new files base on a template

;; Add License

;; Author: Roland Everaert <computing.re@revecloud.xyz>
;; Version: 0.1
;; Keywords: tool, programming, project

;;; Commentary:

;; This package provides functions to create various types of source
;; files, with preloaded content like a package definition in
;; common-lisp for example.

;; At the moment, only common-lisp Lisp and asd files are supported.

;; This package is different from a package like skeletor.el, because
;; it is intented to be used after project creation, to add files to a
;; project with contents like best practices header comments, package
;; definition and more.

;;; Installation:
;;
;; Add this to your load-path.
;; Add the following line in your Emacs configuration file:
;;
;; (require 'file-create.re)

;;; Configuration:
;;
;; The following variables can be set to fill the files with additional
;; content:
;;
;; - reve:cl/asdf-system-version, version of the common-lisp
;; ASDF system to define.
;; - reve:cl/asdf-system-license, the license of the project in which
;; the ASDF system is defined.
;;; Code:

(defvar reve:cl/asdf-system-version nil
  "Version of the ASDF system to define.")

(defvar reve:cl/asdf-system-license nil
  "License for the project.")

(defvar reve:cl/author-initial nil
  "The author initials.")

;;;###autoload
(defun reve:cl/create-lisp-file (cl-file-name cl-package-name package-documentation)
  "Create a new Common Lisp source code file with defpackage.

CL-FILE-NAME, the name of the Lisp file to create.
CL-PACKAGE-NAME, the name of the package to define in the file."
  (interactive "F File name:
M Package name: 
M Package description: ")
  (let ((initial reve:cl/author-initial)
        (saved-mail-address user-mail-address))
    (with-temp-file cl-file-name
      (insert (format ";;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               %s.%s
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
;;;;    <%s> %s <%s>
;;;;MODIFICATIONS
;;;;    YYYY-MM-DD <%s> Some comment describing a modification.
;;;;BUGS
;;;;
;;;;LEGAL
;;;;
;;;; Insert your legalese here.
;;;;****************************************************************************
" (file-name-base cl-file-name)
(file-name-extension cl-file-name)
initial
user-full-name
saved-mail-address
initial))
      (insert (format "(defpackage %s
(:use :cl)
;;; Export functions
;;
;; (:export \"awesome-function-of-doom\" \"terrifying-macro-of-courtesy\")
;;
;;; Import functions
;;
;; (:import-from \"that.great.package.extra\" \"another-awesome-function\" \"that-great-function-i-like-to-use-in-every-file\")
  (:documentation
   \"%s\"))

(in-package :%s)

;;; Begin to write your code here

" cl-package-name package-documentation cl-package-name))
      (indent-region (point-min) (point-max))
      ;; (write-file cl-file-name)
      )))

;;;###autoload
(defun reve:cl/create-asdf-system (cl-asd-package-name cl-asd-description)
  "Create an ASDF Common Lisp system file and its associated test suite system.

CL-ASD-PACKAGE-NAME, the name of the ASDF system to define.
CL-ASD-DESCRIPTION, the description of the ASDF system.

The first file is the system definition. the second file is an
ASDF system definition for testing the content of the first one."
  (interactive "M Package name:
M Package description: ")
  (progn
    (reve:cl/create-asd-file cl-asd-package-name cl-asd-description)
    (reve:cl/create-asd-test-file cl-asd-package-name cl-asd-description)))

(defun reve:cl/create-asd-file (cl-asd-package-name cl-asd-description)
  "Create the file defining an ASDF system for common-lisp projects.

CL-ASD-PACKAGE-NAME, the name of the ASDF system to define.
CL-ASD-DESCRIPTION, the description of the ASDF system."
  (let ((local-asdf-system-version reve:cl/asdf-system-version)
        (local-asdf-system-license reve:cl/asdf-system-license))
    (with-temp-file (format "%s.asd" cl-asd-package-name)
      (insert (format "(asdf:defsystem \"%s\"\n" cl-asd-package-name))
      (when local-asdf-system-version
        (insert (format ":version \"%s\"\n" local-asdf-system-version)))
      (when user-full-name
        (insert (format ":author \"%s\"\n" user-full-name)))
      (when local-asdf-system-license
        (insert (format ":license \"%s\"\n" local-asdf-system-license)))
      (insert (format ";;; Dependencies:
;;
;;  :depends-on (\"cl-fad\" \"alexandria\")
;;
;;; Components:
;;
;; Example 1:
;;  :components ((file \"file1\") (file \"file2\"))
;;
;; Example 2:
;;  :components ((:module \"some-subdir\"
;;                      :components
;;                      ((:file \"file3b\"))))
;;
:description \"%s\"
:in-order-to ((test-op (test-op \"%s/tests\"))))" cl-asd-description cl-asd-package-name))
      (indent-region (point-min) (point-max)))))

;;; Create the asd file for the test suite linked to the package.
(defun reve:cl/create-asd-test-file (cl-asd-package-name cl-asd-description)
  "Create the file defining an ASDF system for testing the parent ASDF system.

CL-ASD-PACKAGE-NAME, the name of the base ASDF system.
CL-ASD-DESCRIPTION, the description of the base ASDF system."
  (let ((local-asdf-system-license reve:cl/asdf-system-license))
    (with-temp-file (format "%s.test.asd" cl-asd-package-name)
      (insert (format "(asdf:defsystem \"%s/test\"\n" cl-asd-package-name))
      (when user-full-name
        (insert (format ":author \"%s\"\n" user-full-name)))
      (when local-asdf-system-license
        (insert (format ":license \"%s\"\n" local-asdf-system-license)))
      (insert (format ":depends-on (\"%s\"
                   \"rove\")
    :description \"Test for %s\"
    :components ((:module \"tests\"
                          :components
                          ((:file \"main\"))))
    :perform (test-op (op c) (symbol-call :rove :run c)))" cl-asd-package-name cl-asd-description))
      (indent-region (point-min) (point-max)))))

(provide 'file-create.re)
;;; file-create.re ends here
