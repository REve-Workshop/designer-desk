;;; cl-skeletons.re.el --- Generate new files base on a template

;; Copyright (C) 2020 by Roland Everaert

;; Author: Roland Everaert <computing.re@revecloud.xyz>
;; Version: 0.1
;; Keywords: tool, programming, project, template

;;; Commentary:

;; This package provides skeletons for common-lisp.
;;
;; The header comment templates are inspired by the one in
;; common-lisp source files at https://github.com/informatimago/lisp

;;; Installation:
;;
;; Add this to your load-path.
;; Add the following line in your Emacs configuration file:
;;
;; (require 'cl-skeleton.re)

;;; Configuration:
;;
;; The following variables can be set to fill the files with additional
;; content:
;;
;; - reve:author-initial, the initial of the author.
;; - user-full-name, this is an Emacs variable. It is used as the
;;   author full name.
;; - user-mail-address, this is an Emacs variable. It used to provide
;;   the e-mail address.
;; - reve:project-version, version of the common-lisp
;;   ASDF system to define.
;; - reve:license-abbreviation, the license of the project in which
;;   the ASDF system is defined.
;;
;; To auto insert those skeletons when creating new common lisp files,
;; you can add the following to your Emacs initialization file.
;;
;; (eval-after-load 'auto-insert
;;   (define-auto-insert "\\.lisp" 'reve:cl/skel/lisp-file))
;; (eval-after-load 'auto-insert
;;   (define-auto-insert "\\.asd" 'reve:cl/skel/asdf-system))

;;; Code:
(require 's)

(define-skeleton reve:cl/skel/header
  "The header skeleton of all common lisp files."
  nil
  ";;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               " (file-name-nondirectory buffer-file-name) ?\n
  ";;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;
;;;;    See " (if (string= (file-name-extension buffer-file-name) "asd") "defsystem description" "defpackage documentation") " string.
;;;;
;;;; File originally generated with Emacs template found at
;;;; 'https://github.com/montaropdf/reve-workshop/elisp/'.
;;;;
;;;;AUTHORS
;;;;    <" reve:author-initial "> " user-full-name " <" user-mail-address ">
;;;;MODIFICATIONS
;;;;    YYYY-MM-DD <" reve:author-initial "> Some comment describing a modification.
;;;;BUGS
;;;; " ?\n
  ";;;;LEGAL" ?\n
  (when reve:license-abbreviation
    (concat ";;;;\n;;;; " reve:license-abbreviation "\n"))
  ";;;;
;;;; Copyright (C) " (format-time-string "%Y") " by " user-full-name ?\n
  ";;;;" ?\n
  (when reve:short-license
    (let ((commented-short-license ""))
      (dolist (line (s-lines reve:short-license) commented-short-license)
        (setq commented-short-license (concat commented-short-license ";;;; " line "\n")))))
  ";;;;****************************************************************************\n")

(define-skeleton reve:cl/skel/lisp-file
  "Insert a common lisp source file skeleton."
  nil
  "(defpackage " (setq v1 (skeleton-read "Package name: " (file-name-base))) ?\n
  >  "(:use :cl)
;;; Export functions
;;
;; (:export \"awesome-function-of-doom\" \"terrifying-macro-of-courtesy\")
;;
;;; Import functions
;;
;; (:import-from \"that.great.package.extra\" \"another-awesome-function\" \"that-great-function-i-like-to-use-in-every-file\")" ?\n
  > "(:documentation" ?\n
  > "\"" (skeleton-read "Documentation: ") "\"))

(in-package :" v1 ")

;;; Begin to write your code here.










;;; Code ends here.")

(define-skeleton reve:cl/skel/asdf-system
  "Insert an ASDF definition.

This skeleton will check if the asd file is for defining a test
system or not. To do so, it looks if the file, linked to the
buffer, ends with \".test.asd\".

If it is the case, it will consider the file to be an ASDF
system for testing a system with a name without the test part."
  nil
  '(if (s-ends-with-p ".test.asd" buffer-file-name)
       (progn
         (setq v2
               (s-chop-suffix ".test.asd"
                              (file-name-nondirectory buffer-file-name)))
         (setq v1 nil))
     (progn
       (setq v1 t)
       (setq v2 (file-name-base))))
  (if v1
      (progn
        (indent-according-to-mode)
        (concat "(asdf:defsystem \"" v2 "\"\n"))
    (progn
      (indent-according-to-mode)
      (concat "(asdf:defsystem \""
              (skeleton-read "ASDF Test system name: "
                             (concat v2 "/test"))
              "\"\n")))
  (when (and v1 reve:project-version)
    (indent-according-to-mode)
    (format ":version \"%s\"\n" reve:project-version))
  (when user-full-name
    (indent-according-to-mode)
    (format ":author \"%s\"\n" user-full-name))
  (when reve:license-abbreviation
    (indent-according-to-mode)
    (format ":license \"%s\"\n" reve:license-abbreviation))
  (if v1
      (progn
        (indent-according-to-mode)
        (concat ";;; Dependencies:
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
:description \"" (skeleton-read "Description: ") "\"
:in-order-to ((test-op (test-op \"" v2 "/tests\"))))\n"))
    (progn
      (indent-according-to-mode)
      (concat ":depends-on (\"" (skeleton-read "ASDF system dependency name: " v2) "\"\n"
              "\"rove\")\n"
              ":description \"Test for " (skeleton-read "Description: ") "\"\n"
              ":components ((:module \"tests\"\n"
              ":components\n"
              "((:file \"main\"))))\n"
              ":perform (test-op (op c) (symbol-call :rove :run c)))\n"))))

(provide 'cl-skeletons.re)
;;; cl-skeletons.re.el ends here
