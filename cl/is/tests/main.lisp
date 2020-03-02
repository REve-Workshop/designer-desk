;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               main.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;; File originally generated with Emacs template found at
;;;; 'https://github.com/montaropdf/reve-workshop/elisp/'.
;;;;
;;;;AUTHORS
;;;;    <RE> Roland Everaert <computing.re@revecloud.xyz>
;;;;MODIFICATIONS
;;;;    YYYY-MM-DD <RE> Some comment describing a modification.
;;;;BUGS
;;;; 
;;;;LEGAL
;;;;
;;;; GNU AGPL
;;;;
;;;; Copyright (C) 2020 by Roland Everaert
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;;****************************************************************************

(defpackage xyz.revecloud.is/test
  (:use :cl :rove :xyz.revecloud.is.tree)
;;; Export functions
  ;;
  ;; (:export "awesome-function-of-doom" "terrifying-macro-of-courtesy")
  ;;
;;; Import functions
  ;;
  (:import-from "xyz.revecloud.is/test" "walk" "node-sane-p")
  (:documentation
   "Testing of the information system package."))

(in-package :xyz.revecloud.is/test)

;;; Begin to write your code here.

;; NOTE: To run this test file, execute `(asdf:test-system :revesh)' in your Lisp.

(defparameter *a-tree* '(prime (a (a-prime 1) (a-second r)) (b (4r (var #\%)))))

(deftest walk-this-way
    (testing "(walk '(b 4r var) *a-tree*) to return #\%."
             (ok (outputs (walk '(b 4r var) *a-tree*) #\%))))

(defparameter *a-malformed-tree* '(sec (a ("meh" 1) (a r)) (b (4r (var #\%)))))

(deftest walk-are-you-talking-to-me
    (testing "(walk '(b 4r var) *a-malformed-tree*) to signal an error."
             (ok (signals (walk '(b 4r var) *a-tree*)
                          'error)
                 "Invalid path: (b 4r var)")))


(defparameter *another-malformed-tree* '(ter ((truc 1) (a r)) (b (4r (var #\%)))))

(deftest walk-are-you-talking-to-me-too
    (testing "(walk '(b 4r var) *another-malformed-tree*) to signal an error."
             (ok (signals (walk '(b 4r var) *a-tree*)
                          'error)
                 "Invalid path: (b 4r var)")))

(defparameter *a-thriving-tree* '(prime
                                  (a (a-prime 1 5 9 "bidule")
                                   (a-second r)
                                   (a-ter (w a)
                                    (l 47)
                                    (f #P"/home/roland/.sbclrc")))
                                  (b (4r
                                      (var #\%)))))

(defparameter *yet-another-malformed-tree* '(prime
                                             (a (a-prime 1 5 9 "bidule")
                                              (a-second r)
                                              (a-ter w a
                                               (l 47)
                                               (f #P"/home/roland/.sbclrc ")))
                                             (b (4r
                                                 (var #\%)))))

(defparameter *a-path* '(b 4r var))

(defparameter *une-hache* (make-hash-table))

(defparameter *wrong-path* '(a var))

(defstruct test-type
  (bla 2)
  (bli "ret"))

(defparameter *une-structure* (make-test-type))

(defclass truc ()
  (one
   (two
    :reader two)
   three))

(defparameter *une-classe* (make-instance 'truc))










;;; Code ends here.
