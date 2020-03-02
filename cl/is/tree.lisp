;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               tree.lisp
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
(defpackage xyz.revecloud.is.tree
  (:use :cl)
  (:export "walk" "node-sane-p" "path-exists-p")
;;; Import functions
  ;;
  ;; (:import-from "that.great.package.extra" "another-awesome-function" "that-great-function-i-like-to-use-in-every-file")
  (:documentation
   "Tree manipulation."))

(in-package :xyz.revecloud.is.tree)

;;; Begin to write your code here.

;; * Tree design
;;   A node, is composed of a CAR and a CDR
;;   - The CAR must be a symbol
;;   - The CDR can be
;;     - a spliced list of atoms
;;     - a spliced list of nodes
;;
;;   - Example 1: One node made of a symbol and an atom
;;     (a 1)
;;   - Example 2: One node made of a symbol and several atoms
;;     (a 1 2 3 567 "things")
;;   - Example 3: A node containing another node
;;     (a (b fur so da))
;;   - Example 4: A node made of a symbol and several nodes
;;     (a (b fus ro da) (c 3) (bar "foo") (e (grad 4) (more e-nough)))

;; How to grow the tree:
;; * Graft design
;;   A graft, can be
;;   - A node, the rule of a tree node applies
;;   - a list of atoms. They will be spliced
;; * Use cases:
;;   1. Add a graft to a branch
;;      - The graft must be a node
;;   2. Replace a branch with a graft
;; * Work flow
;;   1. Ensure the path exists (use the walk function)
;;   2. Ensure the graft, is valid
;;   3. Which case to perform?
;;     a. Add a graft to a branch
;;       - If the path lead to a leaf:
;;         - Raise an error
;;       - Else, Push the tree in the branch
;;     b. Replace a branch with a graft
;;       - Remove the branch at path
;;       - Add the graft to the parent branch
;; * The grow function
;; ** Input
;;    - the path to the affected node
;;    - the tree affected
;;    - the graft
;;    - the type of operation
;; ** Output
;;    - t, the operation succeed
;;    - nil, the operation failed
;;    - error condition, invalid parameters


(defun walk (path tree &key (operation :walk) (new-nodes-or-atoms nil))
  "Walk in the TREE following PATH and sweat the element found or nil.

This function does not use any loop construct to walk through the nodes
at the same level. It calls itself with the CDR of TREE."
  (let ((breath (car tree))
        (node-head nil))
    (cond ((eq operation :walk)
           ;; Is the exhaled node a cons or a symbol?
           (unless (setf node-head (if (consp breath) (car breath)
                                       (when (symbolp breath) breath)))
             (error "Bad node type: ~a / ~a" (type-of breath) breath)))
          ((find operation '(:replace :graft))
           (unless (or (node-sane-p new-nodes-or-atoms)
                       (every #'(lambda (x) (typep x 'atom)) new-nodes-or-atoms))
             (error "Bad graft: ~a~%" new-nodes-or-atoms)))
          ((eq operation :cut)
           (unless (path-exists-p path tree)
             (error "Invalid path: ~a~%" path))
           (setf new-nodes-or-atoms nil))
          (t
           (error "Unknown operation: ~a~%" operation)))
    (setf node-head (when (consp breath) (car breath)))
    ;; Is inhaling keep us on track?
    (if (eq (car path) node-head)
        ;; Yes, check if we finish walking
        (if (cdr path)
            ;; Yes, continue on this path
            (walk (cdr path)
                  (cdr breath)
                  :operation operation
                  :new-nodes-or-atoms new-nodes-or-atoms)
            ;; No, time to sweat off
            (cond ((eq operation :replace)
                   (rplacd (car tree) new-nodes-or-atoms)
                   t)
                  ((eq operation :cut)
                   (rplaca tree nil)
                   t)
                  ((eq operation :graft)
                   (push new-nodes-or-atoms (cdar tree))
                   t)
                  ((eq operation :walk)
                   (cdr breath))
                  (t
                   (error "Bad operation: ~a~%" operation))))
        ;; No, time to exhale and keep walking
        ;; If not at the end of the run...
        (when (cdr tree)
          ;; continue with a side track
          (walk path
                (cdr tree)
                :operation operation
                :new-nodes-or-atoms new-nodes-or-atoms)))))

(defun path-exists-p (path node)
  (let ((breath (car node))
        (node-head nil))
    ;; Is the exhaled node a cons or a symbol?
    (setf node-head (when (consp breath) (car breath)))
    ;; Is inhaling keep us on track?
    (if (eq (car path) node-head)
        ;; Yes, check if we finish walking
        ;; Do we need to continue?
        (if (cdr path)
            ;; Yes, continue on this path
            (when (path-exists-p (cdr path) (cdr breath)) t)
            ;; No, time to sweat off
            t)
        ;; No, time to exhale and keep walking
        (when (and (cdr node) (path-exists-p path (cdr node))) t))))

(defun node-sane-p (node)
  "Return t if the NODE and its children are sane.

A sane node is a cons made of a car with a symbol and a cdr with one
or more nodes or one or more atoms."
  (let ((node-head nil))
    (setf node-head (when (consp node) (car node)))
    (when (and node-head (symbolp node-head))
      (let ((node-body (cdr node)))
        (if (every #'(lambda (x) (typep x 'atom)) node-body)
            t
            (every #'node-sane-p node-body))))))

;; (defun graft (path node new-node-or-atoms operation)
;;   "Add nodes or atoms at path or replace the content of a node at path."
;;   (let ((breath (car node))
;;         (node-head nil))
;;     (unless (or (node-sane-p new-node-or-atoms)
;;                 (every #'(lambda (x) (typep x 'atom)) new-node-or-atoms))
;;       (error "Bad graft: ~a~%" new-node-or-atoms))
;;     (unless (find operation '(:replace :add))
;;       (error "Bad operation: ~a~%" operation))
;;     ;; Is the exhaled node a cons or a symbol?
;;     (setf node-head (when (consp breath) (car breath)))
;;     ;; Is inhaling keep us on track?
;;     (if (eq (car path) node-head)
;;         ;; Yes, check if we are done walking
;;         (if (cdr path)
;;             ;; Yes, continue on this path
;;             (graft (cdr path) (cdr breath) new-node-or-atoms operation)
;;             ;; No, time to sweat off
;;             (cond ((eq operation :replace)
;;                    (rplacd (car node) new-node-or-atoms))
;;                   ((eq operation :add)
;;                    (push new-node-or-atoms (cdar node)))
;;                   (t
;;                    (error "Bad operation: ~a~%" operation))))
;;         ;; No, time to exhale and keep walking
;;         ;; If not at the end of the run...
;;         (when (and (cdr node)
;;                    (graft path
;;                           (cdr node)
;;                           new-node-or-atoms
;;                           operation))
;;           t))))

;;; Code ends here.
