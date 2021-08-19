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
(defpackage xyz.revecloud.re.is.tree
  (:use :cl)
  (:export "WALK" "NODE-PATH-P" "NODE-SANE-P" "LOCATE-FIRST-CORRUPTED-NODE"
           "PATH-EXISTS-P" "COPY-NODE-PATH")
;;; Import functions
  ;;
  ;; (:import-from "that.great.package.extra" "another-awesome-function" "that-great-function-i-like-to-use-in-every-file")
  (:documentation
   "Tree manipulation."))

(in-package :xyz.revecloud.re.is.tree)

;;; Begin to write your code here.

;; * Tree design
;;   A node, is a cons, composed of
;;   - The CAR, a symbol starting with the character '@'. Those
;;     symbols will be referenced as the key of a node.
;;   - The CDR can contain one of
;;     - a list of atoms, at least one.
;;     - a list of nodes, at least one.
;;
;;   Nodes with atoms are called leaf.
;;
;;   - Example 1: One node made of a key and an atom
;;     (@a 1)
;;   - Example 2: One node made of a key and several atoms
;;     (@a 1 2 3 567 "things")
;;   - Example 3: A basic tree. A node containing a node
;;     (@a (@b fus ro da))
;;   - Example 4: A more complex tree. A node made of a symbol and several nodes
;;     (@a (@b fus ro da) (@c 3) (@bar "foo") (@e (@grad 4) (@more e-nough)))
;;   - Example 5: A special case of tree. A node containing a node containing one node. In this case, if the node with the key @fus is removed, the tree will be removed.
;;     (@a (@b (@fus ro da)))
;;
;; TO-STUDY: Consider using a class to define a node. 
;;           - How will be stored the data, in the end?
;;           - How to differentiate between multiple node types and instances?
;;           - How to ensure all the fields are present and how to add new ones, possibly.

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

(defconstant +node-key-char+ #\@
  "The first character of a node key.")

(defun node-key-p (element)
  "Return T if ELEMENT is a node key."
  (when (and (symbolp element)
             (char= (elt (symbol-name element) 0)
                    +node-key-char+))
    t))

(defun node-key-or-index-p (element)
  "Return T if ELEMENT is a node key or an integer."
  (when (or (integerp element)
            (node-key-p element))
    t))

(defun node-path-p (&rest path)
  "Return T if PATH is valid.

A valid path contains only symbols starting with the character defined
in constant +NODE-KEY-CHAR+ or integers."
  (progn
    (unless path
      (error "PATH is missing!!!"))
    (if (consp path)
        (every #'node-key-or-index-p path)
        (node-key-or-index-p path))))

(defun copy-node-path (&rest path)
  "Return a copy of PATH as a one-dimension list.

If PATH is not a valid path, an error is raised."
  (let ((sanitized-node-path (if (= (length path) 1) (car path) path)))
    (cond ((node-key-p sanitized-node-path)
           (intern (symbol-name sanitized-node-path)))
          ((consp sanitized-node-path)
           (let ((new-path '()))
             (dolist (element sanitized-node-path new-path)
               (setf new-path (append new-path (list (copy-node-path element)))))
             new-path))
          ((integerp sanitized-node-path)
           sanitized-node-path)
          (t
           (error "PATH is not valid: ~a" path)))
    ))

(defun walk (tree &rest path)
  "Walk in the TREE following PATH and sweat the element found or nil.

- TREE is a tree of nodes or a part of it to scan.

- PATH must be present or an error will be raised. After the TREE
  parameter any remaining parameters will be gathered in PATH. Valid
  remaining parameters are symbols and integers. If PATH is nil or is
  invalid according to node-path-p, an error will be raised.

Symbols, in path, must respect the definition of a node key, numbers
represent the index of an element to reach at the current level.

- Example 1:
A tree with 2 nodes, at level 1 and 1 atom in each node.
(defparameter *tree-1* '(@level-1 (@level-1-1 hello) (@level-1-2 world)))

Valid paths:
(defparameter *path-1* (list 0)) --> will find the symbol @level-1

(defparameter *path-1* (list @level-1)) --> will find the sexp ((@level-1-1 hello) (level-1-2 world))

(defparameter *path-1* (list @level-1 1)) --> will find (@level-1-2 world)

(defparameter *path-1* (list @level-1 level-1-1)) --> will find the symbol hello 

Example 2:
(defparameter *tree-2* '(@level-1 (@level-1-1 (@hello \"all\")) (@level-1-2 world)))

Valid paths:
(defparameter *path-2* (list @level-1 @level-1-1)) --> will find the cons (@hello \"all\")

(defparameter *path-2* (list @level-1 @level-1-1 hello)) --> will find the string \"all\"

Example 3:
(defparameter *tree-3* '(@level-1 (@level-1-1 (@hello \"all\")) (@level-1-2 world) (@level-1-3 2 5 567 #\G)))

Valid paths:
(defparameter *path-3* (list @level-1 @level-1-3)) --> will find the list (2 5 567 #\G)

(defparameter *path-3* (list @level-1 @level-1-3 2)) --> will find the atom 567

Example 4:
(defparameter *tree-4* '((@level-1-1 (@hello \"all\")) (@level-1-2 world) (@level-1-3 2 5 567 #\G)))

Valid paths:
(defparameter *path-4* (list 0)) --> will find the tree (@level-1-1 (@hello \"all\"))

(defparameter *path-4* (list 0 @level-1-3 3)) --> will find the character #\G
"
  (let ((breath nil)
        (step (car path))
        (remaining-path (copy-list path)))
    ;; (format t "WALK/TREE: ~a~%" tree)
    (unless path
      (error "PATH is missing!!!"))
    (unless (apply #'node-path-p path)
      (error "PATH is not valid: ~a" path))
    (cond ((integerp step)
           (setf breath (nth step tree)))
          ((symbolp step)
           (if (eq step (car tree))
               (setf breath (cdr tree))
               (when (consp (car tree))
                 (setf breath (cdr (find step tree :key #'car)))))))
    ;; Do we need to keep walking?
    (pop remaining-path)
    (if remaining-path
        ;; Is the element extracted a node?
        (if (consp breath)
            ;; Yes, walk
            (let ((new-path (apply #'copy-node-path remaining-path)))
              (when (integerp step)
                (push (car breath) new-path))
              (setf breath (walk breath new-path)))
            ;; No return nothing, path to long
            (setf breath nil)))
    breath))

(defun path-exists-p (tree path)
  "Lookup, recursively in NODE for PATH.

Same as the WALK function, but return T if a node or atom i found at
PATH.
"
  (when (walk tree path) t))

(defun node-sane-p (node)
  "Return T if the NODE and its children are sane.

A sane node is a cons made of a car with a node key and a cdr with one
or more nodes or one or more atoms."
  (let ((node-head nil))
    (setf node-head (when (consp node) (car node)))
    (when (and node-head (node-key-or-index-p node-head))
      (let ((node-body (cdr node)))
        (when node-body
          (if (every #'(lambda (x) (typep x 'atom)) node-body)
              t
              (every #'node-sane-p node-body)))))))

(defun locate-first-corrupted-node (tree &optional (path nil))
  "Return the first malformed node found in tree, with some contextual information.

If PATH is non-nil and a valid path, only the tree as returned by the
walk function will be scanned."
  (let ((node-head nil)
        (tree-to-analize tree)
        (path-to-malformed-node '()))
    (when path
      (flet ((build-node (path-to-look-in data)
               (cons (find-if #'node-key-p path-to-look-in :from-end t)
                     data)))
        (setf tree-to-analize (walk tree path))
        (cond ((typep tree-to-analize 'atom)
               (setf tree-to-analize (build-node path tree-to-analize)))
              ((consp tree-to-analize)
               (unless (node-key-p (car tree-to-analize))
                 (setf tree-to-analize (build-node path tree-to-analize)))))))
    (setf node-head (when (consp tree-to-analize) (car tree-to-analize)))
    (if (and node-head (node-key-p node-head))
        (let ((node-body (cdr tree-to-analize)))
          (if node-body
              (unless (every #'(lambda (x) (typep x 'atom)) node-body)
                (setf path-to-malformed-node
                      (loop for node in node-body
                              thereis (locate-first-corrupted-node node)))
                (when path-to-malformed-node
                  (push (copy-symbol node-head) path-to-malformed-node)))
              (setf path-to-malformed-node (list (copy-symbol node-head) 'empty))))
        (if node-head node-head (setf path-to-malformed-node 'empty)))
    (when (and path path-to-malformed-node)
      (setf path-to-malformed-node
            (concatenate 'list
                         (subseq path 0 (1- (length path)))
                         path-to-malformed-node)))
    path-to-malformed-node))


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
