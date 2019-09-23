(defpackage revesh
  (:use :cl :inferior-shell))
(in-package :revesh)


;; REVESH>                                                                         (inferior-shell:run/s '(progn (cd /home/roland/codes/reve-config-public/alacritty) (git status) ))
;; "Sur la branche master
;; rien à valider, la copie de travail est propre
;; "
;; NIL
;; 0
;; REVESH>                                                                         (inferior-shell:run/s '(progn (cd /home/roland/codes/reve-config-public/alacritty) (git remove -v) ))
;; * ; Evaluation aborted on #<UIOP/RUN-PROGRAM:SUBPROCESS-ERROR {10059A83E3}>.
;; REVESH>                                                                         (inferior-shell:run/s '(progn (cd /home/roland/codes/reve-config-public/alacritty) (git remote -v) ))

;; ""
;; NIL
;; 0
;; (defun make-git-remote-branch (remote branch local-repo remote-repo)
;;   "Generate a git REMOTE BRANCH for a local REPO."
;;   ())

(defmacro is-git-repo (dir)
  "Check if DIR is a git repository."
  `(probe-file (namestring (merge-pathnames ".git" ,dir))))

(defun get-git-repo-list (rootdir)
  "Return the list of git repositories available in a given directory"
  (if (is-git-repo rootdir)
      (list (file-pathname rootdir))
      ;; (mapcar is-git-repo)
      ;; (loop for repo in (inferior-shell:run/s `(ls ,rootdir))
      ;;    do (if )
      ;;      )
      ))
