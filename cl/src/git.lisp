(defpackage reve-workshop.git
  (:use :cl :inferior-shell :str :reve-workshop.tools)
  (:export #:commits-stat
           #:repo-stat
           #:repository-statistics))

(in-package :reve-workshop.git)

(defstruct commits-stat
  (ahead 0 :type number)
  (behind 0 :type number))

(defstruct repo-stat
  (is-repo nil)
  (remotes 0 :type number)
  (branches 0 :type number)
  (branch "master" :type string)
  (untracked 0 :type number)
  (modified 0 :type number)
  (unmerged 0 :type number)
  (commits (make-commits-stat) :type commits-stat))

(defun is-repo-p (dir)
  "Check if DIR is a git repository."
  (probe-file (reve-workshop.tools:merge-pathnames-to-string ".git" dir)))

(defun has-remote-p (repo-dir)
  "Return non-nil if the repo in REPO-DIR has a remote configured."
  (not (inferior-shell:run/nil `(progn
                                  (cd ,repo-dir)
                                  (git remote)))))

(defun get-repo-list (rootdir)
  "Return the list of git repositories available in a given directory"
  (if (is-repo-p rootdir)
      (list (file-namestring rootdir))
      (let (repo-list)
        (dolist (dir (inferior-shell:run/lines `(ls ,rootdir)) repo-list)
          (when (is-repo-p (merge-pathnames-to-string rootdir dir))
            (setf repo-list (append repo-list (list dir))))))))

(defun set-remote (local-repo remote-repo &optional remote branch)
  "Bind the local repo, REPO, to the remote REMOTE-REPO."
  (let ((remote-target (or remote "origin")))
    (progn
      (inferior-shell:run/nil `(progn
                                 (cd ,local-repo)
                                 (git remote add ,remote-target ,remote-repo)))
      (when branch
        (inferior-shell:run/nil `(progn
                                   (cd ,local-repo)
                                   (git fetch ,remote)
                                   (git branch ,branch)
                                   (git checkout ,branch)
                                   (git push ,remote ,branch)))))))

(defun make-remote-hash (repo-list repo-url-list &optional (operation #\o))
  "Build a hash map with one entry for each element of REPO-LIST."
  (when (member operation '(#\b #\o))
    (let ((repos-hash (make-hash-table :test 'EQUAL)))
      (dotimes (repo-i (length repo-list) repos-hash)
        (setf (gethash (nth repo-i repo-list) repos-hash)
              (list (nth repo-i repo-url-list) operation))))))

(defun make-git-repo (repo-dir url)
  "Initialize REPO-DIR as a git repository, following URL."
  (inferior-shell:run/nil `(progn
                             (cd ,repo-dir)
                             (git init)
                             (echo "*~" >> .gitignore)
                             (git remote add origin ,url))))

(defun remotes-count (repo)
  "Counts the number of remotes in the REPO."
  (length (inferior-shell:run/lines `(progn
                                       (cd ,repo)
                                       (git remote))
                                    :on-error nil)))

(defun branches-count (repo)
  "Counts the number of remotes in the REPO."
  (length (inferior-shell:run/lines `(progn
                                       (cd ,repo)
                                       (git branch))
                                    :on-error nil)))

(defun get-status-detail (repo)
  "Get all the information related to the status of REPO.

This use the command:
#begin_example
  git status -b --porcelain=v2
#end_example"
  (let ((repo-status (inferior-shell:run/lines `(progn
                                                  (cd ,repo)
                                                  (git status -b --porcelain=v2))))
        (state :INIT))
    (do ((stat (str:split " " (first repo-status)))
         (repo-statistics (make-hash-table)))
        ((or (eq state :ERROR) (eq state :END) (null stat)) repo-statistics)
      (cond
        ((eq state :INIT)
         (progn
           (setf (gethash :untracked repo-statistics) 0)
           (setf (gethash :modified repo-statistics) 0)
           (setf (gethash :unmerged repo-statistics) 0)
           (setf state :ADD)))
        ((eq state :READ)
         (progn
           (setf repo-status (cdr repo-status))
           (if (null repo-status)
               (setf state :END)
               (progn
                 (setf stat (str:split " " (first repo-status)))
                 (setf state :ADD)))))
        ((eq state :ADD)
         (progn
           (cond
             ((string= (first stat) "#")
              (cond ((string= (second stat) "branch.ab")
                     (setf (gethash :commits repo-statistics (make-commits-stat))
                           (make-commits-stat :ahead (parse-integer (third stat))
                                              :behind (parse-integer (fourth stat)))))
                    ((string= (second stat) "branch.head")
                     (setf (gethash :branch repo-statistics "") (third stat)))))
             ((string= (first stat) "1")
              (incf (gethash :modified repo-statistics)))
             ((string= (first stat) "2")
              (incf (gethash :modified repo-statistics)))
             ((string= (first stat) "u")
              (incf (gethash :unmerged repo-statistics)))
             ((string= (first stat) "?")
              (incf (gethash :untracked repo-statistics))))
           (setf state :READ)))))))

(defun normalize-git-repos (rootdir remotes-hash)
  "Ensure that all repos in ROOTDIR are configured the same way.

REMOTE-HASH is a hash table specifying what to do for each repositories and there information."
  (dolist (dir (inferior-shell:run/lines `(ls ,rootdir)))
    (let ((fullpath (reve-workshop.tools:merge-pathnames-to-string rootdir dir)))
      (if (is-repo-p fullpath)
          (unless (has-remote-p fullpath)
            (let ((remote-data (gethash dir remotes-hash)))
              (when remote-data
                (cond ((equal (second remote-data) #\o)
                       (progn
                         (set-remote fullpath (first remote-data))
                         (inferior-shell:run/nil `(progn
                                                    (cd ,fullpath)
                                                    (git push -u origin --all)))))
                      ((equal (second remote-data) #\b)
                       (set-remote fullpath (first remote-data) "neo" "neo"))
                      ))))
          (progn
            (let ((remote-data (gethash dir remotes-hash)))
              (when remote-data
                (cond ((equal (second remote-data) #\o)
                       (progn
                         (make-git-repo fullpath (first remote-data))
                         ;; (set-remote fullpath (first remote-data))
                         (inferior-shell:run/nil `(progn
                                                    (cd ,fullpath)
                                                    (git push -u origin --all)))))
                      ((equal (second remote-data) #\b)
                       (progn
                         (make-git-repo fullpath (first remote-data))
                         (set-remote fullpath (first remote-data) "neo" "neo")))))))))))

(defun repository-statistics (repo-dir)
  "Return a REPO-STAT object containing statistics about REPO-DIR.

If =REPO-DIR= is not a git repository, then =REPO-STAT::IS-REPO= will be
/nil/ and all other fields set to there default values. Else
=REPO-STAT::IS-REPO= will be /T/ and each field of the object will be
filled with the statistics computed from the information gather with
the available git commands."
  (let ((stat (make-repo-stat))
        (repo-status-detail))
    (progn
      (if (not (is-repo-p repo-dir))
          (setf (repo-stat-is-repo stat) nil)
          (progn
            (setf repo-status-detail (get-status-detail repo-dir))
            (setf (repo-stat-is-repo stat) t)
            (setf (repo-stat-remotes stat) (remotes-count repo-dir))
            (setf (repo-stat-branches stat) (branches-count repo-dir))
            (setf (repo-stat-branch stat) (gethash :branch repo-status-detail ""))
            (setf (repo-stat-untracked stat) (gethash :untracked repo-status-detail 0))
            (setf (repo-stat-unmerged stat) (gethash :unmerged repo-status-detail 0))
            (setf (repo-stat-modified stat) (gethash :modified repo-status-detail 0))
            (setf (repo-stat-commits stat) (gethash :commits repo-status-detail (make-commits-stat)))))
      stat)))



;; (with-childs-dir-of "/home/roland/codes/reve/conf.private.d/" (git status))

;; (inferior-shell:run/lines '(progn
;;                             (for repo in {repo1,repo2,repo3,repo4})
;;                             (do)
;;                             (cd $ROOTDIR/repo1)
;;                             (git status)
;;                             (done)
;; ))


;; (defmacro with-childs-dir-of (root &body body)
;;   (let ((child-dir (gensym))
;;         (cmd-list (gensym)))
;;     (progn
;;       (setf cmd-list (apply #'append (lambda (repo) ()))))



;;     ;; `(dolist (,child-dir (inferior-shell:run/lines '(ls ,root)))
;;     ;;    ,@body)
;;     ))
