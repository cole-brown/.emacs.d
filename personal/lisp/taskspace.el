;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; TODO: move elsewhere.
;;   - maybe: personal/dev/domains/{home,work}/dev-directories.el
(defvar spydez/dir/tasks (spydez/dir-name "workspace" spydez/dir/home)
  "User's folder for small work tasks.")

;; Gonna need regex for this eventually, probably. But not now.
(defconst spydez/dir/tasks/always-ignore '("." ".." "00_archive")
  "Always ignore these when getting/determining a taskspace directory.")

;; directory name format: <date>_<#>_<description>
;;   - <date>: yyyy-mm-dd format
;;   - <#>:    two digit number starting at zero, for multiple tasks per day
;;   - <description>: don't care - human info

;;------------------------------------------------------------------------------
;; Taskspace Directory
;;------------------------------------------------------------------------------

;; Get children directories of spydez/dir/tasks, ignoring
;; spydez/dir/tasks/always-ignore.
;; TODO: optional/prefix arg for yesterday, day before, etc...
(defun spydez/taskspace/list (arg)
  "Get children directories of spydez/dir/tasks, ignoring spydez/dir/tasks/always-ignore."
  (interactive "p")

  (let (task-dirs) ;; empty list for return value
    ;; loop on each file in the directory
    (dolist (file (directory-files spydez/dir/tasks 'full) task-dirs)
      (when (and (file-directory-p file) ;; ignore files and...
                 (not (member ;; ignore things in ignore list
                       (file-name-nondirectory file)
                       spydez/dir/tasks/always-ignore)))
        (push file task-dirs)
        ))
    ;; and return our constructed list
    task-dirs))
;; (message "%s" (spydez/taskspace/list 0))

;; TODO: pare list down to today
;;   - split on "_", then string= on today's date's string.
;; TODO: if 0 today, prompt user for short description in minibuffer,
;;   then make dir and stuff happens.
;; TODO: if 1, go to that dir and stuff happens.
;; TODO: if >1, propt user for choice, then go to choice and stuff happens.

;; TODO: what is "stuff happens"?
;;   - make/open org file?
;;   - open dired buffer so user can pick stuff?


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'taskspace)
