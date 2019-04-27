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
(defconst spydez/tasks/separator/dir-name "_"
  "Split directory name on this to extract date, dedup number, and description.")


;;------------------------------------------------------------------------------
;; Taskspace Commands
;;------------------------------------------------------------------------------

;; TODO: open spydez/dir/tasks in dired mode buffer

;; TODO: dwim today's task ? (create if none, choose from multiple, open the one...)

;; TODO: dwim <date>'s task ?

;; TODO: optional/prefix arg for yesterday, day before, etc? Or use org-mode's
;; calendar picker thing?


;;------------------------------------------------------------------------------
;; Taskspace Utils
;;------------------------------------------------------------------------------

;; TODO: util to split up dir name and then give bits back?
;;  - should work for manually made ones that don't have the middle <#> part
(defun spydez/taskspace/split-name (name part)
  "Splits name based on taskspace naming/separator rules and returns the
requested part. Part can be one of: '(date number description)."
  (let ((split-name (split-string name spydez/tasks/separator/dir-name)))
    (when (not (null split-name))
      ;; now cond? cond instead of when?

  ;; TODO-TODAY: hi
      )))

(defun spydez/taskspace/dir= (dir name part)
  ;; todo-today: strip dir down file name
  ;; strip file name down to part
  ;; check against name?
  )


;; Get children directories of spydez/dir/tasks, ignoring
;; spydez/dir/tasks/always-ignore.
(defun spydez/taskspace/list-all ()
  "Get children directories of spydez/dir/tasks, ignoring spydez/dir/tasks/always-ignore."

  (let (task-dirs) ;; empty list for return value
    ;; loop on each file in the directory
    (dolist (file (directory-files spydez/dir/tasks 'full) task-dirs)
      (when (and (file-directory-p file) ;; ignore files and...
                 (not (member ;; ignore things in ignore list
                       (file-name-nondirectory file)
                       spydez/dir/tasks/always-ignore)))
        (push file task-dirs)
        ))
    ;; dolist returns our constructed list since we put it as `result'
    ;; so we're done
  ))
;; (message "%s" (spydez/taskspace/list-all))


;; Get all, pare list down to today, return.
;;   - split on "_", then string= on today's date's string.
(defun spydez/taskspace/list-date (date-str)
  "Get any/all taskspaces for today."
  (let ((task-dirs (spydez/taskspace/list-all))
        (date-dirs)) ;; return val
    (dolist (dir task-dirs date-dirs)
      (let ((split (split-string (file-name-nondirectory dir)
                                 spydez/tasks/separator/dir-name)))
        (when (and (not (null split))
                   (string= date-str (car split)))
          ;; This dir's first bit is equal to today's formatted date.
          ;; Yay.
          (push dir date-dirs)
      )))))
;; (spydez/taskspace/list-date "2019-04-25")

;; TODO: if 0 today, prompt user for short description in minibuffer,
;;   then make dir and stuff happens.

;; TODO: if 1, go to that dir and stuff happens.

;; TODO: if >1, propt user for choice, then go to choice and stuff happens.

;; TODO: what is "stuff happens"?
;;   - make/open org file?
;;   - open dired buffer so user can pick stuff?

;; TODO: a projectile file or something so projectile knows the folder is, like, a thing?


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: move to its own namespace? `taskspace/*' instead of a variety of
;; `spydez/taskspace/*' and `spydez/*'?


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'taskspace)
