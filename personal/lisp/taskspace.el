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

(defconst spydez/tasks/dir-name/separator "_"
  "Split directory name on this to extract date, dedup number, and description.")

(defconst spydez/tasks/dir-name/parts-alists
  '(
    ;; Three part. Code does this all the time?
    ((date . 0)
     (number . 1)
     (description . 2))
    ;; Two part. Human does this most of the time...
    ((date . 0)
     (description . 2))
   )
  "Order of items in task's descriptive directory name. List of alists.
First one of the correct length is used currently.")
;; (cdr (assoc 'date (nth 0 spydez/tasks/dir-name/parts-alists)))


;;------------------------------------------------------------------------------
;; Taskspace Commands
;;------------------------------------------------------------------------------

;; TODO: command to copy full task's dir path to clipboard
;;   - will need the dwim today's task, I think?
;;   - could just stub out dwim with "find today's pre-existing else taskspace root dir"
;; TODO: dwim today's task ? (create if none, choose from multiple, open the one...)

;; TODO: open spydez/dir/tasks in dired mode buffer

;; TODO: dwim <date>'s task ?

;; TODO: optional/prefix arg for yesterday, day before, etc? Or use org-mode's
;; calendar picker thing?


;;------------------------------------------------------------------------------
;; Taskspace Utils
;;------------------------------------------------------------------------------

;; util to split up dir name and then give desired bit back
;;  - should work for manually made ones that don't have the middle <#> part
(defun spydez/taskspace/split-name (name part)
  "Splits name based on taskspace naming/separator rules and returns the
requested part. Part can be one of: '(date number description)."
  (unless (null name)
    ;; unless or if/error?
    (let* ((split-name (split-string name spydez/tasks/dir-name/separator))
           (len-split (length split-name))
           (split-alist))

      ;; find the right alist for parsing the split dir string
      (dolist (alist spydez/tasks/dir-name/parts-alists split-alist)
        (when (= len-split (length alist))
          (setq split-alist alist)
          ))

      ;; figure out what index is desired,
      ;; then pull out the desired string (and return it)
      (nth (cdr (assoc part split-alist)) split-name)
      )))
;; (spydez/taskspace/split-name "2000_0_foo" 'date)

(defun spydez/taskspace/dir= (name dir part)
  "True if `name' is equal to the split-name `part' of `dir'.
Else nil."
  ;; todo-today: strip dir down to file name
  ;; strip file name down to part (if non-nil part)
  ;; check against input name
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
                                 spydez/tasks/dir-name/separator)))
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
