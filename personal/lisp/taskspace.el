;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; TODO: move elsewhere.
;;   - maybe: personal/dev/domains/{home,work}/dev-directories.el
(defvar spydez/dir/tasks
  "C:/home/spydez/taskspaces/" ;; TODO: Debug dir. Use below instead.
  ;; (spydez/dir-name "workspace" spydez/dir/home)
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
(defun spydez/taskspaces/task-dir/clipboard ()
  "Command to copy full task's dir path to clipboard."
  (interactive)

  ;; TODO: what are all the cases here?
  ;;   - 0 tasks today -> nil?
  ;;   - 1 -> that
  ;;   - 2+ -> ???
  ;;   - previous dates???
  (error "Just a stub right now... TODO this.")
  )


;; TODO: interactive? What flag?
;;   - if not interactive, move down to utils section
(defun spydez/taskspace/task-name/dwim (input)
  "Returns task-name from input."

  ;; if not command, return nil instead of error
  (cond ((null input) (error "Input is nil."))

        ;; TODO: if input not correct format -> error
        ;;   - strip to last bit if path

        ;; if dir, naively assume it's a taskspace
        ((file-directory-p input) (file-name-nondirectory input))

        ;; TODO: if not dir, add root and see if dir now?
        ))
;; (spydez/taskspace/task-name/dwim "C:/home/spydez/taskspaces/2019-05-03_0_hello-there")


;; TODO: dwim today's task dir? (create if none, choose from multiple, return if just the one...)
(defun spydez/taskspace/task-dir/dwim (arg)
  "DWIM to return today's task dir string (full path)...
Create if none. Return if just the one. Choose from multiple."
  (interactive "p") ;; numeric prefix arg
  ;; format-time-string: use optional TIME arg for "not-today" (see help)
  (let* ((date (format-time-string spydez/datetime/format/yyyy-mm-dd))
         (taskspaces (spydez/taskspace/list-date date))
         (length-ts (length taskspaces)))

    (cond ((null date) (error "Date string is nil: %s" date))

          ;; TODO: if zero, create one (prompt for short desc)
          ;;  - just alphanumeric and space/-, convert spaces to dashes?
          ((null taskspaces) (error "TODO: No taskspace for date %s. Create one here." date))
          ;; TODO: forward to maker cmd func

          ;; If just one, return it.
          ;; How to create second in that case? Use a non-dwim create func?
          ;;   - I think yes.
          ((= 1 length-ts) (car taskspaces))

          ;; TODO: if 2+, prompt for which to return (ideally by fuzzy match?)
          ((> 1 length-ts)
           (error "TODO: Multiple taskspaces. Prompt for which to return (ideally by fuzzy match?)"))
          ;; TODO: forward to chooser cmd func (one choice should be "new...")

          ;; Don't need a default case... Fall through with nil.
          ;;(t nil)
    )))
;; M-x spydez/taskspace/task-dir/dwim
;; (spydez/taskspace/task-dir/dwim 0)


(defun spydez/taskspace/create (arg)
  "Create a new taskspace."
  (interactive "sShort Description: ")
  ;; Do we need a max len? Leaving out until I feel otherwise.

  ;; Verify alphanumeric w/ hyphen and underscore?
  ;; Verify something looser? "String can be filename?"
  ;; Verify with both:
  ;;   - the 'file-name-invalid-regexp' (sanity)
  ;;   - and also alphanumeric /w -_ (sensibility)

  (message "%s" arg)
  ;; (error "TODO: this")
  )
;; M-x spydez/taskspace/create
;; (spydez/taskspace/create "testing-create")


;; TODO: open spydez/dir/tasks in dired mode buffer? open task dir in dired?

;; TODO: dwim <date>'s task ? (is this a dupe of spydez/taskspace/task-dir/dwim?)

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
  (unless (or (null name) (null part))
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
;; (spydez/taskspace/split-name "2000_0_foo" nil)


(defun spydez/taskspace/dir= (name dir part)
  "True if `name' is equal to the split-name `part' of `dir'.
Else nil."
  ;; don't accept nulls
  (unless (or (null name) (null dir) (null part))
    ;; strip dir down to file name and
    ;; strip file name down to part (if non-nil part)
    (let* ((dir-name (file-name-nondirectory dir))
           (dir-part (spydez/taskspace/split-name dir-name part)))
      (if (null dir-part)
          nil ;; don't accept nulls
        ;; else, usable data
        ;; check against input name
        (string= name dir-part)
        ))))
;; (spydez/taskspace/dir= "2000" "c:/foo/bar/2000_0_testcase" 'date)


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


;; Get all, pare list down to date-str, return.
(defun spydez/taskspace/list-date (date-str)
  "Get any/all taskspaces for today."
  (unless (null date-str)
    (let ((task-dirs (spydez/taskspace/list-all))
          (date-dirs)) ;; return val
      (dolist (dir task-dirs date-dirs)
        (when (spydez/taskspace/dir= date-str dir 'date)
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
