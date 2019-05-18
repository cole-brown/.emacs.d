;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; TODO: move elsewhere.
;;   - maybe: personal/dev/domains/{home,work}/dev-directories.el
;;   - defaults here? maybe a (with-var ...) or something?
(defconst spydez/dir/tasks
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

;; TODO: Turn these into regexes w/ capture groups, I think...
;; Have make-name and split-name use the regexes to make/split.
;; http://ergoemacs.org/emacs/elisp_string_functions.html
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

(defconst spydez/tasks/dir-name/valid-desc-regexp "^[[:alnum:]_\\-]\\{3,\\}$"
  "Letters, numbers, underscore, and hypen are valid.")


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
  (let* ((date (format-time-string spydez/datetime/format/yyyy-mm-dd)) ;; todo: get-date func
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
  "Creates a new taskspace for today with the description supplied."
  (interactive "sShort Description: ")
  ;; Do we need a max len? Leaving out until I feel otherwise.

  ;; is `arg' ok as desc part?
  (if (not (spydez/taskspace/verify-description arg))
      ;; fail w/ message and return nil?
      ;; (progn
      ;;   (message "Invalid description: %s" arg)
      ;;   nil)
      ;; Trying out just erroring out instead.
      ;; We are up to the interactive level now.
      (error "Invalid description: %s" arg)

    ;; else
    ;; create the dir/project for today
    (let ((taskpath (spydez/taskspace/create-dir arg 'today)))
      (if (null taskpath)
          ;; couldn't create it for some reason...
          (error "Error creating taskspace directory for: %s" arg)

        ;; else
        ;; TODO-TODAY
        ;; put a projectile file into dir?
        ;; named: .projectile
        ;;   https://projectile.readthedocs.io/en/latest/projects/#ignoring-files

        ;; put skeleton org into dir?
        ;; or just build buffer to save into dir as org notes.
        ;;   - notes.<desc>.org?
        ;;   - _notes.<desc>.org?

        ;; copy taskpath to clipboard?
        ))))
;; M-x spydez/taskspace/create
;; (spydez/taskspace/create "testing-create")


;; TODO: open spydez/dir/tasks in dired mode buffer? open task dir in dired?

;; TODO: dwim <date>'s task ? (is this a dupe of spydez/taskspace/task-dir/dwim?)

;; TODO: optional/prefix arg for yesterday, day before, etc? Or use org-mode's
;; calendar picker thing?


;;------------------------------------------------------------------------------
;; Taskspace Utils
;;------------------------------------------------------------------------------
(require 'cl) ;; for `some'
(defun spydez/taskspace/create-dir (description date-arg)
  "Creates dir w/ description, date, and (generated) number, if valid & unused description."
         ;; Get today's date.
  (let* ((date (spydez/taskspace/get-date date-arg))
         ;; Get today's dirs.
         (date-dirs (spydez/taskspace/list-date date))
         ;;   - figure out index of this one
         (number (spydez/taskspace/get-number date-dirs))

         ;; Build dir string from all that.
         (dir-name (spydez/taskspace/make-name date number description))
         (dir-full-path (expand-file-name dir-name spydez/dir/tasks)))

    ;; (message "create-dir: %s %s %s %s" date date-dirs number dir-name)

    ;; Only create if:
    ;;   - valid description input and
    ;;   - no dupes or accidental double creates
    ;;   - it doesn't exist (this is probably redundant if verify-description works right)
    (when (and (spydez/taskspace/verify-description description)
               (not (some (lambda (x) (spydez/taskspace/dir= description x 'description)) date-dirs))
               (not (file-exists-p dir-full-path)))

      ;; Make it.
      ;; make-directory helpfully has no data on what it returns or why or when
      ;; or anything. But it returns nil on success so... super useful guys.
      (make-directory dir-full-path)

      ;; How about we report something actually useful maybe?
      ;; Full path of created dir on... success?
      ;; Nil on folder non-existance.
      (if (file-exists-p dir-full-path)
          dir-full-path
        nil))))
;; (spydez/taskspace/create-dir "testcreate" nil)


(defun spydez/taskspace/get-number (dir-list)
  "Checks dirs in list, returns highest number part + 1."
  (let ((number -1))
    ;; check all input dirs (should be only one day's dirs but whatever...
    (dolist (dir dir-list number)
      (let ((dir-num-part (spydez/taskspace/split-name dir 'number)))
        ;; string-to-number can't deal with nil apparently
        (unless (null dir-num-part)
          (setq number (max number (string-to-number dir-num-part)))
          )))
    ;; number is set at max, we return next in sequence
    (1+ number)))
;; (spydez/taskspace/get-number '("foo/bar/2000_0_baz"))
;; (spydez/taskspace/get-number '())
;; (spydez/taskspace/get-number '("foo/bar/2000_0_baz" "foo/bar/2000_qux" "foo/bar/2000_qux_jeff" "foo/bar/2000_8_quux"))


(defun spydez/taskspace/get-date (arg)
  "Returns a date in the correct string format.
TODO: Use arg somehow for not-today dates?"
  ;; TODO: if arg, not today. Else, today.
  (format-time-string spydez/datetime/format/yyyy-mm-dd))
;; Today: (spydez/taskspace/get-date nil)
;; Also Today?: (spydez/taskspace/get-date 'today)
;; TODO: Not Today?: (spydez/taskspace/get-date -1)


(defun spydez/taskspace/verify-description (name)
  "Verifies that `name' is an allowable part of the directory name."

  ;; Sanity check 1: `name' must be a valid filename, for a very loose definition.
  ;; Sanity check 2: Not a path sep in there?
  ;; Valid check:    Verify name obeys my regexp.
  (let ((matched-invalid (string-match file-name-invalid-regexp name))
        (dir-sep-check (file-name-nondirectory name))
        (valid-name (string-match spydez/tasks/dir-name/valid-desc-regexp name)))

    ;; check for bad input, fail if so... Bad if:
    (if (or matched-invalid                    ;; - DOES match /invalid/ filename regexp
            (not (string= name dir-sep-check)) ;; - or non-dir name DOES NOT match input name
            (null valid-name))                 ;; - or DOES NOT match /valid/ name regexp
        ;; Just return nil for fail.
        nil

      ;; else... Ok name. Do something?

      ;; Verify they didn't try to give us the whole thing? (check for date?)
      ;; (Eh... Not gonna bother right now.)

      ;; return input when valid
      name
      )))
;; weird name: (spydez/taskspace/verify-description "\0")
;; too short:  (spydez/taskspace/verify-description "0")
;; good!:      (spydez/taskspace/verify-description "hello-there")
;; dir sep:    (spydez/taskspace/verify-description "hello-there/here")

(defun spydez/taskspace/make-name (date number description)
  "Creates a full name from inputs obeying first formatting order
found in parts-alists."
        ;; How long is the parts-alist we're looking for
  (let* ((name-parts (seq-map (lambda (x) (format "%s" x)) ;; stringify each (don't want nulls here...)
                              (seq-remove #'null ;; but take out nulls?
                                          (list date number description)))) ;; turn inputs into list
         (name-len (length name-parts))
         (split-alist))
    ;; find the right alist for building the dir string
    ;; TODO: pull this out of here and split-name and make func maybe?
    (dolist (alist spydez/tasks/dir-name/parts-alists split-alist)
      (when (= name-len (length alist))
        (setq split-alist alist)))

    ;; (message "make-name: %s->%s %s %s null?%s"
    ;;          name-parts (seq-remove #'null name-parts)
    ;;          name-len
    ;;          split-alist (null split-alist))

    (unless (null split-alist)
      (string-join (seq-remove #'null name-parts) spydez/tasks/dir-name/separator)
        )))
;; (spydez/taskspace/make-name "2000" "1" "hi")
;; (spydez/taskspace/make-name "2000" nil "hi")
;; (spydez/taskspace/make-name "hi" nil nil)
;; (spydez/taskspace/make-name "2019-05-14" 0 "testcreate")


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
          (setq split-alist alist)))

      ;; now try to pull out part requested
      (if (not (assoc part split-alist))
          nil ;; they requested something invalid for this `name'

        ;; figure out what index is desired,
        ;; then pull out the desired string (and return it)
        (nth (cdr (assoc part split-alist)) split-name)
        ))))
;; (spydez/taskspace/split-name "2000_0_foo" 'date)
;; (spydez/taskspace/split-name "2000_0_foo" nil)
;; (spydez/taskspace/split-name "2000_0_foo" 'number)
;; (spydez/taskspace/split-name "2000_foo" 'number)
;; TODO: make work with 3+ where date is 1, number is 2, 3+ are all desc that had "_" in it...
;; (spydez/taskspace/split-name "2000_0_foo_jeff" 'number)

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

;; TODO: rename `spydez/tasks/*' vars to `spydez/taskspace/*'?
;; TODO: move to its own dir like use-tool?

;; TODO: uh... tests?

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'taskspace)
