;;; taskspace.el --- Extremely Simple Taskspace/Workspace management -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2019  Cole Brown
;; License: MIT License
;; Author: Cole Brown <git@spydez.com>
;; Created: 2019-04-24
;; Version: 0.3

;; Package-Requires: ((emacs "26.1"))


;;; Commentary:

;; taskspace.el is an extremely KISS taskspace/workspace generator/manager.

;; FAQ:
;;   1) Can it do X?
;;      - No... I really meant simple.

;; It can make a folder based on a simple dating and numbering scheme, with a
;; simple description tacked on for human usability.

;; It can copy files into the new taskspace. It can generate files based on a
;; static string or a function supplied in taskspace vars.

;; It can copy the taskspace's full path or name to the kill ring/clipboard.

;; It can open the taskspace dir itself (or the taskspace parent dir)
;; in a buffer.


;;---------------
;; Commands:
;;---------------
;;   DWIM commands:
;;     - Accepts numeric prefix arg.
;;       - 0 or no prefix: Today's date
;;       - positive: Future date; N days from now.
;;       - negative: Past date; N days back.
;;     - DWIM means:
;;       - If none for date: Create.
;;       - If just one existing: Return it.
;;       - If multiple: Choose from prompt of options.
;;     `taskspace/task-dir/dwim'
;;       - Fully qualified path to taskspace.
;;         - e.g. "c:/home/user/taskspace/2019-01-01_2_some-task-name"
;;     `taskspace/task-name/dwim'
;;       - Taskspace directory name only.
;;         - e.g. "2019-01-01_2_some-task-name"
;;
;;   DWIM derived:
;;     - Use `taskspace/task-dir/dwim' to determine which taskspace is
;;       intended from the context.
;;     `taskspace/dired'
;;       - Opens the directory of a task in emacs
;;         (uses find-file, so defaults to dired-mode buffer).
;;     `taskspace/shell'
;;       - Opens the directory of all tasks in emacs (aka `taskspace/dir')
;;         (uses find-file, so defaults to dired-mode buffer).
;;
;;   Other Commands:
;;     `taskspace/create'
;;       - Create a new taskspace. Will prompt for the short description.
;;         - e.g. description of "2019-01-01_2_some-task-name" is "some-task-name".
;;     `taskspace/parent-dired'
;;       - Opens the directory of all tasks in emacs (aka `taskspace/dir')
;;         (uses find-file, so defaults to dired-mode buffer).


;;---------------
;; Settings:
;;---------------
;; See 'General Settings' header and all the `defcustom' defined vars for
;; taskspace to find out what all can be customized right now.
;;
;; Or see Customize help for `taskspace' (M-x customize-group RET taskspace)


;;---------------
;; Configuration:
;;---------------
;; Simple to set up with use-package.
;;
;; use-package example:
;;  (use-package taskspace
;;    :custom
;;    (taskspace/datetime/format "%Y-%m-%d")
;;    ;; (taskspace/shell-fn #'shell) ;; leave as default
;;    (taskspace/dir "~/workspace")
;;
;;    (taskspace/gen-files-alist
;;     ;; projectile: empty file
;;     '((".projectile" . "")
;;       ;; notes.org: setup with my org header snippet ready to go
;;       ("_notes.org" . "<org/header")))
;;
;;    ;; others to consider:
;;    ;; (taskspace/dir/copy-files-src ...)
;;    ;; (taskspace/dir/always-ignore ...)
;;    ;; (taskspace/dir-name/separator ...)
;;    ;; (taskspace/dir-name/parts-alists ...)
;;    ;; (taskspace/dir-name/valid-desc-regexp ...)
;;    )



;;; Code:


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------


(require 'cl) ;; for `some'
(require 'seq) ;; for `seq-contains'

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

(defgroup taskspace nil
  "Tool for creating/using simple taskspaces/workspaces for short tasks,
bug investigation, log munging, or whatever."
  :group 'convenience)

(defcustom taskspace/datetime/format "%Y-%m-%d"
  "Date format for parsing/naming taskspace directory names."
  :group 'taskspace
  :type 'string)

(defcustom taskspace/shell-fn #'shell
  "Function to call to open shell buffer. `shell' and `eshell' work."
  :group 'taskspace
  :type 'function)

(defcustom taskspace/dir
  (file-name-as-directory (expand-file-name "taskspace" user-emacs-directory))
  "User's folder for small work tasks."
  :group 'taskspace
  :type 'directory)

(defcustom taskspace/dir/copy-files-src
  (file-name-as-directory (expand-file-name "taskspace-new" taskspace/dir))
  "User's folder for files to copy into new taskspaces."
  :group 'taskspace
  :type 'directory)

;;(defun taskspace/test-gen (taskname taskpath) (format "%s" "testing the file gen func"))
(defcustom taskspace/gen-files-alist
  '((".projectile" . "") ;; empty file
    ("_notes.org" . "")) ;; also empty
  "Files to generate for new taskspaces. Expects an alist like:
'(('file1.name' . 'contents') ('file2.name' . your-gen-function))

Note: `taskname' and `taskpath' are supplied as the args to the
generator functions. Taskpath is the fully expanded file path."
  :group 'taskspace
  :type '(alist :key-type string
                :value-type (choice string function)))

;; Gonna need regex for this eventually, probably. But not now.
(defcustom taskspace/dir/always-ignore
  '("." ".."
    "00_archive"
    (file-name-nondirectory taskspace/dir/copy-files-src))
  "Always ignore these when getting/determining a taskspace directory."
  :group 'taskspace
  :type 'sexp)

;; directory name format: <date>_<#>_<description>
;;   - <date>: yyyy-mm-dd format
;;   - <#>:    two digit number starting at zero, for multiple tasks per day
;;   - <description>: don't care - human info

(defcustom taskspace/dir-name/separator "_"
  "Split directory name on this to extract date, dedup number, and description."
  :group 'taskspace
  :type 'string)

;; TODO: Turn these into regexes w/ capture groups, I think?..
;; Have make-name and split-name use the regexes to make/split.
;; http://ergoemacs.org/emacs/elisp_string_functions.html
;; TODO: use this nice regex builder (elisp sexprs)?:
;;   https://www.reddit.com/r/emacs/comments/cf8r83/easier_editing_of_elisp_regexps/eu84ob1/
(defcustom taskspace/dir-name/parts-alists
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
First one of the correct length is used currently."
  :group 'taskspace
  :type 'sexp)
;; (cdr (assoc 'date (nth 0 taskspace/dir-name/parts-alists)))

(defcustom taskspace/dir-name/valid-desc-regexp "^[[:alnum:]_\\-]\\{3,\\}$"
  "Letters, numbers, underscore, and hypen are valid."
  :group 'taskspace
  :type 'regexp)


;;------------------------------------------------------------------------------
;; Taskspace Commands
;;------------------------------------------------------------------------------


;; TODO-DWIM: If in a taskspace file/folder, return that.

;;;###autoload
(defun taskspace/task-name/dwim ()
  "Interactive. DWIM to clipboard and return today's task string (partial/final path)...
Create if none. Return if just the one. Choose from multiple."
  (interactive)

  ;; Get task's full path, reduce to just task directory...
  (let* ((fullpath (call-interactively #'taskspace/task-dir/dwim))
         (taskname (file-name-nondirectory fullpath)))

    ;; copy to clipboard
    (kill-new taskname)

    ;; We'd need another return from task-dir/dwim to know what it did...
    ;; ;; say what we did
    ;; (message "Existing taskspace: %s" taskname)

    ;; return it
    taskname
    ))
;; (taskspace/task-name/dwim)
;; M-x taskspace/task-name/dwim


;;;###autoload
(defun taskspace/task-dir/dwim (date-input)
  "Interactive. DWIM to clipboard and return today's task dir string (full path)...
Create if none. Return if just the one. Choose from multiple."
  ;; Numeric arg but don't let lower case "p" auto-magic nothing (no prefix arg)
  ;; into 1. Nothing/0/nil is today. 1 is tomorrow.
  (interactive "P")

  ;; Default to "today" if date-input isn't parsable string,
  ;; then get date, taskspaces, etc. for that numerical relative day.
  (let* ((date-input (cond
                      ;; no date-input is today is 0
                      ((null date-input) 0)
                      ;; strings should be converted to numbers
                      ((stringp date-input)
                       (string-to-number date-input))
                      ;; just allow numbers through unscathed
                      ((numberp date-input) date-input)
                      ;; default to... today/0 I guess?
                      (t 0)))
         (date (taskspace/get-date date-input))
         (taskspaces (taskspace/list-date date))
         (length-ts (length taskspaces)))

    (cond
     ;; error out if we have no idea what date to dwim with...
     ((null date) (error "Date string is nil: %s" date))

     ;; if none, create one.
     ((null taskspaces)
      ;; call-interactively will give user prompt for description,
      ;; etc. as if they called themselves.
      (call-interactively #'taskspace/create))

     ;; If just one, return it.
     ;; How to create second in that case? Use a non-dwim create func?
     ;;   - I think yes.
     ((= length-ts 1)

      ;; copy to clipboard
      (kill-new (first taskspaces))
      ;; say what we did
      (message "Existing taskspace: %s" (first taskspaces))
      ;; return it
      (first taskspaces))

     ;; For now, only give existing choices. User can use a non-dwim create func if they want new.
     ((> length-ts 1)

      ;; list available choices to user, get the taskspace they chose
      (let ((choice (taskspace/list-choices taskspaces 'nondirectory)))
        ;; copy to clipboard
        (kill-new choice)
        ;; say what we did
        (message "Chose taskspace: %s" choice)
        ;; return it
        choice
        ))

     ;; Don't need a default case... Fall through with nil.
     ;;(t nil)
     )))
;; M-x taskspace/task-dir/dwim
;; (taskspace/task-dir/dwim)
;; (taskspace/task-dir/dwim -1)


;; TODO: support creating for non-today dates
;;;###autoload
(defun taskspace/create (desc)
  "Interactive. Creates a new taskspace for today with the description supplied."
  ;; Do we need a max len? Leaving out until I feel otherwise.
  (interactive "sNew Task Short Description: ")

  ;; is `desc' ok as description part?
  (if (not (taskspace/verify-description desc))
      ;; fail w/ message and return nil?
      ;; (progn
      ;;   (message "Invalid description: %s" desc)
      ;;   nil)
      ;; Trying out just erroring out instead.
      ;; We are up to the interactive level now.
      (error "Invalid description: %s" desc)

    ;; else:
    ;; create the dir/project for today
    (let ((taskpath (taskspace/create-dir desc 'today)))
      (if (null taskpath)
          ;; couldn't create it for some reason...
          ;; TODO: Better reasons if known. "already exists" would be nice for that case.
          (error "Error creating taskspace directory for: %s" desc)

        ;; else:
        ;; copy files into new taskspace
        (unless (not (file-directory-p taskspace/dir/copy-files-src))
          (apply #'taskspace/copy-files
                 ;; arg 1: our new taskpath
                 taskpath
                 ;; arg &rest: all the files to copy with:
                 ;;   - full path name
                 ;;   - no dot files
                 ;;     - no '.', '..'
                 ;;     - yes actual dotfiles somehow?
                 ;;     - This is what I want, so... ok.
                 (directory-files taskspace/dir/copy-files-src
                                  'full
                                  directory-files-no-dot-files-regexp)))

        ;; gen files into new taskspace
        (unless (not taskspace/gen-files-alist)
          (let ((gen-errors (taskspace/generate-files taskpath taskspace/gen-files-alist)))
            (when gen-errors
              (error "Taskspace file generation errors: %s" gen-errors))))

        ;; Either of those can put a projectile file into the taskspace.
        ;; Just name it: .projectile
        ;;   See: https://projectile.readthedocs.io/en/latest/projects/#ignoring-files

        ;; Can also put skeleton org file. Or just org file with yasnippet ready to go...
        ;; TODO: dynamic names for files?
        ;;   - notes.<desc>.org?
        ;;   - _notes.<desc>.org?

        ;; copy taskpath to clipboard
        (kill-new taskpath)
        ;; say something
        (message "Created taskspace: %s" (file-name-nondirectory taskpath))
        ;; (message "Created taskspace: %s" taskpath)
        ;; return it
        taskpath
        ))))
;; M-x taskspace/create
;; (taskspace/create "testing-create")


;;;###autoload
(defun taskspace/dired ()
  "Interactive. Opens the current taskspace's top dir in emacs."
  (interactive)

  ;; prompt user for the taskspace with an attempt at DWIM
  (let ((task (call-interactively #'taskspace/task-dir/dwim)))
    ;; expecting a path from task-dir/dwim
    (if (not (file-directory-p task))
        ;; not a dir - error out
        (error "Can't find taskspace (not a directory?): '%s'" task)

      ;; ok - message and open (probably in dired but let emacs decide)
      (find-file task)
      ;; say something
      (message "Opening taskspace: %s" (file-name-nondirectory task))
      ;; return the chosen task's dir?
      task
      )))
;; (taskspace/dired)
;; M-x taskspace/dired


;;;###autoload
(defun taskspace/parent-dired ()
  "Interactive. Opens the taskspace's overall top dir in emacs."
  (interactive)

  (if (not (file-directory-p taskspace/dir))
      ;; not a dir - error out
      (error "Can't find taskspace parent directory: '%s'" taskspace/dir)

    ;; ok - message and open (probably in dired but let emacs decide)
    (find-file taskspace/dir)
    ;; say something
    (message "Opening taskspace parent: %s" (file-name-nondirectory taskspace/dir))
    ;; return the top dir?
    taskspace/dir
    ))
;; (taskspace/parent-dired)
;; M-x taskspace/parent-dired


;; TODO: Need to get my shell better. MSYS/Git-Bash shell and emacs
;; don't like each other all that much by default.

;;;###autoload
(defun taskspace/shell ()
  "Interactive. Opens the current taskspace's top dir in an emacs shell buffer.
Shell opened can be set by modifying `taskspace/shell-fn'."
  (interactive)

  (if (not (functionp taskspace/shell-fn))
      (error "`taskspace/shell-fn' is not bound to a fuction. %s" taskspace/shell-fn)

    ;; prompt user for the taskspace with an attempt at DWIM
    (let ((task (call-interactively #'taskspace/task-dir/dwim)))
      ;; expecting a path from task-dir/dwim
      (if (not (file-directory-p task))
          ;; not a dir - error out
          (error "Can't find taskspace (not a directory?): '%s'" task)

        ;; open with shell-fn
        (funcall taskspace/shell-fn)
        ;; say something
        (message "Opening taskspace shell: %s" (file-name-nondirectory task))
        ;; return the chosen task's dir?
        task
        ))))
;; (taskspace/shell)
;; M-x taskspace/shell


;; TODO: dwim <date>'s task ? (is this a dupe of taskspace/task-dir/dwim?)


;;------------------------------------------------------------------------------
;; Taskspace Utils
;;------------------------------------------------------------------------------


(defun taskspace/generate-files (taskpath file-alist)
  "Generates each file in alist into the new taskpath. Expects
((filename . string-or-func)...) from alist. Creates 'filename' in taskspace
and then inserts string into it, or uses func to generate contents.
Does not currently support directory structures/trees. Returns nil or error.
Error is all files not generated in alist: ((filename . 'reason')...)"

  ;; let it just do nothing when empty list
  (let (errors-alist ;; empty return value alist
        ;; Get taskname from path to supply to any file content gen funcs.
        (taskname (file-name-nondirectory taskpath)))
    (dolist (entry file-alist errors-alist)
      (let* ((file (file-name-nondirectory (car entry)))
             (filepath (expand-file-name file taskpath))
             (str-or-func (cdr entry)))

        (cond
         ;; ERROR: already exists...
         ((file-exists-p filepath)
          (push `(,filepath . "file already exist") errors-alist))

;;         ;; ERROR: generator not bound
;;         ((not (boundp str-or-func))
;;          (push `(,filepath . "string/function not bound") errors-alist))

         ;; ERROR: unknown generator
         ((and (not (stringp str-or-func))
               (not (functionp str-or-func)))
          (push `(,filepath . ,(format "generator is not string or function: %s"
                                       str-or-func))
                errors-alist))

         ;; HAPPY!
         (t
          ;; (message "for '%s', copying '%s' to '%s'" taskpath path (expand-file-name (file-name-nondirectory path) taskpath))
          (with-temp-file filepath
            (if (stringp str-or-func)
                (insert str-or-func)
              (insert (funcall str-or-func taskname taskpath)))))

         ;; dolist returns the errors
         )))))


(defun taskspace/copy-files (taskpath &rest filepaths)
  "Copy each of the files in `filepaths'. Expects well-qualified filepaths
(absolute, relative, or otherwise). Does not currently support
directory structures/trees. Returns nil or error. Error is all
files not copied in alist: ((filepath . 'reason')...)"

  ;; let it just do nothing when empty list
  (let (errors-alist) ;; empty return value alist
    (dolist (path filepaths errors-alist)
      (cond
       ;; ERROR: can't find or...
       ((not (file-exists-p path))
        (push `(,path . "file does not exist") errors-alist))
       ;; ERROR: can't read file or...
       ((not (file-readable-p path))
        (push `(,path . "file is not readable") errors-alist))
       ;; ERROR: not a file (dir or symlink or something)
       ((not (file-regular-p path))
        (push `(,path . "path is not a file") errors-alist))

       ;; HAPPY: copy it
       (t
        (copy-file path ;; from "the full path of where it is" to...
                   ;; taskpath + "the filename part of where it is"
                   (expand-file-name (file-name-nondirectory path) taskpath)))

       ;; dolist returns the errors
       ))))


(defun taskspace/create-dir (description date-arg)
  "Creates dir w/ description, date, and (generated) number, if valid & unused description."
         ;; Get today's date.
  (let* ((date (taskspace/get-date date-arg))
         ;; Get today's dirs.
         (date-dirs (taskspace/list-date date))
         ;;   - figure out index of this one
         (number (taskspace/get-number date-dirs))

         ;; Build dir string from all that.
         (dir-name (taskspace/make-name date number description))
         (dir-full-path (expand-file-name dir-name taskspace/dir)))

    ;; (message "create-dir: %s %s %s %s" date date-dirs number dir-name)

    ;; Only create if:
    ;;   - valid description input and
    ;;   - no dupes or accidental double creates
    ;;   - it doesn't exist (this is probably redundant if verify-description works right)
    (when (and (taskspace/verify-description description)
               (not (some (lambda (x) (taskspace/dir= description x 'description)) date-dirs))
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
;; (taskspace/create-dir "testcreate" nil)


(defun taskspace/get-number (dir-list)
  "Checks dirs in list, returns highest number part + 1."
  (let ((number -1))
    ;; check all input dirs (should be only one day's dirs but whatever...
    (dolist (dir dir-list number)
      (let ((dir-num-part (taskspace/split-name dir 'number)))
        ;; string-to-number can't deal with nil apparently
        (unless (null dir-num-part)
          (setq number (max number (string-to-number dir-num-part)))
          )))
    ;; number is set at max, we return next in sequence
    (1+ number)))
;; (taskspace/get-number '("zort/troz/2000_0_baz"))
;; (taskspace/get-number '())
;; (taskspace/get-number '("zort/troz/2000_0_baz" "zort/troz/2000_qux" "zort/troz/2000_qux_jeff" "zort/troz/2000_8_quux"))


(defun taskspace/get-date (arg)
  "Returns a date in the correct string format.
`arg' must be nil or 'today (for today), or numberp.
Returns date requested by arg, or nil."
  (let ((day nil))
    ;; check/convert input arg
    (cond ((null arg)
           ;; nil -> today -> 0
           (setq day 0))

          ((numberp arg)
           ;; if arg numberp: 0 today, negative before, positive after
           (setq day arg))

          ((string= arg 'today)
           ;; 'today -> 0
           (setq day 0))

          ;; error case(s): nil
          (t
           (setq day nil)))

    (unless (eq day nil)
      (let* ((now (current-time)) ;; right now
             (now-adjust-secs (* day 24 60 60)) ;; day arg to seconds
             (target (time-add now now-adjust-secs))) ;; actually when we want
        ;; format to spec and return
        (format-time-string taskspace/datetime/format target)))))
;; Examples/Tests:
;;                 Today: (taskspace/get-date nil)
;;            Also Today: (taskspace/get-date 'today)
;; Today Too... I guess?: (taskspace/get-date "today")
;;             Not Today: (taskspace/get-date -1)
;;             Not Today: (taskspace/get-date 1.9)
;;                 Error: (taskspace/get-date "jeff")
;;                 Error: (taskspace/get-date 'jeff)


(defun taskspace/verify-description (name)
  "Verifies that `name' is an allowable part of the directory name."

  ;; Sanity check 1: `name' must be a valid filename, for a very loose definition.
  ;; Sanity check 2: Not a path sep in there?
  ;; Valid check:    Verify name obeys my regexp.
  (let ((matched-invalid (string-match file-name-invalid-regexp name))
        (dir-sep-check (file-name-nondirectory name))
        (valid-name (string-match taskspace/dir-name/valid-desc-regexp name)))

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
;; weird name: (taskspace/verify-description "\0")
;; too short:  (taskspace/verify-description "0")
;; good!:      (taskspace/verify-description "hello-there")
;; dir sep:    (taskspace/verify-description "hello-there/here")


(defun taskspace/make-name (date number description)
  "Creates a full name from inputs obeying first formatting order
found in parts-alists."
        ;; How long is the parts-alist we're looking for
  (let* ((name-parts (seq-map (lambda (x) (format "%s" x)) ;; stringify each (don't want nulls here...)
                              (seq-remove #'null ;; but take out nulls?
                                          (list date number description)))) ;; turn inputs into list
         (name-len (length name-parts))
         split-alist)
    ;; find the right alist for building the dir string
    ;; TODO: pull this out of here and split-name and make func maybe?
    (dolist (alist taskspace/dir-name/parts-alists split-alist)
      (when (= name-len (length alist))
        (setq split-alist alist)))

    ;; (message "make-name: %s->%s %s %s null?%s"
    ;;          name-parts (seq-remove #'null name-parts)
    ;;          name-len
    ;;          split-alist (null split-alist))

    (unless (null split-alist)
      (mapconcat #'identity (seq-remove #'null name-parts) taskspace/dir-name/separator)
        )))
;; (taskspace/make-name "2000" "1" "hi")
;; (taskspace/make-name "2000" nil "hi")
;; (taskspace/make-name "hi" nil nil)
;; (taskspace/make-name "2019-05-14" 0 "testcreate")


;; util to split up dir name and then give desired bit back
;;  - should work for manually made ones that don't have the middle <#> part
(defun taskspace/split-name (name part)
  "Splits name based on taskspace naming/separator rules and returns the
requested part. Part can be one of: '(date number description)."

  (unless (or (null name) (null part))
    ;; unless or if/error?
    (let* ((split-name (split-string name taskspace/dir-name/separator))
           (len-split (length split-name))
           split-alist)

      ;; find the right alist for parsing the split dir string
      (dolist (alist taskspace/dir-name/parts-alists split-alist)
        (when (= len-split (length alist))
          (setq split-alist alist)))

      ;; now try to pull out part requested
      (if (not (assoc part split-alist))
          nil ;; they requested something invalid for this `name'

        ;; figure out what index is desired,
        ;; then pull out the desired string (and return it)
        (nth (cdr (assoc part split-alist)) split-name)
        ))))
;; (taskspace/split-name "2000_0_zort" 'date)
;; (taskspace/split-name "2000_0_zort" nil)
;; (taskspace/split-name "2000_0_zort" 'number)
;; (taskspace/split-name "2000_zort" 'number)
;; TODO: make work with 3+ where date is 1, number is 2, 3+ are all desc that had "_" in it...
;; (taskspace/split-name "2000_0_zort_jeff" 'number)

(defun taskspace/dir= (name dir part)
  "True if `name' is equal to the split-name `part' of `dir'.
Else nil."
  ;; don't accept nulls
  (unless (or (null name) (null dir) (null part))
    ;; strip dir down to file name and
    ;; strip file name down to part (if non-nil part)
    (let* ((dir-name (file-name-nondirectory dir))
           (dir-part (taskspace/split-name dir-name part)))
      (if (null dir-part)
          nil ;; don't accept nulls
        ;; else, usable data
        ;; check against input name
        (string= name dir-part)
        ))))
;; (taskspace/dir= "2000" "c:/zort/troz/2000_0_testcase" 'date)


;; Get children directories of taskspace/dir, ignoring
;; taskspace/dir/always-ignore.
(defun taskspace/list-all ()
  "Get children directories of taskspace/dir, ignoring taskspace/dir/always-ignore."

  (let (task-dirs) ;; empty list for return value
    ;; loop on each file in the directory
    (dolist (file (directory-files taskspace/dir 'full) task-dirs)
      (when (and (file-directory-p file) ;; ignore files and...
                 (not (member ;; ignore things in ignore list
                       (file-name-nondirectory file)
                       taskspace/dir/always-ignore)))
        (push file task-dirs)
        ))
    ;; dolist returns our constructed list since we put it as `result'
    ;; so we're done
  ))
;; (message "%s" (taskspace/list-all))


;; Get all, pare list down to date-str, return.
(defun taskspace/list-date (date-str)
  "Get any/all taskspaces for today."
  (unless (null date-str)
    (let ((task-dirs (taskspace/list-all))
          date-dirs) ;; return val
      (dolist (dir task-dirs date-dirs)
        (when (taskspace/dir= date-str dir 'date)
          (push dir date-dirs)
          )))))
;; (taskspace/list-date "2019-04-25")


;; Thank you to this thread:
;; https://emacs.stackexchange.com/questions/32248/how-to-write-a-function-with-an-interactive-choice-of-the-value-of-the-argument
;; I was not finding any usable help/tutorials/documentation
;; for my knowledge/skill level until I found that.
(defun taskspace/list-choices (taskspaces &optional display)
  "Given a list of taskspaces from e.g. taskspace/list-date,
prompt user with list of choices, take the user's input, and
match back up with an entry in the list of taskspaces.

`display' can be:
- nil: Pass taskspaces as-is to completion. AKA display as-is.
- nondirectory: Strip each element to `file-name-nondirectory'

Choice is matched back to taskspaces via dumb string matching. First
match in taskspaces that substring matches user's choice from
`completing-read' is returned as choice.

Returns nil or a string in `taskspaces'."

  ;; Figure out how to display to user first.
  (let (display-names)
    (cond
     ;; nil -> as-is
     ((null display)
      (setq display-names taskspaces))

     ;; nondirectory -> strip each to remove all parent dirs
     ((equal display 'nondirectory)
      (setq display-names (mapcar #'file-name-nondirectory taskspaces)))

     ;; unexpected -> error?
     ;;   - TODO: -> nil instead?
     (t (error "Unknown display option `%s'" display))
     )

    ;; Give user their choices...
    ;;
    ;; With helm at the wheel, this goes to helm--completing-read-default.
    ;; `confirm' to force completion to one complete choice.
    ;; TODO: Pretty up for Helm? Name the choices window something nice -
    ;;   it's just "pp-eval-expression" right now.
    (let ((choice (completing-read "Choose Taskspace: "
                                   display-names nil 'confirm)))

      ;; ...and match their choice back up with a taskname.
      (seq-contains taskspaces
                    choice
                    (lambda (input taskname)
                      "Check substring match of user's input against taskname."
                      (string-match-p (regexp-quote input) taskname)))
      )))
;; (taskspace/list-choices (taskspace/list-all) 'nondirectory)


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: Make empty dirs when creating task? (e.g. "archive" or "done" or something)

;; TODO-PKG:
;;   - Comments/layout like a real package.
;;     e.g. https://github.com/tarsius/moody/blob/master/moody.el

;; TODO: uh... tests? idk

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'taskspace)
;;; taskspace.el ends here
