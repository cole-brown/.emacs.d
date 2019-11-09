;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------ZerO--------------------------------------
;;--                           The Pre-Pre-Basics                             --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Defining this early so we don't have to worry about it existing when we get
;; bootstrap & configuration rolling for real.
;; Just a convenient group to chuck all my defcustoms into.
(defgroup spydez/group nil
  "General group namespace for the defcustoms in my emacs init."
  :prefix "spydez/"
  ;; not really sure where to stick it
  :group 'convenience)


;; §-TODO-§ [2019-10-11]: General hook vars for:
;;   - Quiet mode/during-"running"-step hooks
;;   - Quiet mode/during-other-init-steps hooks
;; So could set all mode hooks to quiet, but let anything run during init
;; output its message.


;;------------------------------------------------------------------------------
;; SpydeZ Hooks
;;------------------------------------------------------------------------------

(defvar spydez/hook-runner/finalize/boot-and-config nil
  "Add hooks to here for running in 'finalize' step of init in
'finalize-boot-and-config.el'.")


(defvar spydez/hook-runner/finalize/final-finalities nil
  "Add hooks to here for running in 'finalize' step of init in
'zzz-finalize.el'.")

(defvar spydez/hook-runner/running/post-init nil
  "Add hooks to here for running `spydez/hook/post-init/seconds'
seconds after Emacs is all done. And by 'all done', I mean the
first time it's idle for that long.")

(defconst spydez/hook/post-init/seconds 1.0
  "`spydez/hook-runner/running/post-init will run this many seconds after
Emacs is first idle.")


;;------------------------------------------------------------------------------
;; Post-Init Hook's Timer
;;------------------------------------------------------------------------------

(defun spydez/timer/hook/post-init ()
  "Runs all hooks attached to our
`spydez/hook-runner/running/post-init' variable."
  (run-hooks 'spydez/hook-runner/running/post-init))

;; Can just fire and forget post-init hook here since it's on a timer.
;; Set up once-off idle timer for post-init hook.
(run-with-idle-timer spydez/hook/post-init/seconds
                     nil
                     #'spydez/timer/hook/post-init)


;;-----------------------------------------------------------------------------
;; An Function Helper for my Init Hook Function Helper for my Init Hook Func...
;;-----------------------------------------------------------------------------

;; §-TODO-§ [2019-11-07]: Remove this? Don't use it. Couldn't get it correct for
;; defun-and-hook(er).
(defun spydez/function/source (symbol &optional roots-list location)
  "Returns SYMBOL's filename relative to cars in PATH-ALIST, or
absolute, or relative in C source, or nil if unknown.

If used on only my code with nil PATH-ALIST, returns relative to
`spydez/dir/emacs/personal', to `spydez/dir/secrets', or nil.

If cannot find a path for SYMBOL, will try to use LOCATION as a backup."
  (let ((path (cdr (find-function-library symbol)))
        (roots-list (or roots-list
                        (list spydez/dir/emacs/personal spydez/dir/secrets))))
    (cond
     ;; no path, but have a (backup) location
     ((and (not path)
           location
           (stringp location))
      (message "backup")
      ;; slot backup location into place as 'path' and try it.
      (let ((path location)
            (result location))
        (dolist (root roots-list result)
          ;; Assume anything that gets chopped down by spydez/path/to-relative
          ;; is correct..ish. Correctest is shortest.
          (let* ((rel-candidate (spydez/path/to-relative path root))
                 (rel-can-len (if rel-candidate
                                  (length rel-candidate)
                                most-positive-fixnum)))
            (when (and (< rel-can-len (length path))
                       (< rel-can-len (length result)))
              (message "best backup path: " rel-candidate)
              (setq result rel-candidate))))))

     ;; Can't do anything without backup...
     ((not path)
      (message "no path")
      nil)

     ;; Usual case! After (not path) because stringp can't do nil.
     ((and (stringp path)
           (not (null roots-list)))
      (let ((result path))
        (dolist (root roots-list result)
          ;; Assume anything that gets chopped down by spydez/path/to-relative
          ;; is correct..ish. Correctest is shortest.
          (let* ((rel-candidate (spydez/path/to-relative path root))
                 (rel-can-len (if rel-candidate
                                  (length rel-candidate)
                                most-positive-fixnum)))
            (when (and (< rel-can-len (length path))
                       (< rel-can-len (length result)))
              (message "best path: " rel-candidate)
              (setq result rel-candidate))))))

     ;; Just return best we can?
     ((stringp path)
      (message "no roots")
      path)

     (t
      nil))))
;; (spydez/function/source 'spydez/hook/auto-open-files)


;; §-TODO-§ [2019-10-11]: Move and rename? Actually, may need to leave here...
;; but rename?
(defun spydez/function/insert-location ()
  "Insert path relative to `spydez/dir/emacs/personal', as quoted string, at point."
  (interactive)
  (insert ?\"
          (spydez/path/to-relative (buffer-file-name)
                                   spydez/dir/emacs/personal)
          ?\"))


;;------------------------------------------------------------------------------
;; Init Hooks of My Own?
;;------------------------------------------------------------------------------

;; from here:
;; https://www.reddit.com/r/emacs/comments/1m7fqv/avoid_lambda_in_hooks_use_defun_instead/cc83axz/
(defmacro spydez/hook/defun-and-hooker (hook-var quiet
                                        &optional to-end name postfix location docstring
                                        &rest body)
  "Macro that `defun's a function called
'spydez/hook/<hook-name>' or 'spydez/hook/<hook-name>/<POSTFIX>'
where <hook-name> NAME (or `(symbol-name HOOK-VAR)' if NAME is
nil) with body of BODY. Then hooks it into HOOK-VAR via
`add-hook'.

TO-END is passed to `add-hook' as its 'APPEND' arg. If TO-END is
non-nil, post-pend to end of hook list, else pre-pend.

If VERBOSE is non-nil on macro expansion, this will
`mis/info/when' on run with hook-fn name, and either
source file defined in or (as backup) optional LOCATION string."
  (declare (indent 6) (doc-string 7))
  (let* ((hook-fn-name (concat "spydez/hook/"
                               (if name
                                   name
                                 ;; §-TODO-§ [2019-10-11]: remove "-hook"?
                                 (symbol-name hook-var))
                               (when postfix
                                 (concat "/" postfix))))
         (hook-fn (intern hook-fn-name)))
    `(progn
       (defun ,hook-fn () ,docstring
              (unless ,quiet
                ;; Nice info message maybe?
                (mis/info/when
                 '(,@spydez/init/step/completed)
                 "Running hook `%s'%s..."
                 ,hook-fn-name
                 (if (not (stringp ,location))
                     ""
                   (concat " from "
                           (spydez/path/to-relative ,location)))))

              ,@body)
       (add-hook ',hook-var #',hook-fn ',to-end))))
;; (setq test-hook nil)
;; (makunbound spydez/hook/test-hook)
;; (spydez/hook/defun-and-hooker test-hook nil nil nil nil nil (message "Hello there."))
;; (spydez/hook/defun-and-hooker test-hook nil nil nil nil nil (message "Hello there."))
;; test-hook
;; (run-hooks 'test-hook)
;; (setq debug-on-error t)
;; (setq test-hook nil)
;; (spydez/hook/defun-and-hooker test-hook nil nil "jeff" "mcjefferson" "here" (message "Hello there."))
;; test-hook
;; (run-hooks 'test-hook)
;; (spydez/function/source 'spydez/hook/test-hook nil "~/.emacs.d/personal/init/zeroth/zeroth-zero.el")
;; (find-function-library 'spydez/hook/test-hook)
;; (find-function-advised-original 'spydez/hook/test-hook)


(defmacro spydez/hook/defun (hook-var quiet
                             &optional name postfix location docstring
                             &rest body)
  "Macro that `defun's a function called
'spydez/hook/<hook-name>' or 'spydez/hook/<hook-name>/<POSTFIX>'
where <hook-name> NAME (or `(symbol-name HOOK-VAR)' if NAME is
nil) with body of BODY. Returns the defun'd function symbol.

TO-END is passed to `add-hook' as its 'APPEND' arg. If TO-END is
non-nil, post-pend to end of hook list, else pre-pend.

Will `mis/info/when' on run with hook-fn name, and
either source file defined in or (as backup) optional LOCATION
string.

Use this over `spydez/hook/defun-and-hooker' only in cases where you aren't
`add-hook'ing directly (e.g. for use-package's ':hook')."
  (declare (indent 5) (doc-string 6))
  (let* ((hook-fn-name (concat "spydez/hook/"
                               (if name
                                   name
                                 ;; §-TODO-§ [2019-10-11]: remove "-hook"?
                                 (symbol-name hook-var))
                               (when postfix
                                 (concat "/" postfix))))
         (hook-fn (intern hook-fn-name)))
    `(defun ,hook-fn () ,docstring
            ;; Nice info message maybe?
              (unless ,quiet
                (mis/info/when
                 '(,@spydez/init/step/completed)
                 "Running hook `%s'%s..."
                 ,hook-fn-name
                 (if (not (stringp ,location))
                     ""
                   (concat " from "
                           (spydez/path/to-relative ,location)))))

            ,@body)))
;; (setq test-hook nil)
;; (spydez/hook/defun test-hook nil nil nil (message "Hello there."))
;; (add-hook 'test-hook 'spydez/hook/test-hook)
;; test-hook
;; (run-hooks 'test-hook)
;; (setq test-hook nil)
;; (spydez/hook/defun test-hook "richard" "mcrichard" "here" (message "hi."))
;; (add-hook 'test-hook 'spydez/hook/richard/mcrichard)
;; test-hook
;; (run-hooks 'test-hook)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'zeroth-zero)
