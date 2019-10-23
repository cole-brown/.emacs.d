;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Time
;;------------------------------------------------------------------------------
;; Macro to measure how long a command takes.
;;   From https://zzamboni.org/post/my-emacs-configuration-with-commentary/#other-tools
;;     From https://stackoverflow.com/questions/23622296/emacs-timing-execution-of-function-calls-in-emacs-lisp
(defmacro spydez/debug/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))
;; (spydez/debug/measure-time (message "Time!"))


;;------------------------------------------------------------------------------
;; Setup Warnings for More Help in Fixing Things.
;;------------------------------------------------------------------------------

(require 'warnings)
;; `:warning' should pop up the *Warning* buffer
(setq warning-minimum-level :warning)
;; `:debug' should be logged, but not pop up the *Warning* buffer
(setq warning-minimum-log-level :debug)

(setq spydez/message/warning/current-level ':warning)
;; Shouldn't really need adjusting much; maybe to :debug.
;;
;; Can debug into warnings buffer if level is `:debug'.
;; (mis/warning nil :debug "Couldn't do a thing.")
;; (mis/warning
;;  (spydez/init/step/to-type nil 'info)
;;  :debug
;;  "Couldn't do a thing.")

(defun spydez/warning/current-level ()
  "Callback for getting my current level."
  spydez/message/warning/current-level)
;; (spydez/warning/current-level)

;; and tell mis about it
(customize-set-variable 'mis/warning/level
                        #'spydez/warning/current-level)


;;------------------------------------------------------------------------------
;; Debugging Predicate for My Init...
;;------------------------------------------------------------------------------

(defconst spydez/debug-init t) ;; nil)
(defun spydez/debugging-p ()
  (or (bound-and-true-p spydez/debug-init)
      (bound-and-true-p debug-on-error)))
(customize-set-variable 'mis/debug/predicate #'spydez/debugging-p)


;;------------------------------------------------------------------------------
;; A Nice Require with Both Debug/Messages and Piggybacking.
;;------------------------------------------------------------------------------

(defcustom spydez/require/piggyback-format
  "%s-secret"
  "Format for piggybackers: <original provide/require symbol>-secret"
  :group 'taskspace
  :type 'string)


(defun spydez/require (symbol &optional filename noerror)
  "Print helpful `mis/init/sequence' message (if `spydez/debugging-p') at
`mis/init/indent/require'. Then (require 'symbol). Then (require 'symbol-secret
nil 'noerror) and print another message if anything loaded."

  ;; Require the Actual Thing.
  (mis/init/sequence (mis/init/get-indent 'require)
                     nil
                     "(require '%s)" symbol)
  ;; ...and return the value for the Actual Require of the Actual Thing.
  (prog1
      (require symbol filename noerror)

    ;; Look for piggy backing/addons.
    ;;---
    ;;   E.g.: Say we have configure-dungeon.el in our .emacs.d, which
    ;; configures our dungeon for adventurers.
    ;;     (spydez/require 'configure-dungeon)
    ;;   Some of our adventurers may have peeked at our elisp file in our public
    ;; git repo, so maybe all the good stuff (secret rooms, treasure, loot,
    ;; BBEG...) are in a different, secret git repo. We don't want to overwrite
    ;; our dungeon, but we do want to add the secret stuff in after. So look for
    ;; that secret file.
    (when (null filename)
      (let* ((require-name (symbol-name symbol))
             (secret-name (format spydez/require/piggyback-format
                                  require-name))
             (secret-symbol (intern secret-name)))
        ;; Want to print then load, if exists, to mirror print/require above.
        (when (locate-library secret-name)
          (mis/init/sequence
           (mis/init/get-indent 'require-piggyback)
           nil
           "(require '%s)"
           secret-symbol)
          ;; Never error for piggybackers.
          (require secret-symbol nil 'noerror))))))
;; (spydez/require 'asdf nil 'noerror)
;; (spydez/require 'cl)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: also log all into my own buffer (regardless of level, debugging-p?)


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-debug)
