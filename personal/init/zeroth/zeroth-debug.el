;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Functions
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
;; 'Themed' Warnings/Messages for More Help in Fixing Things 
;;------------------------------------------------------------------------------

(require 'warnings)
;; `:warning' should pop up the *Warning* buffer
(setq warning-minimum-level :warning)
;; `:debug' should be logged, but not pop up the *Warning* buffer
(setq warning-minimum-log-level :debug)

;; shouldn't really need adjusting much. maybe to :debug
(setq spydez/warning/current-level ':warning)

;; First type in list: always 'spydez for my stuff
;;
;; Second type in list: current part of init sequence.
;;   - post (power-on-self-test)
;;   - early (early-init)
;;   - interstitial-prose (aka don't have code here plz part of init)
;;   - bootstrap (first bit of init)
;;   - config (setup packages, tools, keybinds, etc)
;;   - finalize (checks and cleanup)
;;   - running (totally done with init)
;;
;; Third and greater type: whatever sub-types you want.
(setq spydez/warning/current-type '(spydez early))

;; TODO: Move that thing about "failure is not an option in init (for my code)" to here?
;; NOTE: Keep the lwarn at the beginning of this file roughly like this.
(defun spydez/warning/message (type level message &rest args)
  "type: list with spydez first or nil e.g. nil will become '(spydez bootstrap)
level: level for lwarn e.g. nil will become :warning
https://www.gnu.org/software/emacs/manual/html_node/elisp/Warning-Basics.html#Warning-Basics"
  (let* ((type (or type spydez/warning/current-type))
         (level (or level spydez/warning/current-level))
         (injected-message (format "  %s:  %s" type message)))
    (apply 'lwarn type level injected-message args)
    ;; basically becomes e.g.:
    ;; (lwarn '(spydez bootstrap) :warning "  %s:  Update 'Master List' for this system (%s) here." '(spydez bootstrap) spydez/setup/system/hash)
    ))
;; (spydez/warning/message nil nil "My spydez/warning/message test: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;; 'Themed' Debugss/Messages for More Help in Fixing Things.
;;------------------------------------------------------------------------------

;; See bootstrap-debug-early.el for some init debug options, settings, and flags.

;; Can also debug into warnings buffer:
;; (spydez/warning/message nil :debug "Couldn't do a thing.")
;; (spydez/warning/message (append spydez/warning/current-type (list 'info)) :debug "Couldn't do a thing.")

(defconst spydez/init-debug t) ;; nil)
(defun spydez/debugging-p ()
  (bound-and-true-p spydez/init-debug))

;; Third and greater type: whatever sub-types you want.
(defconst spydez/debug/current-type '(spydez debug general))
(defconst spydez/info/current-type '(spydez info))

;; just more pretty
(defconst spydez/info/current-indent 0)
(defconst spydez/info/init-indent 1)
(defconst spydez/info/require-indent 2)

;; TODO: all these types... I'm not using them much.
;; Should I make them take a symbol or list, then append that to their (current) defaults?

;; TODO: currently have spydez/{warning,debug,info}/message[-{if,always}].
;; Is that correct, or should I have e.g.
;;   - spydez/message[-{if,always}]/{warning,debug,info}
;;   - spydez/message/{warning,debug,info}[-{if,always}]

;; obeys global enable/disable flag
(defun spydez/debug/message-if (type message &rest args)
  ;; Figured out a lisp thing.
  ;; Thanks: https://stackoverflow.com/a/26707692
  (when (spydez/debugging-p) (apply #'spydez/debug/message-always type message args)))
;;(spydez/debug/message-if nil "My spydez/debug/message test: %s %s" '(testing list) 'test-symbol)

(defun spydez/debug/message-always (type message &rest args)
  (let* ((type (or type spydez/debug/current-type))
        (injected-message (format "  %s:  %s" type message)))
    (apply 'message injected-message args)))
;;(spydez/debug/message-always nil "My spydez/debug/message test: %s %s" '(testing list) 'test-symbol)

(defun spydez/info/message-if (type message &rest args)
  (let* ((type (or type spydez/info/current-type)))
    (when (spydez/debugging-p) (apply #'spydez/debug/message-always type message args))))
;;(spydez/info/message-if nil "My spydez/info/message-if test: %s %s" '(testing list) 'test-symbol)

;;-----
;; Init Sequence Messages.
;;-----
;; These are a bit special as they are considered info, but default to the /warning/ type list, as
;; that is kept clean and up to date through our init.

(defun spydez/info/init-sequence (indent type message &rest args)
  "Print helpful debug message (if spydez/debugging-p) with init sequence formatting."
  (when (spydez/debugging-p)
    (let* ((indent (or indent spydez/info/current-indent))
           (indent-str (make-string indent ?-))
           (inject-message (format "%s> %s: %s" indent-str spydez/warning/current-type message)))
      (apply 'message inject-message args))))
;; (spydez/info/init-sequence nil nil "My spydez/info/message-if test: %s %s" '(testing list) 'test-symbol)
;; (spydez/info/init-sequence 4 nil "My spydez/info/message-if test: %s %s" '(testing list) 'test-symbol)

(defun spydez/info/init-message (message &rest args)
  "Print helpful spydez/info/init-sequence message (if spydez/debugging-p) at init-indent."
  (apply #'spydez/info/init-sequence spydez/info/init-indent nil message args))
;; (spydez/info/init-message "start init: %s %s" '(testing list) 'test-symbol)

(defun spydez/info/require (symbol &optional filename noerror)
  "Print helpful spydez/info/init-sequence message (if spydez/debugging-p) at
require-indent. And then (require 'symbol)."
  (spydez/info/init-sequence spydez/info/require-indent nil "(require '%s)" symbol)
  (require symbol filename noerror))
;; (spydez/info/require 'asdf nil 'noerror)
;; (spydez/info/require 'cl)

;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: also log all into my own buffer (regardless of level, debugging-p?)


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-debug)
