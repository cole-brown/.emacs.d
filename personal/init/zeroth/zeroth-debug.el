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


;;-----------------------------------------------------------------------------
;; General
;;-----------------------------------------------------------------------------

;; just more pretty
(defconst spydez/message/indents
  '((none . 0)
    (default . 0)

    ;; init sequence slightly indented
    (init . 1)

    ;; require more indented, piggybacks double that
    (require           . 2)
    (require-piggyback . 4))
  "Alist for various indent levels.")

(defun spydez/message/get-indent (level)
  "Get an indent level. If LEVEL is a symbol, looks in
`spydez/message/indents'; will return `default' from alist if not
found. If LEVEL is a numberp, returns LEVEL."
  (if (numberp level)
      level
    ;; Return level if exists, else 'default.
    (or (alist-get level spydez/message/indents)
        (alist-get 'default spydez/message/indents))))
;; (spydez/message/get-indent 'require)


;;------------------------------------------------------------------------------
;; 'Themed' Warnings for More Help in Fixing Things.
;;------------------------------------------------------------------------------

(require 'warnings)
;; `:warning' should pop up the *Warning* buffer
(setq warning-minimum-level :warning)
;; `:debug' should be logged, but not pop up the *Warning* buffer
(setq warning-minimum-log-level :debug)

;; shouldn't really need adjusting much. maybe to :debug
(setq spydez/message/warning/current-level ':warning)

;; `spydez/message/warning/current-type' is for putting warnings/debugs/infos
;; into my init sequence without having to worry about what the current thing
;; is. It will be `setq' when it changes and messages can just pass
;; `spydez/message/warning/current-type'.
;;
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
(setq spydez/message/warning/current-type '(spydez early))


;;------------------------------------------------------------------------------
;; 'Themed' Debugs/Messages for More Help in Fixing Things.
;;------------------------------------------------------------------------------

;; Can also debug into warnings buffer:
;; (spydez/message/warning nil :debug "Couldn't do a thing.")
;; (spydez/message/warning
;;  (append spydez/message/warning/current-type (list 'info))
;;  :debug
;;  "Couldn't do a thing.")

(defconst spydez/init-debug t) ;; nil)
(defun spydez/debugging-p ()
  (bound-and-true-p spydez/init-debug))

;; Third and greater type: whatever sub-types you want.
(defconst spydez/message/debug/current-type '(spydez debug general))
(defconst spydez/message/info/current-type '(spydez info))


;;-----------------------------------------------------------------------------
;; Message Functions Themselves
;;-----------------------------------------------------------------------------

;;---
;; Warning
;;---

;; NOTE: Keep the lwarn at the beginning of this file roughly like this.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Warning-Basics.html#Warning-Basics
(defun spydez/message/warning (type level message &rest args)
  "Prints message to *Warnings* buffer at LEVEL.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become `spydez/message/debug/current-type'
LEVEL: level for lwarn; nil will become `spydez/message/warning/current-level'"
  (let* ((type (or type spydez/message/warning/current-type))
         (level (or level spydez/message/warning/current-level))
         (injected-message (format "  %s:  %s" type message)))
    (apply 'lwarn type level injected-message args)
    ;; basically becomes e.g.:
    ;; (lwarn '(spydez bootstrap) :warning
    ;;  "  %s:  Update 'Master List' for this system (%s) here."
    ;;  '(spydez bootstrap) spydez/setup/system/hash)
    ))
;; (spydez/message/warning nil nil "Test: %s %s" '(testing list) 'test-symbol)


;;---
;; Debug
;;---

(defun spydez/message/debug (type message &rest args)
  "Debug message to *Messages* buffer.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become `spydez/message/debug/current-type'"
  (let* ((type (or type spydez/message/debug/current-type))
        (injected-message (format "  %s:  %s" type message)))
    (apply 'message injected-message args)))
;;(spydez/message/debug nil "Test: %s %s" '(testing list) 'test-symbol)


(defun spydez/message/debug/when (type message &rest args)
  "Debug message which obeys my global 'enable/disable debugging stuff' flag via
`spydez/debugging-p'.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become `spydez/message/debug/current-type'"
  ;; Figured out a lisp thing.
  ;; Thanks: https://stackoverflow.com/a/26707692
  (when (spydez/debugging-p) (apply #'spydez/message/debug type message args)))
;;(spydez/message/debug/when nil "Test: %s %s" '(testing list) 'test-symbol)


;;---
;; Info
;;---

(defun spydez/message/info (type message &rest args)
  "Info message to *Messages* buffer.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become `spydez/message/info/current-type'"
  (let* ((type (or type spydez/message/info/current-type)))
    (apply #'spydez/message/debug type message args)))
;;(spydez/message/info/when nil "Test: %s %s" '(testing list) 'test-symbol)


(defun spydez/message/info/when (type message &rest args)
  "Info message which obeys my global 'enable/disable debugging stuff' flag via
`spydez/debugging-p'.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become `spydez/message/info/current-type'"
  (when (spydez/debugging-p) (apply #'spydez/message/info type message args)))
;;(spydez/message/info/when nil "Test: %s %s" '(testing list) 'test-symbol)


;;-----
;; Init Sequence Messages.
;;-----
;; These are a bit special as they are considered info, but default to the
;; /warning/ type list, as that is kept clean and up to date through our init.

(defun spydez/message/init-sequence (indent type message &rest args)
  "Print helpful debug message (if spydez/debugging-p) with init
sequence formatting."
  (when (spydez/debugging-p)
    (let* ((indent (spydez/message/get-indent indent))
           (indent-str (make-string indent ?-))
           (inject-message (format "%s> %s: %s"
                                   indent-str
                                   spydez/message/warning/current-type
                                   message)))
      (apply 'message inject-message args))))
;; (spydez/message/init-sequence nil nil "Test: %s %s" nil 'test-symbol)
;; (spydez/message/init-sequence 4 nil "Test: %s %s" '(a b c) 'test-symbol)


(defun spydez/message/init (message &rest args)
  "Print helpful spydez/message/init-sequence message (if
spydez/debugging-p) at 'init indent. Really just a way to not
bother with INDENT/TYPE."
  (apply #'spydez/message/init-sequence 'init nil message args))
;; (spydez/message/init "start init: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;; A Nice Require with Both Debug/Messages and Piggybacking.
;;------------------------------------------------------------------------------

(defcustom spydez/info/require/piggyback-format
  "%s-secret"
  "Format for piggybackers: <original provide/require symbol>-secret"
  :group 'taskspace
  :type 'string)


(defun spydez/info/require (symbol &optional filename noerror)
  "Print helpful `spydez/message/init-sequence' message (if
spydez/debugging-p) at indent/require. Then (require 'symbol).
Then (require 'symbol-secret nil 'noerror) and print another
message if anything loaded."

  ;; Require the Actual Thing.
  (spydez/message/init-sequence (spydez/message/get-indent 'require)
                                nil
                                "(require '%s)" symbol)
  ;; ...and return the value for the Actual Require of the Actual Thing.
  (prog1
      (require symbol filename noerror)

    ;; Look for piggy backing/addons.
    ;;---
    ;;   E.g.: Say we have configure-dungeon.el in our .emacs.d, which
    ;; configures our dungeon for adventurers.
    ;;     (spydez/info/require 'configure-dungeon)
    ;;   Some of our adventurers may have peeked at our elisp file in our public
    ;; git repo, so maybe all the good stuff (secret rooms, treasure, loot,
    ;; BBEG...) are in a different, secret git repo. We don't want to overwrite
    ;; our dungeon, but we do want to add the secret stuff in after. So look for
    ;; that secret file.
    (when (null filename)
      (let* ((require-name (symbol-name symbol))
             (secret-name (format spydez/info/require/piggyback-format
                                  require-name))
             (secret-symbol (intern secret-name)))
        ;; Want to print then load, if exists, to mirror print/require above.
        (when (locate-library secret-name)
          (spydez/message/init-sequence
           (spydez/message/get-indent 'require-piggyback)
           nil
           "(require '%s)"
           secret-symbol)
          ;; Never error for piggybackers.
          (require secret-symbol nil 'noerror))))))
;; (spydez/info/require 'asdf nil 'noerror)
;; (spydez/info/require 'cl)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: also log all into my own buffer (regardless of level, debugging-p?)


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-debug)
