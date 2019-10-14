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
    (require           . 3)
    (require-piggyback . 5))
  "Alist for various indent levels.")

(defconst spydez/message/indent-gutter 8
  "Amount of space to use for indents, indent arrows in init messages.")

;; fine: - ascii hypen
;; tried: ─ unicode box drawing light horizontal
;;   - doesn't jive w/ < > as arrow heads though
(defconst spydez/message/indent-arrow/tail ?-
  "Character to use to draw indent arrow's tail.")

(defconst spydez/message/indent-arrow/head
  '(;; rightwards
    (nil   . ">")
    (right . ">")
    ;; leftwards
    (t     . "<")
    (left  . "<"))
  "Character to use to draw indent arrow's tail.")
;; (alist-get nil spydez/message/indent-arrow/head)

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


;; Current arrows:
;;->      ;; message/init
;; <-     ;; message/init-step/done
;;--->    ;; require
;;   <--- ;; (FUTURE?) something/done @ require level?
;;----->  ;; require-piggyback
;;01234567;; So 8 space "gutter" to work with ATM?
(defun spydez/message/indent-arrow (level &optional direction)
  "Returns a string which is an ascii arrow with a tail of length
defined by LEVEL in `spydez/message/indents'. Will be left
pointing if DIRECTION is 'left, else right. All strings returned
should be of length `spydez/message/indent-gutter'."
  (let* ((indent (spydez/message/get-indent level))
         (left (eq direction 'left)) ;; defaults to right
         (prefix-str (make-string indent
                                  (if left
                                      ?\s
                                    spydez/message/indent-arrow/tail)))
         (postfix-str (make-string indent
                                   (if left
                                       spydez/message/indent-arrow/tail
                                     ?\s)))
         (arrow-head (or (alist-get direction spydez/message/indent-arrow/head)
                         (alist-get 'right spydez/message/indent-arrow/head)))
         (gutter-format (format "%%-%ds" spydez/message/indent-gutter)))
    ;; make sure we don't go over length
    (truncate-string-to-width
     ;; format the whole gutter for correct right space str pad
     ;; (make sure we don't go under length)
     (format gutter-format
             ;; format the left/right arrow
             (concat prefix-str arrow-head postfix-str))
     spydez/message/indent-gutter)))
;; (length (spydez/message/indent-arrow 'init) )
;; (length (spydez/message/indent-arrow 'init 'left) )
;; (length (spydez/message/indent-arrow 'require) )
;; (length (spydez/message/indent-arrow 'require 'left) )
;; (length (spydez/message/indent-arrow 'require-piggyback) )
;; (length (spydez/message/indent-arrow 'require-piggyback 'left) )


;;-----------------------------------------------------------------------------
;; 'Themed' Types for Warning/Debug/Info Messages
;;-----------------------------------------------------------------------------

;; TYPE in these functions is for putting init sequence info into
;; warnings/debugs/infos messages without having to worry about where we are too
;; much. It will be set via `spydez/init/step/set-completed' when it changes.
;; Messages can just use nil as TYPE unless override is desired; nil will
;; resolve to output from `spydez/init/step/to-type'.
;;
;; First type in list: always 'spydez for my stuff
;;
;; Second/third/fourth(?) type in list: current part of init sequence.
;; See zeroth-steps.el.
;;
;; Fourth/Fifth and greater type: whatever sub-types you want.


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


;;------------------------------------------------------------------------------
;; 'Themed' Debugs/Messages for More Help in Fixing Things.
;;------------------------------------------------------------------------------

;; Can also debug into warnings buffer:
;; (spydez/message/warning nil :debug "Couldn't do a thing.")
;; (spydez/message/warning
;;  (spydez/init/step/to-type nil 'info)
;;  :debug
;;  "Couldn't do a thing.")

(defconst spydez/init-debug t) ;; nil)
(defun spydez/debugging-p ()
  (or (bound-and-true-p spydez/init-debug)
      (bound-and-true-p debug-on-error)))


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
TYPE: list with symbols; nil will become (spydez/init/step/to-type nil)
LEVEL: level for lwarn; nil will become `spydez/message/warning/current-level'"
  (let* ((type (or type (spydez/init/step/to-type nil)))
         (level (or level spydez/message/warning/current-level))
         (injected-message (format "  %s:  %s" type message)))
    (message "%s %s %s" type level injected-message)
    (apply 'lwarn type level injected-message args)
    ;; basically becomes e.g.:
    ;; (lwarn '(spydez bootstrap) :warning
    ;;  "  %s:  Update 'Master List' for this system (%s) here."
    ;;  '(spydez bootstrap) spydez/setup/system/hash)
    ))
;; (spydez/message/warning nil nil "Test: %s %s" '(testing list) 'test-symbol)
;; (spydez/message/warning nil nil "Update 'Master List' for this system (%s) here." spydez/setup/system/hash)


;;---
;; Debug
;;---

(defun spydez/message/debug (type message &rest args)
  "Debug message to *Messages* buffer.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become (spydez/init/step/to-type nil)"
  (let* ((type (or type (spydez/init/step/to-type nil)))
        (injected-message (format "  %s:  %s" type message)))
    (apply 'message injected-message args)))
;;(spydez/message/debug nil "Test: %s %s" '(testing list) 'test-symbol)


(defun spydez/message/debug/when (type message &rest args)
  "Debug message which obeys my global 'enable/disable debugging stuff' flag via
`spydez/debugging-p'.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become (spydez/init/step/to-type nil)"
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
TYPE: list with symbols; nil will become (spydez/init/step/to-type nil)"
  (let* ((type (or type (spydez/init/step/to-type nil))))
    (apply #'spydez/message/debug type message args)))
;;(spydez/message/info/when nil "Test: %s %s" '(testing list) 'test-symbol)


(defun spydez/message/info/when (type message &rest args)
  "Info message which obeys my global 'enable/disable debugging stuff' flag via
`spydez/debugging-p'.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become (spydez/init/step/to-type nil)"
  (when (spydez/debugging-p) (apply #'spydez/message/info type message args)))
;;(spydez/message/info/when nil "Test: %s %s" '(testing list) 'test-symbol)


;;---
;; Pretty Messages in *Messages* Buffer?
;;---

;; https://emacs.stackexchange.com/a/20178
(defun spydez/message/preserve-properties (format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
M-x list-faces-display for all defined faces. Call with a propertized string."
  (let ((message-log-max nil))
    (apply 'message format args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (zerop (current-column)) (insert "\n"))
        (insert (apply 'format format args))
        (insert "\n")))))
;; (spydez/message/preserve-properties
;;  (concat
;;   (propertize "--->    " 'face 'spydez/message/face/gutter)
;;   " "
;;   (propertize "├" 'face 'spydez/message/face/types-sep)
;;   " "
;;   (propertize "(spydez zeroth debug)" 'face 'spydez/message/face/types)
;;   " "
;;   (propertize "┤:" 'face 'spydez/message/face/types-sep)
;;   " "
;;   (propertize "early-init.el... Zeroth step." 'face 'spydez/message/face/text)))
;; (spydez/message/preserve-properties
;;  (concat
;;   (propertize "--->    " 'face 'font-lock-variable-name-face)
;;   " "
;;   (propertize "├" 'face 'font-lock-comment-delimiter-face)
;;   " "
;;   (propertize "(spydez zeroth debug)" 'face 'font-lock-comment-face)
;;   " "
;;   (propertize "┤:" 'face 'font-lock-comment-delimiter-face)
;;   " "
;;   (propertize "early-init.el... Zeroth step." 'face 'default)))

;; §-TODO-§ [2019-10-11]: Try font-lock mode instead of string properties?
;; Could add auto detect of my file names, maybe.
;; And other "arrorws"...
;; and 'require'...
;;
;; (spydez/hook/defun example-hook t
;;     nil "simple-list" "init/config/configure-jeff.el"
;;   "Nice up simple lists - replacing hypen with a unicode middle dot."
;;   (font-lock-add-keywords
;;    nil ;; if in a derived mode, doing font lock in a hook could be easier...
;;    '(("^ *\\([-]\\) "
;;       (0 (prog1 () (compose-region (match-beginning 1)
;;                                    (match-end 1) "•")))))))



;;-----
;; Init Sequence Messages.
;;-----

;; separators?:
;;   | - pipe,
;;   │ - box drawing vertical,
;;   ┊ - box drawing quadruple dash vertical,
;;   ├ ┤ - box drawing light vertical and right/left,
(defun spydez/message/init-sequence (indent arrow msg-fmt &rest args)
  "Print helpful debug message (if spydez/debugging-p) with init
sequence formatting. Indented to INDENT level/number with ARROW
'left or 'right. MSG-FMT should be a `format' style string which
ARGS will fill in."
  (when (spydez/debugging-p)
    (let* ((indent-gutter (spydez/message/indent-arrow indent arrow))
           (inject-msg-fmt
            (concat
             (propertize indent-gutter 'face 'font-lock-string-face)
             " "
             (propertize "├" 'face 'font-lock-comment-delimiter-face)
             " "
             ;; current init type via nil
             (propertize (format "%s" (spydez/init/step/to-type nil))
                         'face 'font-lock-comment-face)
             " "
             (propertize "┤:" 'face 'font-lock-comment-delimiter-face)
             " "
             (propertize msg-fmt 'face 'default))))
      (apply 'spydez/message/preserve-properties inject-msg-fmt args))))
;; (spydez/message/init-sequence nil nil "Test: %s %s" nil 'symbol)
;; (spydez/message/init-sequence 4 nil "Test: %s %s" '(a b c) 'symbol)
;; (spydez/message/init-sequence 'init 'right "Test: %s" 'jeff)
;; (spydez/message/init-sequence 'init 'left "Test: %s" 'jeff)
;; (spydez/message/init-sequence 'require nil "Test: %s" 'jeff)
;; (spydez/message/init-sequence 'require 'left "Test: %s" 'jeff)
;; (spydez/message/init-sequence 'require-piggyback nil "Test: %s" 'jeff)
;; (spydez/message/init-sequence 'require-piggyback 'left "Test: %s" 'jeff)


(defun spydez/message/init (msg-fmt &rest args)
  "Print helpful spydez/message/init-sequence message (if
spydez/debugging-p) at 'init indent. Really just a way to not
bother with INDENT/TYPE."
  (apply #'spydez/message/init-sequence 'init nil msg-fmt args))
;; (spydez/message/init "start init: %s %s" '(testing list) 'test-symbol)


(defun spydez/message/init-step/done (prev curr &rest args)
  "Info about init step changes. See zeroth-steps.el."
  (when (spydez/debugging-p)
    (apply 'spydez/message/init-sequence 'init 'left
           "step completed: %s <-from- %s"
           ;; Need this nil here to prevent:
           ;;   '((curr list things) prev list things)
           ;; With nil we have:
           ;;   '((curr list things) (prev list things))
           ;; Yay lisp.
           curr prev nil)))
;; (spydez/message/init-step/done '(bootstrap tie aglets) '(boot adjust tongue))


;;------------------------------------------------------------------------------
;; A Nice Require with Both Debug/Messages and Piggybacking.
;;------------------------------------------------------------------------------

(defcustom spydez/require/piggyback-format
  "%s-secret"
  "Format for piggybackers: <original provide/require symbol>-secret"
  :group 'taskspace
  :type 'string)


(defun spydez/require (symbol &optional filename noerror)
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
          (spydez/message/init-sequence
           (spydez/message/get-indent 'require-piggyback)
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
