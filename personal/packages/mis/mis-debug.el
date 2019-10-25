;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------------------~♫~99 Little Bugs In The Code!~♫~------------------------
;;--               ~♫~Take One Down and Patch It Around...~♫~                 --
;;---------------------~♫~127 Little Bugs in the Code.~♫~-----------------------
;;                                    ...
;;                       (programmer sobs in binary...)



;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defcustom mis/debug/predicate nil
  "Predicate function to call for the output-if-debugging functions
 (usually named `mis/*/when')."
  :group 'mis
  :type 'function)


;;-----------------------------------------------------------------------------
;; TYPE for Warning/Debug/Info Messages
;;-----------------------------------------------------------------------------

;; TYPE in these functions is for putting init sequence info into
;; warnings/debugs/infos messages without having to worry about where we are too
;; much. It will be set via `spydez/init/step/set-completed' when it changes.
;; Messages can just use nil as TYPE unless override is desired; nil will
;; resolve to output from `mis/setting/get-with-default' with `mis/debug/type'.
;;
;; First type in list: always 'spydez for my stuff
;;
;; Second/third/fourth(?) type in list: current part of init sequence.
;; See zeroth-steps.el.
;;
;; Fourth/Fifth and greater type: whatever sub-types you want.

(defcustom mis/debug/type '(mis default)
  "Default type used if none provided. Types are e.g.:
'(spydez warning)
'(spydez bootstrap (system specific))
'(spydez running none)
etc...

They will show up in *Messages* output or be used for warning funcs."
  :group 'mis
  :type '(choice (sexp :tag "list of symbols - most important first")
                 (function :tag "function to call to get list of symbols")))


;;------------------------------------------------------------------------------
;; Warnings for More Help in Fixing Things.
;;------------------------------------------------------------------------------

(require 'warnings)

(defcustom mis/warning/level ':warning
  "Default warning level used if none provided."
  :group 'mis
  :type '(choice (symbol :tag "lwarn LEVEL arg")
                 (function :tag "functino to call to get lwarn LEVEL arg"))
  ;; not "meaningful" for symbol type, but uh... these are the options.
  ;; :options '(:emergency :error :warning :debug))
  )


;;------------------------------------------------------------------------------
;; Info Functions
;;------------------------------------------------------------------------------
;; §-TODO-§ [2019-10-23]: Maybe could lose info and just use debug? They're
;; not used differently right now...

(defun mis/info (type message &rest args)
  "Info message to *Messages* buffer.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become (mis/debug/type nil)"
  (let* ((type (mis/setting/get-with-default type mis/debug/type)))

    (apply #'mis/debug type message args)))
;;(mis/info/when nil "Test: %s %s" '(testing list) 'test-symbol)


(defun mis/info/when (type message &rest args)
  "Info message which obeys my global 'enable/disable debugging stuff' flag via
`mis/debugging-p'.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become (mis/debug/type nil)"
  (when (mis/debugging-p) (apply #'mis/info type message args)))
;;(mis/info/when nil "Test: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;; Debug Functions
;;------------------------------------------------------------------------------

(defun mis/debug (type message &rest args)
  "Debug message to *Messages* buffer.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become (mis/debug/type nil)"
  (let* ((type (mis/setting/get-with-default type mis/debug/type))
         (injected-message (format "  %s:  %s" type message)))
    (apply 'message injected-message args)))
;;(mis/debug nil "Test: %s %s" '(testing list) 'test-symbol)


(defun mis/debug/when (type message &rest args)
  "Debug message which obeys my global 'enable/disable debugging stuff' flag via
`mis/debugging-p'.
Formats MESSAGE and ARGS according to `format'.
TYPE: list with symbols; nil will become (mis/debug/type nil)"
  ;; Figured out a lisp thing.
  ;; Thanks: https://stackoverflow.com/a/26707692
  (when (mis/debugging-p) (apply #'mis/debug type message args)))
;;(mis/debug/when nil "Test: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!
;; Warning! Functions! Oh no!
;;-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!
;;------------------------------------------------------------------------------

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Warning-Basics.html#Warning-Basics
(defun mis/warning (type level message &rest args)
  "Prints message to *Warnings* buffer at LEVEL.
Formats MESSAGE and ARGS according to `format'.

TYPE: list with symbols; nil will become (mis/debug/type nil)
LEVEL: level for lwarn; nil will become `mis/warning/level'"
  (let* ((type (mis/setting/get-with-default type mis/debug/type))
         (level (mis/setting/get-with-default level
                                              mis/warning/level))
         (injected-message (format "  %s:  %s" type message)))
    (apply 'lwarn type level injected-message args)
    ;; basically becomes e.g.:
    ;; (lwarn '(spydez bootstrap) :warning
    ;;  "  %s:  Update 'Master List' for this system (%s) here."
    ;;  '(spydez bootstrap) spydez/dev/system/hash)
    ))
;; (mis/warning nil nil "Test: %s %s" '(testing list) 'test-symbol)
;; (mis/warning nil nil "Update 'Master List' for this system (%s) here." spydez/dev/system/hash)


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun mis/debugging-p ()
  "Wraps `mis/debug/predicate' into null check and function call."
  (when mis/debug/predicate
    ;; I guess be nice and let symbols in?..
    (if (functionp mis/debug/predicate)
  (funcall mis/debug/predicate)
      mis/debug/predicate)))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-debug)
