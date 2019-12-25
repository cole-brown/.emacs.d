;;; with.el --- conditional eval wrappers -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; Commentary:

;; I originally got from dholm. Not 100% of origin - other emacs configs have
;; similar versions...
;;   https://github.com/dholm/dotemacs/blob/31066f829b49d50079ffb2e8e82f69eea20c1aaa/.emacs.d/lisp/lib/with.el
;;   https://github.com/jackscott/dot-emacs/blob/2840f42d2bd5e45dd622bdeecfd4b5100a9d5bc0/external/troels.el

;;; Code:

;;-----------------------------------elisp--------------------------------------
;;--                Conditional Utility Functions for Elisp.                  --
;;------------------------------------------------------------------------------

(require 'cl)

;;------------------------------------------------------------------------------
;; Feature/Function/Executable Wrappers
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-12-18]: Namespace these.
;;   spydez/with/feature
;;   spydez/with/all-features
;;   spydez/with/___
;;   spydez/without/___?

;; TODO: with-var, with-all-vars, with-any-vars?
;; TODO?: with-{any,all}-functions?
;; TODO?: with-all-executables?
;; TODO?: with-___-when, with-___-unless, with-___-if
;;   e.g. (with-function-when #'spydez/zort/using-bar ...)
;;          -> when function is bound and returns true...
;;   e.g. (with-function-when #'spydez/zort/using-bar 'args ...)
;;          -> when function is bound and returns true when passed args?..

;; TODO-SOON: Change from `when' to `if'. Add a required error message to all
;;   withs. Add a const/custom for what function to call (default error).
;;   Use `apply'? So as to get multiple args into there?
;;     e.g. `error': format string, args.
;;     e.g. `mis/warning': (mis/warning nil nil "string")


;;---------
;; Features
;;---------

(defmacro spydez/with/demotable-errors (demote format &rest body)
  "Run BODY and demote any errors to simple messages if DEMOTE is non-nil.

FORMAT is a string passed to message to format any error message
if demoting them. It should contain a single %-sequence; e.g.,
\"Error: %S\".

If debug-on-error is non-nil, run BODY without catching its errors.

This is the `with-demoted-errors' macro guarded/on-off'd by the demote flag.
"
  (declare (indent 2))

  `(if ,demote
       (with-demoted-errors ,format
         ,@body)
     ,@body))


;;---------
;; Features
;;---------
(defmacro with-feature (feature &rest body)
  "If FEATURE is available, load it and evaluate BODY."
  (declare (indent defun))
  `(when (require ,feature nil :noerror)
     ,@body))


(defmacro with-all-features (features &rest body)
  "If all of FEATURES are available (via `require), evaluate BODY."
  (declare (indent defun))
  `(when (cl-every (lambda (x) (require x nil :noerror)) ,features)
     ,@body))


;;---------
;; Functions
;;---------
(defmacro with-function (function &rest body)
  "If FUNCTION is available, evaluate BODY."
  (declare (indent defun))
  `(when (functionp ,function)
     ,@body))


;;---------
;; Executables
;;---------
(defmacro with-executable (executable &rest body)
  "If EXECUTABLE is available in path, evaluate BODY."
  (declare (indent defun))
  `(when (executable-find (symbol-name ,executable))
     ,@body))


(defmacro with-any-executable (executables &rest body)
  "If any of EXECUTABLES are available in the path, evaluate BODY."
  (declare (indent defun))
  `(when (some (lambda (x) (executable-find (symbol-name x))) ,executables)
     ,@body))


;;---------
;; Faces
;;---------
(defmacro with-face (str &rest properties)
  "Print STR using PROPERTIES."
  `(propertize ,str 'face (list ,@properties)))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: Get whole ";;;", "Commentary", "Code" header setup for all my
;;   elisp files?
;;   - Eh... maybe if I turn into another 'package' like taskspace
;;      and use-tool...
;; TODO: Turn into a package?


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'with)
;;; with.el ends here
