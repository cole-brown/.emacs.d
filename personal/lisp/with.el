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

;;------------------------------------------------------------------------------
;; Feature/Function/Executable Wrappers
;;------------------------------------------------------------------------------
(defmacro with-feature (feature &rest body)
  "If FEATURE is available, load it and evaluate BODY."
  (declare (indent defun))
  `(when (require ,feature nil :noerror)
     ,@body))


(defmacro with-function (function &rest body)
  "If FUNCTION is available, evaluate BODY."
  (declare (indent defun))
  `(when (functionp ,function)
     ,@body))


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


(defmacro with-face (str &rest properties)
  "Print STR using PROPERTIES."
  `(propertize ,str 'face (list ,@properties)))

;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; Delete whole file if I don't end up actually using it.

;; Get whole ";;;", "Commentary", "Code" header setup for all my elisp files.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'with)
;;; with.el ends here
