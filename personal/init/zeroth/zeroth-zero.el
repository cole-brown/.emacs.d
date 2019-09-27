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


;;------------------------------------------------------------------------------
;; Init Hooks of My Own?
;;------------------------------------------------------------------------------

(defvar spydez/hook/finalize/run-boot-and-config-hooks nil
  "Add hooks to here for running at beginning of 'finalize' step of init.")
;; spydez/hook/finalize/run-hooks


(defvar spydez/hook/finalize/run-the-final-hooks nil
  "Add hooks to here for running at beginning of 'finalize' step of init.")
;; spydez/hook/finalize/run-hooks


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'zeroth-zero)
