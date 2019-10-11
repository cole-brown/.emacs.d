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

;; ;; §-TODO-§ [2019-10-10]: This one is better... maybe?
;; ;; See finalize domain for use attempts...
;; (defmacro spydez/hook/defun (name &rest body)
;;   "Turns HOOK-FN into a closure with a helpful
;; steal of the file-name this macro is called from."
;;   `(let ((fn ,name))
;;      ;; §-TODO-§ [2019-10-10]: defun part?
;;      (spydez/message/info/when '(,@spydez/init/step/completed)
;;                                "Running hook `%s' from %s..."
;;                                fn ,(buffer-name))
;;      ,@body))

;; [2019-10-10]: This is fucky too...
;; ;; This could go with spydez/message/* things or here with hook things...
;; ;; *shrug*
;; ;; But if I switch to a better macro it belongs here so stay here with this?
;; (defmacro spydez/message/hook/info (fn-symbol)
;;   "Easy lil' macro to chuck message with FN-SYMBOL and buffer name into hooks."
;;   `(let ((fn ,fn-symbol))
;;      (spydez/message/info/when '(,@spydez/init/step/completed)
;;                                "Running hook `%s' from %s..."
;;                                fn ,(buffer-name))))


(defvar spydez/hook-runner/finalize/boot-and-config nil
  "Add hooks to here for running at beginning of 'finalize' step of init.")

(defvar spydez/hook-runner/finalize/final-finalities nil
  "Add hooks to here for running at beginning of 'finalize' step of init.")


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'zeroth-zero)
