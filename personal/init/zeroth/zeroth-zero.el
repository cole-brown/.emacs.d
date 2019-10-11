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

;; §-TODO-§ [2019-10-10]: Start using this for spydez/hook-runner/*?
;; from here:
;; https://www.reddit.com/r/emacs/comments/1m7fqv/avoid_lambda_in_hooks_use_defun_instead/cc83axz/
(defmacro spydez/hook/defun-and-hooker (hook-var &optional postfix &rest body)
  "Macro that `defun's a function called
'spydez/hook/<HOOK-VAR-symbol-name>' or
'spydez/hook/<HOOK-VAR-symbol-name>/<POSTFIX>' with body of
BODY. Then hooks it into HOOK-VAR via `add-hook'."
  (declare (indent 1))
  (let* ((hook-fn-name (concat "spydez/hook/"
                               (symbol-name hook-var)
                               (when postfix
                                 (concat "/" postfix))))
         (hook-fn (intern hook-fn-name)))
    `(progn
       (defun ,hook-fn ()
         (spydez/message/info/when '(,@spydez/init/step/completed)
                               "Running hook `%s' from %s..."
                                ,hook-fn-name ,(buffer-name))

         ;; §-TODO-§ [2019-10-10]: add message here...
         ,@body)
       (add-hook ',hook-var #',hook-fn))))
;; (setq test-hook nil)
;; (spydez/hook/defun-and-hooker test-hook nil (message "Hello there."))
;; (run-hooks 'test-hook)


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
