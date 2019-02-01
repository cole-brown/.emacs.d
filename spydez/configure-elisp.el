;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; TODO: these might belong in their own file...

;;---
;; Jumping to code
;;---
;; TODO: put these in elisp hook?
;; (define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
;; (bind-key "C-c f" 'find-function) ;; TODO: is this useful in other modes?


;;------------------------------------------------------------------------------
;; Elisp
;;------------------------------------------------------------------------------

;; Don't think I have settings in hook in old .emacs...
;; Might not have any settings at all besides tab size...
;; TODO: few to check here: https://www.emacswiki.org/emacs/EmacsLispMode
(defun spydez/hook/emacs-lisp-mode ()
  ;; TODO: more settings for elisp?
  (setq tab-width spydez/dev-env/tab/min) ; lisp gets the smaller tab width
  (define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp) ; todo: bind-key?
  )

(add-hook 'emacs-lisp-mode-hook 'spydez/hook/emacs-lisp-mode)

;;------------------------------------------------------------------------------
;; ElDoc
;;------------------------------------------------------------------------------
;; TODO: Move diminish here?


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: (use-package paredit) looks neat


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-elisp)
