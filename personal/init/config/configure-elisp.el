;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------


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
  (define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp) ; todo: bind-key instead?
  )

(add-hook 'emacs-lisp-mode-hook 'spydez/hook/emacs-lisp-mode)


;;------------------------------------------------------------------------------
;; ElDoc
;;------------------------------------------------------------------------------
;; Moved to configure-dev-env as it's usable in more than just elisp?


;;------------------------------------------------------------------------------
;; Parentheses
;;------------------------------------------------------------------------------
;; Trying out rainbow-delimiters for emacs lisp mode; could expand to
;; all prog-mode if super useful.
;; Trial: [2019-03-14]
(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: (use-package paredit) looks neat


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-elisp)
