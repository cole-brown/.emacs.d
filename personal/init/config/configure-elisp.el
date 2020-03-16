;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;;---
;; Jumping to code
;;---
(bind-key "M-\\" 'find-function-at-point emacs-lisp-mode-map)


;;------------------------------------------------------------------------------
;; Elisp
;;------------------------------------------------------------------------------

;; NOTE! This is pp, not anything generic like `emacs-lisp'.
;; Could change to `emacs-lisp', and put an `:after pp'...
;; TODO: If that, change bind-key to a `:bind' section.
(use-package pp

  ;;---
  :init
  ;;---
  ;; Don't think I have settings in hook in old .emacs...
  ;; Might not have any settings at all besides tab size...
  ;; TODO: few to check here: https://www.emacswiki.org/emacs/EmacsLispMode

  ;; Setup my elisp mode hook
  (spydez/hook/defun emacs-lisp-mode-hook t
      nil nil
      "init/config/configure-elisp.el"
    "Hook into emacs-lisp-mode."
    ;; TODO: more settings for elisp?
    (setq tab-width spydez/dev-env/tab/min) ;; lisp gets the smaller tab width
    (bind-key "C-x C-e" 'pp-eval-last-sexp emacs-lisp-mode-map))

  ;;---
  :hook
  ;;---
  (emacs-lisp-mode . spydez/hook/emacs-lisp-mode-hook))


;;------------------------------------------------------------------------------
;; ElDoc
;;------------------------------------------------------------------------------
;; Moved to configure-dev-env as it's usable in more than just elisp?


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: (use-package paredit) looks neat


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-elisp)
