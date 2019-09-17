;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------LSP---------------------------------------
;;--                        Language Server Protocol                          --
;;-----------------------(...not Lumpy Space Princess)--------------------------

;; Syntax Time
;; Come on grab your code
;; We'll go to very distant files
;; With Python the Language and Me the Human
;; The fun will never end, it's Syntax Time!

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; LSP Mode - The Main Attraction
;;------------------------------------------------------------------------------

;; Trial [2019-08-12]
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)

  ;;-----
  :hook
  ;;-----
  ;; make sure we have lsp-imenu everywhere we have LSP?
  (lsp-after-open-hook . lsp-enable-imenu)


  ;;-----
  :config
  ;;-----

  ;; Anything for generic LSP here? Specific languages should hold off for their
  ;; own config.

  ;; Do I need this?
  ;;(require 'lsp-clients)


  ;; TODO: any lsp settings to set?
  ;; TODO: any sub-package settings to set?

  ;;-----
  ;; lsp sub-packages
  ;;-----
  (use-package lsp-ui
    ;; [2019-09-17] DISABLING FOR NOW
    ;; Just to see how non-ui looks/acts...
    :disabled

    :commands lsp-ui-mode

    :hook
    (lsp-mode-hook . lsp-ui-mode)

    :config
    (setq lsp-ui-sideline-ignore-duplicate t))

  (use-package company-lsp
    :after company
    :commands company-lsp
    :config
    (push 'company-lsp company-backends))

  (use-package helm-lsp
    :after helm
    :commands helm-lsp-workspace-symbol
    )

  (use-package lsp-treemacs
    :after treemacs
    :commands lsp-treemacs-errors-list
    )
  )

;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-lsp)
