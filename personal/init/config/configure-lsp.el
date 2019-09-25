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
(require 'with)
(use-package lsp-mode
  ;; :commands (lsp lsp-deferred)
  :demand t


  ;;-----
  :hook
  ;;-----
  ;; make sure we have lsp-imenu everywhere we have LSP?
  (lsp-after-open-hook . lsp-enable-imenu)


  ;;-----
  :custom
  ;;-----
  (lsp-enable-indentation nil
                          "This seemed to mangle & munge more than it helped.")
  (lsp-auto-guess-root t
                       "Try to detect project root automatically.")
  (lsp-prefer-flymake nil
                      "Use lsp-ui and flycheck instead.")
  (flymake-fringe-indicator-position 'right-fringe
                                     "Left fringe is busy with git stuff.")


  ;;-----
  :config
  ;;-----
  (defun spydez/hook/lsp-generic (lsp-fn)
    "LSP hook helper for both deferred and regular."
    (funcall lsp-fn)
    ;; TODO: convert to just straight call when flycheck gets gotten to...
    (with-function 'flycheck-mode
      (spydez/warning/message
       nil nil
       "TODO: convert to just straight call when flycheck gets gotten to...")
      (flycheck-mode)))

  (defun spydez/hook/lsp-deferred ()
    "General LSP hook for any mode."
    (spydez/hook/lsp-generic #'lsp-deferred))

  (defun spydez/hook/lsp-immediate ()
    "General LSP hook for any mode."
    (spydez/hook/lsp-generic #'lsp))

  ;; Anything for generic LSP here? Specific languages should hold off for their
  ;; own config.

  ;; [2019-09-18]: Moved sub-packages out.
  )


;;------------------------------------------------------------------------------
;; LSP Sub-Packages
;;------------------------------------------------------------------------------

(use-package lsp-ui
  :after lsp-mode

  ;;---
  :init
  ;;---
  ;; `xref-find-definitions' and `xref-find-references' are defaulted to:
  ;; "M-?" and "M-.", which are not close to each other and hand-mangly for
  ;; Dvorak. Remove their bindings then bind similar lsp-ui functions to
  ;; better keys.

  (unbind-key "M-.") ;; was xref-find-definitions
  (unbind-key "M-?") ;; was xref-find-references

  ;;---
  :hook
  ;;---
  (lsp-mode . lsp-ui-mode)

  ;;---
  :bind
  ;;---
   ;; Better bindings, hopefully, than the xref ones we undefined.
  (:map lsp-ui-mode-map
        ;; was/is count-words-region in global-map
        ("M-=" . lsp-ui-peek-find-references)
        ;; was/is delete-horizontal-space in global-map
        ("M-\\" . lsp-ui-peek-find-definitions))

  ;; TODO: Config?
  ;; Check out what's in here?: M-x customize-group [RET] lsp-ui [RET]
  )


(use-package company-lsp
;;  :after lsp-mode
  :after (lsp-mode company)
  :demand t
  :commands company-lsp

  ;;---
  :init
  ;;---
  (push 'company-lsp company-backends)


  ;;---
  :custom
  ;;---
  ;; TRIAL: [2019-09-18]
  (company-lsp-cache-candidates 'auto
    "Experimenting. Could switch back to default (never cache)."))


(use-package helm-lsp
;;  :after lsp-mode
  :commands helm-lsp-workspace-symbol)


(use-package lsp-treemacs
;;  :after lsp-mode
  :commands lsp-treemacs-errors-list)


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-lsp)
