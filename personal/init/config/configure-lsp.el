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

(defun spydez/lsp/kill-all ()
  "Interactive call to `spydez/lsp/shutdown-all'. Prints how many LSP workspaces
were shut down.
"
  (interactive)
  (mis/message/propertize t :default
                          :title
                          "Killed %s LSP servers."
                          (let ((shutdown (spydez/lsp/shutdown-all)))
                            (if (and shutdown (listp shutdown))
                                (length shutdown)
                              0))))


(defun spydez/lsp/shutdown-all ()
  "Checks all open buffers for LSP servers. Shuts them each down
as they are found. Returns list of kill LSP workspaces.
"
  ;; Couldn't just have an easy way to do this?..
  (let ((killed))
    (dolist (buffer (buffer-list) killed)
      (when (and buffer (buffer-live-p buffer))
        (save-mark-and-excursion
          (with-current-buffer buffer
            (lsp-foreach-workspace
             (unless (seq-contains killed it)
               ;; Could check for pyls vs others here if wanted.
               (lsp-workspace-shutdown it)
               (push it killed)))))))))


;;------------------------------------------------------------------------------
;; LSP Mode - The Main Attraction
;;------------------------------------------------------------------------------

;; Trial [2019-08-12]
;; https://github.com/emacs-lsp/lsp-mode
(require 'with)
(use-package lsp-mode
  :when (spydez/packages/enabled-p 'lsp-mode)

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


  ;; ;;-----
  ;; :config
  ;; ;;-----

  ;; Anything for generic LSP here? Specific languages should hold off for their
  ;; own config.

  ;; [2019-09-18]: Moved sub-packages out.
  )


;;---
;; LSP-Mode Hooks
;;---
;; [2019-10-07]: Moved these hooks out of config so they're always defined even
;; when lsp is disabled.

(defun spydez/hook/lsp-generic (lsp-fn)
  "LSP hook helper for both deferred and regular."
  (funcall lsp-fn)
  (with-function 'flycheck-mode
    ;; the python codebase I have has... a lot of
    ;; PYLS/pylint/pycodestyle errors... :/
    (customize-set-variable 'flycheck-checker-error-threshold 800)
    (flycheck-mode)))


(spydez/hook/defun 'lsp-mode-hook t
    "lsp-deferred" nil "init/config/configure-lsp.el"
  "General LSP hook for any mode. Defers LSP startup until buffer is visible."
  (if (not (spydez/packages/enabled-p 'lsp-mode))
      (mis/debug
       '(spydez debug hook)
       (concat "spydez/hook/lsp-deferred: ignoring due to "
               "(spydez/packages/enabled-p 'lsp-mode) disabled flag."))
    (spydez/hook/lsp-generic #'lsp-deferred)))


(spydez/hook/defun 'lsp-mode-hook t
    "lsp-immediate" nil "init/config/configure-lsp.el"
  "General LSP hook for any mode. Starts LSP immediately."
  (if (not (spydez/packages/enabled-p 'lsp-mode))
      (mis/debug
       '(spydez debug hook)
       (concat "spydez/hook/lsp-immediate: ignoring due to "
               "(spydez/packages/enabled-p 'lsp-mode) disabled flag."))
    (spydez/hook/lsp-generic #'lsp)))


;;------------------------------------------------------------------------------
;; LSP Sub-Packages
;;------------------------------------------------------------------------------

(use-package lsp-ui
  :when (spydez/packages/enabled-p 'lsp-mode)

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
  :when (spydez/packages/enabled-p 'lsp-mode)

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
  :when (spydez/packages/enabled-p 'lsp-mode)

;;  :after lsp-mode
  :commands helm-lsp-workspace-symbol)


(use-package lsp-treemacs
  :when (spydez/packages/enabled-p 'lsp-mode)

;;  :after lsp-mode
  :commands lsp-treemacs-errors-list)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-lsp)
