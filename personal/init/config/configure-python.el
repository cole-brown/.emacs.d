;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Setting up virtualenv to use with python:
;;   http://nhoffman.github.io/.emacs.d/#org766cea6
;; Works for pythons 2 and 3.


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

;; TODO: python-mode vs elpy...?
;; https://github.com/jorgenschaefer/elpy

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)

  ;;---
  ;; Language Server Protocol for Python (LSP)
  ;;---
  :after lsp-mode
  ;;:demand t


  ;; ;;-----
  ;; :init
  ;; ;;-----
  ;; (spydez/message/warning nil nil "Uh... hi? python:init")


  ;;-----
  :hook
  ;;-----
  ;;---
  ;; Language Server Protocol for Python (LSP)
  ;;---
  (python-mode . spydez/hook/lsp-deferred)


  ;; No global binds for python ATM.

  ;;-----
  :bind ;; python-mode-map
  ;;-----
  (:map python-mode-map
        ;; [2019-08-12]
        ;;   - Moved `comment-or-uncomment-region' to configure-dev-env.el
        ;;     and upgraded to prog-mode-map.
        ;;   - Found out that isn't enough and I have to also map it here to
        ;;     overwrite `python-shell-send-buffer' keybind.
        ;;   - So `comment-or-uncomment-region' is bound in 2 maps.
        ;; `python-shell-send-buffer' still sounds useful, but not on "C-c C-c"
        ("C-c C-c" . comment-or-uncomment-region))


  ;;-----
  :custom
  ;;-----
  ;; Set this undocumented thingy to same as tab-width so complaints stop?
  ;;   - "Can't guess" complaint didn't stop.
  (python-indent-offset spydez/dev-env/tab/normal "Set indent to tab-width.")

  (python-fill-docstring-style 'symmetric)


  ;; ;;-----
  ;; :config
  ;; ;;-----
  ;; (spydez/message/warning nil nil "Uh... hi? python:config")

  ;; ;; Language Server Protocol for Python (LSP)
  ;; ;;---
  ;; (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
  ;;                   :major-modes '(python-mode)
  ;;                   :server-id 'pyls))
  ;; ;;---


  ;; TODO: Do I want any of these things? If so, do I need to turn on or are
  ;;   they global/prog-mode already?
  ;; (paredit-mode 1)
  ;; (omnisharp-mode 1)
  ;; (my/disable-paredit-spaces-before-paren)
  ;; (company-mode 1)
  ;; (yas-minor-mode 1)
  ;; (flycheck-mode 1)

  ;; TODO: set up autopep8 for checking code formatting (works on 2.7)?
  ;;   - https://nhoffman.github.io/.emacs.d/#orgaa724de


  ;; ;; NB: only required if you prefer flake8 instead of the default
  ;; ;; send pyls config via lsp-after-initialize-hook -- harmless for
  ;; ;; other servers due to pyls key, but would prefer only sending this
  ;; ;; when pyls gets initialised (:initialize function in
  ;; ;; lsp-define-stdio-client is invoked too early (before server
  ;; ;; start)) -- cpbotha
  ;; (defun lsp-set-cfg ()
  ;;   (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
  ;;     ;; TODO: check lsp--cur-workspace here to decide per server / project
  ;;     (lsp--set-configuration lsp-cfg)))

  ;; (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
  )

;; Django has several modes for different parts:
;; https://code.djangoproject.com/wiki/Emacs
;;   - i.e. the python, the html, the css,
;;     the templates w/ html and template language interwoven


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------
;; TODO: check out python in emacs repl?

;; TODO: django mode?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-python)
