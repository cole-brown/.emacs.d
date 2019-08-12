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
  :after (lsp-mode projectile)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)


  ;;-----
  :hook
  ;;-----
  (python-mode . lsp-deferred)


  ;;-----
  :bind
  ;;-----
  ;; [2019-08-12]
  ;;   - moved `comment-or-uncomment-region' to configure-dev-env.el
  ;;     and upgraded to prog-mode-map.
  ;;   - Found out that isn't enough and I have to also map it here to overwrite
  ;;     `python-shell-send-buffer' keybind.
  (;; No global binds for python ATM.
   :map python-mode-map
        ("C-c C-c" . comment-or-uncomment-region)
        )


  ;;-----
  :custom
  ;;-----
  ;; Set this undocumented thingy to same as tab-width so complaints stop?
  ;;   - "Can't guess" complaint didn't stop.
  (python-indent-offset spydez/dev-env/tab/normal "Set indent to tab-width.")


  ;;-----
  :config
  ;;-----

  ;; Language Server Protocol for Python
  (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
                    :major-modes '(python-mode)
                    :server-id 'pyls))

  (setq lsp-auto-guess-root t       ; Detect project root
        lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
        flymake-fringe-indicator-position 'right-fringe)

  ;; ;; make sure this is activated when python-mode is activated
  ;; ;; lsp-python-enable is created by macro above
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (lsp-python-enable)))

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

;; python-mode itself comes with emacs now.
;; (setq python-fill-docstring-style 'django)

;; Django has several modes for different parts:
;; https://code.djangoproject.com/wiki/Emacs
;;   - i.e. the python, the html, the css,
;;     the templates w/ html and template language interwoven


;;------------------------------------------------------------------------------
;; Compilation
;;------------------------------------------------------------------------------
;; ;; TODO: Is this key the usual for compiling? Do I need to compile?
;; ;; Is there a better key? `C-c C-c' is/was comment-region in cpp-mode and
;; ;; I like that command...
;; (eval-after-load 'python-mode
;;   '(bind-key "C-c C-c" 'compile python-mode-map))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------
;; TODO: check out python in emacs repl?

;; TODO: django mode?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-python)
