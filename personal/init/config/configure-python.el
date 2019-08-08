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

  ;; Don't need hook right now as I'm using my default tab-width.
  ;; :init
  ;; ;; define my hook
  ;; (defun spydez/hook/python-mode ()
  ;;   (setq tab-width spydez/dev-env/tab/normal))
  ;;
  ;; :hook spydez/hook/python-mode

  :bind
  (;; No global binds for python ATM.
   :map python-mode-map
        ("C-c C-c" . comment-or-uncomment-region)
        ;; TODO: A better key for compile, if needed?
        ;; `C-c C-c' is comment-region in cpp-mode and I like that...
        ;;("C-c C-c" . compile)
        ;; "C-c p c" is projectile compile project...
        )

  :custom
  ;; Set this undocumented thingy to same as tab-width so complaints stop?
  ;;   - "Can't guess" complaint didn't stop.
  (python-indent-offset spydez/dev-env/tab/normal "Set indent to tab-width.")

  ;; TODO: Do I want any of these things? If so, do I need to turn on or are
  ;;   they global/prog-mode already?
  ;; :config
  ;; (paredit-mode 1)
  ;; (omnisharp-mode 1)
  ;; (my/disable-paredit-spaces-before-paren)
  ;; (company-mode 1)
  ;; (yas-minor-mode 1)
  ;; (flycheck-mode 1)

  ;; TODO: set up autopep8 for checking code formatting (works on 2.7)?
  ;;   - https://nhoffman.github.io/.emacs.d/#orgaa724de
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
;; TODO: Is this key the usual for compiling? Do I need to compile?
;; Is there a better key? `C-c C-c' is/was comment-region in cpp-mode and
;; I like that command...
(eval-after-load 'python-mode
  '(bind-key "C-c C-c" 'compile python-mode-map))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------
;; TODO: check out python in emacs repl?

;; TODO: django mode?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-python)
