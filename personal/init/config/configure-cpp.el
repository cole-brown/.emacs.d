;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; TODO: these might belong in their own file...


;;------------------------------------------------------------------------------
;; C, C++ Hooks.
;;------------------------------------------------------------------------------

;; Some from old emacs config, some from someone else's.
;; C and C++ are exactly the same right now, so here's the hook for both.
(defun spydez/hook/c-common-mode ()
;;; <my old emacs>

  ;; Use BSD in C, C++.
  (c-set-style "bsd") ;; See c-style-alist for supported types

  (setq c-basic-offset spydez/dev-env/tab/normal)
  (c-set-offset 'innamespace 0) ; Don't indent namespace - waste of indent level
  (c-set-offset 'case-label '+) ; indent case labels by c-indent-level, too

  ;; Have this set globally.
  ;; (setq indent-tabs-mode nil)

  (setq c-indent-level spydez/dev-env/tab/normal)

  ;; electric-indent-mode is true and might take care of this?
  ;; (local-set-key [return] 'newline-and-indent)

  ;; fill-column can use default for now.
  ;; (setq fill-column spydez/dev-env/fill-column/normal)

  ;; line numbers already on globally

;;; </my old emacs>

  ;; TODO: I probably want most or all of these, or some competing package/feature
  ;; (paredit-mode 1)
  ;; (omnisharp-mode 1)
  ;; (my/disable-paredit-spaces-before-paren)
  ;; (company-mode 1)
  ;; (yas-minor-mode 1)
  ;; (flycheck-mode 1)

  ;; Ideas from: https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el

  ;; Separate camel-case into separate words.
  ;; (subword-mode t)
  )

;; My actual C++ hook - just the default for now
(defun spydez/hook/c++-mode ()
  (spydez/hook/c-common-mode)
  )

;; My actual C hook - just the default for now
(defun spydez/hook/c-mode ()
  (spydez/hook/c-common-mode))


;;------------------------------------------------------------------------------
;; Packages.
;;------------------------------------------------------------------------------

;; TODO: check this https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el
(use-package cc-mode
  ;; TODO: switch all package hook from add-hook to this if possible?
  :hook ((c++-mode-hook . spydez/hook/c++-mode)
         (c-mode-hook . spydez/hook/c-mode))

  :config
  (add-to-list 'auto-mode-alist '("\\.cxx$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))

  ;; Can setup auto-complete, company, flycheck, lots others here.
  ;; TODO: lots more here maybe when I do some C/C++ work again.

  ;; ;; Available C style:
  ;; ;; "gnu": The default style for GNU projects
  ;; ;; "k&r": What Kernighan and Ritchie, the authors of C used in their book
  ;; ;; "bsd": What BSD developers use, aka "Allman style" after Eric Allman.
  ;; ;; "whitesmith": Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
  ;; ;; "stroustrup": What Stroustrup, the author of C++ used in his book
  ;; ;; "ellemtel": Popular C++ coding standards as defined by "Programming in C++, Rules and Recommendations," Erik Nyquist and Mats Henricson, Ellemtel
  ;; ;; "linux": What the Linux developers use for kernel development
  ;; ;; "python": What Python developers use for extension modules
  ;; ;; "java": The default style for java-mode (see below)
  ;; ;; "user": When you want to define your own style
  ;; (setq c-default-style "linux") ;; set style to "linux"
  ;; Think my style choice is... bsd?
  )


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: configure-csharp seems to be the place right now for C-Family TODOs.
;; Check there.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-cpp)
