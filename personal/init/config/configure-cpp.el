;; -*- emacs-lisp -*-


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
  )


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: configure-csharp seems to be the place right now for C-Family TODOs.
;; Check there.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-cpp)
