;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; TODO: check this out for stuff:
;;   https://www.emacswiki.org/emacs/CSharpMode

;; TODO: these might belong in their own file...

;; TODO: check this for any useful things?
;;   https://www.gnu.org/software/emacs/manual/html_mono/efaq-w32.html#DevStudio

;; TODO: flycheck? Here? configure-cpp? configure-prog (prog-mode)?
;;   https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el


;;------------------------------------------------------------------------------
;; C#
;;------------------------------------------------------------------------------

;; C# isn't very popular as an emacs mode. Who'd've thunk.
;; From https://github.com/cbaggers/dotemacs/blob/master/init/csharp.el

;; Some from old emacs config, some from someone else's.
(defun spydez/hook/csharp-mode ()
;;; <my old emacs>

  ;; Use BSD in C, C++. Used to use it in C#.
  ;; But now C# shares a style with Java, so see if that's better.
  ;; Or go back to BSD if I hate it.
  ;; (c-set-style "bsd") ;; See c-style-alist for supported types
  ;; Trial [2019-02-15]
  (c-set-style "C#") ;; See c-style-alist for supported types

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

  ;; TODO: check out these ideas from: https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el
  ;; TODO: enable/disable in c-common-hook as well.

  ;; Separate camel-case into separate words.
  ;; (subword-mode t)
  )

;; TODO: check this https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/csharp.el
(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook 'spydez/hook/csharp-mode)

;; TODO: and... again with the do-i-want-this-i-don't-know-yet
;  :bind
;  (("{" 'paredit-open-curly csharp-mode-map)
;   ("}" 'paredit-close-curly csharp-mode-map))
  )

;; §-TODO-TODAY-§: omnisharp probably want
;; (use-package omnisharp
;;   :after csharp-mode)

;; https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/csharp.el
;; (use-package omnisharp
;;   :config
;;   (validate-setq
;;    omnisharp-server-executable-path
;;    (path-join *user-omnisharp-path* "omnisharp")))

;; dunno where I got this one...
;;  (use-package omnisharp
;;    :ensure t
;;    :config
;;    (setq omnisharp-server-executable-path "/home/baggers/Programs/omnisharp/myrun.sh")
;;    (bind-key "M-." 'omnisharp-go-to-definition csharp-mode-map)
;;    (bind-key "M-," 'pop-tag-mark csharp-mode-map)
;;    (bind-key "C-c C-w C-c" 'omnisharp-find-usages csharp-mode-map)
;;    (add-to-list 'company-backends 'company-omnisharp))



;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: some sanity check for things-I-want-in-mode-X-but-are-set-globally?
;;   - On the one hand, they're set globally, so it's likely I want a bunch of
;;     things that aren't even in this file or on my mind.
;;   - On the other hand, I don't like throwing away things that were useful
;;     before and might be again.
;;   - On the gripping hand, might as well do everything I can to catch things
;;     that may go wrong when I'm not familiar with the inner workings of
;;     all my init files?

;; TODO: CEDET?
;;   - https://github.com/tuhdo/tuhdo.github.io/blob/master/emacs-tutor/cedet.org

;; TODO: Omnisharp? 
;;   - http://www.omnisharp.net/
;;   - https://github.com/OmniSharp/omnisharp-emacs


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-csharp)
