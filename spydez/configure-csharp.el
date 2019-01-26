;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; TODO: these might belong in their own file...


;;------------------------------------------------------------------------------
;; C#
;;------------------------------------------------------------------------------

;; C# isn't very popular as an emacs mode. Who'd've thunk.
;; From https://github.com/cbaggers/dotemacs/blob/master/init/csharp.el

;; I know I have settings in hook in old .emacs
;; but for now, just... leave all this commented out until I get to it.
(defun spydez/hook/csharp-mode ()
  ;; TODO: I probably want most or all of these, or some competing package/feature
;  (paredit-mode 1)
;  (omnisharp-mode 1)
;  (my/disable-paredit-spaces-before-paren)
;  (company-mode 1)
;  (yas-minor-mode 1)
;  (flycheck-mode 1)
  )

;; TODO: check my old .emacs
;; TODO: check this https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/csharp.el
(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook 'spydez/hook/csharp-mode)
  ;; TODO: omnisharp probably want
  ;; TODO: also probably take out of this config - nesting isn't very common
;  (use-package omnisharp
;    :ensure t
;    :config
;    (setq omnisharp-server-executable-path "/home/baggers/Programs/omnisharp/myrun.sh")
;    (bind-key "M-." 'omnisharp-go-to-definition csharp-mode-map)
;    (bind-key "M-," 'pop-tag-mark csharp-mode-map)
;    (bind-key "C-c C-w C-c" 'omnisharp-find-usages csharp-mode-map)
;    (add-to-list 'company-backends 'company-omnisharp))

;; TODO: and... again with the do-i-want-this-i-don't-know-yet
;  :bind
;  (("{" 'paredit-open-curly csharp-mode-map)
;   ("}" 'paredit-close-curly csharp-mode-map))
  )


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: CEDET?
;;   - https://github.com/tuhdo/tuhdo.github.io/blob/master/emacs-tutor/cedet.org

;; TODO: Omnisharp? 
;;   - http://www.omnisharp.net/
;;   - https://github.com/OmniSharp/omnisharp-emacs


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-csharp)
