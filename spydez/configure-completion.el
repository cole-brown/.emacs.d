;; -*- emacs-lisp -*-

;; TODO:
;;   I was using ido in previous .emacs setup.
;;   Fuzzy file completion is a must for whatever I'll be using now?

;; ido:
;;   old ido setup: https://github.com/spydez/emacs/blob/master/libs/custom/ido-config.el
;;   ido intro blog: https://www.masteringemacs.org/article/introduction-to-ido-mode

;; Helm:
;;   Hacker News replies about Helm: https://news.ycombinator.com/item?id=11100312

;; ivy:
;;   https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/


;;------------------------------------------------------------------------------
;; Trial: Helm package
;;------------------------------------------------------------------------------
;; Try out this newfangle Helm thing...
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org04e47b9
;; https://github.com/emacs-helm/helm/wiki
;; Trial: [2019-01-18]
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x) ; Invoke M-x w/o the Alt key. Useful for Dvorak.
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)
         ("C-x C-f" . helm-find-files) ; Use helm for finding instead of find-files
	 ))

;;---
;; Helm: Swoop
;;---
;; List match lines to another buffer, which is able to squeeze by any words you
;; input. At the same time, the original buffer's cursor is jumping line to line
;; according to moving up and down the line list.
;;   https://github.com/ShingoFukuyama/helm-swoop
;;   https://wikemacs.org/wiki/Helm-swoop

;; http://pages.sachachua.com/.emacs.d/Sacha.html#orga9c79c3
;; Trial: [2019-01-23]
(use-package helm-swoop
  :after helm
  :bind
  (("C-S-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-s s" . helm-swoop)
   ("M-s M-s" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   )
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
  )

;; Use C-S-s for search. Can go into "Edit Mode" with C-c C-e.
;; Before enter the edit mode, you can choose some lines marked by C-SPC
;; or M-SPC to edit. Apply changes to original buffer type C-x C-s.

;;---
;; Helm: Fuzzy Matching
;;---

;; "As for the fuzzy matching, look into the "helm-flx" package. It gives you real fuzzy matching, much better than the default helm matching."
;;   - https://news.ycombinator.com/item?id=11100341

;; or all this for default helm fuzzy matching?
;;  - https://tuhdo.github.io/helm-intro.html
;;
;; Starting from 1.6.5, helm-M-x can fuzzy match candidates, but this is not
;; enabled by default. To enable fuzzy matching, add the following setting:
;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
;;
;; (setq helm-buffers-fuzzy-matching t
;;      helm-recentf-fuzzy-match    t)
;;
;; To enable fuzzy matching for both Semantic and Imenu listing, add the following setting:
;; (setq helm-semantic-fuzzy-match t
;;       helm-imenu-fuzzy-match    t)
;;
;; To enable fuzzy matching in helm-locate, add this setting:
;; (setq helm-locate-fuzzy-match t)
;;
;; To enable fuzzy matching, add this setting:
;; (setq helm-apropos-fuzzy-match t)
;;
;; there's... so many...
;; (setq helm-lisp-fuzzy-completion t)


;;---
;; Helm: Misc
;;---

;; "Great for describing bindings. I'll replace the binding for where-is too."
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#org04e47b9
;; https://github.com/emacs-helm/helm-descbinds
(use-package helm-descbinds
  :defer t ;; no need for ":after" with this. https://github.com/jwiegley/use-package/issues/575
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

;; Bound these in Helm right now. Put these in (use-package ido...) :bind section if needed again?
;; Invoke M-x w/o the Alt key.
;; from: http://steve.yegge.googlepages.com/effective-emacs
;; (global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\C-c\C-m" 'execute-extended-command) ; todo: I never use this one... lose it?


;;------------------------------------------------------------------------------
;; Maybe: ivy
;;------------------------------------------------------------------------------
;; TODO: Try out ivy if Helm is to big and clunky...
;;
;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;; https://www.reddit.com/r/emacs/comments/7vcrwo/helm_vs_ivy_what_are_the_differences_what_are_the/dtrcktb


;;------------------------------------------------------------------------------
;; Disabled: ido package
;;------------------------------------------------------------------------------
;; used to use ido - disabling it for a test of Helm

;; combination of my old ido setup and zzamboni's to try to modernize it:
;; https://github.com/zzamboni/dot-emacs/blob/master/init.org
;; but I'm starting off with it disabled for Helm trial, so not sure how working this is.

(use-package ido
  :disabled
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-ignore-buffers '("\\` " "^\*" ".*Completion" "^irc\." "\.TAGS$") ; ignore buffers like these
        ido-case-fold t               ; be case-insensitive
        ido-enable-flex-matching t    ; be flexable in search if nothing better
        ido-use-filename-at-point nil ; annoying:       try to use filename...
        ido-use-url-at-point nil      ; quite annoying: ... or url at point
        ido-use-virtual-buffers t     ; if recentf enabled, allow visit to recently closed files
        ido-auto-merge-work-directories-length -1 ; ido should let me make my new file instead of going on a search for it
        ; ido-max-prospects 5             ; don't spam my minibuffer
        ; ido-confirm-unique-completion t ; wait for RET, even with unique completion
        ))

(use-package ido-completing-read+
  :disabled
  :config
  (ido-ubiquitous-mode 1))

;; ido-related function for find in tags...
;; Can enable and get working if I'm back in a place with ido and tags files.
;; (defun spydez/ido/find-file-in-tag-files ()
;;   (interactive)
;;   (save-excursion
;;     (let ((enable-recursive-minibuffers t))
;;       (visit-tags-table-buffer))
;;     (find-file (ido-completing-read "Project file: "
;;                          (tags-table-files)
;;                          nil t))))
;; 
;; (global-set-key "\C-cf" 'spydez/ido/find-file-in-tag-files)


;;------------------------------------------------------------------------------
;; Company Mode (COMPlete ANYthing)
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orga5f9808
;; http://company-mode.github.io/
;; Trial [2019-02-06]
(use-package company
  :config
  ;; Enable Company-Mode in any programming mode.
  (add-hook 'prog-mode-hook 'company-mode))

;; Completion will start automatically after you type a few letters. Use M-n and
;; M-p to select, <return> to complete or <tab> to complete the common part.
;; Search through the completions with C-s, C-r and C-o. Press M-(digit) to
;; quickly complete with one of the first 10 candidates.

;; https://melpa.org/#/helm-company
;; TODO: use-package helm-company?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-completion)
