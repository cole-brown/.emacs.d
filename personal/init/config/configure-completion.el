;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; helm-mini: C-c h
;;   - open buffers, recentf files, make new buffer
;;   - No `open file plz' though.
;; Wildcards and stuff available.
;;   https://tuhdo.github.io/helm-intro.html#orgheadline7
;;   *<major mode>  - filter to files in <major mode>
;;   *!<major mode> - the opposite
;;   /<directory>   - filter to a dir
;;   !/<directory>  - the opposite
;;   @<regexp>      - here there be dragons. er, regexs.
;; and combine too cuz why not: *lisp ^helm @moc
;;   1) lisp mode, 2) buffer name starts with "helm", 3) has "moc" in buffer


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
  :delight helm-mode
  :demand t

  ;;---
  :custom
  ;;---
  (helm-candidate-number-limit 500
    "Trying a larger limit. Default is 100. Bigger is slower.")
  (helm-M-x-requires-pattern nil
    "Let M-x start off with candidates.")

  (helm-ff-skip-boring-files t
    "Helm's 'Find files' ignores `helm-boring-file-regexp-list'.")

  (helm-buffer-max-length 30
    (concat "Buffer names were clipped too often. "
            "Especially special mode buffer names like "
            "'*helpful variable: helm-buffer-max-length*'. "
            "Default was 20. Upping to 30 to see how it goes."))

  ;;-----
  :bind ;; global
  ;;-----
  ;; [2019-03-01]: swapped helm-buffers-list with this.
  (("C-x b"     . helm-mini)
   ;; [2019-05-01]: helm-mini is more useful than helm-buffers-list, IMO.
   ("C-x C-b"   . helm-mini)
   ("C-c h"     . helm-buffers-list)
   ("C-h a"     . helm-apropos)
   ("M-y"       . helm-show-kill-ring)

   ("M-x"       . helm-M-x)
   ;; from my old emacs config, muscle memory and ultimately:
   ;;   http://steve.yegge.googlepages.com/effective-emacs
   ("C-x C-m"   . helm-M-x) ; Invoke M-x w/o the Alt key. Useful for Dvorak.

   ("C-x c o"   . helm-occur)
   ("C-x c s"   . helm-swoop)
   ("C-x c y"   . helm-yas-complete)
   ("C-x c Y"   . helm-yas-create-snippet-on-region)
   ("C-x c SPC" . helm-all-mark-rings)
   ("C-x C-f"   . helm-find-files)) ; Use helm for finding instead of find-files

  ;;---
  :config
  ;;---
  (helm-mode))

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

  ;;-----
  :bind ;; global
  ;;-----
  (("C-S-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-s s" . helm-swoop)
   ("M-s M-s" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))

  ;;-----
  :bind ;; isearch-mode-map
  ;;-----
  (:map isearch-mode-map
        ("M-i" . helm-swoop-from-isearch))

  ;;-----
  :bind ;; helm-swoop-map
  ;;-----
   (:map helm-swoop-map
         ("M-i" . 'helm-multi-swoop-all-from-helm-swoop))

  ;; :config?
  )

;; Use C-S-s for search. Can go into "Edit Mode" with C-c C-e.
;; Before enter the edit mode, you can choose some lines marked by C-SPC
;; or M-SPC to edit. Apply changes to original buffer type C-x C-s.

;;---
;; Helm: Fuzzy Matching
;;---

;; "As for the fuzzy matching, look into the "helm-flx" package. It gives you real fuzzy matching, much better than the default helm matching."
;;   - https://news.ycombinator.com/item?id=11100341
;; https://github.com/PythonNut/helm-flx
;; Trial [2019-03-01]
(use-package helm-flx
  :after helm

  ;;---
  :custom
  ;;---

  ;; "Turn it on"
  (helm-flx-for-helm-find-files t)
  (helm-flx-for-helm-locate     t)

  ;; "Make it fuzzy... everywhere."

  ;; These are Helm vars...
  ;; Do I... do I use these with helm-flx?
  ;;  Or only when not using helm-flx?
  ;;    Or what?
  ;; https://tuhdo.github.io/helm-intro.html
  (helm-M-x-fuzzy-match                  t)
  (helm-buffers-fuzzy-matching           t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-file-cache-fuzzy-match           t)
  (helm-imenu-fuzzy-match                t)
  (helm-mode-fuzzy-match                 t)
  (helm-locate-fuzzy-match               t)
  (helm-recentf-fuzzy-match              t)
  (helm-semantic-fuzzy-match             t)

  ;;---
  :config
  ;;---

  ;; and enable
  (helm-flx-mode +1))


;; helm-fuzzier will only enhance matching for sources that have fuzzy-matching
;; enabled, so be sure to enable fuzzy-matching for the sources you're
;; interested in by setting the appropriate variable.
;;   https://github.com/EphramPerdition/helm-fuzzier
;; Use in conjunction with helm-flx.
;; Trial [2019-03-01]
;;   - Yeah this is better.
;;     - Without flx or fuzzier: M-x helm-projectile RET concom finds nothing.
;;     - With flx, no fuzzier:   M-x helm-projectile RET concom has *con*fig/*c*onfigure-*o*rg-*m*ode as best.
;;     - With flx, and fuzzier:  M-x helm-projectile RET concom has config/*con*figure-*com*pletion as best.
(use-package helm-fuzzier
  :after helm

  ;;---
  :custom
  ;;---
  ;; These are also set in my helm-flx config...
  ;; I don't know if they're needed there, but they are
  ;; wanted here by helm-fuzzier.
  (helm-M-x-fuzzy-match                  t)
  (helm-buffers-fuzzy-matching           t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-file-cache-fuzzy-match           t)
  (helm-imenu-fuzzy-match                t)
  (helm-mode-fuzzy-match                 t)
  (helm-locate-fuzzy-match               t)
  (helm-recentf-fuzzy-match              t)
  (helm-semantic-fuzzy-match             t)

  ;;---
  :config
  ;;---

  (helm-fuzzier-mode 1))


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
;; TODO: Any way to make this use the other-window instead of steal-the-whole-frame?

;; Bound these in Helm right now. Put these in (use-package ido...) :bind section if needed again?
;; Invoke M-x w/o the Alt key.
;; from: http://steve.yegge.googlepages.com/effective-emacs
;; (global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\C-c\C-m" 'execute-extended-command) ; todo: I never use this one... lose it?

;; https://tuhdo.github.io/helm-intro.html#orgheadline27
;; TODO: try helm-color? may not need...
;; color picked, show color in emacs somewhere, customize faces nicely?

;; https://tuhdo.github.io/helm-intro.html#orgheadline4
;; TODO: try helm-autoresize-mode if helm buffer is annoying? Should be ok with
;; my side-by-side buffers.


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
  ;;---
  :custom
  ;;---

  ;; ignore buffers like these
  (ido-ignore-buffers '("\\` " "^\*" ".*Completion" "^irc\." "\.TAGS$"))
  (ido-case-fold             t   "Be case-insensitive")
  (ido-enable-flex-matching  t   "Be flexable in search if nothing better")
  (ido-use-filename-at-point nil "Annoying: try to use filename...")
  (ido-use-url-at-point      nil "Quite Annoying: try to use url at point.")
  (ido-use-virtual-buffers   t
    "If recentf enabled, allow visit to recently closed files")
  (ido-auto-merge-work-directories-length -1
    "ido should let me make my new file instead of going on a search for it")
  ;; Considerations:
  ;; ido-max-prospects 5             ; don't spam my minibuffer
  ;; ido-confirm-unique-completion t ; wait for RET, even with unique completion

  ;;---
  :config
  ;;---

  (ido-mode t)
  (ido-everywhere 1))


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
  :delight
  :demand t

  ;;---
  :custom
  ;;---
  (company-dabbrev-downcase nil
   "Don't downcase candidates from dabbrev backend.")
  (company-dabbrev-ignore-case t
   (concat "Ignore case for completion candidates and ignore "
           "typed text for completion insertions."))

  (company-dabbrev-code-everywhere t
   "Completion in comments and strings.")
  (company-dabbrev-code-ignore-case t
   (concat "Ignore case for completion candidates and ignore "
           "typed text for completion insertions."))
  ;; May need to update `company-dabbrev-code-modes' some day, but it's got
  ;; prog-mode so it seems ok right now.

  ;;---
  :hook
  ;;---
  ;; Enable Company-Mode in any programming mode.
  (prog-mode . company-mode))


;; Completion will start automatically after you type a few letters. Use M-n and
;; M-p to select, <return> to complete or <tab> to complete the common part.
;; Search through the completions with C-s, C-r and C-o. Press M-(digit) to
;; quickly complete with one of the first 10 candidates.


;; https://melpa.org/#/helm-company
;; https://github.com/Sodel-the-Vociferous/helm-company
;; Trial [2019-09-17]
(use-package helm-company
  :delight

  ;;---
  :bind ;; company-mode-map
  ;;---
  (:map company-mode-map
        ("C-;" . helm-company))

  ;;---
  :bind ;; company-active-map
  ;;---
  (:map company-active-map
        ("C-;" . helm-company)))

;; Maybe `company-box' for some pretty icons?
;; https://github.com/sebastiencs/company-box
;; https://www.reddit.com/r/emacs/comments/b06grn/companybox_icon_too_large/
;; https://www.reddit.com/r/emacs/comments/8z4jcs/tip_how_to_integrate_company_as_completion/


;;------------------------------------------------------------------------------
;; dabbrev - Dynamic Abbreviation Expand
;;------------------------------------------------------------------------------
;; Exclude very large buffers from dabbrev
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org84a9889
(defconst spydez/size-char/dabbrev-ignore (* 5 1024 1024)
  "Ignore large buffers for dabbrev expansion.")
;; TODO: does emacs have a nice X MB/GB/Whatever -> bytes func?
;;   I have one or two places where I'm doing this multiplying...
(defun spydez/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) spydez/dabbrev-friend-buffer))

(setq dabbrev-friend-buffer-function 'spydez/dabbrev-friend-buffer)

;; Make sure case is preserved when using M-/ completion
(setq dabbrev-case-replace nil)


;;------------------------------------------------------------------------------
;; Hippie Expand
;;------------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/HippieExpand
;; Replace dabbrev with hippie, which uses dabbrev as part of its expand check.
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org84a9889
(bind-key "M-/" 'hippie-expand)

;; I was getting full lines completed first... Are they from yas or what?
;; I don't want them. Maybe last - I should try last.
;;   `try-expand-list' is the function ruining everything right now... So 'or what.'
;;
;; hippie-expand-try-functions-list is currently:
;;   (try-complete-file-name-partially try-complete-file-name
;;   try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev
;;   try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
;;   try-complete-lisp-symbol-partially try-complete-lisp-symbol)
;;
;; That... is terrible. Who wants file names and repeating old lines of code
;; first, wtf?
;;
;; This order instead. Mainly moved dabbrev up to top.
;; Thanks to this guy for having run into this first and telling everyone:
;; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list
      ;; dabbrev first, as it is usually useful
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill

        ;; http://pages.sachachua.com/.emacs.d/Sacha.html#org84a9889
        yas-hippie-try-expand

        ;; ...sure. All those filenames I need auto-completed.
        try-complete-file-name-partially
        try-complete-file-name

        ;; I have read the docstring and am only more clueless.
        try-expand-all-abbrevs

        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol

        ;; What sane person needs these, and why don't they use functions
        ;; or something...
        ;; try-expand-list
        ;; try-expand-line
        ))

;; Do we leave hippie and M-/ as one thing, and company as a totally different
;; thing? Think I'll try that for now.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-completion)
