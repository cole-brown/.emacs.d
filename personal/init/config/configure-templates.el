;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; http://ergoemacs.org/emacs/emacs_templates.html

;; TODO:  Check yasnippet settings:
;;   https://github.com/manuel-uberti/.emacs.d/blob/master/lisp/mu-completion.el


;;------------------------------------------------------------------------------
;; Yasnippets
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
;; https://www.emacswiki.org/emacs/Yasnippet
;; https://joaotavora.github.io/yasnippet/snippet-expansion.html
(use-package yasnippet
  :demand t
  :delight yas-minor-mode

  ;;---
  :init
  ;;---
  (defun spydez/yas/list ()
    "Calls yas-insert-snippet to show list of possible snippets.
Cuz I can't remember what to call so `spydez/yas` and wait for
auto-complete to have pity is my game."
    (interactive)
    (call-interactively #'yas-insert-snippet nil))

  (spydez/hook/defun snippet-mode t nil nil "init/config/configure-templates.el"
    "Hook for yasnippet editting."
    (setq require-final-newline nil))


  ;;---
  :custom
  ;;---
  (yas-indent-line 'auto
    "Try auto-indenting snippet.")
  (yas-also-auto-indent-first-line t
    "Also auto indent first line if yas-indent-line.")
  (yas-also-indent-empty-lines t
    "Also indent the empty lines...")
  (yas-triggers-in-field t
    "Allows snippets inside of snippets")

  ;;---
  :hook
  ;;---
  (snippet-mode-hook . spydez/hook/snippet-mode-hook)


  ;;---
  :config
  ;;---

  ;; This modifies how yas looks for matching keys to expand into templates.
  ;;   - https://emacs.stackexchange.com/a/35603
  ;;   - Also check out its documentation at: C-h v yas-key-syntaxes
  ;; So if I have keys that don't feel like they're getting triggered right
  ;; from wherever my cursor is when I try, we can adjust this.
  ;; Don't see a need right now, though.
  ;; (setq yas-key-syntaxes '("w_" "w_." "^ "))

  ;; Want my snippets at the front of the list (ahead of yasnippet-snippets')
  ;; Currently this goes:
  ;;   mine, yasnippet's snippets dir, yasnippet-snippets' dir.
  (add-to-list 'yas-snippet-dirs spydez/dir/yasnippets)

  ;; Get rid of `yas-expand' binding on TAB. Cannot do this from the `:bind'
  ;; section, annoyingly. And other annoyinglies as well. See:
  ;;   (spydez/help/issue/visit "yasnippet" "unbind-tab.org")
  ;; for more details.
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map)

  (yas-global-mode 1))

;; Should I try this? I am not actually quite sure what it does but it has a lot
;; of installs on MELPA and it may or may not be a helm/yasnippet integration
;; package...
;;   - helm-c-yasnippet
;;   - https://github.com/emacs-jp/helm-c-yasnippet/
;;     - "helm source for yasnippet.el" Thank you. Very useful info.


;; ...Was `:disabled' not enough?!
;; ;;------------------------------------------------------------------------------
;; ;; Default/External Snippets
;; ;;------------------------------------------------------------------------------
;; ;; "Official Collection of Snippets"
;; ;; https://github.com/AndreaCrotti/yasnippet-snippets
;; ;; https://github.com/manuel-uberti/.emacs.d/blob/master/lisp/mu-completion.el
;; ;;   Looks like no real setup - just make sure my snippets are ahead of these
;; ;;   in the list `yas-snippet-dirs'.
;; ;; Some people have this inside of (use-package yasnippet)
;; (use-package yasnippet-snippets
;;   :after yasnippet
;;   :demand t

;;   ;; Trial [2020-03-18]
;;   ;; This triggers on single/dual letters way too often when I wanted to just
;;   ;; autocomplete a word/name that's used a lot in current context. I don't
;;   ;; think I'm using many of the snippets, if any, anyways, so... disable
;;   ;; for now.
;;   :disabled)


;;------------------------------------------------------------------------------
;; Disabled/Broken: Yasnippet Insert Indicators on Cursor
;;------------------------------------------------------------------------------
;; (setq spydez/default-cursor-color "gray")
;; (setq spydez/yasnippet-can-fire-cursor-color "purple")
;;
;; ;; It will test whether it can expand, if yes, cursor color -> can-fire-cursor-color.
;; (defun spydez/yasnippet-can-fire-p (&optional field)
;;   (interactive)
;;   (setq yas--condition-cache-timestamp (current-time))
;;   (let (templates-and-pos)
;;     (unless (and yas-expand-only-for-last-commands
;;                  (not (member last-command yas-expand-only-for-last-commands)))
;;       (setq templates-and-pos (if field
;;                                   (save-restriction
;;                                     (narrow-to-region (yas--field-start field)
;;                                                       (yas--field-end field))
;;                                     (yas--templates-for-key-at-point))
;;                                 (yas--templates-for-key-at-point))))
;;     (and templates-and-pos (first templates-and-pos))))
;;
;; (defun spydez/change-cursor-color-when-can-expand (&optional field)
;;   (interactive)
;;   (when (eq last-command 'self-insert-command)
;;     (set-cursor-color (if (spydez/can-expand)
;;                           spydez/yasnippet-can-fire-cursor-color
;;                         spydez/default-cursor-color))))
;;
;; (defun spydez/can-expand ()
;;   "Return true if right after an expandable thing."
;;   (or (abbrev--before-point) (spydez/yasnippet-can-fire-p)))

;; DISABLED:
;; So this is neat and all but it needs way better condition checking for resetting
;; cursor color (or way stupider (e.g. always reset to normal, set to purp after if can-expand)).
;; Easy way to break it: `up' is tag for `use-package' elisp snippet.
;;   `up C-<backspace>' breaks this.
;; Will have to hook into more stuff, or improve the predicate.
;; Disabled for now.
;; (add-hook 'post-command-hook 'spydez/change-cursor-color-when-can-expand)
;; NOTE: See this for a library that does cursor changes:
;;   https://www.emacswiki.org/emacs/ChangingCursorDynamically


;;------------------------------------------------------------------------------
;; Yasnippet -> Hydra/Key-Chord/Hippie
;;------------------------------------------------------------------------------
;; Stuff Yasnippet into more things.

;; TODO [2019-09-19]: not using these. Delete if this is still true.

;; used in config hydra
(defun spydez/insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (spydez/hippie-expand-maybe nil) (insert "  "))))

;; Modify the behaviour of hippie-expand so that it doesn't ding so much...
;; todo: is this function used in my init (of hippie?)?

(defun spydez/hippie-expand-maybe (arg)
  "Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument],
undoes the expansion."
  (interactive "P")
  (require 'hippie-exp)
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn (setq he-num -1) nil)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list)))))
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Undoing expansions"))))))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: check this for anything useful:
;;   https://www.reddit.com/r/emacs/comments/8vdhb4/tip_how_to_integrate_snippets_with_yasnippets/
;; TODO: Grab old snippets?
;; TODO: Some new snippets for C# or Django or whatever? As needed maybe?
;; TODO: Just noticed auto-complete (hippie?) for elisp is a bit overzealous...
;;   - `(use-package yas<tab>' was completing to the full sexpr of the up above.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-templates)
