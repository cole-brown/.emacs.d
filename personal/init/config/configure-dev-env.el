;; -*- emacs-lisp -*-

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces


;;------------------------------------------------------------------------------
;; Hex Editor
;;------------------------------------------------------------------------------
;; Hex mode is: hexl


;;------------------------------------------------------------------------------
;; dabbrev - Dynamic Abbreviation Expand
;;------------------------------------------------------------------------------
;; Exclude very large buffers from dabbrev
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org84a9889
(defconst spydez/size-char/dabbrev-ignore (* 5 1024 1024)
  "Ignore large buffers for dabbrev expansion.")
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

;; http://pages.sachachua.com/.emacs.d/Sacha.html#org84a9889
;; TODO: when yasnippets is added, add its completion to hippie's list.
(add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
;; TODO: others as well?


;;------------------------------------------------------------------------------
;; Auto-revert mode
;;------------------------------------------------------------------------------
;; For updating a file/buffer when it's changed on disk.
;; e.g. Change in Visual Studio, auto-revert will notice and update here if no
;; changes here. Else it'll just quietly leave it alone.
;;
;; TODO: message/complain if can't auto-revert but wants to?
;; 
;; Right now it's setting itself in some modes but not all. That's probably ok.

;; TODO: Consider magit-auto-revert-mode?

;; If we want to lose the "ARev"...
(require 'diminish)
(eval-after-load 'auto-revert-mode
  '(diminish 'auto-revert-mode))


;;------------------------------------------------------------------------------
;; Defaults
;;------------------------------------------------------------------------------

;;---
;; Tabs
;;---
;; TODO: tabs as spaces setting? We added that somewhere. Maybe these settings
;; don't belong in this file anymore.

;; tab width
(setq-default tab-width spydez/dev-env/tab/normal)

;; TODO: is this global or per-mode in old .emacs?
;; New lines are always indented
;;(global-set-key (kbd "RET") 'newline-and-indent)

;;---
;; Undo Size
;;---

;; give a larger undo buffer
(when (and (boundp 'spydez/undo-limit)
           (> spydez/undo-limit undo-limit))
  (setq undo-limit spydez/undo-limit))
(when (and (boundp 'spydez/undo-strong-limit)
           (> spydez/undo-strong-limit undo-strong-limit))
  (setq undo-strong-limit spydez/undo-strong-limit))


;;------------------------------------------------------------------------------
;; Parenthesis
;;------------------------------------------------------------------------------
;; TODO: disable electric-pair-mode if needed?

;; Used mic-paren in old .emacs, but it looks like:
;;   a) mic-paren is basically derelict (last updated 2013)
;;   b) emacs 26 has equivalent enough replacement in show-paren-mode now.
(require 'paren)

;; good show-paren-mode info here: http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html

;; Trying out default 0.125 - might like it. Might want faster but still non-instant?
;; NOTE: It can be deactivated with the following (which you have to do before
;; activating show-paren-mode in your .emacs):
;;   - https://www.emacswiki.org/emacs/ShowParenMode
;; (setq show-paren-delay 0)

(setq blink-matching-paren nil)

;; 'parenthesis: for just the parens
;; 'expression: for hilighting everything
;; 'mixed: will behave like 'parenthesis when the matching parenthesis is visible,
;;         and like 'expression otherwise. 
;;(setq show-paren-style 'expression)
;;(setq show-paren-style 'parenthesis)
;; 'expression has its uses in elisp code, might could set it to that for the mode

(show-paren-mode t)

;; TODO: Set up a red face for missing (like in old .emacs custom.el)?
;; To change the color/face:
;; (require 'paren)
;; (set-face-background 'show-paren-match (face-background 'default))
;; (set-face-foreground 'show-paren-match "#def")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
;; Note that show-paren-mode can also detect mismatches, e.g. when try to close
;;   a '(' with a ']'. You can tune this with show-paren-mismatch-face, e.g.:
;; (set-face-foreground 'show-paren-mismatch-face "red") 
;; (set-face-attribute 'show-paren-mismatch-face nil 
;;                     :weight 'bold :underline t :overline nil :slant 'normal)
;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Blinking.html
;; orig name: interactive-blink-matching-open
(defun spydez/blink-paren ()
  "Indicate momentarily the start of parenthesized sexp before point."
  (interactive)
  (let ((blink-matching-paren-distance
         (buffer-size))
        (blink-matching-paren t))
    (blink-matching-open)))

;; There are default keybindings for jumping to opening and closing delimiters
;;   C-M b (move backward)
;;   C-M f (move forward)
;; See http://www.emacswiki.org/emacs/ParenthesisMatching#toc2 for details 


;; TODO: Try smartparen and/or rainbow delimiters.

;;---
;; Smartparen
;;---
;; TODO: try out smartparen. But I want to try it out after basic paren stuff is
;; in place (hilighting paired paren reliably).
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orge88d9c9
;; https://github.com/Fuco1/smartparens


;;---
;; Rainbow Delimiters
;;---
;; TODO: Trial this... Does it slows things down a little.
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org36b0308
;; (use-package rainbow-delimiters
;;   :disabled t)


;;------------------------------------------------------------------------------
;; Code Folding
;;------------------------------------------------------------------------------
;; TODO: do I want/need this?
;; from https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html#sec-10-3-2
;; org-mode folding trees is nice, but I dunno if I do/don't want for code.
;; TODO: add in, try out?
;; (setq hs-hide-comments-when-hiding-all +1)
;; (setq hs-isearch-open t)
;; (require 'hideshow-org)
;; ; Displaying overlay content in echo area or tooltip
;; (defun display-code-line-counts (ov)
;;       (when (eq 'code (overlay-get ov 'hs))
;;         (overlay-put ov 'help-echo
;;                      (buffer-substring (overlay-start ov)
;;                                       (overlay-end ov)))))
;;
;;     (setq hs-set-up-overlay 'display-code-line-counts)
;; ; How do I get it to expand upon a goto-line?
;; (defadvice goto-line (after expand-after-goto-line
;;                                 activate compile)
;;         "hideshow-expand affected block when using goto-line in a collapsed buffer"
;;         (save-excursion
;;            (hs-show-block)))


;;------------------------------------------------------------------------------
;; TODO: Kill back to indentation?
;;------------------------------------------------------------------------------
;;  From https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el
;; (defun spydez/kill-back-to-indentation ()
;;   "Kill from point back to the first non-whitespace character on the line."
;;   (interactive)
;;   (let ((prev-pos (point)))
;;     (back-to-indentation)
;;     (kill-region (point) prev-pos)))
;;
;; (bind-key "C-M-<backspace>" 'spydez/kill-back-to-indentation)


;;------------------------------------------------------------------------------
;; Expand Region
;;------------------------------------------------------------------------------
;; This gradually expands the selection. Handy for Emacs Lisp? But will it be
;; actually used?.. Not sure.
;; Trial [2019-01-31]
;; TODO: Might should go elsewhere. configure-kill-ring maybe as it might be used
;; mostly for killing regions? (Already moved it from hydra to dev-env...)
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :defer t
  :bind
  ("C-=" . er/expand-region)
  ("C-<prior>" . er/expand-region)
  ("C-<next>" . er/contract-region)
  )
;; See readme on GitHub for info on improving in certain modes.

;; NOTE: f.lux uses M-<PageUp> and M-<PageDown> (aka M-<prev>/M-<next>)
;; Disable it in the f.lux settings.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-dev-env)
