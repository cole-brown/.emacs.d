;; -*- emacs-lisp -*-

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces

;;------------------------------------------------------------------------------
;; General text settings, global settings, etc
;;------------------------------------------------------------------------------

;; "visual-line-mode is so much better than auto-fill-mode. It doesn't actually break the text into multiple lines - it only looks that way."
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#org3dd06d8
;; Trial [2019-01-30]
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; Can do more Visual Line Mode stuff if I like it, like limit to 80 columns...
;;   https://www.emacswiki.org/emacs/VisualLineMode
;; ...But try visual-fill-column before those shenanigans.
;;   https://www.reddit.com/r/emacs/comments/9td154/is_there_a_way_to_get_better_word_wrapping_in/

;; Sentences end with a single space. This makes sentence navigation commands work better?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org892ee89
;; TODO: What does this do, exactly?
(setq sentence-end-double-space nil)

;; probably want this overridable
(setq-default fill-column 80)

;; Randomize. Shuffle. Chaos...
;; shuffle-lines-in-region from Sacha:
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#orgf6f5f9d
;; Shuffle/randomize from StackOverflow:
;;   - https://stackoverflow.com/questions/6172054/how-can-i-random-sort-lines-in-a-buffer
;;     - randomize-region.el https://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg00034.html


;;------------------------------------------------------------------------------
;; UTF-8
;;------------------------------------------------------------------------------
;; Prefer utf-8

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/International.html#International
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Output-Coding.html

;; May need a way of checking for smart quotes and em dashes and stuff when we don't want utf-8...
;; TODO: That probably best for a helper function/macro than forcing myself into ASCII at all times. Right?
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


;;------------------------------------------------------------------------------
;; Line numbers... everywhere.
;;------------------------------------------------------------------------------

;; Used to use linum-mode...
(when (< emacs-major-version 26)
;; (global-linum-mode 1)) ; show line numbers everywhere
  (error "Really old emacs. Enable linum in config?"))

;; But... See this:
;;   "Show line numbers. I used linum-mode before, but it caused severe performance
;;    issues on large files. Emacs 26 introduces display-line-numbers-mode, which
;;    has no perceivable performance impact even on very large files. I still have
;;    it disabled by default because I find it a bit distracting."
;;   - https://github.com/zzamboni/dot-emacs/blob/master/init.org

(when (>= emacs-major-version 26)
  (use-package display-line-numbers
;    :disabled
;    :ensure nil
    :defer nil
    :config
    (global-display-line-numbers-mode)))

;; TODO: Do we have more setting for it? Like default size/format of line number gutter?

;;---
;; Also column numbers
;;---
(column-number-mode t)


;;------------------------------------------------------------------------------
;; Smartscan for jumping to next instance of symbol-at-point
;;------------------------------------------------------------------------------
;; TODO: here in text, or elsewhere? configure-dev-env?
;; From http://pages.sachachua.com/.emacs.d/Sacha.html
;; Trial [2019-01-29]
;; M-n and M-p for next/previous symbol
;; TODO: M-n/M-p fucks with magit status buffer...
(use-package smartscan
  ;; This defer seems to make it not load?
  ;; TODO: see if e.g. `:init' instead of `:config' makes it any better w/ defer?
  ;; :defer t
  :config
  (global-smartscan-mode t))


;;------------------------------------------------------------------------------
;; TODO: these. Parenthesis, bells?
;;------------------------------------------------------------------------------

;; parenthesis?
;; (setq blink-matching-paren nil)
;; (show-paren-mode t)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'expression)

;; bell? (this doesn't work...)
;; (setq ring-bell-function 'ignore)
;; (setq visible-bell t)


;;------------------------------------------------------------------------------
;; Reading Mode (Documents, Novels, whatever)
;;------------------------------------------------------------------------------
;; Make stuff look a bit weird... but more readable maybe?
;; http://ergoemacs.org/emacs/emacs_novel_reading_mode.html
;; Trial [2019-01-30]
(defun xah-toggle-read-novel-mode ()
  "Setup current buffer to be suitable for reading long novel/article text.

• Line wrap at word boundaries.
• Set a right margin.
• line spacing is increased.
• variable width font is used.

Call again to toggle back.
URL `http://ergoemacs.org/emacs/emacs_novel_reading_mode.html'
Version 2017-02-27"
  (interactive)
  (if (null (get this-command 'state-on-p))
      (progn
        (set-window-margins nil 0 9)
        (variable-pitch-mode 1)
        (setq line-spacing 0.4)
        (setq word-wrap t)
        (put this-command 'state-on-p t))
    (progn
      (set-window-margins nil 0 0)
      (variable-pitch-mode 0)
      (setq line-spacing nil)
      (setq word-wrap nil)
      (put this-command 'state-on-p nil)))
  (redraw-frame (selected-frame)))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-text)
