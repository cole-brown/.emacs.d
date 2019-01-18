;; -*- emacs-lisp -*-

;;------------------------------------------------------------------------------
;; General text settings, global settings, etc
;;------------------------------------------------------------------------------

;; Sentences end with a single space. This makes sentence navigation commands work better?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org892ee89
;; TODO: What does this do, exactly?
(setq sentence-end-double-space nil)

;; probably want this overridable
(setq-default fill-column 80)


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
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-text)
