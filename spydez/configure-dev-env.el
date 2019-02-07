;; -*- emacs-lisp -*-

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces


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

;; TODO: tabs as spaces setting?

;; tab width
(setq-default tab-width spydez/dev-env/tab/normal)

;; TODO: is this global or per-mode in old .emacs?
;; New lines are always indented
;;(global-set-key (kbd "RET") 'newline-and-indent)


;;------------------------------------------------------------------------------
;; Parenthesis
;;------------------------------------------------------------------------------
;; TODO: disable electric-pair-mode if needed.

;; TODO: used mic-paren.el in old .emacs.
;;   Try smartparen and/or rainbow delimiters.
;;   Decided which I like.

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
