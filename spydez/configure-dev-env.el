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
;;(add-to-list 'hippie-expand-try-functions-list yas-hippie-try-expand)
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
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-dev-env)
