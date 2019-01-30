;; -*- emacs-lisp -*-


;; Sacha.org has tons, but I use, like, 1 percent of org mode right now...
;; So there won't actually be much here right now.
;; Like... heck. Probably more like 1 per cent mille.

;;------------------------------------------------------------------------------
;; Org Mode!
;;------------------------------------------------------------------------------
;; Don't need to use-package it or anything...

;; TODO: set these to spydez/foobar consts if defined
;; (setq org-directory "~/personal")
;; (setq org-default-notes-file "~/personal/organizer.org") ;; work.org file?

;; This makes it easier to add links from outside
;; (e.g. clipboard from browser, C-l, copy/paste).
;; Trial [2019-01-30]
(defun spydez/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
;; Do I want a key bound for this?
;;(global-set-key (kbd "<f6>") 'spydez/yank-more)


;; Editing the Org tree
;; I often cut and paste subtrees. This makes it easier to cut something and
;; paste it elsewhere in the hierarchy.
;; TODO: trial this
;; (with-eval-after-load 'org
;;      (bind-key "C-c k" 'org-cut-subtree org-mode-map)
;;      (setq org-yank-adjusted-subtrees t))


;;------------------------------------------------------------------------------
;; Org Keybinds
;;------------------------------------------------------------------------------
;; Don't think I have any right now but here's some if I do eventually:
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org44fc1f8


;;------------------------------------------------------------------------------
;; Org Navigation
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgf7563c2
;; https://stackoverflow.com/questions/15011703/is-there-an-emacs-org-mode-command-to-jump-to-an-org-heading


;;------------------------------------------------------------------------------
;; Org Modules
;;------------------------------------------------------------------------------
;; Org has a whole bunch of optional modules.
;; These are the ones I'm currently experimenting with.
;; Or, well, if I was, they would be here...
;; (setq org-modules '(org-bbdb
;;                     org-gnus
;;                     org-drill
;;                     org-info
;;                     org-jsinfo
;;                     org-habit
;;                     org-irc
;;                     org-mouse
;;                     org-protocol
;;                     org-annotate-file
;;                     org-eval
;;                     org-expiry
;;                     org-interactive-query
;;                     org-man
;;                     org-collector
;;                     org-panel
;;                     org-screen
;;                     org-toc))
;; (eval-after-load 'org
;;   '(org-load-modules-maybe t))
;; ;; Prepare stuff for org-export-backends
;; (setq org-export-backends '(org latex icalendar html ascii))


;;------------------------------------------------------------------------------
;; Calendar
;;------------------------------------------------------------------------------
;; TODO: some way to use this or something like it just to check for upcoming
;; (today/this week/whatever) gcal appointments?

;; Synchronizing with Google Calendar
;; 
;; (defun my/org-gcal-notify (title mes)
;;   (message "%s - %s" title mes))
;; (use-package org-gcal
;;   :load-path "~/elisp/org-gcal.el"
;;   :init (fset 'org-gcal-notify 'my/org-gcal-notify))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-org-mode)
