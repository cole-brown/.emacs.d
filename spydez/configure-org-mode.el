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
;; Publishing
;;------------------------------------------------------------------------------
;; Maybe I'll need some of this in the future...
;; But I'm definitely not a horder.
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org763275b

;; ;; Timestamps and section numbers make my published files look more complicated
;; ;; than they are. Let's turn them off by default.
;; (setq org-export-with-section-numbers nil)
;; (setq org-html-include-timestamps nil)
;; (setq org-export-with-sub-superscripts nil)
;; (setq org-export-with-toc nil)
;; (setq org-html-toplevel-hlevel 2)
;; (setq org-export-htmlize-output-type 'css)
;;
;; ;; Sometimes I have broken or local links, and that's okay.
;; (setq org-export-with-broken-links t)
;;
;; ;; Don't wrap ASCII exports.
;; (setq org-ascii-text-width 10000)


;; UTF-8 checkboxes
;;
;; This snippet turns - [X] into ☑ and - [ ] into ☐, but leaves [-] alone.
;;
;; (setq org-html-checkbox-type 'unicode)
;; (setq org-html-checkbox-types
;;  '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
;;             (off . "<span class=\"task-todo\">&#x2610;</span>")
;;             (trans . "<span class=\"task-in-progress\">[-]</span>"))))


;;------------------------------------------------------------------------------
;; Org-Mode and Links
;;------------------------------------------------------------------------------
;; Uh... does this belong in org-mode or links?..

;; Quick Links
;; (setq org-link-abbrev-alist
;;       '(("google" . "http://www.google.com/search?q=")
;; 	("gmap" . "http://maps.google.com/maps?q=%s")))


;;------------------------------------------------------------------------------
;;  Attachments..?
;;------------------------------------------------------------------------------

;; Org lets you attach files to an Org file. Apparently.
;; (setq org-attach-store-link-p 'attached)
;; (setq org-attach-auto-tag nil)


;;------------------------------------------------------------------------------
;; Org-Mode HTTP Requests?!
;;------------------------------------------------------------------------------
;; ...this is getting ridiculuous.
;; https://github.com/zweifisch/ob-http
;; (use-package ob-http)


;;------------------------------------------------------------------------------
;; Diagrams and Graphics
;;------------------------------------------------------------------------------
;; It can be done. Ditaa and Graphviz or something.
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org1683357


;;------------------------------------------------------------------------------
;; Literate Programming
;;------------------------------------------------------------------------------
;; I do want to do this... but it would involve rewriting all my emacs files?
;; ...which I'm barely started on... but there's so much there already...

;; TODO: try this literate programming out
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org2f334cf


;;------------------------------------------------------------------------------
;; Invoices, Invoicing
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org0d10995
;; Don't do any clocking or consultant work, but I have in the past and
;; this would've been useful then.


;;------------------------------------------------------------------------------
;; Presentations
;;------------------------------------------------------------------------------
;; Absolutely no use for this org->HTML/JS slideshow right now but kinda neat.
;; https://github.com/hexmode/ox-reveal
;; (use-package ox-reveal :disabled t)


;;------------------------------------------------------------------------------
;; Reddit
;;------------------------------------------------------------------------------
;; TODO: org and reddit here, or in configure-fun? Probably fun...
;; TODO: throw my reddit users' JSON feeds into emacs.secrets.
;; TODO: make this more general so it can work on saved, upvotes, others.
;;   - Multiple funcs? (spydez/reddit-list/upvoted ...) (spydez/reddit-list/saved ...)
;;   - More generic? (spydez/reddit-list 'user-list-var ...)
;;   - All of the above?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org1807373
(defconst spydez/reddit-upvoted-json "todo")
(defun spydez/reddit-list-upvoted (date)
  (interactive (list (org-read-date)))
  (let ((threshold (org-read-date nil t (concat (substring date 0 (min (length date) 10)) " 0:00"))))
    (with-current-buffer (url-retrieve-synchronously spydez/reddit-upvoted-json)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((data (json-read))
             (items (assoc-default 'children (assoc-default 'data data)))
             (result
              (mapconcat
               (lambda (item)
                 (let* ((o (assoc-default 'data item))
                        (title (assoc-default 'title o))
                        (url (assoc-default 'url o))
                        (date (seconds-to-time (assoc-default 'created_utc o)))
                        (permalink (concat "https://reddit.com" (assoc-default 'permalink o)))
                        (num-comments (assoc-default 'num_comments o 'eq 0)))
                   (when (time-less-p threshold date)
                     (if (and (> num-comments 0) (not (string-match "reddit\\.com" url)))
                         (format "- [[%s][%s]] ([[%s][Reddit]])\n" url title permalink)
                       (format "- [[%s][%s]]\n" url title)))))
               items "")))
        (if (called-interactively-p 'any)
            (message "%s" result)
          result)))))

;; Usage: (spydez/reddit-list-upvoted "-mon")
;; Usage: (spydez/reddit-list-upvoted "[2019-01-31 Thu]")

;; TODO: more reddit stuff?
;; https://www.reddit.com/r/emacs/comments/7ro9xx/an_emacs_package_for_browsing_reddit_just/
;; https://github.com/ahungry/md4rd


;;------------------------------------------------------------------------------
;; Kitchen Sink
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-org-mode)
