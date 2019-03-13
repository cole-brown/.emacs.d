;; -*- emacs-lisp -*-


;; Sacha.org has tons, but I use, like, 1 percent of org mode right now...
;; So there won't actually be much here right now.
;; Like... heck. Probably more like 1 per cent mille.


;;------------------------------------------------------------------------------
;; Notes
;;------------------------------------------------------------------------------

;; Way too much info about org-mode.
;;   - Possibly very outdated...
;;   - http://doc.norang.ca/org-mode.html

;; Org-Mode reference card pdf
;; https://orgmode.org/orgcard.pdf

;; TODO: Check out this and see if I want to use any of it?:
;;   https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; My instinct is no... not til I use org-mode a lot more.


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
;;TODO: (use-package org ??
(setq org-log-done t           ; auto-timestamp when TODOs are turned to DONE state

      org-src-fontify-natively t ; fontify code block by their native mode?

      ;; let's see how default of 2 is before changing
      ;; org-edit-src-content-indentation 0 ; # of spaces added to src indent

      ;; Don't know about soft word wrap. But we could do it in a hook for
      ;; org-mode if desired. (visual-line-mode 1)

      ;; Well structured indentation. Freehand notes/text stay indented to
      ;; headline level.
      org-startup-indented t
      ;; Note 1: This changes how it /looks/, not how the raw text is formatted.
      ;; Note 2: This also hides leading stars for headlines.

      ;; Not 100% sold on these this time around... Keeping for now.
      ;; TODO: maybe disabling these... and make a new work.org at the same
      ;;   time? My work.org is getting annoying - I may have to find a better
      ;;   format for notes than whatever structure and monolithic file I'm
      ;;   trying to do now. Also could go to new todo sequence and logbook.
      ;;org-hide-leading-stars t ; make outline a bit cleaner
      ;;org-odd-levels-only t    ; make outline a bit cleaner
      )

;; TODO sequence to pop up shortcuts to on running `C-c C-t' on a headline
;;   (n) - n key will be shortcut into this state
;;    @  - timestamp on enter
;;    !  - prompt for note w/ timestamp on enter
;;   /!  - prompt for note w/ timestamp on exit if none for next state
;;    |  - separates "need action" states from "finished" states
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; `Drawer' to log to. (Property/subheading thing).
;; "LOGBOOK" is default if this is set to `t'.
(setq org-log-into-drawer "LOGBOOK")

;; TODO: an org-mode use-package for grouping all these settings?


;;------------------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------------------

;; TODO: not sure if we need to hook yas and org together anymore.
;;  - make a test org-mode snippet and see if it Just Works (tm)?
;;
;; (defun spydez/org-mode-hook ()
;;   ;; yasnippet
;;   (make-variable-buffer-local 'yas/trigger-key)
;;   (setq yas/trigger-key [tab])
;;   (define-key yas/keymap [tab] 'yas/next-field-group))
;; (add-hook 'org-mode-hook 'spydez/org-mode-hook)

;;   "Enable Speed Keys, which allows quick single-key commands when the cursor
;; is placed on a heading. Usually the cursor needs to be at the beginning of a
;; headline line, but defining it with this function makes them active on any of
;; the asterisks at the beginning of the line (useful with the font highlighting
;; I use, as all but the last asterisk are sometimes not visible)."
;;   https://zzamboni.org/post/my-emacs-configuration-with-commentary/
;; Manual:
;;   https://orgmode.org/manual/Speed-keys.html
(setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
;; Not exactly a hook, but kinda...


;;------------------------------------------------------------------------------
;; Org Mode!
;;------------------------------------------------------------------------------
;; Don't need to use-package it or anything...

;; TODO: set these to spydez/{dir,file}/org/something consts if defined
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


;; Strike through DONE headlines?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orga092ba0
;; TODO: try it out? Hopefully with a zenburn face?
;;
;; I wanted a quick way to visually distinguish DONE tasks from tasks I still need to do. This handy snippet from the Emacs Org-mode mailing list does the trick by striking through the headlines for DONE tasks.
;;
;; (setq org-fontify-done-headline t)
;; (custom-set-faces
;;  '(org-done ((t (:foreground "PaleGreen"
;;                  :weight normal
;;                  :strike-through t))))
;;  '(org-headline-done
;;             ((((class color) (min-colors 16) (background dark))
;;                (:foreground "LightSalmon" :strike-through t)))))


;; send to bottom of list?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org28359a7


;; encryption
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org712e999
;; org-crypt

;; By default, org-indent produces an indicator "Ind" in the modeline. We use diminish to hide it.
(use-package org-indent
  :ensure nil
  :after org
  :diminish)


;;------------------------------------------------------------------------------
;; Org Keybinds
;;------------------------------------------------------------------------------

;; Bind a few keys for globally useful org stuff.
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(bind-keys*
  ;; Set up C-c l to store a link to the current org object, in counterpart to
  ;; the default C-c C-l to insert a link.
  ("C-c l" . org-store-link)

  ;; Set up C-c a to call up agenda mode.
  ("C-c a" . org-agenda)
  ;; TODO: try using org agenda mode maybe

  ;; I have read the help for `org-capture' and have one question...
  ;; WTF is org-capture?
  ("C-c c" . org-capture)
  ;; TODO: ...I don't know - comment this out?
  )

;; Some more keybinds to consider here:
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org44fc1f8


;;------------------------------------------------------------------------------
;; Org Navigation
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgf7563c2
;; https://stackoverflow.com/questions/15011703/is-there-an-emacs-org-mode-command-to-jump-to-an-org-heading


;;------------------------------------------------------------------------------
;; Literate Programming
;;------------------------------------------------------------------------------
;; I do want to do this... but it would involve rewriting all my emacs files?
;; ...which I'm barely started on... but there's so much there already...

;; TODO: try this literate programming out
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org2f334cf
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/#literate-programming-using-org-babel


;;------------------------------------------------------------------------------
;; Org Journal
;;------------------------------------------------------------------------------
;; TODO: try out org-journal? this guy hasn't gotten around to it either.
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/#keeping-a-journal
;; (use-package org-journal
;;   :after org
;;   :custom
;;   (org-journal-dir "~/Documents/logbook"))


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
;;   - wish it set [-]...
;; TODO: make that wish come true.
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

;; See configure-web.el for what I'm using now: non-org-mode package `restclient'


;;------------------------------------------------------------------------------
;; Diagrams and Graphics
;;------------------------------------------------------------------------------
;; It can be done. Ditaa and Graphviz or something.
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org1683357


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
;; https://github.com/yjwen/org-reveal
;; (use-package ox-reveal :disabled t)
;; Uh... wait. Is it org-reveal or ox-reveal?


;;------------------------------------------------------------------------------
;; Exporting
;;------------------------------------------------------------------------------
;; Many many ways to export. html, latex, markdown...
;; Some here:
;;   https://zzamboni.org/post/my-emacs-configuration-with-commentary/#various-exporters
;;   - ox-md:  Markdown
;;   - ox-gfm: GitHub Flavored Markdown
;;   - org-jira/ox-jira: Jira markup
;;   - ox-confluence:    Confluence markup
;;   - ox-texinfo:
;;   - ox-latex
;;   - ...


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
