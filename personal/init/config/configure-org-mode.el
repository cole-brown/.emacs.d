;; -*- mode: emacs-lisp; lexical-binding: t -*-


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
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                        ORG-MODE - The Thing Itself
;;  The Cthulu of Emacs. Required and will drive you insane trying to grok.
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------
(use-package org
  ;; specify ':ensure nil' so that use-package doesn’t try to install it,
  ;; and just loads and configures it.
  :ensure nil


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Init & General Settings
  ;;----------------------------------------------------------------------------
  :init ;; can do multiple forms until next keyword

  ;;-----------------
  ;; General Settings
  ;;-----------------

  ;; auto-timestamp when TODOs are turned to DONE state
  (setq org-log-done t)

  ;; let's see how default of 2 is before changing
  ;; (setq org-edit-src-content-indentation 0) ; # of spaces added to src indent

  ;; Don't know about soft word wrap. But we could do it in a hook for
  ;; org-mode if desired.
  ;; Wait... it's already on in org-mode. Well if we want to turn it off...
  ;; (visual-line-mode 1)

  ;; Well structured indentation. Freehand notes/text stay indented to
  ;; headline level.
  (setq org-startup-indented t)
  ;; Note 1: This changes how it /looks/, not how the raw text is formatted.
  ;; Note 2: This also hides leading stars for headlines.

  ;; Not 100% sold on these this time around... Keeping for now.
  ;; TODO: maybe disabling these... and make a new work.org at the same
  ;;   time? My work.org is getting annoying - I may have to find a better
  ;;   format for notes than whatever structure and monolithic file I'm
  ;;   trying to do now. Also could go to new todo sequence and logbook.
  ;;(setq org-hide-leading-stars t) ; make outline a bit cleaner
  ;;(setq org-odd-levels-only t)    ; make outline a bit cleaner

  ;; TODO sequence to pop up shortcuts to on running `C-c C-t' on a headline
  ;;   (n) - n key will be shortcut into this state
  ;;    @  - timestamp on enter
  ;;    !  - prompt for note w/ timestamp on enter
  ;;   /!  - prompt for note w/ timestamp on exit if none for next state
  ;;    |  - separates "need action" states from "finished" states
  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "STARTED(s!)"
                    "WAITING(w@/!)"
                    "|"
                    "DONE(d!)"
                    "CANCELLED(c@)")))

  ;; `Drawer' to log to. (Property/subheading thing). "LOGBOOK" is default if this
  ;; is set to `t'. This is for the place org-mode puts those todo timestamps,
  ;; state change notes, and user notes just under the headline.
  (setq org-log-into-drawer "LOGBOOK")

  ;; Put .org.txt into the mode list for org-mode
  (add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

  ;; Don't allow accidental edits of invisible regions in org files.
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  (setq org-catch-invisible-edits 'show-and-error)

  ;; Hide extra newlines between (sub)trees
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  (setq org-cycle-separator-lines 0)

  ;; [[link:tag]] becomes something else.
  ;; e.g.: [[google:test]] becomes link:
  ;;       'https://www.google.com/search?q=test' when clicked
  ;;   - '%s' in link-alist replaced with link's 'tag'
  ;;   - '%h' in link-alist replaced with link's (html encoded) 'tag'
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt4/
  (setq org-link-abbrev-alist
        '(("google" . "https://www.google.com/search?q=%h")
          ("map" . "https://maps.google.com/maps?q=%h")
          ))

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
  ;; TODO: I'd rather have that function where I can comment it and name it
  ;; and stuff but I need to figure out how to elisp for "You want a variable?
  ;; Well here's a function to run instead." You know... lambda things.

  ;;-----------------
  ;; /General Settings
  ;;-----------------


  ;;-----------------
  ;; Making It Pretty
  ;;-----------------

  ;; Set org-hid-emphasis-markers so that the markup indicators are not shown.
  ;; (so +strike+, /italic/, *bold* show font change, but hides the +/*)
  (setq org-hide-emphasis-markers t)

  ;; Check box visual upgrade.
  ;;   empty, indeterminate, marked: [ ], [-], [X] -> ☐, ▣, ☒
  ;;     aka 'unchecked', 'mixed checked/unchecked children', 'checked'
  ;; Could go in `:hook' section but I kinda just want it here in
  ;; the pretty section with the rest of the pretty things.
  ;; Trial: [2019-07-30 Tue]
  ;;   - Con: This doesn't update until point has moved off the line... Possibly
  ;;     interacting with my highlight row thing/mode?
  ;;   - Con: This only works on some checkbox lists and I've wasted hours
  ;;     trying to find out why. :(
  ;; Nice lil search for symbols: http://www.unicode.org/charts/
  (add-hook 'org-mode-hook
            (lambda ()
              "Beautify Org Checkbox Symbol"
              (setq prettify-symbols-alist
                    '(("[ ]" . "☐")
                      ;; other options:
                      ;;   - ☐ - 2610 ballot box
                      ;;     https://www.unicode.org/charts/PDF/U2600.pdf
                      ("[X]" . "☒")
                      ;; other options:
                      ;;   - ☒ - 2612 ballot box with X
                      ;;     https://www.unicode.org/charts/PDF/U2600.pdf
                      ;;   - ☑ - 2611 ballot box with check
                      ("[-]" . "▣")
                      ;; other options:
                      ;;   - ▣ - 25A3 white square containing black small square
                      ;;     https://www.unicode.org/charts/PDF/U25A0.pdf
                      ;;   - ❍ - ...idk, what other people used at reddit thread.
                      ;;   - ▽ - 25BD white down-pointing triangle
                      ;;   - ◎ - 25CE bullseye
                      ;;   - ☯ - 262F yin-yang
                      ))
              (prettify-symbols-mode 1)))

  ;; Show list markers with a middle dot instead of the original character.
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; TODO: try this out?
  ;;   org custom id helpers
  ;;   https://writequit.org/articles/emacs-org-mode-generate-ids.html#the-solution

  ;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/#beautifying-org-mode
  ;; More font stuff:
  ;;
  ;; ;; Choose a nice font for the document title and the section headings. The first
  ;; ;; one found in the system from the list below is used, and the same font is
  ;; ;; used for the different levels, in varying sizes.
  ;; (let* ((variable-tuple
  ;;         (cond ((x-list-fonts   "Source Sans Pro") '(:font   "Source Sans Pro"))
  ;;               ((x-list-fonts   "Lucida Grande")   '(:font   "Lucida Grande"))
  ;;               ((x-list-fonts   "Verdana")         '(:font   "Verdana"))
  ;;               ((x-family-fonts "Sans Serif")      '(:family "Sans Serif"))
  ;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;        (base-font-color (face-foreground 'default nil 'default))
  ;;        (headline       `(:inherit default :weight bold :foreground ,base-font-color)))
  ;;
  ;;   (custom-theme-set-faces
  ;;    'user
  ;;    `(org-level-8        ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-7        ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-6        ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-5        ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-4        ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;    `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 1.25 :underline nil))))))
  ;; Not sure I like it. Needs a lot of tweaking and not convinced about the height.
  ;; Does make the org-bullets nicer, though.

  ;; Can also set up a variable-pitch face to a proportional font I like...
  ;; (variable-pitch ((t (:family "Source Sans Pro" :height 160 :weight light))))
  ;; ;;(variable-pitch ((t (:family "Avenir Next" :height 160 :weight light))))
  ;;
  ;; Setting up the fixed-pitch face to be the same as my usual default face. My
  ;; current one is Inconsolata.
  ;; (fixed-pitch ((t (:family "Inconsolata"))))

  ;;-----------------
  ;; /Making It Pretty
  ;;-----------------

  ;;------------------
  ;; /':init' section.
  ;;------------------


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Keybinds
  ;;----------------------------------------------------------------------------
  ;; NOTE: THESE ARE ORG-MODE ONLY! And that is intended.

  :bind (:map org-mode-map
              ;; 'C-c <tab>' to show headings only (no top parent notes, no
              ;; children notes, just headings). Org-Mode had 'C-c <tab>' as
              ;; outline-show-children, which only shows direct children
              ;; headings, not all descendants' headings.
              ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
              ("C-c <tab>" . #'org-kill-note-or-show-branches))

  ;;-----------------
  ;; /':bind' section
  ;;-----------------


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Hooks
  ;;----------------------------------------------------------------------------
  ;;:hook

  ;; Thought I had two but I don't actually have any at all right now...

  ;;-----------------
  ;; /':hook' section
  ;;-----------------


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Config (Post-Load)
  ;;----------------------------------------------------------------------------
  ;;:config ;; can do multiple forms until next keyword

  ;; (anything?) *crickets*

  ;;------------------------------------
  ;; /':config' section - nothing after.
  ;;------------------------------------
  )


;; TODO: rest of these into the use-package as appropriate.

;;------------------------------------------------------------------------------
;; Making org-mode pretty
;;------------------------------------------------------------------------------

;; Display the titles with nice unicode bullets instead of the text ones.
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))


;;------------------------------------------------------------------------------
;; Org-Mode Functions & Misc
;;------------------------------------------------------------------------------

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
;;   "I often cut and paste subtrees. This makes it easier to cut something and
;; paste it elsewhere in the hierarchy."
;;   - Sacha, probably.
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

;; Enable org-indent-mode for a cleaner outline view.
;; https://orgmode.org/manual/Clean-view.html
(use-package org-indent
  :ensure nil
  :after org

  ;; By default, org-indent-mode produces an indicator "Ind" in the modeline.
  ;; Use diminish to hide it.
  :diminish

  ;;--- FAIL
  ;; Attempt #2984:
  ;; This changes indent boundary char, but then font-lock doesn't set its face.
  ;; :custom
  ;; (org-indent-boundary-char ?☥)
  ;; ;; options:
  ;; ;;   - ☥ - 2625 ankh
  ;; ;;     https://www.unicode.org/charts/PDF/U2600.pdf
  ;;
  ;; :config
  ;; ;; Change the face of `org-indent-boundary-char' to `org-indent'
  ;; (font-lock-add-keywords 'org-mode '(("☥" . 'org-indent)))
  ;; ;; This... does not get the indent ankh. Just any others existing in the file.
  ;;---

  ;;--- FAIL
  ;; Attempt #3371:
  ;;   Redef org-indent-initialize maybe?!
  ;; :config
  ;; ;; This does double the work on the org-indent-strings array, but meh.
  ;; (require 'cl)
  ;; (defun spydez/org-indent-initialize ()
  ;;   "Initialize the indentation strings with stars instead of spaces."
  ;;   (setq org-indent-strings (make-vector (1+ org-indent-max) nil))
  ;;   (aset org-indent-strings 0 nil)
  ;;   (loop for i from 1 to org-indent-max do
  ;;         (aset org-indent-strings i
  ;;               (org-add-props (make-string i ?x)
  ;;                   nil 'face 'org-indent))))
  ;; (advice-add 'org-indent-initialize :after #'org-indent-use-stars-for-strings)
  ;;---
  )

;; A function that reformats the current buffer by regenerating the text from
;; its internal parsed representation.
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/#reformatting-an-org-buffer
;; original name: zz/org-reformat-buffer
(defun spydez/org-reformat-buffer ()
  (interactive)
  (when (yes-or-no-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))


;;------------------------------------------------------------------------------
;; Global Org Keybinds
;;------------------------------------------------------------------------------
;; NOTE: THESE ARE GLOBAL BINDS. And that is intended.

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
;;   - Also has some UML stuff.

;; Fontify code block by their native mode? Yes please.
(setq org-src-fontify-natively t)

;;   "In principle this makes it so that indentation in src blocks works as in
;; their native mode, but in my experience it does not always work reliably. For
;; full proper indentation, always edit the code in a native buffer by pressing
;; "C-c '"."
;;   - https://zzamboni.org/post/my-emacs-configuration-with-commentary/#literate-programming-using-org-babel
(setq org-src-tab-acts-natively t)

;; Automatically runs org-babel-tangle upon saving any org-mode buffer, which
;; means the resulting files will be automatically kept up to date.
;; (org-mode . (lambda () (add-hook 'after-save-hook 'org-babel-tangle
;;                                  'run-at-end 'only-in-org-mode)))
;; Need to translate out of use-package format.

;; Also add hooks to measure and report how long the tangling took.
;; (org-babel-pre-tangle  . (lambda ()
;;                            (setq zz/pre-tangle-time (current-time))))
;; (org-babel-post-tangle . (lambda ()
;;                            (message "org-babel-tangle took %s"
;;                                            (format "%.2f seconds"
;;                                                    (float-time (time-since zz/pre-tangle-time))))))
;; Need to translate out of use-package format.


;;------------------------------------------------------------------------------
;; Org Journal
;;------------------------------------------------------------------------------
;; TODO: try out org-journal?
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/#keeping-a-journal
;; https://arenzana.org/2019/04/emacs-org-mode/#orgeaaf198
;; https://github.com/bastibe/org-journal
(use-package org-journal
  :after org

  ;;-----
  :custom
  ;;-----

  ;; Top dir for org-journals.
  ;; NOTE: Placeholder! Should get overridden in <secrets>/finalize-domain.el
  ;; or somewhere.
  ;; TODO: a default for spydez/dir/org-docs? And spydez/dir-doc-save-common?
  (org-journal-dir (spydez/path/to-dir spydez/dir/home "logbook"))

  ;; tack day name onto our format
  ;; TODO: put in date-and-time.el with other formats?
  (org-journal-date-format (concat spydez/datetime/format/yyyy-mm-dd ", %A"))
  ;; This can be a function if more is wanted. E.g. inserting new header text
  ;; into empty files.
  ;;  - https://github.com/bastibe/org-journal#journal-file-content

  ;; A year per file. Could do monthly if too big. Weekly and daily are also
  ;; options. Daily is the default.
  (org-journal-file-type 'yearly)

  ;; org-journal-file-format: Make it a bit more ISO-ish (yyyy-mm-dd).
  ;;   - default: yyyymmdd
  ;;   - better:  yyyy-mm-dd.org
  (org-journal-file-format (concat spydez/datetime/format/yyyy-mm-dd ".org"))

  ;; TODO: encryption?
  ;; https://github.com/bastibe/org-journal#encryption
  )


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
;; Org-Mode & Links
;;------------------------------------------------------------------------------

;; Quick Links
;; (setq org-link-abbrev-alist
;;       '(("google" . "http://www.google.com/search?q=")
;;         ("gmap" . "http://maps.google.com/maps?q=%s")))

;;   "The following elisp function will take a link around the current point as
;; recognised by org-bracket-link-regexp, so either [[Link][Description]] or
;; [[Link]], and replace it by Description in the first case or Link in the
;; second case."
;; From https://emacs.stackexchange.com/a/10714/11843
;; original name: afs/org-replace-link-by-link-description
(defun spydez/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))


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
