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
  :demand t


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Init
  ;;----------------------------------------------------------------------------
  :init ;; can do multiple forms until next keyword

  ;; Put .org.txt into the mode list for org-mode
  (add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

  ;; Check box visual upgrade.
  ;;   empty, indeterminate, marked: [ ], [-], [X] -> ☐, ▣, ☒
  ;;     aka 'unchecked', 'mixed checked/unchecked children', 'checked'
  ;; Trial: [2019-07-30 Tue]
  ;;   - Con: This doesn't update until point has moved off the line... Possibly
  ;;     interacting with my highlight row thing/mode?
  ;; Nice lil search for symbols: http://www.unicode.org/charts/
  (defun spydez/hook/org-mode/checkboxes ()
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
    (prettify-symbols-mode 1))

  ;; Show list markers with a middle dot instead of the
  ;; original character.
  (defun spydez/hook/org-mode/simple-list ()
    "Nice up simple lists - replacing hypen with a unicode middle dot."
    (font-lock-add-keywords
     nil ;; 'org-mode - some org-mode stuff (e.g. org-journal) is a derived
         ;; major mode and thus needed either more than just `org-mode', or to
         ;; be `nil' and put in the org-mode hook.
     '(("^ *\\([-]\\) "
        (0 (prog1 () (compose-region (match-beginning 1)
                                     (match-end 1) "•"))))))
    )

  ;;   "Enable Speed Keys, which allows quick single-key commands when the
  ;; cursor is placed on a heading. Usually the cursor needs to be at the
  ;; beginning of a headline line, but defining it with this function makes them
  ;; active on any of the asterisks at the beginning of the line (useful with
  ;; the font highlighting I use, as all but the last asterisk are sometimes not
  ;; visible)."
  ;;   https://zzamboni.org/post/my-emacs-configuration-with-commentary/
  ;; Manual:
  ;;   https://orgmode.org/manual/Speed-keys.html
  (defun spydez/custom/org-mode/speed-commands-p ()
    "Allow speed keys when at any headline *, not just beginning of line."
    (and (looking-at org-outline-regexp) (looking-back "^\**")))

  ;; This may get away from "Org-Mode" and into "Random Shit"...
  (defhydra spydez/hydra/org-mode (:color blue)
    "Org-Mode"
    ("s" (spydez/signature/insert spydez/signature/short-pre) "insert sig")
    ("-" (spydez/signature/insert spydez/signature/name-post) "insert -name")
    ("t" (spydez/signature/insert spydez/signature/todo) "insert todo")

    ("n" org-journal-new-entry "new journal entry")
    ("v" (funcall org-journal-find-file
                  (org-journal-get-entry-path)) "visit journal")
    )

  ;;------------------
  ;; /':init' section.
  ;;------------------


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Hooks
  ;;----------------------------------------------------------------------------
  :hook
  ((org-mode . spydez/hook/org-mode/checkboxes)
   (org-mode . spydez/hook/org-mode/simple-list))
  ;;-----------------
  ;; /':hook' section
  ;;-----------------


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Custom Settings
  ;;----------------------------------------------------------------------------
  :custom

  (org-log-done t "auto-timestamp when TODOs are turned to DONE state")

  ;; Leave headings to figure out if they want a newline or not.
  ;; But change plain-list-item to not try to be clever - it's annoying more
  ;; than it's helpful.
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . nil))
                              "No auto newline in plain list items.")

  ;; Well structured indentation. Freehand notes/text stay indented to
  ;; headline level.
  (org-startup-indented t "I'm starting to like this. Leave globally on.")
  ;; Note 1: This changes how it /looks/, not how the raw text is formatted.
  ;; Note 2: This also hides leading stars for headlines.

  ;; TODO sequence to pop up shortcuts to on running `C-c C-t' on a headline
  ;;   (n) - n key will be shortcut into this state
  ;;    @  - timestamp on enter
  ;;    !  - prompt for note w/ timestamp on enter
  ;;   /!  - prompt for note w/ timestamp on exit if none for next state
  ;;    |  - separates "need action" states from "finished" states
  (org-todo-keywords
   '((sequence "TODO(t)"
               "STARTED(s!)"
               "WAITING(w@/!)"
               "|"
               "DONE(d!)"
               "CANCELLED(c@)"))
   "Custom sequence of keywords & actions.")

  ;; `Drawer' to log to. (Property/subheading thing). "LOGBOOK" is default if
  ;; this is set to `t'. This is for the place org-mode puts those todo
  ;; timestamps, state change notes, and user notes just under the headline.
  (org-log-into-drawer "LOGBOOK"
                       "See my comment or emacs help for more details.")

  ;; Don't allow accidental edits of invisible regions in org files.
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  (org-catch-invisible-edits 'show-and-error
                             "Never allow edits of invisible regions.")

  ;; Hide extra newlines between (sub)trees.
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  ;; Really useful because I tend to like the bonus whitespace for visually
  ;; separating one tree from the next...
  (org-cycle-separator-lines 0
                             "Hide extra newlines between (sub)trees")

  ;; [[link:tag]] becomes something else.
  ;; e.g.: [[google:test]] becomes link:
  ;;       'https://www.google.com/search?q=test' when clicked
  ;;   - '%s' in link-alist replaced with link's 'tag'
  ;;   - '%h' in link-alist replaced with link's (html encoded) 'tag'
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt4/
  (org-link-abbrev-alist
   '(("google" . "https://www.google.com/search?q=%h")
     ("map" . "https://maps.google.com/maps?q=%h"))
   "Shortcuts for links. Translates [[link:tag]] (and [[link:tag][desc]]).")

  ;; Enable Speed Keys as per my speed-commands predicate function.
  (org-use-speed-commands
   #'spydez/custom/org-mode/speed-commands-p
   "Allow speed keys when at any headline *, not just beginning of line.")

  ;; Set org-hid-emphasis-markers so that the markup indicators are not shown.
  ;; (so +strike+, /italic/, *bold* show font change, but hides the +/*)
  (org-hide-emphasis-markers t
   "Hide the ASCII font change markers, show the font change.")

  ;; Org Source Code Blocks: Fontify by their native mode.
  (org-src-fontify-natively t
   "Fontify code block by their native mode? Yes please.")

  ;; Org Source Code Blocks: Tab behavior.
  ;;   "In principle this makes it so that indentation in src blocks works as in
  ;; their native mode, but in my experience it does not always work reliably.
  ;; For full proper indentation, always edit the code in a native buffer by
  ;; pressing "C-c '"."
  ;;   - https://zzamboni.org/post/my-emacs-configuration-with-commentary/#literate-programming-using-org-babel
  (org-src-tab-acts-natively t
   "Tab (tries to) act like the prog-mode in code blocks.")

  ;;---
  ;; "Not Sure About These Yet" sub-section of ':custom' section.
  ;;---

  ;; let's see how default of 2 is before changing
  ;; (org-edit-src-content-indentation 0 "# of spaces added to src indent")

  ;; NOTE: These are overshadowed by `org-startup-indented', and
  ;; org-startup-indented does a much nicer, cleaner job, I think.
  ;; ;; Not 100% sold on these this time around... Keeping for now.
  ;; ;;(setq org-hide-leading-stars t) ; make outline a bit cleaner
  ;; ;;(setq org-odd-levels-only t)    ; make outline a bit cleaner

  ;; TODO: try this out?
  ;;   org custom id helpers
  ;;   https://writequit.org/articles/emacs-org-mode-generate-ids.html#the-solution

  ;;------------------
  ;; /':custom' section.
  ;;------------------


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Keybinds
  ;;----------------------------------------------------------------------------

  ;; `:bind*' to force some bindings (overrides minor mode binds).
  :bind*
  (;;---
   ;; :map global
   ;;---
   ;; org-mode binds `org-cycle-agenda-files' to C-, and also C-', which seems
   ;; stupid (especially with Dvorak). Stealing C-, back for myself.
   ("C-," . spydez/hydra/org-mode/body)


   ;;---
   :map org-mode-map
   ;;---
   ;; NOTE: THESE ARE ORG-MODE ONLY! And that is intended.

   ;; 'C-c <tab>' to show headings only (no top parent notes, no
   ;; children notes, just headings). Org-Mode had 'C-c <tab>' as
   ;; outline-show-children, which only shows direct children
   ;; headings, not all descendants' headings.
   ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
   ("C-c <tab>" . #'org-kill-note-or-show-branches))

  ;;-----------------
  ;; /':bind*' section
  ;;-----------------


  ;;----------------------------------------------------------------------------
  ;; Org-Mode Config (Post-Load)
  ;;----------------------------------------------------------------------------
  :config

  ;; There is a `:custom-face' section of use-packge, but I don't think I can do
  ;; the zenburn feature check or the `zenburn-with-color-variables' call.
  ;;
  ;; Change some headline colors.
  (require 'with)
  (with-feature 'zenburn-theme
    (zenburn-with-color-variables
      ;; I don't like green in these as it confuses me with the "DONE" etc
      ;; flags, and I apparently like having my level 2 headings set to DONE and
      ;; it was exactly the same color as far as my eyes could tell.
      (set-face-foreground 'org-level-1 zenburn-orange)
      (set-face-foreground 'org-level-2 zenburn-blue-1)
      (set-face-foreground 'org-level-3 zenburn-yellow-2)
      (set-face-foreground 'org-level-4 zenburn-cyan)
      (set-face-foreground 'org-level-5 zenburn-red-4)
      (set-face-foreground 'org-level-6 zenburn-blue-4)
      ;; these get a bit weird but we're really super deeper than I've been
      (set-face-foreground 'org-level-7 zenburn-cyan)
      (set-face-foreground 'org-level-8 zenburn-yellow-2)
      ;; and after 8 it repeats from 1
      ))

  ;;---
  ;; "Not Sure About These Yet" sub-section of ':config' section.
  ;;---

  ;; Don't know about soft word wrap. But we could do it in a hook for
  ;; org-mode if desired.
  ;; Wait... it's already on in org-mode. Well if we want to turn it off...
  ;; (visual-line-mode 1)

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

  ;;------------------------------------
  ;; /':config' section - nothing after.
  ;;------------------------------------
  )


;;------------------------------------------------------------------------------
;; Org-Mode Headline Bullets: (Making Org-Mode Pretty)
;;------------------------------------------------------------------------------

;; Display the titles with nice unicode bullets instead of the text ones.
(use-package org-bullets
  :after org
  :demand t
  :hook (org-mode . org-bullets-mode)

  :custom
  (org-bullets-bullet-list
   ;; default: '("◉" "○" "✸" "✿")
   '("◆" "♦" "●" "○")
    ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
    ;; ► • ★ ▸
    )
  )


;;------------------------------------------------------------------------------
;; Org-Mode Indent: (Making Org-Mode Pretty)
;;------------------------------------------------------------------------------

;; Enable org-indent-mode for a cleaner outline view.
;; https://orgmode.org/manual/Clean-view.html
(use-package org-indent
  :ensure nil
  :demand t
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
  ;; (advice-add 'org-indent-initialize :after #'spydez/org-indent-initialize)
  ;;---

  ;;--- FAIL
  ;; Attempt #3371-a:
  ;; Too much indent?
  ;;   - No change. -_-
  ;; :config
  ;; ;; This does double the work on the org-indent-strings array, but meh.
  ;; (require 'cl)
  ;; (defun spydez/org-indent-initialize ()
  ;;   "Initialize the indentation strings with stars instead of spaces."
  ;;   (setq org-indent-strings (make-vector (1+ org-indent-max) nil))
  ;;   (aset org-indent-strings 0 nil)
  ;;   (loop for i from 1 to org-indent-max do
  ;;         (aset org-indent-strings i
  ;;               (org-add-props (make-string (+ 3 i) ?x)
  ;;                   nil 'face 'org-indent))))
  ;; (advice-add 'org-indent-initialize :after #'spydez/org-indent-initialize)
  ;;---

  ;;--- FAIL
  ;; Attempt #3371-a-4:
  ;; Too much indent and `:init' instead of `:config'?
  ;; :init
  ;; ;; This does double the work on the org-indent-strings array, but meh.
  ;; (require 'cl)
  ;; (defun spydez/org-indent-initialize ()
  ;;   "Initialize the indentation strings with stars instead of spaces."
  ;;   (setq org-indent-strings (make-vector (1+ org-indent-max) nil))
  ;;   (aset org-indent-strings 0 nil)
  ;;   (loop for i from 1 to org-indent-max do
  ;;         (aset org-indent-strings i
  ;;               (org-add-props (make-string (+ 3 i) ?x)
  ;;                   nil 'face 'org-indent))))
  ;; (advice-add 'org-indent-initialize :after #'spydez/org-indent-initialize)
  ;;---

  ;;--- SUCCESS
  ;; Attempt #4188:
  ;; 1) Actually look at source code, see how out of date google answers are.
  ;; 2) Cry.
  ;; 3) Wonder who the fuck decided the change to not set any face
  ;;    on `org-indent-boundary-char' was a smart change.
  :config
  ;; This does double the work on the org-indent-strings array, but meh.
  (require 'cl-lib)
  (defun spydez/advice/org-indent/prefix-munger ()
    "Initialize the indentation strings so the motherfucking
`org-indent-boundary-char' is set with a proper face you god damn
savages."
    (setq org-indent--text-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (dotimes (n org-indent--deepest-level)
      (let ((indentation (if (<= n 1) 0
                           (* (1- org-indent-indentation-per-level)
                              (1- n)))))
        ;; Text line prefixes.
        (aset org-indent--text-line-prefixes
              n
              ;; spydez change: concat org-indent-boundary-char inside
              ;;   of org-add-props, not outside.
              (org-add-props (concat
                              (make-string (+ n indentation) ?\s)
                              (and (> n 0)
                                   (char-to-string org-indent-boundary-char)))
                  nil 'face 'org-indent)
              ))))
  (advice-add 'org-indent--compute-prefixes
              :after #'spydez/advice/org-indent/prefix-munger)
  ;; Thank the fuckin' Lord of Sarcasm and Brick Walls. Fuck.
  ;;---

  ;; TODO: Attempt #Next: Try changing org-indent-boundary-char to be a function
  ;; that maybe returns a properly faced string of length 1?

  ;; TODO-DOC: Document these failure shenanigans in an issue.org to get 'em outta here?
  )



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

;; Automatically runs org-babel-tangle upon saving any org-mode buffer, which
;; means the resulting files will be automatically kept up to date.
;; (org-mode . (lambda () (add-hook 'after-save-hook 'org-babel-tangle
;;                                  'run-at-end 'only-in-org-mode)))

;; Also add hooks to measure and report how long the tangling took.
;; (org-babel-pre-tangle  . (lambda ()
;;                            (setq zz/pre-tangle-time (current-time))))
;; (org-babel-post-tangle . (lambda ()
;;                            (message "org-babel-tangle took %s"
;;                                            (format "%.2f seconds"
;;                                                    (float-time (time-since zz/pre-tangle-time))))))


;;------------------------------------------------------------------------------
;; Org Journal
;;------------------------------------------------------------------------------
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/#keeping-a-journal
;; https://arenzana.org/2019/04/emacs-org-mode/#orgeaaf198
;; https://github.com/bastibe/org-journal
;; Trial: [2019-08-07]
;;   - [2019-08-19]: Working out pretty well for vague summary/todo of
;;     day's tasks.
(use-package org-journal
  :after org
  :demand t


  ;; ;;-----
  ;; :bind*
  ;; ;;-----
  ;; ;; Force some bindings. ':bind*' overrides minor mode binds.
  ;; ("C-," . spydez/hydra/journal/body)

  ;; ;; Note: C-c C-j is the default for `org-journal-new-entry', but that seems
  ;; ;; popular. Org-mode and python-mode both bind it to something else.
  ;; ;; Trying this binding/hydra out instead.


  ;;-----
  :custom
  ;;-----

  ;; Top dir for org-journals.
  ;; NOTE: Placeholder! Should get overridden in <secrets>/finalize-domain.el
  ;; or somewhere.
  ;; TODO: a default for spydez/dir/org-docs? And spydez/dir/doc-save-common?
  (org-journal-dir (spydez/path/to-dir spydez/dir/home "logbook"))

  ;; Tack day name onto our format for the org-journal headline.
  ;; Could put in date-and-time.el with other formats, but I don't think it's a
  ;; common enough thing for that right now...
  (org-journal-date-format (concat spydez/datetime/format/yyyy-mm-dd ", %A"))
  ;; This can be a function if more is wanted. E.g. inserting new header text
  ;; into empty files.
  ;;  - https://github.com/bastibe/org-journal#journal-file-content

  ;; A year per file. Could do monthly if too big. Weekly and daily are also
  ;; options. Daily is the default.
  (org-journal-file-type 'yearly)

  ;; org-journal-file-format: Make it a bit more ISO-ish (yyyy-mm-dd).
  ;;   - default:   yyyymmdd
  ;;   - better:    yyyy-mm-dd.org
  ;; But those both are difficult to switch to when various other buffers open,
  ;; so we'll go to this:
  ;;   - betterer:  yyyy-mm-dd.journal.org
  (org-journal-file-format (concat spydez/datetime/format/yyyy-mm-dd
                                   ".journal.org"))


  ;;-----
  :config
  ;;-----
  ;; move cache to no-littering's /var
  (setq org-journal-cache-file
        (no-littering-expand-var-file-name "org-journal.cache"))

  ;; TODO: encryption?
  ;; https://github.com/bastibe/org-journal#encryption
  )


;;------------------------------------------------------------------------------
;; Org Modules
;;------------------------------------------------------------------------------
;; Org has a whole bunch of optional modules.
;; These are the ones I'm currently experimenting with.
;; Or, well, if I was, they would be here...
;; Or, well, if I was, I would move this into the 'use-package org-mode' and
;; they would be there.
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
;; Publishing / Exporting
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
;; TODO: If I start exporting to HTML, will my pretty unicode checkboxes
;; persist, or do I need to do something similar to this? If something similar,
;; set one up for translating '[-]' to the boxed-box thing.
;;
;; (setq org-html-checkbox-type 'unicode)
;; (setq org-html-checkbox-types
;;  '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
;;             (off . "<span class=\"task-todo\">&#x2610;</span>")
;;             (trans . "<span class=\"task-in-progress\">[-]</span>"))))


;;------------------------------------------------------------------------------
;; Org-Mode & Links
;;------------------------------------------------------------------------------

;;   "The following elisp function will take a link around the current point as
;; recognised by org-bracket-link-regexp, so either [[Link][Description]] or
;; [[Link]], and replace it by Description in the first case or Link in the
;; second case."
;; From https://emacs.stackexchange.com/a/10714/11843
;; original name: afs/org-replace-link-by-link-description
(defun spydez/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address.
AKA 'unlink this plz'."
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
;; TODOs
;;------------------------------------------------------------------------------

;; Org-Bullets works well.
;; The prettifying of checkboxes, and the prettifying/bulleting of simple lists
;; ...doesn't work well. Steal from Org-Bullets mode? Patch and send upstream
;; if hunky dory?
;;   - [2019-08-19] - Think I made the pretty shenanigans reliable... I hope.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-org-mode)
