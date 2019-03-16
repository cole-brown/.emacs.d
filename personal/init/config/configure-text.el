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

;; /normal is usually around 80 can do /long for usually around 100...
(setq-default fill-column spydez/dev-env/fill-column/normal)

;; Randomize. Shuffle. Chaos...
;; shuffle-lines-in-region from Sacha:
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#orgf6f5f9d
;; Shuffle/randomize from StackOverflow:
;;   - https://stackoverflow.com/questions/6172054/how-can-i-random-sort-lines-in-a-buffer
;;     - randomize-region.el https://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg00034.html

;; NOTE: Parenthesis settings are in dev-env.


;;------------------------------------------------------------------------------
;; Spellchecking
;;------------------------------------------------------------------------------

;; - ispell is ancient.
;; - aspell's Windows version is so old emacs (26.1+) no longer supports it.
;; - So... hunspell!

;; Note: `hunspell -D' is suggested to be run in command line to see what
;; dictionaries hunspell knows about. That, however, just gets me stuck
;; in hunspell's executable waiting for input if run from Git Bash.
;; You gotta use cmd.exe shell.

;; Helpful or confusing links.
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; https://github.com/manugoyal/.emacs.d

;; TODO: on-off toggle would be nice, probably...
;;   - maybe in that main hydra or function keys thing I've been thinking about
;; TODO: personal dict for adding stuff like "hunspell" and "dict"
;; TODO: what are main keybinds for spellchecking?
;;   Can you do it without a popup dialog and a mouse?

;; TODO: some check with use-tool instead?
(setq spydez/file/hunspell (executable-find "hunspell"))
(if spydez/file/hunspell
    ;; Result from executable-find: hunspell is installed
    ;; So set up flyspell with that.
    (use-package flyspell
      ;; :delight ;; Not sure whether I want to kill its modeline or not
      :init
      (progn
        (setq ispell-program-name (executable-find "hunspell"))

        ;; "en_US" is key to lookup in `ispell-local-dictionary-alist`.
        ;; Please note it will be passed as default value to hunspell CLI `-d` option
        ;; if you don't manually setup `-d` in `ispell-local-dictionary-alist`
        (setq ispell-dictionary "en_US")

        ;; TODO: get this from use-tool?.. Or do I need this at all?
        ;; Don't think it's needed.
        ;; (setq spydez/dir/hunspell-data "C:/bin/hunspell-1.3.2-3-w32-bin/share/hunspell")

        ;; Really hard to figure out if this is needed at all besides in xml mode(s)...
        (setq ispell-local-dictionary-alist
              '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

        ;; allow some hunspell-only stuff
        (setq ispell-really-hunspell t)

        ;; Verbosity? These are t by default but not showing anything in *Messages* right now...
        ;; (setq flyspell-issue-message-flag nil
        ;;       flyspell-issue-welcome-flag nil)

        ;; Personal Dictionary:
        ;;   Looks like maybe can pass personal dict in as "-p" with "-d" list:
        ;;     e.g. ("-d" "en_US" "-p" "path/to/personal.en")
        ;;   Or maybe this is better:
        ;;     TODO: a personal dictionary: C-h v ispell-personal-dictionary
        )

      :hook ;; only one list
      ;; TODO: shitty perf on an org-mode file with longish lines
      ;; (or just lots of 'misspelled' words?)
      ((prog-mode . flyspell-prog-mode)
       (text-mode . flyspell-mode))

      ;; If we want global flyspell:
      ;; :config
      ;; (flyspell-mode 1)
      )
  ;; else: no result from executable-find - warn and don't set up.
  (spydez/warning/message nil nil "No backend tool for flyspell. Checked for `hunspell': %s"
                          spydez/file/hunspell)
  )


;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------
;; get a markdown mode if need to work with markdown more.


;;------------------------------------------------------------------------------
;; UTF-8
;;------------------------------------------------------------------------------
;; Prefer utf-8

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/International.html#International
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Output-Coding.html

;; May need a way of checking for smart quotes and em dashes and stuff when we don't want utf-8...
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; There are many many more ways of asking Emacs for utf-8 in many many more
;; places... Like, many. Small sample (from http://emacs-bootstrap.com/ ):
;; ;; UTF-8 please
;; (set-charset-priority 'unicode)
;; (setq locale-coding-system   'utf-8)   ; pretty
;; (set-terminal-coding-system  'utf-8)   ; pretty
;; (set-keyboard-coding-system  'utf-8)   ; pretty
;; (set-selection-coding-system 'utf-8)   ; please
;; (prefer-coding-system        'utf-8)   ; with sugar on top
;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Sticking with just the minimum until something unicode related
;; is wrong for me.


;;------------------------------------------------------------------------------
;; Emoji / Icons
;;------------------------------------------------------------------------------

;; all-the-icons package for some nice icons.
;; However, it has some manual setup the first time:
;;---
;; Install all-the-icons' fonts:
;; https://github.com/domtronn/all-the-icons.el#installing-fonts
;;
;; In order for the icons to work it is very important that you install the
;; Resource Fonts included in this package, they are available in the fonts
;; directory. You can also install the latest fonts for this package in the
;; (guessed?) based on the OS by calling the following function:
;;
;;   M-x all-the-icons-install-fonts
;;
;; Bear in mind, this will also run fc-cache -f -v on MacOS and Linux which can
;; take some time to complete. For Windows, this function will prompt for a
;; download directory for you to install them manually.
;;---
(if (package-installed-p 'all-the-icons)
    (use-package all-the-icons
      :if (display-graphic-p)
      ;; TODO: try out this hook
      ;; TODO: or try these: https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line#wiki-content
      ;; TODO: or both?
      ;; :config
      ;; (add-hook 'after-change-major-mode-hook
      ;;           (lambda ()
      ;;             (let* ((icon (all-the-icons-icon-for-mode major-mode))
      ;;                    (face-prop (and (stringp icon) (get-text-property 0 'face icon))))
      ;;               (when (and (stringp icon) (not (string= major-mode icon)) face-prop)
      ;;                 (setq mode-name (propertize icon 'display '(:ascent center)))))))
      )

  ;; Bit questionable cuz I've never done this, but....
  ;; To fix this warning:
  ;;   Windows 7:
  ;;     1) invoke the use-package call
  ;;     2) M-x all-the-icons-install-fonts
  ;;     3) Install wherever. $HOME/fonts maybe.
  ;;     4) Be admin.
  ;;     5) Select all, right click, "Install"
  ;;     6) Pray
  ;;     7) Curse
  ;;  Windows 10:
  ;;     1) Hope Win7 instructions work for Win10, but like actually work.
  (spydez/warning/message nil nil
                          "all-the-icons not installed. Will need manual help after install. See configure-text.el for details."))

;; So now on a Windows 7 machine I have... some icons? Run both these:
;;   (insert (all-the-icons-octicon "file-binary"))
;;   (insert (all-the-icons-wicon "tornado"))
;; Tornado shows up, but GitHub's File-Binary does not.
;; ...
;; ...er... Tornado does show up but only when it's not in a comment?
;; I may have lots of work to do. Maybe just see if it works in Win10 and
;; pray for a new work computer soon.
;;
;; Similar issue: https://github.com/domtronn/all-the-icons.el/issues/89
;;   Troubleshooting: https://github.com/domtronn/all-the-icons.el/blob/master/README.md#troubleshooting
;;
;; Oh wait, maybe that tofu is the "file-binary". -_-
;; They're still wrong/tofu when commented though.


;; todo: emoji-cheat-sheet-plus?
;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;; https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus


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
    :defer nil
    :config
    ;; TODO: I think I want this, or display-line-numbers-width. Need a long
    ;; file to figure out which.
    (setq display-line-numbers-width-start 't)
    (global-display-line-numbers-mode)))


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
;; iedit: multi-point editting
;;------------------------------------------------------------------------------
;; TODO: try this out.
;; Invoke C-; and all occurrences of the symbol under the cursor (or the
;; current selection) are highlighted and any changes you make on one of them
;; will be automatically applied to all others.
;;   https://zzamboni.org/post/my-emacs-configuration-with-commentary/#general-settings-and-modules
;; (use-package iedit
;;   :custom
;;   (iedit-toggle-key-default (kbd "C-;"))
;;   :config
;;   (set-face-background 'iedit-occurrence "Magenta"))


;;------------------------------------------------------------------------------
;; TODO: bells?
;;------------------------------------------------------------------------------

;; maybe this with ring bell sound too?
;;
;; (setq visible-bell nil
;;       ring-bell-function 'flash-mode-line)
;; (defun flash-mode-line ()
;;   (invert-face 'mode-line)
;;   (run-with-timer 0.1 nil #'invert-face 'mode-line))
;; https://www.emacswiki.org/emacs/AlarmBell
;; http://pragmaticemacs.com/emacs/using-a-visible-bell-in-emacs/
;; https://www.google.com/search?q=emacs+better+visual+bell&hl=en


;; bell? (this doesn't work...)
;; (setq ring-bell-function 'ignore)
;; (setq visible-bell t)

;; from old .emacs for aluminum
;; bell ringing sucks, but may be wanted occasionally...
;; (setq ring-bell-function 
;;   (lambda ()
;;     (unless (memq this-command
;;                   '(isearch-abort abort-recursive-edit 
;;                     exit-minibuffer keyboard-quit mwheel-scroll
;;                     next-line previous-line))
;;       (ding))))


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
