;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; Bind -> Politely asks for a keybind
;; Bind* -> Overrides other (Minor Mode) binds
;;          - Does not override Major Mode bins. :/

;; Want to insist on "C-c C-c" being `comment-or-uncomment-region' in /all/ prog
;; modes, but still use bind-keys... Which seems un-possible to do here in one
;; spot because python-mode-map steals it but doesn't exist yet and I want
;; python config all in configure-python.el
(bind-keys
 ;;-----
 ;; Global Keybinds
 ;;-----
 ;; no global dev-env keybinds

 ;;-----
 ;; Grandpa Programming Mode
 :map prog-mode-map
 ;;-----
 ;; Comment/Uncomment
 ("C-c C-c" . comment-or-uncomment-region)

 ;; NOTE: Can't bind here as python-mode-map is undefined. So... Callback?
 ;; Meh. Want to try to conform to use-package, so this is awkward. In two
 ;; places for now.
 ;; ;;-----
 ;; ;; Python Programming Mode
 ;; :map python-mode-map
 ;; ;;-----
 ;; ("C-c C-c" . comment-or-uncomment-region)
 )


;;------------------------------------------------------------------------------
;; Logs
;;------------------------------------------------------------------------------
;; See `configure-logs.el'


;;------------------------------------------------------------------------------
;; Client for Language Server Protocol
;;------------------------------------------------------------------------------

;; [2019-08-12 Mon]
;;   Trying out LSP in its own file: configure-lsp.el
;;
;; ;; Don't need/can't use this right now, but eventually maybe I'll join the
;; ;; futuristic 2015+ technology train?
;;
;; ;; https://www.reddit.com/r/emacs/comments/ahzrg0/announcement_lspmode_60_released/
;; ;; Emacs packages:
;; ;;   https://github.com/emacs-lsp/lsp-mode
;; ;;   https://github.com/emacs-lsp/lsp-ui
;; ;;   https://github.com/emacs-lsp/dap-mode
;;
;; ;; Though... OmniSharp does have a C# server...
;; ;;   https://langserver.org/
;; ;;   https://github.com/Microsoft/language-server-protocol


;;------------------------------------------------------------------------------
;; Flycheck
;;------------------------------------------------------------------------------
;; https://www.flycheck.org/en/latest/
;; TODO: Flycheck? Here? configure-cpp? configure-prog (prog-mode)?
;; TODO: Flycheck as its own thing? Does it loop into lsp-mode now?


;;------------------------------------------------------------------------------
;; Documentation
;;------------------------------------------------------------------------------

;; TODO: Dash w/ Helm-Dash
;; https://github.com/areina/helm-dash
;;   https://kapeli.com/dash
;; Has Unity3d and stuff. Probably not our old version, though.

;; Turn on the online documentation mode for all programming modes (not all of
;; them support it).
;; Emacs 26 has it by default. Should I use use-package in that case?
;; Let's try and see how it goes.
(use-package eldoc
  :ensure nil
  :delight
  :hook
  (prog-mode . turn-on-eldoc-mode))


;;------------------------------------------------------------------------------
;; Visual Studio build command
;;------------------------------------------------------------------------------

(defcustom spydez/path/dev-env/visual-studio
  nil
  "Path to currently used Visual Studio."
  :group 'spydez/group
  :type 'string)


(defcustom spydez/exe/dev-env/unit-test
  nil
  "Path to executable to use to run unit tests."
  :group 'spydez/group
  :type 'string)


(defcustom spydez/dev-env/unit-test/args
  nil
  "Args to pass to unit-test runner."
  :group 'spydez/group
  :type 'string)


;; §-TODO-§ [2019-10-18]: Do I need to add these error regexps for Visual Studio
;; compile? It seems to be catching things so far... This answer was from 2011
;; so...
;;  - https://stackoverflow.com/a/4589933

;; Partially blood, sweat, and tears...
;; but started from: https://gist.github.com/lionicsheriff/5971015
(defun spydez/dev-env/visual-studio/compile/setup ()
  "Set up compile command for Visual Studio"
;;  (interactive)
  (let* ((error-message nil)
         (vsvars
          ;; path/to/vsvars32.bat
          (spydez/path/to-file spydez/path/dev-env/visual-studio
                               "Common7" "Tools" "vsvars32.bat"))
         (solution-file
          ;; catch when no sln file (i.e. ran from here instead of .cs buffer)
          (condition-case-unless-debug err
              ;; `locate-dominating-file' returns the directory for the
              ;; file so when you use a pattern to find a file, you need to run it
              ;; again in the directory itself to get the file name.
              (car (directory-files
                    (locate-dominating-file default-directory
                                            (lambda (dir)
                                              (directory-files dir
                                                               nil
                                                               ".*\\.sln$"
                                                               t)))
                    t
                    ".*\\.sln$"))
            ;; handler for error: message bad thing, return nil
            (error (setq error-message
                         (format "Compile Setup Error: %s --> %s"
                                 "No .sln file found."
                                 (error-message-string err)))
                   nil)))

         (build-config "Debug")
         ;; ;; switch back to cmd for this...
         ;; (shell-file-name (spydez/shell/system-default))
         ;; Didn't fix the "(file-error "Spawning child process" "Invalid
         ;; argument")", so no.
         )

    ;; check for specific error message first
    (cond
     (error-message (message error-message))
     ((null vsvars) (message "Compile Setup Error: No vsvars."))
     ((null solution-file) (message "Compile Setup Error: No solution file."))
     ((null build-config) (message "Compile Setup Error: No build config."))
     (t ;; Alrighty... Let's do this.
      (setq compile-command
            ;; Don't shell quote these maybe?? No, uh...
            ;; Double quote maybe??
            ;; Those were all before other bugs were found so they're back
            ;; on the menu as valid options.
            (concat "call "
                    "\"" vsvars "\""
                    ;; vsvars
                    ;;(shell-quote-argument vsvars)
                    " && devenv "
                    "\"" solution-file "\""
                    ;; solution-file
                    ;;(shell-quote-argument solution-file)
                    " /Build " build-config))
      (message "Compile Setup: Ready for '%s' @ '%s'"
               (file-name-nondirectory solution-file)
               build-config)))))
;; (spydez/dev-env/visual-studio/compile/setup)


;; Partially blood, sweat, and tears...
;; but started from: https://gist.github.com/lionicsheriff/5971015
(defun spydez/dev-env/visual-studio/compile ()
  "Compile in Visual Studio, maybe?"
  (interactive)
  ;; try to be smart... probably don't need to, but hey.
  ;; If compile-command has the magic words, skip setup.
  (unless (string-match-p (rx word-start "Visual" word-end
                              (+? whitespace)
                              word-start "Studio" word-end)
                          compile-command)
    (spydez/dev-env/visual-studio/compile/setup))

  (let ((shell-file-name (spydez/shell/system-default)))
    (spydez/buffer/kill-special (rx "unit"
                                    (one-or-more whitespace)
                                    "test" (optional "s")
                                    (*? printing)))
    (call-interactively #'compile)))


(defun spydez/dev-env/visual-studio/unit-test ()
  "Run unit tests."
  (interactive)
  (if (not (and (boundp 'spydez/exe/dev-env/unit-test)
                (boundp 'spydez/dev-env/unit-test/args)))
      (spydez/message/warning nil :error
                              "Needed variables not found. %s: %s, %s: %s"
                              (symbol-name 'spydez/exe/dev-env/unit-test)
                              (boundp 'spydez/exe/dev-env/unit-test)
                              (symbol-name 'spydez/dev-env/unit-test/args)
                              (boundp 'spydez/dev-env/unit-test/args))
    (spydez/shell/command-async
     (concat "\"" spydez/exe/dev-env/unit-test "\" "
             (mapconcat 'identity spydez/dev-env/unit-test/args " ")
             " ")
     "Unit Tests" "Visual Studio"
     "Running unit tests...")))
;; (spydez/dev-env/visual-studio/unit-test)


;;-----------------------------------------------------------------------------
;; Compilation Mode
;;-----------------------------------------------------------------------------

;; §-TODO-§ [2019-10-01]: use-package block?

;; Scroll compilation buffer to follow output, but stop scrolling at first error
;; in output.
(customize-set-variable 'compilation-scroll-output 'first-error)

;; Turn off confirm compile command.
(customize-set-variable 'compilation-read-command nil)


;;------------------------------------------------------------------------------
;; Hex Editor
;;------------------------------------------------------------------------------
;; Hex mode is: hexl


;;------------------------------------------------------------------------------
;; Auto-revert mode
;;------------------------------------------------------------------------------
;; For updating a file/buffer when it's changed on disk.
;; e.g. Change in Visual Studio, auto-revert will notice and update here if no
;; changes here. Else it'll just quietly leave it alone.
;;
;; [2019-03-07]: enabled global auto-revert

;; TODO: Consider magit-auto-revert-mode?
;;   [2019-03-07] - Trying out global-auto-revert-mode so magit-auto-revert-mode
;;                  is eclipsed.

;; auto-revert-mode is hiding in autorevert feature/file
(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode t))


;;------------------------------------------------------------------------------
;; Defaults
;;------------------------------------------------------------------------------

;; TODO: (use-package prog ...) for things used by all prog-modes? Like this:
;;   TODO: prog-mode hook for making this a default?
;;          ("C-c C-c" . comment-or-uncomment-region)


;;---
;; Tabs
;;---
;; See `configure-whitespace'. All default tab settings there now.

;; TODO: is this global or per-mode in old .emacs?
;; New lines are always indented
;;(global-set-key (kbd "RET") 'newline-and-indent)

;;---
;; Undo Size
;;---

;; give a larger undo buffer
(when (and (bound-and-true-p spydez/undo-limit)
           (> spydez/undo-limit undo-limit))
  (setq undo-limit spydez/undo-limit))
(when (and (bound-and-true-p spydez/undo-strong-limit)
           (> spydez/undo-strong-limit undo-strong-limit))
  (setq undo-strong-limit spydez/undo-strong-limit))


;;------------------------------------------------------------------------------
;; Parenthesis
;;------------------------------------------------------------------------------
;; TODO: disable electric-pair-mode if needed?

;; Used mic-paren in old .emacs, but it looks like:
;;   a) mic-paren is basically derelict (last updated 2013)
;;   b) emacs 26 has equivalent enough replacement in show-paren-mode now.
(require 'paren)

;; good show-paren-mode info here: http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html

;; Trying out default 0.125 - might like it. Might want faster but still non-instant?
;; NOTE: It can be deactivated with the following (which you have to do before
;; activating show-paren-mode in your .emacs):
;;   - https://www.emacswiki.org/emacs/ShowParenMode
;; (setq show-paren-delay 0)

(setq blink-matching-paren nil)

;; 'parenthesis: for just the parens
;; 'expression: for hilighting everything
;; 'mixed: will behave like 'parenthesis when the matching parenthesis is visible,
;;         and like 'expression otherwise. 
;;(setq show-paren-style 'expression)
;;(setq show-paren-style 'parenthesis)
;; 'expression has its uses in elisp code, might could set it to that for the mode

(show-paren-mode t)

;; TODO: Set up a red face for missing (like in old .emacs custom.el)?
;; To change the color/face:
;; (require 'paren)
;; (set-face-background 'show-paren-match (face-background 'default))
;; (set-face-foreground 'show-paren-match "#def")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
;; Note that show-paren-mode can also detect mismatches, e.g. when try to close
;;   a '(' with a ']'. You can tune this with show-paren-mismatch-face, e.g.:
;; (set-face-foreground 'show-paren-mismatch-face "red") 
;; (set-face-attribute 'show-paren-mismatch-face nil 
;;                     :weight 'bold :underline t :overline nil :slant 'normal)
;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Blinking.html
;; orig name: interactive-blink-matching-open
;; §-TODO-§ [2019-10-01]: Put in a hydra or something? Useful if match is
;; off-screen, maybe... But probably only if not in c-mode derivatives with
;; brace-on-separate-line code style?
(defun spydez/blink-paren ()
  "Indicate momentarily the start of parenthesized sexp before point."
  (interactive)
  (let ((blink-matching-paren-distance
         (buffer-size))
        (blink-matching-paren t))
    (blink-matching-open)))

;; There are default keybindings for jumping to opening and closing delimiters
;;   C-M b (move backward)
;;   C-M f (move forward)
;; See http://www.emacswiki.org/emacs/ParenthesisMatching#toc2 for details 


;; TODO: Try smartparen and/or rainbow delimiters.


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
;; [2019-03-14] Was in configure-elisp. Tried out rainbow-delimiters for emacs
;;   lisp mode; could expand to all prog-mode if super useful.
;; [2019-08-09] Moved back to configure-dev-env as I like it in elisp. Trying in
;;   org and all prog modes.

;; Does this package slows things down? Sacha said she thought so but probably
;; outdated... (~4 years old blame as of [2019-08-09])
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#org36b0308
;; So yeah, combined with this I think it's fine...:
;;     "Great care has been taken to make this mode fast. You shouldn't see any
;;   change in scrolling or editing speed when it's on even when working in
;;   delimiter-rich languages like Clojure or Emacs Lisp. It can be used with
;;   any language."
;;     - https://github.com/Fanael/rainbow-delimiters

;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   ;; TRIAL [2019-10-16]: rainbow delimiters were getting a bit annoying what
   ;; with copying incomplete code, using "->" a lot, and other things I do in
   ;; org-mode... apparently. Trying with them off
   ;;(org-mode . rainbow-delimiters-mode)))
   ))


;;------------------------------------------------------------------------------
;; Code Folding
;;------------------------------------------------------------------------------
;; TODO: do I want/need this?
;; from https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html#sec-10-3-2
;; org-mode folding trees is nice, but I dunno if I do/don't want for code.
;; TODO: add in, try out?
;; (setq hs-hide-comments-when-hiding-all +1)
;; (setq hs-isearch-open t)
;; (require 'hideshow-org)
;; ; Displaying overlay content in echo area or tooltip
;; (defun display-code-line-counts (ov)
;;       (when (eq 'code (overlay-get ov 'hs))
;;         (overlay-put ov 'help-echo
;;                      (buffer-substring (overlay-start ov)
;;                                       (overlay-end ov)))))
;;
;;     (setq hs-set-up-overlay 'display-code-line-counts)
;; ; How do I get it to expand upon a goto-line?
;; (defadvice goto-line (after expand-after-goto-line
;;                                 activate compile)
;;         "hideshow-expand affected block when using goto-line in a collapsed buffer"
;;         (save-excursion
;;            (hs-show-block)))


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
  :commands (er/expand-region er/contract-region)

  ;;---
  :bind ;; global
  ;;---
  (("C-=" . er/expand-region)
   ("C-<prior>" . er/expand-region)    ;; PageUp
   ("C-<next>" . er/contract-region))) ;; PageDown
;; See readme on GitHub for info on improving in certain modes.

;; NOTE: f.lux uses M-<PageUp> and M-<PageDown> (aka M-<prev>/M-<next>)
;; Disable it in the f.lux settings.


;;------------------------------------------------------------------------------
;; Duplicate region, and then comment one out.
;; AKA Copy and Comment Region
;;------------------------------------------------------------------------------
;; Few different ways to do it out there - this one in my tiny test gave the
;; best formatting to the commented region and its new copy.
;;   https://stackoverflow.com/a/23588908
(defun spydez/region/copy-and-comment (beg end &optional arg)
  "Duplicate the region and comment-out the copied text.
See `comment-region' for behavior of a prefix arg."
  (interactive "r\nP")
  (copy-region-as-kill beg end)
  (goto-char end)
  (yank)
  (comment-region beg end arg))

;; TODO: Make a spydez/region/duplicate function?
;;   - Copy region, add newline, insert after...
;; TODO: region hydra with the spydez/region/* cmds and maybe fill cmds/hydra?


;;------------------------------------------------------------------------------
;; Fill/Unfill Commands, Functions, Hydras
;;------------------------------------------------------------------------------
;; Hydræ.

;; from: nhoffman http://nhoffman.github.io/.emacs.d/#org40b27e4
;;   which is from: http://defindit.com/readme_files/emacs_hints_tricks.html
;;     which is from: Stefan Monnier <foo at acm.org>
;;       which is probably from the turtles that go all the way down
;;
;; This is actually the inverse of fill-paragraph. Takes a multi-line paragraph
;; and makes it into a single line of text.
(defun spydez/fill/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(defun spydez/fill/paragraph/fn-for-mode ()
  "Mode-aware fill-paragraph so I only have to bind one thing in
the fill hydra. Separated the 'get func' out here so I can see if
in a mode with a special fill for hydra hinting."
  (cond
   ((derived-mode-p 'csharp-mode)
    #'c-fill-paragraph)

   ;; c-mode and all derivatives
   ((and (functionp 'c-buffer-is-cc-mode)
         (c-buffer-is-cc-mode))
    #'c-fill-paragraph)

   ;; elisp, other lispses
   ((or (derived-mode-p 'emacs-lisp-mode)
        (derived-mode-p 'lisp-mode))
    #'lisp-fill-paragraph)
   ;; Might just use `fill-paragraph'?
   ;; Seems to be what "M-q" is using right now?

   ;; python-mode
   ((derived-mode-p 'python-mode) #'python-fill-paragraph)

   ;; org-mode
   ((derived-mode-p 'org-mode) #'org-fill-paragraph)

   ;; default to the usual fill-paragraph
   (t #'fill-paragraph)))


(defun spydez/fill/paragraph/per-mode (&optional justify)
  "Mode-aware fill-paragraph so I only have to bind one thing in
the fill hydra."
  (interactive)
  (funcall (spydez/fill/paragraph/fn-for-mode) justify))


(defun spydez/fill/region/single-line (&optional justify)
  "Grab start/end of current line and call `fill-region'. i.e.
  \"'Fill Region' on just this line, please.\""
  (interactive)

  (let ((from (save-excursion (beginning-of-line) (point)))
        (to   (save-excursion (end-of-line)       (point))))
    (fill-region from to justify)))


(require 'with)
(with-feature 'hydra
  (defhydra spydez/hydra/fill (:color blue ;; default exit heads
                               :idle 1.0   ;; no help for this many seconds
                               :hint none)  ;; no hint - just fancy docstr
    "
^Regions^          ^Paragraphs^         ^Unfills^
^-^----------------^-^------------------^-^----------
_r_: Region        _p_: ?p?  _u_: Unfill ¶
_a_: As Paragraph  _i_: Individual ¶
_l_: Single Line   _n_: Non-Uniform ¶
^ ^                _d_: Default Fill ¶
"

    ;;---
    ;; region...ish?
    ;;---
    ("r" fill-region) ;; "Region (selected)") ;; ole faithful
    ("a" fill-region-as-paragraph) ;; "region as paragraph")
    ("l" spydez/fill/region/single-line) ;; "This line as region")

    ;;---
    ;; paragraph
    ;;---
    ;; this one is mode-aware, and hint should indicate mode vs default
    ("p" spydez/fill/paragraph/per-mode
     ;; fancy head's doc string used in above fancy body's docstring
     (format "%-14s"
             (if (eq (spydez/fill/paragraph/fn-for-mode) #'fill-paragraph)
                 "Default Fill ¶"
               "Mode-Aware ¶")))

    ("i" fill-individual-paragraphs) ;; "individual paragraphs")
    ("n" fill-nonuniform-paragraphs) ;; "non-uniform paragraphs")

    ;; TRIAL [2019-10-01]: Stick default in too in case mode-aware isn't what was wanted?
    ("d" fill-paragraph) ;; "\"default paragraph\"")

    ;; don't need anymore - mode-aware catches this.
    ;; ("o" org-fill-paragraph) ;; "paragraph (org-mode)")

    ;;---
    ;; unfill
    ;;---
    ("u" spydez/fill/unfill-paragraph) ;; "unfill paragraph")
    )

  ;; 1) The first try was "C-i", but that is (by definition) TAB, and really
  ;;    hairy to split those two keys because history... So let's try "C-'"?
  ;; 2) `bind-key*' instead of `bind-key' lets this have its keybind in org-mode
  ;;    over some org-agenda thing.
  (bind-key* "C-'" 'spydez/hydra/fill/body))

;; TODO: if spydez/fill/unfill-paragraph is any use, there's a package...
;;   "Add “unfill” commands to parallel the “fill” ones, bind A-q to
;; unfill-paragraph and rebind M-q to the unfill-toggle command, which
;; fills/unfills paragraphs alternatively."
;;   - https://zzamboni.org/post/my-emacs-configuration-with-commentary/
;;(use-package unfill
;;  ;;---
;;  :bind ;; global
;;  ;;---
;;  (("M-q" . unfill-toggle)
;;   ("A-q" . unfill-paragraph)))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-dev-env)
