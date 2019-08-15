;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; Bind -> Politely asks for a keybind
;; Bind* -> Overrides Other (Minor Mode) Binds

;; Want to insist on "C-c C-c" being `comment-or-uncomment-region' in /all/ prog
;; modes, but still use bind-keys... Which seems un-possible to do bere in one
;; spot because python-mode-map steals it but doesn't exist yet.
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

;; TODO: (try to) get this working maybe
;; TODO: also try to use use-tool for vsvars?
;;
;; ;; Naming these?
;; ;; Or get use-tool better and put these funcs in the tool?
;; ;; spydez/devenv/visual-studio-2010/setup (from mbg/devenv-compile)?
;; ;; spydez/devenv/visual-studio-201/compile (from weird-compile)?
;;
;; ;; TODO: make compile use windows shell instead of bash
;; ;; https://gist.github.com/lionicsheriff/5971015
;; ;; Usage: M-x mbg/devenv-compile
;; ;;        M-x compile
;; (defun mbg/devenv-compile nil
;;   "Set up compile command for Visual Studio"
;;   (interactive)
;;   (let ((vsvars (shell-quote-argument "C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\Tools\\vsvars32.bat"));; C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\Common7\\Tools\\vsvars32.bat"))
;;           (solution-file (shell-quote-argument
;;                               ;; awesomely, locate-dominating-file returns the directory for the file
;;                               ;; so when you use a pattern to find a file, you need to run it again in
;;                               ;; the directory itself to get the file name. Who knew.
;;                               (car (directory-files
;;                                     (locate-dominating-file default-directory
;;                                                                   (lambda (dir)
;;                                                                       (directory-files dir
;;                                                                                            nil
;;                                                                                            ".*\\.sln$"
;;                                                                                            t)))
;;                                     t
;;                                     ".*\\.sln$"))))
;;           (build-config "Debug"))
;;     (message (concat "sln: " solution-file " @ Config: " build-config))
;;     (setq compile-command (concat "call " vsvars " && devenv " solution-file " /Build " build-config))))
;;
;; ;; TODO: also try these for getting Visual Studio to compile
;; ;; https://stackoverflow.com/a/4589933
;;
;; ;; TODO: try different shells? need cmd for Visual Studio?
;; ;; https://www.reddit.com/r/emacs/comments/8qsvp9/question_is_there_a_way_to_run_different_shells/
;; ;;
;; ;; This one is better:
;; ;;   "This will set shell-file-name locally when you call weird-compile, which
;; ;; you can bind to the key of your choice."
;; ;;   - Sean Allred: https://superuser.com/a/806388
;; (defun weird-compile () (interactive)
;;   (let ((shell-file-name "/bin/my-weird-sh"))
;;     (call-interactively #'compile)))


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
   (org-mode . rainbow-delimiters-mode)))




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
  :defer t
  :bind
  ("C-=" . er/expand-region)
  ("C-<prior>" . er/expand-region)  ;; PageUp
  ("C-<next>" . er/contract-region) ;; PageDown
  )
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
;; Unfill Paragraph, Fill/Unfill Hydra
;;------------------------------------------------------------------------------
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

(require 'with)
(with-feature 'hydra
  (defhydra spydez/hydra/fill ()
    "Fill Commands"

    ;; region...ish?
    ("r" fill-region "region (selected)") ;; ole faithful
    ("a" fill-region-as-paragraph "region as paragraph")

    ;; paragraph
    ("i" fill-individual-paragraphs "individual paragraphs")
    ("n" fill-nonuniform-paragraphs "non-uniform paragraphs")
    ("p" fill-paragraph "paragraph") ;; the standard
    ("o" org-fill-paragraph "paragraph (org-mode)")

    ;; unfill
    ("u" spydez/fill/unfill-paragraph "unfill")

    ;; fill-paragraph for specific modes (bound over `fill-paragraph'
    ;; in those modes). Ignore until I think I need 'em?
    ;; c-fill-paragraph
    ;; lisp-fill-paragraph
    ;; python-fill-paragraph
    ;; message-fill-paragraph
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
;;  :bind
;;  ("M-q" . unfill-toggle)
;;  ("A-q" . unfill-paragraph))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-dev-env)
