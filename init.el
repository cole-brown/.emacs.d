;; -*- emacs-lisp -*-

;; init.el - non-computer-specific emacs init
;; TODO: get rid of this filename line in all files..?

;; TODO: add license? MIT probably.
;; https://snyk.io/blog/mit-apache-bsd-fairest-of-them-all/
;; TODO: Short copyright in header pointing to LICENSE.txt or something.

;;------------------------------------------------------------------------------
;; Notes, TODOs, Links
;;------------------------------------------------------------------------------

;; can 'literate programming' do multiple files?
;; http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html

;; Special extra useful links:
;; TODO: Finish: Sacha init.el: http://pages.sachachua.com/.emacs.d/Sacha.html
;; TODO: Start: zzamboni init.el: https://github.com/zzamboni/dot-emacs/blob/master/init.org
;; Many neat things?: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
;;   todo: see if I want more of them.
;; TODO: https://www.gitignore.io/api/emacs
;; And of course my old setup: https://github.com/spydez/emacs
;; TODO: check this? https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;;    from: https://www.reddit.com/r/emacs/comments/2wzhxh/how_to_organize_initel/covmnl5
;; general layout ideas:
;;   "How to organize init.el?" https://www.reddit.com/r/emacs/comments/2wzhxh/how_to_organize_initel/
;;
;; Left off at "Projectile", but have to use Google Cache:
;; http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
;; https://webcache.googleusercontent.com/search?q=cache:pccrs3LhmCoJ:https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html+&cd=1&hl=en&ct=clnk&gl=us&client=firefox-b-1

;; TODO: try a let or something for vars that should be different during my init
;; e.g. gc threshold.
;; like...
;; (let ((gc-cons-threshold most-positive-fixnum))
;;  # existing init code
;;  )
;; But a nicer way to do that? (let ((vars...)) (load "actual-init")) maybe?

;; TODO: for my init files...
;;   do: (load "file") instead of (load "file.el") ??
;;   or even: (require 'file)/(provides 'file) ???
;; https://emacs.stackexchange.com/questions/3310/difference-between-load-file-and-load

;; TODO: check if missed any of old .emacs files

;; TODO: reduce this down to as few lines as possible? 
;;  e.g. http://ergoemacs.org/emacs/organize_your_dot_emacs.html
;;  but I'd still need all the stuff setting up the paths, probably?


;;------------------------------------------------------------------------------
;; Layout.
;;------------------------------------------------------------------------------

;; early-init.el:
;;   Not used right now.
;;   Can do some even earlier stuff with early-init.el if needed...
;;     https://www.reddit.com/r/emacs/comments/7yns85/emacs_adds_support_for_a_second_read_earlier_init/
;;     https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b

;; init.el:
;;   First, emacs loads init.el.
;;   Our init.el will then load our files in this order:
;;     - bootstrap-*.el
;;     - init-*.el
;;     - configure-*.el
;; TODO: I moved from setup-*.el to configure-*.el because it seemed more apropros.
;;   However now my ordering is not also alphabetical...
;; TODO: bootstrap-*, configure-*, finalize-* maybe?

;; Trying require/provide now.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Named-Features.html
;; Note: 'require' works off of file name just like load if not already
;; loaded. 'provide' can provide anything.  But if we want 'require' to find the
;; right thing, we'll have to have require/provide/filename on the same page.
;; Alternative is, like, (require 'spydez/bootstrap/debug "bootstrap-debug")?
;; Don't think that looks great.


;;------------------------------------------------------------------------------
;; Initial vars bootstrap.
;;------------------------------------------------------------------------------
;; We probably need to setup some vars, load paths, etc in our init.el before getting going.

;;---
;; Setup some very basics, so we can get moving...
;;---
;; These are the defaults. Override with setq later if needed.

;; defconst vs defvar vs setq
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html
;; Looks like I can use defconst for desired functionality/hinting, while also being
;; allowed to ignore the const-ness and override via a setq in a later init sub-file.

;; cross-platform dir and file names:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html#Directory-Names

(defun spydez/dir-name (name parent)
  "Expand name as child dir of parent in platform-agnostic manner."
    (file-name-as-directory (expand-file-name name parent)))

(defconst spydez/name/setup-domain "work"
  "A domain/folder for setups similar to this. E.g. work vs personal.")
(defconst spydez/name/setup-comp (system-name)
  "Intended for this specific computer's setup folder.")
(defconst spydez/dir/setup-emacs (expand-file-name user-emacs-directory)
  ;; user-init-file and user-emacs-directory can be helpful here
  "This should be a platform-agnostic way to find .emacs.d.")

;; Path dirs, and places for overrides to exist.
;;   ./spydez/defaults/
;;   ./spydez/
;;   ./spydez/work/
;;   ./spydez/work/WORK-PC-NAME/
(defconst spydez/dir/setup-personal (spydez/dir-name "spydez" spydez/dir/setup-emacs)
  "All of my own personal/custom setup code/vars/definitions...")
(defconst spydez/dir/setup-defaults (spydez/dir-name "defaults" spydez/dir/setup-personal)
  "All of my optional/default setup elisp files...")
(defconst spydez/dir/setup-domain-specific (spydez/dir-name spydez/name/setup-domain spydez/dir/setup-personal)
  "Anything that has to be domain specific. Tab widths or whatnot.")
(defconst spydez/dir/setup-comp-specific (spydez/dir-name spydez/name/setup-comp spydez/dir/setup-domain-specific)
  "Anything that has to be computer specific. Overriding tab widths or whatnot.")

;; todo: updated to c:/home/<user>/documents
(defconst spydez/dir/common-doc-save "c:/home/documents"
  "Place for auto-open files or secrets or something to be.")

;; todo: move to an end script or something?
;; auto-open file list
(defvar spydez/auto-open-list
  '(
    (expand-file-name "work.org" spydez/dir/common-doc-save)
    ))

;; folders for auto-save files and backup-files (#*# and *~)
(defconst spydez/dir/backup-files
  (spydez/dir-name "backups" spydez/dir/setup-emacs))

(defconst spydez/dir/auto-save-files
  (spydez/dir-name "auto-save-list" spydez/dir/setup-emacs))

(defconst spydez/file/save-history
  (expand-file-name "savehist" spydez/dir/setup-emacs)
  "History of commands, etc.")

;;---
;; Identity / Personal Information
;;---
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/User-Identification.html
(setq user-full-name "Cole Brown"
      user-mail-address "git@spydez.com")
;; user-login-name exists if needed

;;---
;; Add stuff to our load path.
;;---
;; Setting overrides towards front of load-path (add-to-list does this for us).
;;
;; Don't use .emacs.d. 
;; https://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path
;; (add-to-list 'load-path spydez/dir/setup-emacs)
(add-to-list 'load-path spydez/dir/setup-defaults) ;; defaults first so everything else overrides.
(add-to-list 'load-path spydez/dir/setup-personal)
(add-to-list 'load-path spydez/dir/setup-domain-specific)
(add-to-list 'load-path spydez/dir/setup-comp-specific) ;; most specific to this computer last

;;---
;; Debug
;;---
;; All the way down here because I want my load paths, but we could put at the top if needed with a little adjustment.
(require 'bootstrap-debug)

;;---
;; Custom file
;;---
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

;; Some packages want to write to our custom file, so set that up first.
;; An unadorned filename (just "custom.el") wasn't getting picked up as the custom file, so for now:
(setq custom-file (expand-file-name "custom.el" spydez/dir/setup-personal))
;; May need a better setter if custom-file needs adjusted per computer...
;; todo: Helper func to look for file to define place or maybe try provide/require?
;; Possibly move custom-file setting up, and loading down below loading of bootstrap-vars overrides.
(load custom-file t)

;;---
;; Misc Stuff
;;---
;; Load sensitive information from outside of .emacs.d
(when
    (not
     (load (expand-file-name ".emacs.secrets" spydez/dir/common-doc-save) 'noerror))
  (message "No secrets to load."))

;;---
;; Try-Load overrides (from bootstrap-vars.el)?
;;---
;(when (require bootstrap-vars nil 'noerror)
;  (message "Empty bootstrap-vars."))
(require 'bootstrap-vars nil 'noerror)
;; todo: rename to override-bootstrap-vars?
;; override-vars? bootstrap-override?
;; bootstrap-refine? bootstrap-post? post-bootstrap?

;;------------------------------------------------------------------------------
;; Bootstrap.
;;------------------------------------------------------------------------------
;; Actual bootstrapping, or finalizing of bootstrap, depending on how you look
;; at it. The above was, essentially, the min needed to get ready for
;; use-package which is the min needed for init-debug which is possibly useful
;; for debugging as I go through the rest of this init.el rewrite.

;; Keep near (require 'bootstrap-package):
;;---
;; Not sure how true this is, but...
;; source: https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; (package-initialize) ; Do NOT delete this comment
;;   In emacs 25+, the `package-initialize' call is auto-added to the top of
;; init.el unless the user already has a commented or uncommented
;; `(package-initialize)' line present in their init.el.
;;   I call this function in bootstrap-packages.el and so am keeping the
;; commented out version here so that package.el does not add it again.

;; Init use-package so we can use use-package for the rest of the packages we use.
(require 'bootstrap-package)

;; ASAP after use-package is available
(require 'init-debug)

;; TODO: Libraries here? E.g. dash
;;  - do we need any?
;;  - do they really go here, or down in packages?

;; Setup backups, autosaves, and history.
(require 'bootstrap-backups)

;; todo:
;; conditional use-package stuff? 
;; https://jwiegley.github.io/use-package/keywords/


;;------------------------------------------------------------------------------
;; Packages.
;;------------------------------------------------------------------------------

;; TODO: pull out into one or more include files when needed

;; todo: up this a waybunch? May be a more annoying hit to gc huge chunks infrequently instead of tiny chunks frequently.
;gc-cons-threshold
;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; or hooks... http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;; TODO: move `Packages` below `Setup`?


;;------------------------------------------------------------------------------
;; Configuration.
;;------------------------------------------------------------------------------
;; Loading and init are done - now do any more required setup.

;; TODO: HELM HERE PLZ
;; Helm/ido/etc
(require 'configure-completion)

;; Minibuffer and mode line tweaks
(require 'configure-minibuffer)

;; Window setup (menu bar, color theme, etc)
(require 'configure-window)

;;---
;; Kill/Yank Ring (aka Undo/Redo)
;;---

;; This lets you use C-x u (undo-tree-visualize) to visually walk through the
;; changes you've made, undo back to a certain point (or redo), and go down
;; different branches. (C-n/p/f/b mostly?)
(use-package undo-tree
  :diminish undo-tree-mode
  ;; TODO: undo tree not being diminished?
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))
;; [2019-01-17] Trial package...

;; Emacs has a powerful buffer tracking change system. Unfortunately, I don't understand any of it. Undo should "just work".

;; (require 'undo-tree)
;; (global-undo-tree-mode 1)
;; (eval-after-load "diminish"
;;   '(progn
;;      (eval-after-load "undo-tree"
;;        '(diminish 'undo-tree-mode "↺"))))

;;---
;; Help?
;;---
;; I need somebody...

;; It's hard to remember keyboard shortcuts. The guide-key package pops up help after a short delay. 
(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))  ; Enable guide-key-mode
;; [2019-01-17] Trial package...
;; TODO: Doesn't work til turned off/back on. Need debugged?

;;---
;; Misc Config
;;---

;; Sentences end with a single space. This makes sentence navigation commands work better?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org892ee89
;; TODO: What does this do, exactly?
(setq sentence-end-double-space nil)

;; probably want this overridable
(setq-default fill-column 80)

;;---
;; UTF-8
;;---
;; Prefer utf-8
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/International.html#International
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Output-Coding.html
;; May need a way of checking for smart quotes and em dashes and stuff when we don't want utf-8...
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; todo: these?
;; (global-linum-mode 1) ; show line numbers everywhere

;; parenthesis?
;; (setq blink-matching-paren nil)
;; (show-paren-mode t)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'expression)

;; bell? (this doesn't work...)
;; (setq ring-bell-function 'ignore)
;; (setq visible-bell t)

;; pull whatever I have in my old config, check against this:
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)

;; Ban whitespace at end of lines, globally. 56
;; (add-hook 'write-file-hooks
;;           '(lambda ()
;;              (gcr/delete-trailing-whitespace)))


;; autocomplete?
;; Can you thrive and profit without auto-completion? Surely. The feature is
;; kind of a comfort blanket for most of us; you will never fail to bild a
;; system without it (unless you are using Java, then you need IntelliJ). Still
;; it is quite nice to have popup documentation.
;; (require 'fuzzy)
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-auto-start nil)
;; (ac-set-trigger-key "TAB")
;; (eval-after-load "diminish"
;;   '(progn
;;      (eval-after-load "auto-complete"
;;        '(diminish 'auto-complete-mode "↝"))))
;; TODO: see what I was using first. Some of this is dead links.
;; https://webcache.googleusercontent.com/search?q=cache:nvHM1b9JhGcJ:https://github.com/suzp1984/auto-complete+&cd=1&hl=en&ct=clnk&gl=us
;; https://webcache.googleusercontent.com/search?q=cache:pccrs3LhmCoJ:https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html+&cd=1&hl=en&ct=clnk&gl=us&client=firefox-b-1


;; Whitespace
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
;; (require 'whitespace)
;; (setq whitespace-style '(trailing lines tab-mark))
;; (setq whitespace-line-column 80)
;; (global-whitespace-mode 1)
;; (eval-after-load "diminish"
;;   '(progn
;;      (eval-after-load "whitespace"
;;        '(diminish 'global-whitespace-mode "ᗣ"))
;;      (eval-after-load "whitespace"
;;        '(diminish 'whitespace-mode ""))))

;; Templates/snippets
;; (require 'yasnippet)
;; (yas-load-directory (concat (cask-elpa-dir)
;;                             "/yasnippet-20140306.5/snippets"))
;; (eval-after-load "diminish"
;;   '(progn
;;      (eval-after-load "yasnippet"
;;        '(diminish 'yas-minor-mode "✂"))))
;; (yas-global-mode 1)

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------

;; todo: initial-buffer-choice vs spydez/auto-open-list???

;; todo: Maybe start a spydez/debugging file?..
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/message.html
;(message "%s" use-package-always-ensure)

(require 'zzz-finalize)
;; fin

;; TODO: check out old cole-PC.emacs and bootstrap.el.
;; old setup: https://github.com/spydez/emacs
;; See how much this init.el can be reduced to minimum?
