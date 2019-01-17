;; -*- emacs-lisp -*-

;; init.el - non-computer-specific emacs init

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

;; First, emacs loads init.el.
;; Our init.el will then load our files in this order:
;;   - bootstrap-*.el
;;   - init-*.el
;;   - setup-*.el

;; We probably need to setup some vars, load paths, etc in our init.el before getting going.

;;------------------------------------------------------------------------------
;; Initial vars bootstrap.
;;------------------------------------------------------------------------------

;; Can do some even earlier stuff with early-init.el if needed...
;; https://www.reddit.com/r/emacs/comments/7yns85/emacs_adds_support_for_a_second_read_earlier_init/
;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b

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

;; todo: can we get a (default) user name from emacs?

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
(setq user-full-name "Cole Brown"
      user-mail-address "git@spydez.com")

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
;; Helper func to look for file to define place or maybe try provide/require?
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
;; Try-Load overrides (from init-vars.el)?
;;---
;(when (load "init-vars.el" 'noerror)
;  (message "hi?"))
(load "bootstrap-vars.el" 'noerror)

;;------------------------------------------------------------------------------
;; Bootstrap.
;;------------------------------------------------------------------------------

;; Not sure how true this is, but...
;; source: https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; (package-initialize) ; Do NOT delete this comment
;;   In emacs 25+, the `package-initialize' call is auto-added to the top of
;; init.el unless the user already has a commented or uncommented
;; `(package-initialize)' line present in their init.el.
;;   I call this function in setup-packages.el and so am keeping the
;; commented out version here so that package.el does not add it again.

;; Init use-package so we can use use-package for the rest of the packages we use.
(load "bootstrap-package.el")

;; ASAP after use-package is available
(require 'init-debug)

;; TODO: Libraries here? E.g. dash
;;  - do we need any?
;;  - do they really go here, or down in packages?

;; Setup backups, autosaves, and history.
(load "bootstrap-backups.el")

;; todo: utf-8

;; todo:
;; conditional use-package stuff? 
;; https://jwiegley.github.io/use-package/keywords/

;;------------------------------------------------------------------------------
;; Packages.
;;------------------------------------------------------------------------------

;; TODO: pull out into one or more include files when needed

;; put my stuff after all those packages are loaded
;; todo: check these out?
;(load-file (concat kooru/emacs-libs "bootstrap.el"))
;(bootstrap-init kooru/comp-domain kooru/comp-name)

;; todo: up this a waybunch? May be a more annoying hit to gc huge chunks infrequently instead of tiny chunks frequently.
;gc-cons-threshold
;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; or hooks... http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;; TODO: move `Packages` below `Setup`?

;;------------------------------------------------------------------------------
;; Setup.
;;------------------------------------------------------------------------------
;; Loading and init are done - now do any more required setup.

(load "setup-completion.el")

;;---
;; Window/GUI Setup
;;---

;; Don't show the GNU splash.
(setq inhibit-startup-screen t)

;; todo: a load for a file for vars that is in this part of the init... but isn't init-vars.el

;; I like the menu bar right now... (File, Edit, etc)
(when (fboundp 'menu-bar-mode) (menu-bar-mode 1))
;; Tool bar must go. (new, open, etc buttons).
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Scroll bar useful for buffer size/position at-a-glance.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))

;; Winner-mode lets you use C-c <left> and C-c <right> to switch between window
;; configurations. This is handy when something has popped up a buffer that you
;; want to look at briefly before returning to whatever you were working
;; on. When you're done, press C-c <left>.
(when (fboundp 'winner-mode) (winner-mode 1))
;; https://www.emacswiki.org/emacs/WinnerMode
;; Some use use-package for this... http://pages.sachachua.com/.emacs.d/Sacha.html#org59481f4

;; todo: window config?
;; https://www.emacswiki.org/emacs/WindowConfiguration

;;---
;; Color scheme: Zenburn
;;---
(use-package zenburn-theme)
;; Seems to work fine without 'load-theme

;;---
;; Time in the modeline
;;---
;; Puts a clock down in the mode line.

;; For simple 24hr time:
;; (setq display-time-24hr-format t)
;; (display-time-mode t)

;; For ISO time:
;; https://emacs.stackexchange.com/questions/7365/how-to-display-date-in-julian-in-the-mode-line
(require 'calendar)
;; Set format to: yyyy-mm-dd HH:MM
;; (trimmed down from: yy-mm-dd HH:MM:SS (Time Zone) <Mail notify>
(setq display-time-string-forms
    ;; 2 digit year: '((substring year -2) "/" month "/" day
    '(year "/" month "/" day
      " " 24-hours ":" minutes ; ":" seconds
      ; Long-ass TZ: (if time-zone " (") time-zone (if time-zone ")")
      ; Mail notice: (if mail " Mail" "")
      ))
(display-time-mode t)

;;---
;; Misc Config
;;---

;; Sentences end with a single space. This makes sentence navigation commands work better?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org892ee89
;; TODO: What does this do, exactly?
(setq sentence-end-double-space nil)

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------

;; todo: initial-buffer-choice vs spydez/auto-open-list???

;; todo: Maybe start a spydez/debugging file?..
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/message.html
;(message "%s" use-package-always-ensure)

(load "zzz-finalize.el")
;; fin
