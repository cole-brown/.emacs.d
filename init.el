;; -*- emacs-lisp -*-

;; init.el - non-computer-specific emacs init

;; can 'literate programming' do multiple files?
;; http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html

;; Special extra useful links:
;; Sacha init.el: http://pages.sachachua.com/.emacs.d/Sacha.html
;; zzamboni init.el: https://github.com/zzamboni/dot-emacs/blob/master/init.org
;; Many neat things?: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
;;   todo: see if I want more of them.

;;------------------------------------------------------------------------------
;; Initial vars setup.
;;------------------------------------------------------------------------------

;;---
;; Setup some very basics, so we can get moving...
;;---
;; These are the defaults. Override with setq later if needed.

;; defconst vs defvar vs setq
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html

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
(load "debug.el")

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
(load "init-vars.el" 'noerror)

;;------------------------------------------------------------------------------
;; Bootstrap.
;;------------------------------------------------------------------------------

;; Init use-package so we can use use-package for the rest of the packages we use.
(load "init-package.el")

;; TODO: Libraries here? E.g. dash
;;  - do we need any?
;;  - do they really go here, or down in packages?

;; Setup backups, autosaves, and history.
(load "backups.el")

;; todo: utf-8

;; todo:
;; conditional use-package stuff? 
;; https://jwiegley.github.io/use-package/keywords/

;;------------------------------------------------------------------------------
;; Packages.
;;------------------------------------------------------------------------------

;; TODO: pull out into one or more include files when needed

;;---
;; Color scheme: Zenburn
;;---
(use-package zenburn-theme)
;; Seems to work fine without 'load-theme

;; put my stuff after all those packages are loaded
;; todo: check these out?
;(load-file (concat kooru/emacs-libs "bootstrap.el"))
;(bootstrap-init kooru/comp-domain kooru/comp-name)


;;------------------------------------------------------------------------------
;; Setup.
;;------------------------------------------------------------------------------
;; Loading and init are done - now do any more required setup.

;;---
;; Window/GUI Setup
;;---

;; Don't show the GNU splash.
(setq inhibit-startup-screen t)

;; todo: a load for a file for vars that is in this part of the init... but isn't init-vars.el

;; I like the menu bar right now... (File, Edit, etc)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
;; Tool bar must go. (new, open, etc buttons).
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Scroll bar useful for buffer size/position at-a-glance.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))

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

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------

;; todo: initial-buffer-choice vs spydez/auto-open-list???

;; todo: Maybe start a spydez/debugging file?..
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/message.html
;(message "%s" use-package-always-ensure)

(load "finalize.el")
;; fin
