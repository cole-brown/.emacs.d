;; -*- emacs-lisp -*-

;; init.el - non-computer-specific emacs init

;; can 'literate programming' do multiple files?
;; http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html


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

(defconst spydez/name/setup-domain "work"
  "A domain/folder for setups similar to this. E.g. work vs personal.")
(defconst spydez/name/setup-comp (system-name)
  "Intended for this specific computer's setup folder.")
(defconst spydez/dir/setup-emacs (expand-file-name user-emacs-directory)
  ;; user-init-file and user-emacs-directory can be helpful here
  "This should be a platform-agnostic way to find .emacs.d.")

;; Path dirs, and places for overrides to exist.
;;   ./spydez/
;;   ./spydez/defaults/
;;   ./spydez/work/
;;   ./spydez/work/WORK-PC-NAME
(defconst spydez/dir/setup-personal (expand-file-name "spydez" spydez/dir/setup-emacs)
  "All of my own personal/custom setup code/vars/definitions...")
(defconst spydez/dir/setup-defaults (expand-file-name "defaults" spydez/dir/setup-personal)
  "All of my optional/default setup elisp files...")
(defconst spydez/dir/setup-domain-specific (expand-file-name spydez/name/setup-domain spydez/dir/setup-personal)
  "Anything that has to be domain specific. Tab widths or whatnot.")
(defconst spydez/dir/setup-comp-specific (expand-file-name spydez/name/setup-comp spydez/dir/setup-domain-specific)
  "Anything that has to be computer specific. Overriding tab widths or whatnot.")

;; todo: move to an end script or something?
;; auto-open file list
(defvar spydez/auto-open-list
  '("c:/home/documents/work.org"))

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

;; todo: inhibit startup stuff
;ptions affect some aspects of the startup sequence.
;- User Option: inhibit-startup-screen
;
;    This variable, if non-nil, inhibits the startup screen. In that case, Emacs typically displays the *scratch* buffer; but see initial-buffer-choice, below.
;
;    Do not set this variable in the init file of a new user, or in a way that affects more than one user, as that would prevent new users from receiving information about copyleft and basic Emacs usage.
;
;    inhibit-startup-message and inhibit-splash-screen are aliases for this variable. 
;
;- User Option: initial-buffer-choice
;
;    If non-nil, this variable is a string that specifies a file or directory for Emacs to display after starting up, instead of the startup screen. If its value is a function, Emacs calls that function which must return a buffer which is then displayed. If its value is t, Emacs displays the *scratch* buffer. 
;
;- User Option: inhibit-startup-echo-area-message
;
;    This variable controls the display of the startup echo area message. You can suppress the startup echo area message by adding text with this form to your init file: 

;; todo: utf-8

;; conditional use-package stuff? 
;; https://jwiegley.github.io/use-package/keywords/

;; TODO: Libraries here?
;;  - do we need any?
;;  - do they really go here, or down in packages?

;;------------------------------------------------------------------------------
;; Packages.
;;------------------------------------------------------------------------------

;; TODO: pull out into one or more include files when needed

;; Uh... use-package-always-ensure is void out here?
;; Maybe start a spydez/debugging file...
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/message.html
;(message "%s" use-package-always-ensure)

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
;; The... Something?.. TODO a title
;;------------------------------------------------------------------------------

;; todo: a load for a vars that is in this part of the init... but isn't init-vars.el
;; I like the menu bar right now... (File, Edit, etc)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
;; Tool bar must go. (new, open, etc buttons).
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Scroll bar useful for buffer size/position at-a-glance.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------

;; todo: initial-buffer-choice vs spydez/auto-open-list???

(load "finalize.el")
;; fin
