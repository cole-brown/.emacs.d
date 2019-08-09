;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; use-package git repo has helpful readme: https://github.com/jwiegley/use-package

;;------------------------------------------------------------------------------
;; Initial setup of packages and use-package.
;;------------------------------------------------------------------------------

;; Set package and use-package for downloading/using use-package.
;; http://cachestocaches.com/2015/8/getting-started-use-package/

(require 'package)

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Set some use-packages vars
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgf1ae4f6

;; use-package options:

;; verbose is useful when debugging startup. Might gain a bit of time disabling this when
;; not debugging. (Maybe look into setting based on spydez/debugging-p?)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0)

;; Not sure if we want this disabled...
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
(setq package-enable-at-startup nil)
;; Everyone seems to always use this anyways. Ensure packages are installed if missing.
(setq use-package-always-ensure t)

;; This will set defer to default true for packages to not load on startup.
;; Would make for a faster init time but slower random actions until everything
;; happens to be auto-loaded.
;; (setq use-package-always-defer t)


;;------------------------------------------------------------------------------
;; Package Setup
;;------------------------------------------------------------------------------

;; http -> https ?? Trying to get use-package and unkillable-scratch to work
;; right now so going back to http after briefly trying https...
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Keep it down to just elpa and melpa for now.
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;;
;; Package Archives I Know of Right Now:
;; - Default:
;;   - gnu          : http://elpa.gnu.org/packages/
;; - Popular:
;;   - melpa        : http://melpa.org/packages/
;;   - melpa-stable : http://stable.melpa.org/packages/
;; - New (to me) / Less Popular:
;;   - marmalade    : http://marmalade-repo.org/packages/
;;   - org          : http://orgmode.org/elpa/
;;   - ELPA         : http://tromey.com/elpa/
;;   - elpy         : http://jorgenschaefer.github.io/packages/
;; eval for current: C-x C-e package-archives
;;
;; Gathered from these folks and other places:
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#org47943cc
;;   http://nhoffman.github.io/.emacs.d/#orgf46780c

;; Can use `package-pinned-packages' to choose which packages come from where.

;; Can use `package-archive-priorities' to assign priority levels (integers?)
;; to package archives (by name). Can also set `package-menu-hide-low-priority'
;; to hide in UI.

(package-initialize)


;;------------------------------------------------------------------------------
;; Install Use-Package When Needed
;;------------------------------------------------------------------------------

;;---
;; Install If...
;;---
;; Automatic install if not found.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;---
;; Old Require: Just Require
;;---
;; (require 'use-package)
;;
;;---
;; New Require: use-package.el is no longer needed at runtime
;;---
;;   "This means you should put the following at the top of your Emacs, to
;; further reduce load time:"
;;   - https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))


;;-
;; There's a neat "check if use-package is installed, if not ask, if no make a
;; macro that just warns that it's not installed"...
;; But it is GPL so I had to chuck it.
;; See:
;;   http://nhoffman.github.io/.emacs.d/#orgf46780c
;;-


;;-
;; (use-package is now bootstrapped)
;;-


;;---
;; Post Use-Package Bootstrap
;;---
;; Packages I want ASAP.

;; https://github.com/emacscollective/auto-compile
;; Pull in auto-compile here, set to on-load mode.
(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; auto-compile options: recompile if the uncompiled (.el) is
;; newer than the compiled (.elc)
(setq load-prefer-newer t)

;; no-littering to clean up .emacs.d a bit
(use-package no-littering
  :demand t
  ;;:config
  ;;   Leave `no-littering-etc-directory' and `no-littering-var-directory'
  ;;   at default: .emacs.d/etc and .emacs.d/var
  )
;; Make it so `M-x package-autoremove' doesn't try to remove this...
;; Seems it's complicated: https://github.com/purcell/emacs.d/issues/415
;; But hand editting it into custom.el:package-selected-packages does it.
;;
;; Well... Been a week or three and done some autoremoves and its still ok.
;; I think maybe my custom.el didn't get saved once.


;;------------------------------------------------------------------------------
;; Custom File
;;------------------------------------------------------------------------------
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

;; Custom file is annoying, especially because it gathers everything into one
;; weird place. Also because it insists on having only one place (when it
;; saves). So some stuff I want early (so custom.el has to be early), and other
;; stuff I want late (like gpg) and can't do much about.
;;
;; I'm hoping this will relegate custom.el to sorta just be package.el's vars
;; (basically ignorable totally as far as I care) by using:
;;   1) use-package's ':custom' section
;;      e.g. https://github.com/a13/emacs.d
;;   2) (customize-set-variable var ...)
;;
;;   X) NOTE: Do /not/ use (custom-set-variables ...). I've tried and it hasn't
;;      worked well yet...
;;
;; So this is a try at that. Have no-littering squirrel it away somewhere I
;; don't care and hope weird settings don't creep in and mess things up without
;; me noticing.
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)

;; No-Littering note:
;;   They have a suggestion for storing custom file in a no-littering dir, but I
;; think I need no-littering loaded first, which means this must be after
;; everything that comes before that.
;;
;;   Previously, custom.el was always in my source control. I think this is
;; better now as the only settings I had in it as saved (that I didn't init
;; myself) were `package-selected-packages' and `custom-safe-themes'.

;;---
;; Old Way
;;   - Was just below `(require 'bootstrap-debug-early)' in init.el.
;;---
;; ;; Some packages want to write to our custom file, so set that up first. An
;; ;; unadorned filename (just "custom.el") wasn't getting picked up as the custom
;; ;; file, so expanded:
;; (setq custom-file (expand-file-name "custom.el" spydez/dir/emacs/personal))
;; ;; May need a better setter if custom-file needs adjusted per computer...
;; ;;
;; ;; Possibly move custom-file setting up, and loading down below loading of
;; ;; bootstrap-this-late overrides.
;; (load custom-file t)


;;------------------------------------------------------------------------------
;; Update/Upgrade Packages Process
;;------------------------------------------------------------------------------

;; TODO: make this into a help command I can call that'll do the pop open
;;   thing a help function does? Or a hook into use-package function?
;; If you get an error like this on installing a new package:
;;   Error (use-package): Failed to install magit: http://melpa.org/packages/magit-20190113.1949.tar: Not found
;;   Error (use-package): Cannot load magit
;; Just evalulate this again: (package-refresh-contents)


;; TODO: make this into a help command I can call that'll do the pop open
;;   thing a help function does?
;; There are automated ways to upgrade, but I'm thinking manual for now...
;;   M-x list-packages
;;   'U' to mark upgrades
;;   'x' to upgrade?
;; After it's all done, maybe run:
;;   M-x package-autoremove
;;
;; Almost what I want in a function?:
;;   https://emacs.stackexchange.com/questions/16398/noninteractively-upgrade-all-packages

;; Options for auto-update, or packages for helping update:
;;   https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package
;;   https://github.com/Malabarba/paradox

;; http://nhoffman.github.io/.emacs.d/#orgf46780c
;; Some useful ELPA variables and functions:
;;   M-x package-list-packages  - open list of packages
;;   package-activated-list     - variable containing list of the names of currently activated packages
;;   package-install            - install a package
;;   package-installed-p        - true if package is installed


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-package)
