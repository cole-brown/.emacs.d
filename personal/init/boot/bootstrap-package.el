;; -*- emacs-lisp -*-

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
;; not debugging. (Maybe look into setting based on spydez/init-debug?)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0)

;; Not sure if we want this disabled...
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
(setq package-enable-at-startup nil)
;; Everyone seems to always use this anyways. Ensure packages are installed if missing.
(setq use-package-always-ensure t)


;;------------------------------------------------------------------------------
;; Package Setup
;;------------------------------------------------------------------------------

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Keep it down to just elpa and melpa for now.
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;;
;; Even more:
;; http://nhoffman.github.io/.emacs.d/#orgf46780c
;; '(("ELPA" . "http://tromey.com/elpa/")
;;   ("gnu" . "http://elpa.gnu.org/packages/")
;;   ("melpa" . "http://melpa.org/packages/")
;;   ("melpa-stable" . "http://stable.melpa.org/packages/")
;;   ("marmalade" . "http://marmalade-repo.org/packages/")
;;   ("org" . "http://orgmode.org/elpa/")
;;   ("elpy" . "http://jorgenschaefer.github.io/packages/")))
;; eval for current: C-x C-e package-archives

;; Can use package-pinned-packages to choose which packages come from where.

(package-initialize)

;;   "Starting in emacs 25.1, repositories can be assigned a priority, which can
;; be used to hide packages in low priority repositories also represented in
;; higher-priority repositories."
;;   - http://nhoffman.github.io/.emacs.d/#orgf46780c
;; Don't need it now with just two package archives, but if I get more...
;; (setq package-archive-priorities
;;       '(("org" . 30)
;;         ("elpy" . 30)
;;         ("melpa-stable" . 20)
;;         ("marmalade" . 10)
;;         ("gnu" . 10)
;;         ("melpa" . 5)))
;; (setq package-menu-hide-low-priority t)

;;------------------------------------------------------------------------------
;; Install Use-Package When Needed
;;------------------------------------------------------------------------------

;;---
;; Old Install If...
;;---
;; Automatic install if not found.
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

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
;; https://github.com/jwiegley/use-package
;; (eval-when-compile
;;   (require 'use-package))


;;---
;; Newer Install If (Also Require)...
;;---
;;   "This init file is designed to fail gracefully when packages are not yet
;; installed. use-package can automate the installation of missing packages
;; (when :ensure t is specified), or provides a warning that a package is not
;; installed. However, we have a bit of a problem when it comes to use-package
;; itself, which must be installed to avoid failures on startup. Here's the
;; solution that I've arrived at:
;;   - when use-package is not available, give the user the option of
;;     installing it
;;     - if yes, do so
;;     - otherwise, define a macro that generates warning messages wherever
;;       use-package is invoked."
;; from http://nhoffman.github.io/.emacs.d/#orgf46780c
(if (package-installed-p 'use-package)
    ;; if installed, do the thing use-package says to do:
    ;;   "This means you should put the following at the top of your Emacs, to
    ;; further reduce load time:"
    ;;   - https://github.com/jwiegley/use-package
    (eval-when-compile
      (require 'use-package))

  ;; else, do what nhoffman's init does: ask, then install or make empty macro.
  (if (yes-or-no-p "use-package is not installed yet - install it? ")
      ;; then
      (progn
        ;; bootstrap use-package
        (spydez/warning/message '(spydez bootstrap use-package-install)
                                ':warning "Installing use-package...")
        (package-refresh-contents)
        (package-install 'use-package))
    ;; else
    (spydez/warning/message '(spydez bootstrap use-package-install)
                            ':error "Defining fake use-package macro...")
    (defmacro use-package (pkg &rest args)
      ;; could bump down to :warning, but my init leans really heavily on use-package
      (spydez/warning/message nil ':error
                              "use-package is not installed - could not activate %s" (symbol-name pkg))
      )))


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

;; auto-compile options:
(setq load-prefer-newer t)

;; no-littering to clean up .emacs.d a bit
(use-package no-littering
  :demand t
  ;;:config
  ;;   Leave `no-littering-etc-directory' and `no-littering-var-directory'
  ;;   at default: .emacs.d/etc and .emacs.d/var
  )
;; Make it so `M-X package-autoremove' doesn't try to remove this...
;; Seems it's complicated: https://github.com/purcell/emacs.d/issues/415
;; But hand editting it into custom.el:package-selected-packages does it.
;;
;; Well... Been a week or three and done some autoremoves and its still ok.
;; I think maybe my custom.el didn't get saved once.


;;------------------------------------------------------------------------------
;; Update/Upgrade Packages Process
;;------------------------------------------------------------------------------

;; TODO: make this into a help command I can call that'll do the pop open
;;   thing a help function does? Or a hook into use-package function?
;; If you get an error like this on installing a new package:
;;   Error (use-package): Failed to install magit: http://melpa.org/packages/magit-20190113.1949.tar: Not found
;;   Error (use-package): Cannot load magit
;; Just evalulate (package-refresh-contents) again.


;; TODO: make this into a help command I can call that'll do the pop open
;;   thing a help function does?
;; There are automated ways to upgrade, but I'm thinking manual for now...
;;   M-x list-packages
;;   'U' to mark upgrades
;;   'x' to upgrade?
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
