;; -*- emacs-lisp -*-

;; use-package git repo has helpful readme: https://github.com/jwiegley/use-package

;;------------------------------------------------------------------------------
;; Initial setup of packages and use-package.
;;------------------------------------------------------------------------------

;; Set package and use-package for downloading/using use-package.
;; http://cachestocaches.com/2015/8/getting-started-use-package/

(require 'package)

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

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Keep it down to just elpa and melpa for now.
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; old: just require
;; (require 'use-package)
;;
;; new: use-package.el is no longer needed at runtime
;; This means you should put the following at the top of your Emacs, to further
;; reduce load time:
;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))

;; use-package is now bootstrapped.

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
;; TODO: Make it so `M-X package-autoremove' doesn't try to remove this...
;; Seems it's complicated: https://github.com/purcell/emacs.d/issues/415
;; But hand editting it into custom.el:package-selected-packages does it.

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


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-package)
