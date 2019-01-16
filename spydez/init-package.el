;; -*- emacs-lisp -*-

;; emacs.el - non-computer-specific emacs init

;;------------------------------------------------------------------------------
;; Initial setup of packages and use-package.
;;------------------------------------------------------------------------------

;; Set package and use-package for downloading/using use-package.
;; http://cachestocaches.com/2015/8/getting-started-use-package/

(require 'package)

;; Set some use-packages vars
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgf1ae4f6

;; use-package options:
(setq use-package-verbose t)
;; Not sure if we want this disabled...
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
(setq package-enable-at-startup nil)
;; Everyone seems to always use this anyways.
(setq use-package-always-ensure t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Keep it down to just elpa and melpa for now.
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; todo: require vs eval-when-compile require?
;;(eval-when-compile
;;  (require 'use-package))
(require 'use-package)

;; https://github.com/emacscollective/auto-compile
;; Pull in auto-compile here, set to on-load mode.
(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; auto-compile options:
(setq load-prefer-newer t)
