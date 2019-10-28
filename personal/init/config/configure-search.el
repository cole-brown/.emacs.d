;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; If emacs grep or <package>-grep are looking in dirs you don't want, try
;; adding to this list: `grep-find-ignored-directories'
;; e.g. from http://nhoffman.github.io/.emacs.d/#org9119f86
;;   - Add ".eggs" and "src" to list for python development.


;;------------------------------------------------------------------------------
;; ripgrep & deadgrep - grep faster
;;------------------------------------------------------------------------------

;;-----
;; ripgrep
;;-----
;; https://github.com/BurntSushi/ripgrep
;; Installed:
;;   binary: ripgrep-11.0.2-x86_64-pc-windows-gnu
;;   date:   [2019-08-30]
;; Installed GNU version as MSVC version needs DLLs or something so meh.

;;   "ripgrep is a line-oriented search tool that recursively searches your
;; current directory for a regex pattern. By default, ripgrep will respect your
;; .gitignore and automatically skip hidden files/directories and binary files.
;; ripgrep has first class support on Windows, macOS and Linux, with binary
;; downloads available for every release. ripgrep is similar to other popular
;; search tools like The Silver Searcher, ack and grep."

;; TODO: Add a use-tool here for making sure that ripgrep is installed?
;;   sub-TODO: see what happens for deadgrep when it has no ripgrep?


;;-----
;; deadgrep
;;-----
;; https://github.com/Wilfred/deadgrep
;; "Deadgrep is the fast, beautiful text search that your Emacs deserves."
;; Trial: [2019-08-30]
(use-package deadgrep
  :demand t

  ;;---
  :bind ;; global
  ;;---
  ;; Try binding to F5?
  ;; Maybe replace projectile search or something if don't like F5?
  (("<f5>" . #'deadgrep))

  ;;---
  :bind ;; deadgrep-mode-map
  ;;---
  (:map deadgrep-mode-map
        ;; Ok'd: [2019-08-27] -> [2019-09-24]
        ;; Kill buffer instead of quit.
        ;; TRIAL: [2019-10-28]
        ;; kill-or-quit instead of original quit-or-kill?
        ("q" . kill-this-buffer))

  ;;---
  :bind ;; projectile-command-map
  ;;---
  (:map projectile-command-map
        ;; Also map into the projectile search group
        ;; (projectile-grep, projectile-deadgrep, et al):
        ("s d" . deadgrep)))


;;------------------------------------------------------------------------------
;; Anzu - Show Number of Matches in Mode-Line While Searching
;;------------------------------------------------------------------------------
(use-package anzu
  ;; Force load cuz ':bind' wants to defer til one of those is called.
  ;; But I want it loaded for normal searchs too (M-s, etc).
  :demand t

  ;; TODO: mess with colors?

  ;; TODO: give it a tab or something in moody?

  ;;---
  :custom
  ;;---
  (anzu-replace-to-string-separator "→")

  ;;---
  :bind
  ;;---
  ;; Rebind to Anzu versions.
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))

  ;;---
  :config
  ;;---
  ;; and enable
  (global-anzu-mode))


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

;; If using venv (virtual env), try using advice-add to dynamically put the
;; project's venv dir into the grep ignored list.
;; (Have to add it to each grep function: rgrep, <package>-grep...)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: try out ag / silver searcher
;;   - if no, ack?

;; TODO: get grep working?
;;   M-x rgrep
;;   projectile-grep
;;   helm-projectile-grep
;;   ...

;; TODO: try out this visual help for emacs-style regex
;;   https://github.com/benma/visual-regexp.el
;; Also check out their visual-regexp-setroids for normal-style
;; (perl style? modern style?) regexs.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-search)

