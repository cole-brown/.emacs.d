;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;;---
;; Identity / Personal Information
;;---
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/User-Identification.html
;; TODO: change these in a projectile hook or something for repo-dependant git
;;       identity?
;; TODO: but only change after secrets are loaded so as not to leak irrelevant
;;       work email addrs
(setq user-full-name "Cole Brown"
      user-mail-address "git@spydez.com")
;; user-login-name exists if needed

;; TODO: Some consts for significant files or folders to jump to for e.g.
;;       opening a work project file.
;;       That... should not be in bootstrap. If I even still want it.


;;---
;; Undo
;;---
(defconst spydez/undo-limit 160000
  "Upscale the default soft undo-limit. 80kB isn't a lot, so double it?")
(defconst spydez/undo-strong-limit (* 2 spydez/undo-limit)
  "Upscale default hard undo-limit as well. 120kB -> double soft limit.")


;;---
;; Dev Env vars
;;---
;; TODO: put in bootstrap-consts? bootstrap-this-late? bootstrap-data?
;; dev-consts? dev-dev-env? dev-dev-env-envs?
;; development-environment-for-this-device-consts?

;; Tab widths. I like a smaller one for verbose/highly indented code (web dev
;; mainly). Normally use a larger one for non-web programming.
(defconst spydez/dev-env/tab/min 2
  "Small tab width for more compact but readable code.")
(defconst spydez/dev-env/tab/normal 4
  "Normal tab width for more usual use cases for code languages like C++, C#.")
(defconst spydez/dev-env/fill-column/normal 80
  "Normal tab width for more usual use cases for code languages like C++, C#.")
(defconst spydez/dev-env/fill-column/long 100 ;; 120
  ;; 120 would be nice, but 2 equal panes on 1080p monitor is more like 100
  ;; (~109 actual, 100 to be safer)
  "Normal tab width for more usual use cases for code languages like C++, C#.")

;; TODOS:
;;  bootstrap-this-early, bootstrap-this-late?
;;  copy external tools to bootstrap-this-late for this pc
;;    - make default/empty here
;;    - do tool exec/env step after bootstrap (or at end of bootstrap?)


;;---
;; External Tools
;;---
;; Do I want to do the "find git" on windows like this or like in the Windows
;; PATH env var? Associative List of tool symbol to... tool path.
(defconst spydez/tools/external
  '(
    ;; configure-shell wants bash (git bash (same path as diff))
    ;; here and in Windows PATH atm... (debugging eshell)
    ("bash" . "")

    ;; Emacs EPA (EasyPG Assistant) should have latest GPG version
    ;; TODO: add in a check for version numbers when checking for tools?
    ("gpg" . "")

    ;; magit wants git and diff
    ("git" . "") ; in windows system env var PATH  right now
    ("diff" . "")
    )
  "An alist for tool name -> exec path. These will be
 front-to-back appended to list, so if e.g. there's several git
 binaries and only one will work, put git in front of this
 alist."
  ;; If I need more than a pair or triple tuple:
  ;;   Options for Structured Data in Emacs Lisp:
  ;;     https://nullprogram.com/blog/2018/02/14/
  )


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-consts)
