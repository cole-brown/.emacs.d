;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;;---
;; No Littering vs the Self-Police
;;---
;; These should all be protected by if/when spydez/dir/self-policing-p sexprs.
;; They are put in no-littering's var or etc right now.

;; folders for auto-save files and backup-files (#*# and *~)
(defconst spydez/dir/backup-files
  (spydez/path/to-dir spydez/dir/emacs "backups")
  "Full path before no-littering package.")

(defconst spydez/dir/auto-save-files
  (spydez/path/to-dir spydez/dir/emacs "auto-save-list"))

(defconst spydez/file/save-history
  (spydez/path/to-file spydez/dir/emacs "savehist")
  "History of commands, etc.")


;;---
;; My Lisp
;;---

(defconst spydez/dir/yasnippets
  (spydez/path/to-file spydez/dir/emacs/personal "snippets")
  "My Yasnippets directory.")
;; Could add an override of my own snippets if needed.

(defconst spydez/dir/personal/packages
  (spydez/path/to-file spydez/dir/emacs/personal "packages")
  "Custom/personal emacs 'packages' directory.")

(defconst spydez/dir/packages/use-tool
  (spydez/path/to-file spydez/dir/personal/packages "use-tool")
  "use-tool directory.")

(defconst spydez/dir/packages/taskspace
  (spydez/path/to-file spydez/dir/personal/packages "taskspace")
  "taskspace directory.")


;;-----------------------------------------------------------------------------
;; Load Path
;;-----------------------------------------------------------------------------

;; Reset to orginal first. We had some subset in for bootstrapping. Now we're
;; ready for the full set.
;; TODO-EASY: sanity check? boundp and set to anything...
(setq load-path spydez/dir/load-path/orig)

;; TODO-reorg-done: updated this comment?
;; Load-Path dirs, and places for overrides to exist (in ascending order):
;;   ./personal/dev/defaults/
;;   ./personal/
;;   ./personal/dev/domains/[work, home, whatever]
;;   ./personal/dev/computers/[pfo-dead-beef, home-1234-abcd, whatever]
;; (Assuming default dir names for personal, etc.)

;; This is setting priorities for overrides towards the front/head/car of
;; load-path (add-to-list does this for us).
;;
;; Don't use .emacs.d.
;; https://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path
;; (add-to-list 'load-path spydez/dir/emacs)

;; Personal packages.
(add-to-list 'load-path spydez/dir/packages/use-tool)
(add-to-list 'load-path spydez/dir/packages/taskspace)

 ;; non-init; don't care about and should be overridable.
(add-to-list 'load-path spydez/dir/personal/lisp)

;; Defaults first so everything else overrides.
(add-to-list 'load-path spydez/dir/dev/defaults)
(add-to-list 'load-path spydez/dir/emacs/personal)
(add-to-list 'load-path spydez/dir/personal/init)

;; Now get into the actual bulk of emacs init and setup.
;; Do we include zeroth or not? I think sure... for now.
(add-to-list 'load-path spydez/dir/init/zeroth)
(add-to-list 'load-path spydez/dir/init/boot)
(add-to-list 'load-path spydez/dir/init/config)
(add-to-list 'load-path spydez/dir/init/finalize)

;; Overrides start here. Most specific to this computer last.
(add-to-list 'load-path spydez/dir/dev/domain-all)
(add-to-list 'load-path spydez/dir/dev/system-all)
(add-to-list 'load-path spydez/dir/dev/domain-this)
(add-to-list 'load-path spydez/dir/dev/system-this)
;; TODO-reorg-done: more in the load path? new dirs (dev, init...)?


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-directories)
