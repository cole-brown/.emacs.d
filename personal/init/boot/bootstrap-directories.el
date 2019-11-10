;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; Package Archives
;;-----------------------------------------------------------------------------


;;---
;; Not-Mine Packages
;;---

(defconst spydez/dir/packages/all-but-mine
  (spydez/path/to-dir spydez/dir/emacs "packages")
  "Emacs 'packages' directory parent for various package sources: elpa, git
  submodules, git subtrees, manually copied files, etc.")


(defconst spydez/dir/packages/elpa
  (spydez/path/to-dir spydez/dir/packages/all-but-mine "elpa")
  "The standard Emacs Lisp Package Archive.")

;; And tell emacs we moved it.
(customize-set-variable 'package-user-dir spydez/dir/packages/elpa)


(defconst spydez/dir/packages/manual
  (spydez/path/to-dir spydez/dir/packages/all-but-mine "manual")
  "These were copy/pasted from somewhere and I don't want to fold them into my
  own code or make them my own.")


(defconst spydez/dir/packages/git-subtrees
  (spydez/path/to-dir spydez/dir/packages/all-but-mine "subtrees")
  "These are git subtrees. Git subtree vs module is different
  enough for me to want to differentiate now so I know later...")


(defconst spydez/dir/packages/git-submodules
  (spydez/path/to-dir spydez/dir/packages/all-but-mine "submodules")
  "These are git modules. Git subtree vs module is different
  enough for me to want to differentiate now so I know later...")


;;---
;; Mine!
;;---
;; "Packages"...
;; Basically a big enough little feature that I moved it away from my files,
;; removed all "spydez/*" and gave it a bit of love.

(defconst spydez/dir/personal/packages
  (spydez/path/to-dir spydez/dir/emacs/personal "packages")
  "Custom/personal emacs 'packages' directory.")


;;------------------------------------------------------------------------------
;; Other Emacs Dirs
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
  (spydez/path/to-dir spydez/dir/emacs/personal "snippets")
  "My Yasnippets directory.")
;; Could add an override of my own snippets if needed.

(defconst spydez/dir/packages/use-tool
  (spydez/path/to-dir spydez/dir/personal/packages "use-tool")
  "use-tool directory.")

(defconst spydez/dir/packages/taskspace
  (spydez/path/to-dir spydez/dir/personal/packages "taskspace")
  "taskspace directory.")


;;-----------------------------------------------------------------------------
;; Add to Load Path
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
;; Secrets Directories
;;------------------------------------------------------------------------------

;; TODO-SECRETS: Change from spydez/.../secrets... to secrets/... ??
;;   ...not convinced.

(defconst spydez/dir/secrets
  (spydez/path/to-dir spydez/dir/home ".secrets.d")
  "Location of secrets dir on this computer.")

(defconst spydez/dir/secrets/classified
  (spydez/path/to-dir spydez/dir/secrets "classified")
  "Location of secrets dir on this computer.")

(defconst spydez/file/classified/emacs-general
  (spydez/path/to-file spydez/dir/secrets/classified "emacs.classified.el.gpg")
  "Location of emacs' elisp secrets.")

(defconst spydez/dir/secrets/dev
  (spydez/path/to-dir spydez/dir/secrets "dev")
  "Dev config outside of .emacs.d.")

(defconst spydez/dir/secrets/dev/defaults
  (spydez/path/to-dir spydez/dir/secrets/dev "defaults")
  "All of my optional/default setup elisp files...")

(defconst spydez/dir/secrets/dev/domain-all
  (spydez/path/to-dir spydez/dir/secrets/dev "domains")
  "Domains folder. For subdirs of work, home, etc.")

(defconst spydez/dir/secrets/dev/domain-this
  (spydez/path/to-dir spydez/dir/secrets/dev/domain-all
                      spydez/dev/domain/name)
  "Anything that has to be domain specific. Tab widths or whatnot.")

(defconst spydez/dir/secrets/dev/system-all
  (spydez/path/to-dir spydez/dir/secrets/dev "computers")
  "Computers folder. For subdirs of different computers.")

(defconst spydez/dir/secrets/dev/system-this
  (spydez/path/to-dir spydez/dir/secrets/dev/system-all
                      spydez/dev/system/hash)
  "Anything that has to be computer specific.
Overriding tab widths or whatnot.")

;; (make-directory spydez/dir/secrets/dev/system-this t)


;;------------------------------------------------------------------------------
;; Add Secrets to Load-Path
;;------------------------------------------------------------------------------

;; NOTE: these are all higher priority than any .emacs.d path, right now,
;; so take care.
;; Leaving Off: spydez/dir/secrets/dev
(add-to-list 'load-path spydez/dir/secrets/dev/defaults)
(add-to-list 'load-path spydez/dir/secrets/classified)
(add-to-list 'load-path spydez/dir/secrets/dev/domain-all)
(add-to-list 'load-path spydez/dir/secrets/dev/domain-this)
(add-to-list 'load-path spydez/dir/secrets/dev/system-all)
(add-to-list 'load-path spydez/dir/secrets/dev/system-this)
;; Add highest-priority/most specific to this computer last.


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-directories)
