;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; Package Archives
;;-----------------------------------------------------------------------------


;; This is the directory where all packages obtained from elsewhere are kept.
(let ((packages-dir (list :emacs.d "packages")))
  ;; Now set up more stuff for init, package setup, load-path, etc...

  ;;---
  ;; Not-Mine Packages
  ;;---
  (spydez/dirky/add :init :packages/elpa
                  ;; Minor shenanigans to append to end of  list...
                  (apply #'append packages-dir (cons "elpa" nil) nil)
                  "The standard Emacs Lisp Package Archive.")

  ;; Ye Olde Way (Copy/Paste)
  (spydez/dirky/add :init :packages/manual
                    ;; Minor shenanigans to append to end of  list...
                    (apply #'append packages-dir (cons "manual" nil) nil)
                    (concat "These were copy/pasted from somewhere and I "
                            "don't want to fold them into my own code or "
                            "make them my own."))

  ;; Ye Newe Way (git sub-repos)
  (spydez/dirky/add :init :packages/subtrees
                    ;; Minor shenanigans to append to end of  list...
                    (apply #'append packages-dir (cons "subtrees" nil) nil)
                    "Git Subtrees in my .emacs.d Git Repo.")
  (spydez/dirky/add :init :packages/submodules
                    ;; Minor shenanigans to append to end of  list...
                    (apply #'append packages-dir (cons "submodules" nil) nil)
                    "Git Submodules in my .emacs.d Git Repo.")

  ;;---
  ;; Mine!
  ;;---
  ;; "Packages"...
  ;; Basically a big enough little feature that I moved it away from my files,
  ;; removed all "spydez/*" and gave it a bit of love.

  (spydez/dirky/add :load-path :personal/packages
                    '(:personal "packages")
                    "Custom/personal emacs 'packages' directory."))


;; And tell emacs we moved elpa.
(customize-set-variable 'package-user-dir
                        (spydez/dirky/path :init :packages/elpa))


;;------------------------------------------------------------------------------
;; Other Emacs Dirs
;;------------------------------------------------------------------------------

;;---
;; No Littering vs the Self-Police
;;---
;; These should all be protected by if/when spydez/dir/self-policing-p sexprs.
;; They are put in no-littering's var or etc right now.

;; Folders for auto-save files and backup-files (#*# and *~)
(spydez/dirky/add :emacs :backup-files
                  '(:emacs.d "backups")
                  "Full path for backups before/sans no-littering package.")

(spydez/dirky/add :emacs :auto-save-files
                  '(:emacs.d "auto-save-list")
                  "Auto-save location before/sans no-littering package.")

;; §-TODO-§ [2020-07-09]: Not used? Get rid of if so.
;; (spydez/dirky/add :emacs :save-history
;;                   '(:emacs.d "savehist")
;;                   "History of commands, etc.")


;;---
;; My Lisp
;;---

(spydez/dirky/add :emacs :yasnippets
                  '(:personal "snippets")
                  "My Yasnippets directory.")
;; Could add an override of my own snippets if needed.
(spydez/dirky/path :emacs :yasnippets)


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
;; (add-to-list 'load-path (spydez/dirky/path :default :emacs.d))

;; Personal packages.
(add-to-list 'load-path (spydez/path/to-dir
                         (spydez/dirky/path :load-path :personal/packages)
                         "use-tool"))
(add-to-list 'load-path (spydez/path/to-dir
                         (spydez/dirky/path :load-path :personal/packages)
                         "taskspace"))

;; Need these ordered for overrides to work correctly, so don't just do the
;; dirky list in whatever order it's in.

 ;; non-init; don't care about and should be overridable.
(add-to-list 'load-path (spydez/dirky/path :load-path :lisp))


;; Defaults first so everything else overrides.
(add-to-list 'load-path (spydez/dirky/path :load-path :dev/defaults))
(add-to-list 'load-path (spydez/dirky/path :default   :personal))
;; This one isn't in dirky since it has nothing in it... It would be
;; .emacs.d/personal/init
;; (add-to-list 'load-path (spydez/dirky/path :load-path :init))

;; Now get into the actual bulk of emacs init and setup.
;; Do we include zeroth or not? I think sure... for now.
(add-to-list 'load-path (spydez/dirky/path :load-path :zeroth))
(add-to-list 'load-path (spydez/dirky/path :load-path :boot))
(add-to-list 'load-path (spydez/dirky/path :load-path :config))
(add-to-list 'load-path (spydez/dirky/path :load-path :finalize))

;; Overrides start here. Most specific to this computer last.
;; Don't have anything here now... skipping: .emacs.d/personal/dev/domains/
;; (add-to-list 'load-path (spydez/dirky/path :load-path :domains))
;; Don't have anything here now... skipping: .emacs.d/personal/dev/computers
;; (add-to-list 'load-path (spydez/dirky/path :load-path :systems))
(add-to-list 'load-path (spydez/dirky/path :load-path :dev/domain))
(add-to-list 'load-path (spydez/dirky/path :load-path :dev/system))


;;------------------------------------------------------------------------------
;; Secrets Directories
;;------------------------------------------------------------------------------

(spydez/dirky/add :secrets :secrets.d
                  '(:home ".secrets.d")
                  "Location of secrets dir on this computer.")

(spydez/dirky/add :secrets :classified
                  '(:secrets.d "classified")
                  "Location of more secret stuff?")

(spydez/dirky/add :secrets :dev/defaults
                  '(:secrets.d "dev" "defaults")
                  "All of my optional/default setup elisp files...")

(spydez/dirky/add :secrets :dev/domains
                  '(:secrets.d "dev" "domains")
                  "Domains folder. For subdirs of work, home, etc.")

(spydez/dirky/add :secrets :dev/domain
                  '(:dev/domains spydez/dev/domain/name)
                  (concat "Anything that has to be domain specific. "
                          "Tab widths or whatnot."))

(spydez/dirky/add :secrets :dev/systems
                  '(:secrets.d "dev" "computers")
                  "Computers folder. For subdirs of different computers.")

(spydez/dirky/add :secrets :dev/system
                  '(:dev/systems spydez/dev/system/hash)
                  (concat "Anything that has to be computer specific. "
                          "Overriding tab widths or whatnot."))

;; (make-directory (spydez/dirky/path :secrets :dev/system) t)


;;------------------------------------------------------------------------------
;; Add Secrets to Load-Path
;;------------------------------------------------------------------------------

;; NOTE: these are all higher priority than any .emacs.d path, right now,
;; so take care.
;; Leaving Off: spydez/dir/secrets/dev
(add-to-list 'load-path (spydez/dirky/path :secrets :dev/defaults))
(add-to-list 'load-path (spydez/dirky/path :secrets :classified))
(add-to-list 'load-path (spydez/dirky/path :secrets :dev/domains))
(add-to-list 'load-path (spydez/dirky/path :secrets :dev/domain))
(add-to-list 'load-path (spydez/dirky/path :secrets :dev/systems))
(add-to-list 'load-path (spydez/dirky/path :secrets :dev/system))
;; Add highest-priority/most specific to this computer last.


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-directories)
