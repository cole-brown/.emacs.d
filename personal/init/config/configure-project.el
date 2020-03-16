;; -*- mode: emacs-lisp; lexical-binding: t -*-


;; Just try helm-projectile:
;; C-c p h


;;---
;; Helm Projectile Commands:
;;---
;; C-c p h: All-in-one command (helm-projectile)
;;   Usage: This command, by default, is the combination of these 5 commands:
;;     - helm-projectile-switch-to-buffer
;;     - helm-projectile-find-file
;;     - helm-projectile-switch-project
;; C-c p p: Enter project portal (helm-projectile-switch-project)
;;   Usage: This is the very first command you need to use before using other
;;   commands, because it is the entrance to all of your projects and the only
;;   command that can be used outside of a project, aside from
;;   helm-projectile-find-file-in-known-projects. The command lists all visited
;;   projects. If you first use Projectile, you have to visit at least a project
;;   supported by Projectile to let it remember the location of this project.
;;   The next time you won't have to manually navigate to that project but jump
;;   to it instantly using helm-projectile-switch-project.
;;
;; Lots more: https://tuhdo.github.io/helm-projectile.html
;;   Bit too verbose for here right now.

;;---
;; Projectile Commands:
;;---
;; C-c p f: Find file in current project
;; C-c p p: Switch project
;; C-c p s g: Grep in project
;; C-c p r: Replace in project
;; C-c p m: Invoke a command via the Projectile Commander
;; C-c p m ?: Projectile Commander: get help for Projectile Commander
;;
;; many many more: https://projectile.readthedocs.io/en/latest/usage/


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; TODO: get grep working? C-c p s g
;; Or ag? C-c p s s
;; is ag any good? Better than grep? Better than grep that doesn't work?
;; Well it's also an external tool so... *shrug*
;;   https://github.com/ggreer/the_silver_searcher
;;   Needs an emacs package too. Not sure which one Projectile wants.
;;
;; TODO: hm... M-x lgrep works. M-x grep not so much. M-x rgrep no.


;;------------------------------------------------------------------------------
;; Projectile
;;------------------------------------------------------------------------------
;; https://github.com/bbatsov/projectile
;; https://www.projectile.mx/en/latest/
;; https://wikemacs.org/wiki/Projectile
;; https://tuhdo.github.io/helm-projectile.html

;; You can go one step further and set a list of folders which Projectile is
;; automatically going to check for projects:
;; https://projectile.readthedocs.io/en/latest/usage/
;; (setq projectile-project-search-path '("~/projects/" "~/work/"))


;; http://pages.sachachua.com/.emacs.d/Sacha.html#org2bcc47a
;; the package itself
(use-package projectile
  ;; deadgrep wants to wait for this to get in the keybind map
  :demand t

  ;; :delight '(:eval (concat " " (projectile-project-name)))
  ;; ...That doesn't work. Ok then. I guess set it in config.

  ;;---
  :bind-keymap
  ;;---
  ("C-c p" . projectile-command-map)

  ;;---
  :custom
  ;;---
  ;; mode line: trying this for now:
  ;;   orig: " Projectile[<proj>:<type>]"
  ;;   curr: " P[<proj>:<type>]"
  ;; ;; May want to get rid of type if it's not useful. (.emacs.d is `generic', really?!)
  ;; (setq ;; projectile-dynamic-mode-line '(:eval (format "[%s]" (projectile-project-name)))
  ;;  projectile-mode-line-prefix " P")
  ;; ;;(projectile-project-type))
  (projectile-mode-line-prefix " P"
   "Reduce minor-mode-line a lot - takes up too much space by default.")

  ;; Completion system used by Projectile. Default is `ido' but we changed to... `default'?
  ;; Should I set this to `helm'? Not sure.
  (projectile-completion-system 'default "See comment.")

  (projectile-enable-caching t "Caching is good, right?")

  ;; ;; Using Emacs Lisp for indexing files is really slow on Windows. To enable
  ;; ;; external indexing, add this setting. The alien indexing method uses
  ;; ;; external tools (e.g. git, find, etc) to speed up the indexing process.
  ;; (setq projectile-indexing-method 'alien)
  ;; ;; Not sure if this works better than `native' or not...
  ;; ;; `native' is default in widows, `alien' is native on other `system-type'.
  ;;
  ;; TRIAL [2019-09-09]
  ;; Going back to native to see if maybe that makes deadgrep work better?
  (projectile-indexing-method 'native
                              "`alien' would be faster but I'm on windows 7...")

  ;; I don't have any ignored yet... Maybe `third-party' if dirs can be ignored.
  ;; (add-to-list 'projectile-globally-ignored-files "node-modules")

  ;;---
  :config
  ;;---
  (projectile-global-mode))

;; integrate with Helm
;; demos and such: https://tuhdo.github.io/helm-projectile.html
(use-package helm-projectile
  :after (helm projectile)
  ;; Adds itself to projectile-command-map? It's at: C-c p h

  :config
  ;; Do I need (helm-projectile-on)? I'm getting C-c p h anyways, so... Dunno?
  (helm-projectile-on))


;;------------------------------------------------------------------------------
;; Taskspace Manager
;;------------------------------------------------------------------------------

(use-package taskspace
  ;; My own personal package - do not package manager it.
  :ensure nil


  ;;-------
  :init
  ;;-------

  (defun spydez/taskspace/gen-org-notes (taskname taskpath)
    "NOTE: Could be redefined later for more work-specific details, so check 
e.g. 'finalize-domain-secret.el' for a redef. Or 'C-h f
spydez/taskspace/gen-org-notes' and see what file it's defined
in.
"
    ;; Format:
    ;; spy-header snippet key
    ;;
    ;; taskname
    ;; taskpath
    ;;
    ;; 'mkdir cmd'
    ;;
    ;; fancy box to separate this stuff from start of normal notes
    (format (concat "%s\n" ;; header
                    "\n"
                    "%s\n" ;; taskname
                    "%s\n" ;; taskpath
                    "\n"
                    "%s\n" ;; mkdir
                    "\n"
                    "%s\n" ;; fancy box top
                    "%s\n" ;; fancy box middle
                    "%s\n" ;; fancy box bottom
                    "\n\n")
            "spy-header"
            taskname
            taskpath
            (format "mkdir ~/temp/%s" taskname)
            "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
            "     ├┼┼┤                             ...                              ├┼┼┤"
            "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"
            ))
  ;; (spydez/taskspace/gen-org-notes "" "")


  ;;-------
  :custom
  ;;-------
  (taskspace/type :noteless)
  (taskspace/dir/remote-notes (spydez/path/to-dir spydez/dir/roam
                                                  "taskspace"
                                                  spydez/dev/system/hash))
  (taskspace/datetime/format spydez/datetime/format/yyyy-mm-dd)
  ;; (taskspace/shell-fn #'shell) ;; leave as default
  (taskspace/dir spydez/dir/taskspace-root)

  ;; (taskspace/dir/copy-files-src ...) ;; don't have any right now

  (taskspace/gen-files-alist
   ;; projectile: empty file
   '((".projectile" "")
     ;; notes.org: setup with org header snippet ready to go
     (taskspace/file-name/notes spydez/taskspace/gen-org-notes)))

  ;; (taskspace/dir/always-ignore ...) ;; may have to adjust soon?

  ;; (taskspace/dir-name/separator ...)
  ;; (taskspace/dir-name/parts-alists ...)
  ;; (taskspace/dir-name/valid-desc-regexp ...)
  )


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: Check out "eyebrowse" package?
;;   http://manuel-uberti.github.io/emacs/2018/03/10/moody-and-minions/
;;     https://manuel-uberti.github.io/emacs/2017/08/06/eyebrowse/
;;       https://github.com/wasamasa/eyebrowse
;;     https://github.com/manuel-uberti/.emacs.d/blob/5c1dcfb8c34e2f5de7af7c7a20a85276d8075baa/lisp/mu-mode-line.el

;; TODO: What needs done to get projectile running on this repo?
;; TODO: What needs done to get projectile running on work repo?
;; TODO: Start trying out those `C-c p ...' commands
;; TODO: save projectile-bookmarks.eld in git? move it to secrets?
;;       - one per system-name, probably...
;;       - ignore here, save in secrets, one per sys is probably a good call...
;;       - might have to pay attention to this bug: https://github.com/bbatsov/projectile/issues/989


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-project)
