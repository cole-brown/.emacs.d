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

  ;;--------------------------
  :bind-keymap
  ;;--------------------------
  ("C-c p" . projectile-command-map)

  ;;--------------------------
  :custom
  ;;--------------------------
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

  ;;--------------------------
  :config
  ;;--------------------------
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
;; Bufler - Buffer Management
;;------------------------------------------------------------------------------
;; https://www.reddit.com/r/emacs/comments/fatjdw/annrfc_buflerel_group_buffers_into_workspaces/
;; https://github.com/alphapapa/bufler.el#commands
;; Trial [2020-03-18]
(use-package bufler
  :after helm
  :demand t
  ;; bufler is opening an interactive window for something my init is doing right now...
  ;; :disabled

  ;; ;;--------------------------
  ;; :custom
  ;; ;;--------------------------


  ;; ;;--------------------------
  ;; :config
  ;; ;;--------------------------
  ;; 
  ;; ;; bufler-defgroups should go here...
  ;; ;; (setf bufler-groups (bufler-defgroups ...))
  )

;; Commands:    https://github.com/alphapapa/bufler.el#commands
;;   bufler
;;       Show the Bufler buffer list.
;;
;;   bufler-mode
;;       Enable the Bufler workspace mode, which allows each frame to have a
;;       chosen workspace from Bufler’s groups.
;;
;;   bufler-tabs-mode
;;       Enable the Bufler workspace tabs mode, which uses tab-bar-mode and
;;       tab-line-mode from Emacs 27+ to display Bufler workspaces and buffers.
;;
;;   bufler-switch-buffer
;;       Switch to a buffer selected from the frame’s workspace. With prefix,
;;       select from all buffers. With two prefixes, also set the frame’s
;;       workspace.
;;
;;   bufler-workspace-frame-set
;;       Set the frame’s workspace. Setting the workspace may be done
;;       automatically by bufler-switch-buffer, but this command may be used to
;;       set the workspace to a group containing other groups, after which
;;       bufler-switch-buffer will present buffers from the selected group and
;;       its subgroups.
;;
;;   bufler-workspace-buffer-set
;;       Set the current buffer’s workspace name. With prefix, unset it. Note
;;       that, in order for a buffer to appear in a named workspace, the buffer
;;       must be matched by an auto-workspace group before any other group.

;; Bindings
;;
;; In the Bufler buffer list, these keys are available (use C-h m to get the
;; most up-to-date listing). They operate on all buffers in the section at
;; point.
;;
;;   ?         : Show key bindings Hydra.
;;   1 – 4     : Cycle section levels at point.
;;   M-1 – M-4 : Cycle top-level sections.
;;   RET       : Switch to buffer.
;;   SPC       : Peek at buffer, keeping focus in buffer list.
;;   g         : Refresh Bufler list (with prefix, force updating
;;             : buffers’ VC state and grouping).
;;   f         : Set the current frame’s workspace to the group
;;             : at point (with prefix, unset).
;;   F         : Make a new frame whose workspace is the group at point.
;;   N         : Add buffers to named workspace (with prefix, remove from it).
;;   k         : Kill buffers.
;;   s         : Save buffers.


;; TODO: This is fucking up way back in bootstrap-packages?! Its autoload during `package-initialize' wants helm to be up and running already.
;;
;; (use-package helm-bufler
;;   :after (helm bufler)
;; 
;;  :demand t
;;   ;;---
;;   :config
;;   ;;---
;; 
;;   ;; hook into Helm
;;   ;; (helm :sources '(helm-bufler-source))
;;   (setq helm-mini-default-sources '(helm-source-buffers-list
;;                                     helm-bufler-source
;;                                     helm-source-recentf
;;                                     helm-source-buffer-not-found)))


;;------------------------------------------------------------------------------
;; Taskspace Manager
;;------------------------------------------------------------------------------

(use-package taskspace
  ;; My own personal package - do not package manager it.
  :ensure nil

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; General (Non-Per-Domain) Init...
  ;;---
  (defun spydez/taskspace/generate (taskname taskpath)
    "NOTE: Could be redefined later for more work-specific details, so check
e.g. 'finalize-domain-secret.el' for a redef. Or 'C-h f
spydez/taskspace/generate' and see what file it's defined
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
                    "#+TASKSPACE: %s\n" ;; taskpath
                    "%s\n" ;; taskname
                    "\n"
                    "%s\n" ;; mkdir
                    "\n"
                    "%s\n" ;; fancy box top
                    "%s\n" ;; fancy box middle
                    "%s\n" ;; fancy box bottom
                    "\n\n")
            "spy-header"
            taskpath
            taskname
            (format "mkdir ~/temp/%s" taskname)
            "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
            "     ├┼┼┤                             ...                              ├┼┼┤"
            "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"
            ))
  ;; (spydez/taskspace/generate "" "")


  ;;---
  ;; "Home" Domain
  ;;---
  (spydez/jerky/set 'custom 'taskspace 'dir 'notes :home
                    :value (spydez/path/to-dir
                            (spydez/dirky/path :default :roam)
                            "taskspace"
                            (spydez/dirky/domain/key-to-str :home))
                    :docstr "dir for home taskspace notes")

  (spydez/jerky/set 'custom 'taskspace 'dir 'root :home
                    :value (spydez/dirky/path :home :taskspace)
                    :docstr "dir for home taskspace data/files")

  (spydez/jerky/set 'custom 'taskspace 'dir 'root :home
                    :value (spydez/dirky/path :home :taskspace)
                    :docstr "dir for home taskspace data/files")

  ;; alias it here; (re)define it later.
  (defalias 'spydez/taskspace/generate/home 'spydez/taskspace/generate)
  (spydez/jerky/set 'custom 'taskspace 'gen-files-alist :home
                    :value 'spydez/taskspace/generate/work
                    :docstr "Alist for what to generate for new taskspace.")


  ;;---
  ;; "Work" Domain
  ;;---
  (spydez/jerky/set 'custom 'taskspace 'dir 'notes :work
                    :value (spydez/path/to-dir
                            (spydez/dirky/path :default :roam)
                            "taskspace"
                            (spydez/dirky/domain/key-to-str :work))
                    :docstr "dir for home taskspace notes")

  (spydez/jerky/set 'custom 'taskspace 'dir 'root :work
                    :value (spydez/dirky/path :work :taskspace)
                    :docstr "dir for home taskspace data/files")

  ;; alias it here; (re)define it later.
  (defalias 'spydez/taskspace/generate/work 'spydez/taskspace/generate)
  (spydez/jerky/set 'custom 'taskspace 'gen-files-alist :work
                    :value 'spydez/taskspace/generate/work
                    :docstr "Alist for what to generate for new taskspace.")


  ;;------------------------------
  :custom
  ;;------------------------------
  (taskspace/type :noteless)
  (taskspace/dir/remote-notes (spydez/path/to-dir
                               (spydez/dirky/path :default :roam)
                               "taskspace"
                               spydez/dev/domain/name))
  (taskspace/datetime/format spydez/datetime/format/yyyy-mm-dd)
  ;; (taskspace/shell-fn #'shell) ;; leave as default
  (taskspace/dir (spydez/dirky/path nil :taskspace))

  ;; (taskspace/dir/copy-files-src ...) ;; don't have any right now

  (taskspace/gen-files-alist
   ;; projectile: empty file
   '((".projectile" "")
     ;; notes.org: setup with org header snippet ready to go
     (taskspace/file-name/notes spydez/taskspace/generate)))

  ;; (taskspace/dir/always-ignore ...) ;; may have to adjust soon?

  ;; (taskspace/dir-name/separator ...)
  ;; (taskspace/dir-name/parts-alists ...)
  ;; (taskspace/dir-name/valid-desc-regexp ...)


  ;;------------------------------
  :config
  ;;------------------------------


  ;;---
  ;; Context Switching...
  ;;---
  (defun spydez/taskspace/around (domain func)
    "Basically an ':around' advice, but temporary. Sets taskspace
custom vars in lexical scope from jerky kvs before taskspace
function call."
    (let ((taskspace/dir/remote-notes (spydez/jerky/get 'custom 'taskspace
                                                        'dir 'notes domain))
          (taskspace/dir (spydez/jerky/get 'custom 'taskspace
                                           'dir 'root domain))
          (taskspace/gen-files-alist (spydez/jerky/set
                                      'custom 'taskspace
                                      'gen-files-alist domain)))
      (call-interactively func)
      ))

  ;;---
  ;; Hydra For Contexts...
  ;;---

  (with-feature 'hydra
    (defhydra spydez/hydra/taskspace (:color blue ;; default exit heads
                                      :idle 0.75  ;; no help for x seconds
                                      :hint none) ;; no hint - just docstr
      "
^Work^                                        | ^Home^
^----^----------------------------------------+-^----^---------------------------------------
_w n_: ?w n?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ | _h n_: ?h n?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_w v_: ?w v?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ | _h v_: ?h v?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_w d_: ?w d?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ | _h d_: ?h d?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_w p_: ?w p?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ | _h p_: ?h p?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_w t_: ?w t?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ | _h t_: ?h t?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_w w_: ?w w?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ | _h w_: ?h w?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"
      ;;------------------------------------------------------------------------
      ;; Work Work...
      ;;------------------------------------------------------------------------

      ("w n" (funcall #'spydez/taskspace/around :work #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("New" "taskspace/create")))
      ("w v" (funcall #'spydez/taskspace/around :work #'taskspace/notes)
       (apply #'format "%-12s: %-24s"
              '("Visit" "taskspace/notes")))
      ("w d" (funcall #'spydez/taskspace/around :work #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Dired" "taskspace/dired")))
      ("w p" (funcall #'spydez/taskspace/around :work #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Parent/Root" "taskspace/parent-dired")))
      ("w t" (funcall #'spydez/taskspace/around :work #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Task Name" "taskspace/task-name/dwim")))
      ("w w" (funcall #'spydez/taskspace/around :work #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Task Dir" "taskspace/task-dir/dwim")))

      ;;------------------------------------------------------------------------
      ;; Home... Work?..
      ;;------------------------------------------------------------------------

      ("h n" (funcall #'spydez/taskspace/around :home #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("New" "taskspace/create")))
      ("h v" (funcall #'spydez/taskspace/around :home #'taskspace/notes)
       (apply #'format "%-12s: %-24s"
              '("Visit" "taskspace/notes")))
      ("h d" (funcall #'spydez/taskspace/around :home #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Dired" "taskspace/dired")))
      ("h p" (funcall #'spydez/taskspace/around :home #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Parent/Root" "taskspace/parent-dired")))
      ("h t" (funcall #'spydez/taskspace/around :home #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Task Name" "taskspace/task-name/dwim")))
      ("h w" (funcall #'spydez/taskspace/around :home #'taskspace/create)
       (apply #'format "%-12s: %-24s"
              '("Task Dir" "taskspace/task-dir/dwim")))
      ))

  ;; global keybind (can override minor mode binds)
  ;; (bind-key* "C-," #'spydez/hydra/taskspace/body)

  ;; Bind the hydra's entry to something more typable.
  (defalias 'spydez/taskspace 'spydez/hydra/taskspace/body))


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
