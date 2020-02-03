;; -*- mode: emacs-lisp; lexical-binding: t -*-


;; magit commit flow:
;;  1) C-x v d         -> magit status buffer
;;  2) s or S on stuff -> stage changes
;;  3) c c             -> commit buffers (commit message and commit diff)
;;  4) C-c C-c         -> Done editting commit message - commit it.

;; magit push to origin flow:
;;  1) C-x v d         -> magit status buffer
;;  2) P               -> (p)ush popup
;;  3) u               -> Push to (u)pstream

;; TODO: magit rebase flow?

;; TODO: magit commit whitespace change? Do I have to turn whitespace back on?

;; magit ignore fuckery:
;; http://mbork.pl/2018-10-14_Magit_and_C-u_i
;; TL;DR: Magit status, press i, choose 'local' (instead of 'global') expecting
;;   .gitignore update... Nope. It's hiding somewhere weird and fucked up.
;;     .git/info/execlude
;; So... Choose global maybe? Just do it in git CLI? IDK.

;; TODO: an encrypted git repo (git crypt or something?) for my emacs.secrets
;;   that has stuff like all my GitHub keys, other stuff. Use 2fa and/or
;;   password manager (1Pass or something)?

;; TODO: magit is supposed to auto-notice changes that happen in emacs, right?
;;   - if so, it's not auto-updating changes I do when magit-status
;;     is up and it should be fixed...

;; TODO: a spinner/throbber for pushing to remote? like this package has nice ones:
;;   https://github.com/Malabarba/spinner.el

;;------------------------------------------------------------------------------
;; General version control settings
;;------------------------------------------------------------------------------

;; TODO: check out the documentation for these, maybe move some here?
;; from bootstrap-backups
;; (setq version-control t)
;; (setq vc-make-backup-files t)


;;------------------------------------------------------------------------------
;; GitHub
;;------------------------------------------------------------------------------
;; TODO: something for figuring out what git(hub) user I am for current buffer?

;;---
;; Notes: Multiple git users / Multiple SSH keypairs
;;---
;; https://stackoverflow.com/questions/3860112/multiple-github-accounts-on-the-same-computer
;;
;; https://code.tutsplus.com/tutorials/quick-tip-how-to-work-with-github-and-multiple-accounts--net-22574
;;   1) SSH key per GitHub account
;;      - ssh-keygen -t rsa -C "your-email-address"
;;   2) Log into GitHub and attach new key to its account.
;;      - Note that my created keys are in one place and the key used by the
;;        GitHub app is in an entirely different place on my computer's HDD.
;;        - GitHub app uses c:/Users/<user>/.ssh whereas I use c:/home/<user>/.ssh currently.
;;   3) Fail everything, go on a wile goose chase. Blame GitHub desktop app vs
;;      git bash, then https vs ssh, then old personal vs new company vs new person
;;      accounts/credentials/voodoo rituals...
;;      - `ssh -T git@github.com` works for seeing who GitHub thinks you are when SSHing.
;;        https://help.github.com/articles/testing-your-ssh-connection/
;;      - `git remote -v` is a nice little command for verifying the remote works for push/pull.
;;   4) ???
;;   5) Google-fu
;;      - This maybe? https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/
;;      - It does matter https vs SSH (e.g. remote origin urls of git@github.com vs https://), maybe.
;;      - You do have to create it on GitHub before you can push it to GitHub.
;;   6) It works.


;;------------------------------------------------------------------------------
;; git
;;------------------------------------------------------------------------------
;; It's all the rage.

;;---
;; Git Messenger
;;---
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgfb77d93
;; Trial: [2019-01-25]
(use-package git-messenger
  ;;---
  :bind ;; global
  ;;---
  (("C-x v m" . git-messenger:popup-message)))
;; Oh wow - that's awesome. Commit message for line at point.
;; TODO: Way to include author in there?

;; Note: I cannot find a person on the internet that has asked about magit and
;; git's IncludeIf. I have repos in HOME set to use one name, and a default
;; global other name. Magit only knows the default global - same for git in eshell.
;; For now, setting a repo-local user.name, user.email for non-default repos.
;;   In git bash in .emacs.d:
;;     $ git config user.name "Cole Brown"
;;     $ git config user.email "git@spydez.com"

;; Don't show whitespace in diff, but show context
;; (setq vc-diff-switches '("-b" "-B" "-u"))
;; (setq vc-git-diff-switches nil)
;; TODO: why set vc-diff-switches instead of vc-git-diff-switches?


;;---
;; Git Gutter (Fringe (+?))
;;---

(when (display-graphic-p)
  (use-package git-gutter-fringe
    :ensure t

    :delight (git-gutter-mode " g" git-gutter-fringe)

    :config
    (global-git-gutter-mode)))

;; TODO: try out diff-hl? Would be nice to have for SVN, but git won the war so
;; SVN is probably on the way to extinct...
;; If that doesn't work, use the 'fringe' version of gutter or gutter+ that's
;; more popular? (non-+ have been updated more recently)


;;------------------------------------------------------------------------------
;; magit
;;------------------------------------------------------------------------------
;; Might as well have its own section why not...

;; http://pages.sachachua.com/.emacs.d/Sacha.html#magit
;; https://github.com/magit/magit

;; No one I've checked in my references has a super simple magit setup to get
;; started on, so... starting with it bare.
;;   https://magit.vc/

;; Alright. First step is to let magit know where msys git or GitHub's git shell
;; or whatever I'm using is, exactly. Because I installed magit and got a ton of
;; this type of warning:
;;   In toplevel form:
;;   git-rebase.el:73:1:Error: Searching for program: No such file or directory, git
;;   In toplevel form:
;;   magit-apply.el:36:1:Error: Searching for program: No such file or directory, git
;; After some googling... Maybe this:
;;   https://stackoverflow.com/questions/26620312/installing-git-in-path-with-github-client-for-windows
;;     - I'm adding this git to my path for now.
;; I should just install a Git in a better place, but now I want to figure this out...
;;
;; Yep - that worked. Added path to PortableGit/cmd dir to end of system PATH var.

;; TODO: some sort of throbber or status buffer or something for magit pushing to upstream?
;;  - I get bored and distracted waiting...

;; TODO: check out these funcs?
;; "When you press C-c C-g, magit-status runs full-screen, but when you press q,
;; it restores your previous window setup. Very handy."
;;   - https://zzamboni.org/post/my-emacs-configuration-with-commentary/#other-tools
;; (use-package magit
;;   :diminish auto-revert-mode
;;   :bind
;;   (("C-c C-g" . magit-status)
;;    :map magit-status-mode-map
;;    ("q"       . magit-quit-session))
;;   :config
;;   (defadvice magit-status (around magit-fullscreen activate)
;;     "Make magit-status run alone in a frame."
;;     (window-configuration-to-register :magit-fullscreen)
;;     ad-do-it
;;     (delete-other-windows))
;;
;;   (defun magit-quit-session ()
;;     "Restore the previous window configuration and kill the magit buffer."
;;     (interactive)
;;     (kill-buffer)
;;     (jump-to-register :magit-fullscreen)))

(use-package magit
  ;;-----
  :custom
  ;;-----

  ;; Show word differences, not just "line has changed".
  ;; https://emacs.stackexchange.com/questions/43643/magit-how-to-show-differences-within-lines
  (magit-diff-refine-hunk 'all
                          "Show word differences, not just 'line has changed'.")

  ;;-----
  :config
  ;;-----

  ;; NOTE: See spydez/tools/external for current git/diff check.
  ;;; If on windows and don't currently know where git is, check spydez/exe/git-path.
  ;;; It may be the answer, or maybe we just complain.
  ;;; todo: Change to check this (executable-find "git")
  ;;;   - in finalize-sanity (also sanity for "diff")
  ;; (when (and (equal system-type 'windows-nt)
  ;;         (not (boundp 'magit-git-executable)))
  ;;   (if (boundp 'spydez/exe/git-path)
  ;;    (setq magit-git-executable spydez/exe/git-path)
  ;;     (error "init/configure-version-control:: use-package magit: no git path known?")))

  ;; TODO: Consider this. But the performance warnings seem a bit... dire.
  ;; "Function: magit-after-save-refresh-status
  ;;   "This function is intended to be added to after-save-hook. After doing
  ;; that the corresponding status buffer is refreshed whenever a buffer is
  ;; saved to a file inside a repository.
  ;;   "Note that refreshing a Magit buffer is done by re-creating its contents
  ;; from scratch, which can be slow in large repositories. If you are not
  ;; satisfied with Magit’s performance, then you should obviously not add this
  ;; function to that hook."
  ;;   - https://magit.vc/manual/magit/Automatic-Refreshing-of-Magit-Buffers.html

  ;;-----
  :bind ;; global
  ;;-----
  ;; Some Defaults: https://www.emacswiki.org/emacs/VersionControl
  ;; I want to overwrite some of those, I think? magit-status is superior to
  ;; vc-directory. Or should git be the VC, and magit be its own special thing?
  ;; Going with overwrite for now.
  ;; TODO: consider "C-x g" for magit-status?
  (("C-x v d" . magit-status)
   ("C-x v p" . magit-push)
   ;; ("C-x v C-d" . my/magit-status-in-directory)
   ;; ("C-x v c" . my/magit-commit-all) ;; this overrides vc "cancel"... any better one to steal?
   ))


;; Trial  : [2019-01-25]
;; Trial 2: [2019-09-24]
;;   - A new magit big version (added 'transient'), broke this so I disabled and
;;     didn't have any chance to try out.
;; (use-package magit-gh-pulls
;;   :after (magit))
;; Trial 2 result:
;;   No. Not maintained (1+ year to last commit).

;; ;; From http://pages.sachachua.com/.emacs.d/Sacha.html#magit
;; ;; From http://endlessparentheses.com/merging-github-pull-requests-from-emacs.html
;; (defun endless/load-gh-pulls-mode ()
;;   "Start `magit-gh-pulls-mode' only after a manual request."
;;   (interactive)
;;   (require 'magit-gh-pulls)
;;   (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;;   (magit-gh-pulls-mode 1)
;;   (magit-gh-pulls-reload))


;; Trying out "Forge" from Magit author(s).
;; https://github.com/magit/forge
;; Trial: [2019-09-24]
;;
;; [2019-09-24]: Failure for now... Needs a compiler on Windows which would want me to
;; install... MinGW? And that's borking around on stuff that's already wonky on
;; my old, creaky Windows 7 install.
;; (use-package forge
;;    :after (magit))


;;------------------------------------------------------------------------------
;; Git Merge Conflicts
;;------------------------------------------------------------------------------

;; smerge instead of ediff?

;;   " If you visit a file with a conflict from a magit diff section it will
;; automatically activate smerge for you.
;;
;;   "I have this in my config to have a nicer interface/keybindings."
;; - https://www.reddit.com/r/emacs/comments/d3jf8o/git_merge_conflicts/f03qs34?utm_source=share&utm_medium=web2x
;; TODO: Try this out when I have a merge conflict that needs fixing.
;; (use-package smerge-mode
;;   :hook (magit-diff-visit-file . (lambda ()
;;                                    (when smerge-mode
;;                                      (hydra-smerge/body))))
;;   :config
;;   (defhydra hydra-smerge
;;     (:color pink :hint nil :post (smerge-auto-leave))
;;     "
;; ^Move^       ^Keep^               ^Diff^                 ^Other^
;; ^^-----------^^-------------------^^---------------------^^-------
;; _n_ext       _b_ase               _<_: upper/base        _C_ombine
;; _p_rev       _u_pper              _=_: upper/lower       _r_esolve
;; ^^           _l_ower              _>_: base/lower        _k_ill current
;; ^^           _a_ll                _R_efine
;; ^^           _RET_: current       _E_diff
;; "
;;     ("n" smerge-next)
;;     ("p" smerge-prev)
;;     ("b" smerge-keep-base)
;;     ("u" smerge-keep-upper)
;;     ("l" smerge-keep-lower)
;;     ("a" smerge-keep-all)
;;     ("RET" smerge-keep-current)
;;     ("\C-m" smerge-keep-current)
;;     ("<" smerge-diff-base-upper)
;;     ("=" smerge-diff-upper-lower)
;;     (">" smerge-diff-base-lower)
;;     ("R" smerge-refine)
;;     ("E" smerge-ediff)
;;     ("C" smerge-combine-with-next)
;;     ("r" smerge-resolve)
;;     ("k" smerge-kill-current)
;;     ("q" nil "cancel" :color blue)))


;;------------------------------------------------------------------------------
;; Auto/Easy Commit of Certain Docs Folders
;;------------------------------------------------------------------------------

(defcustom spydez/dir/git/auto-commit-locations
  (list
   spydez/dir/doc-save-secrets
   spydez/dir/doc-save-vault)
  "List of strings of directories (TODO: add allowance for single
files?) to automatically add/commit/push in their respective git
repos."
  :group 'spydez/group
  :type '(restricted-sexp :tag "String List"
                          :match-alternatives
                          (lambda (x) (and (listp x)
                                           (seq-every-p #'stringp x)))))


(defcustom spydez/dir/git/watch-locations
  (list
   spydez/dir/emacs
   spydez/dir/secrets
   spydez/dir/doc-save-vault)
  "List of strings of directories (TODO: add allowance for single
files?) to automatically add/commit/push in their respective git
repos."
  :group 'spydez/group
  :type '(restricted-sexp :tag "String List"
                          :match-alternatives
                          (lambda (x) (and (listp x)
                                           (seq-every-p #'stringp x)))))


(defun spydez/magit/changes-in-subdir (subdir-abs)
  "Determines if magit knows of any changes (staged, unstaged, untracked), and
if any of them are in this (absolute path to) a sub-dir of the repo. Could be
repo root, but a subdir of the repo is what magit can't handle with e.g.
`magit-anything-modified-p'.
"
  ;; Magit works on `default-directory', so make sure to set that.
  (let* ((default-directory subdir-abs)
         ;; these are all changes in repo, not subdir
         (changes-rel (append (magit-staged-files)
                              (magit-unstaged-files)
                              (magit-untracked-files)))
         (git-root (magit-toplevel))
         (changes-abs (mapcar (lambda (x) (spydez/path/to-file git-root x))
                              changes-rel)))

    ;; Now just filter and return.
    (seq-filter (lambda (x) (string-prefix-p subdir-abs x)) changes-abs)))


;; Could make this async if it takes too long... With messages going
;; to "§- a buffer -§"...
(require 'f)
(require 'subr-x)
(defun spydez/magit/auto-commit ()
  "For each item in `spydez/dir/git/auto-commit-locations', use
Magit to: add files, commit, and push.
"
  (interactive)

  ;; §-TODO-§ [2020-02-03]: replace `message' with `mis/message/propertize'

  ;; Either have to require magit here, or set magit to ":demand t" in
  ;; use-package. Trying out requiring here as magit isn't the fastest to start.
  (require 'magit)

  (let (results)
    ;; walk our list of auto-commit loctaions
    (dolist (location spydez/dir/git/auto-commit-locations)
      ;; Change the default-directory just for this scope...
      (let ((default-directory (if (f-dir? location)
                                   location
                                 (f-dirname location)))
            ;; some silly little message
            (commit-message (format "%s: %s triggered in %s by %s"
                                    (format-time-string
                                     spydez/datetime/format/yyyy-mm-dd_hh-mm-ss)
                                    "auto-commit"
                                    "emacs/magit"
                                    "spydez/magit/auto-commit function."))
            (change-list (spydez/magit/changes-in-subdir location)))

        (message "Checking %s..." location)
        ;; Magit works on `default-directory', so we are checking status
        ;; on our repo with this.
        (if (null change-list)
            (progn
              ;; Save that nothing happened.
              (push (cons location nil) results)
              ;; Say why nothing happened.
              (message "  No changes to auto-commit: %s" default-directory))

          ;; Else, commit changes.
          (let ((change-str (string-join ;; join results with comma
                          ;; gather up all changed files, strip out dir prefix
                          (mapcar (lambda (x)
                                    (string-remove-prefix default-directory x))
                                  change-list)
                          ", ")))
            ;; Add!
            (message "  Adding changes found: %s..." change-str)

            ;; "add <path>" or "add -A ." work to add untracked.
            ;; "add -A ." == "add ." + "add -u ."
            ;; "add ." only adds modified.
            (magit-call-git "add" "-A" ".")

            ;; Commit!
            (message "  Committing changes: %s..." change-str)
            ;; Don't 'commit all' ("commit -a"), so we can commit just whatever
            ;; sub-folder we are in.
            (magit-call-git "commit" "-m" commit-message)

            ;; Push?
            (message "  Pushing changes: %s..." change-str)
            ;; Assume an origin of "origin", I guess?
            ;; Could also just "push" to default...
            (magit-call-git "push" "origin")

            ;; Done. Until I find all the edge cases I guess.
            ;; Like when push fails?
            (message "  Committed and pushed (probably): %s" change-str)

            (push (cons location change-str) results)))))

    (message "Auto-Commit ran on %s locations: \n%s"
             (length results)
             ;; format for each: "path: changed.el, file.py, list.txt"
             (string-join
              (mapcar (lambda (x) (format "  %s: %s"
                                          (car x)
                                          (if (null (cdr x))
                                              "None."
                                            (cdr x))))
                      results)
              "\n"))))
;; (spydez/magit/auto-commit)


(defun spydez/magit/check-status (mis/minibuffer-echo mis/msg-type)
  "For each item in `spydez/dir/git/watch-locations', use
Magit to look for uncommitted(/unpushed?) changes.
"
  (interactive)

  ;; Either have to require magit here, or set magit to ":demand t" in
  ;; use-package. Trying out requiring here as magit isn't the fastest to start.
  (require 'magit)

  ;; Walk our list of auto-commit loctaions.
  (dolist (location spydez/dir/git/watch-locations)
    ;; Change the default-directory just for this scope...
    (let ((default-directory (if (f-dir? location)
                                 location
                               (f-dirname location)))
          (change-list (spydez/magit/changes-in-subdir location)))

      (mis/message/propertize mis/minibuffer-echo
                              mis/msg-type :highlight
                              "Checking %s..." location)
      ;; Magit works on `default-directory', so we are checking status
      ;; on our repo with this.
      (if (null change-list)
          ;;---
          ;; Say nothing happened here.
          ;;---
          (mis/message/propertize mis/minibuffer-echo
                                  mis/msg-type :text
                                  "  %s changes in: %s..."
                                  "No"
                                  default-directory)
        ;;---
        ;; Else, note changes.
        ;;---

        ;; Summary
        (mis/message/propertize mis/minibuffer-echo
                                mis/msg-type :text
                                "  %s changes in: %s..."
                                (length change-list)
                                default-directory))

      ;; Details
      (dolist (change-path change-list)
        (mis/message/propertize mis/minibuffer-echo
                                mis/msg-type :text
                                "    - %s"
                                (string-remove-prefix default-directory
                                                      change-path))))

        (mis/message/propertize mis/minibuffer-echo mis/msg-type ""))

  (mis/message/propertize mis/minibuffer-echo
                          mis/msg-type :highlight
                          "Checked status on %s locations."
                          (length spydez/dir/git/watch-locations)))
;; (spydez/magit/check-status t '(spydez homeward))


;;------------------------------------------------------------------------------
;; Subversion? I do still use this...
;;------------------------------------------------------------------------------

;; TODO: which should take precedence if in a svn repo that I also made a git
;; repo. Probably SVN because this is a rare edge-case.

;; magit's `git svn' extension:
;; https://github.com/magit/magit-svn
;; TODO: Start using that if/when I have to move a work repo from svn to git.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-version-control)
