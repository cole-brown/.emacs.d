;; -*- emacs-lisp -*-


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

;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgfb77d93
;; Trial: [2019-01-25]
(use-package git-messenger
  :bind (("C-x v m" . git-messenger:popup-message)))
;; Oh wow - that's awesome. Commit message for line at point.
;; TODO: Way to include author in there?

;; Note: I cannot find a person on the internet that has asked about magit and
;; git's IncludeIf. I have repos in HOME set to use one name, and a default
;; global other name. Magit only knows the default global - same for git in eshell.
;; For now, setting a repo-local user.name, user.email for non-default repos.
;;   In git bash in .emacs.d:
;;     $ git config user.name "Cole Brown"
;;     $ git config user.email "git@spydez.com"

;; TODO: what is git-gutter and do I want it?

;; I guess mostly we're just using magit...
;; So..... Next section!:


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

(use-package magit
  :config
  ;;---
  ;; git setup
  ;;---
  ;; NOTE: See spydez/tools/external for current git/diff check.
  ;;; If on windows and don't currently know where git is, check spydez/exe/git-path.
  ;;; It may be the answer, or maybe we just complain.
  ;;; todo: Change to check this (executable-find "git")
  ;;;   - in finalize-sanity (also sanity for "diff")
  ;(when (and (equal system-type 'windows-nt)
  ;	     (not (boundp 'magit-git-executable)))
  ;  (if (boundp 'spydez/exe/git-path)
  ;	(setq magit-git-executable spydez/exe/git-path)
  ;    (error "init/configure-version-control:: use-package magit: no git path known?")))

  ;;---
  ;; Diff options
  ;;---

  ;; TODO: not working currently. Does my external `diff` not know inline diffing?
  ;; Show word differences, not just "line has changed".
  ;; https://emacs.stackexchange.com/questions/43643/magit-how-to-show-differences-within-lines
  (setq magit-diff-refine-hunk 'all
	magit-diff-section-arguments '("--ignore-space-change"))
  ;; but for that to work I need it to find the diff exe...
  ;;(executable-find "diff")
  ;;(executable-find "git")
  ;; So we set that up in the spydez/tools/external bootstrap.

  :bind
  ;; some defaults: https://www.emacswiki.org/emacs/VersionControl
  ;; I want to overwrite some of those, I think? magit-status is superior to vc-directory.
  ;; Or should git be the VC, and magit be its own special thing? Going with overwrite for now.
  ;; todo: consider "C-x g" for magit-status? 
  (("C-x v d" . magit-status)
   ("C-x v p" . magit-push)
   ;("C-x v C-d" . my/magit-status-in-directory)
   ;("C-x v c" . my/magit-commit-all) ;; this overrides vc "cancel"... any better one to steal?
   )
  )

;; Trial: [2019-01-25]
(use-package magit-gh-pulls)

;; From http://pages.sachachua.com/.emacs.d/Sacha.html#magit
;; From http://endlessparentheses.com/merging-github-pull-requests-from-emacs.html
(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))


;;------------------------------------------------------------------------------
;; subversion? I do still use this...
;;------------------------------------------------------------------------------

;; todo: which should take precedence if in a svn repo that I also made a git repo.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-version-control)
