;; -*- emacs-lisp -*-


;; TODO: an encrypted git repo (git crypt or something?) for my emacs.secrets
;;   that has stuff like all my GitHub keys, other stuff. Use 2fa and/or
;;   password manager (1Pass or something)?

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
;;
;; Note: GitHub app uses c:/Users/<user>/.ssh whereas I use c:/home/<user>/.ssh currently.


;;------------------------------------------------------------------------------
;; git
;;------------------------------------------------------------------------------
;; It's all the rage.

;; todo: anything for git in general that isn't magit?


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
;; TODO: Check this path for git executable for making our own better warning?
;;   (defconst spydez/git-path "C:/Users/<user>/AppData/Local/GitHub/PortableGit_<guid>/cmd")
;;
;; Yep - that worked. Added path to PortableGit/cmd dir to end of system PATH var.

(use-package magit)

;;------------------------------------------------------------------------------
;; subversion? I do still use this...
;;------------------------------------------------------------------------------

;; todo: which should take precedence if in a svn repo that I also made a git repo.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-version-control)
