;; -*- emacs-lisp -*-


;; TODO: an encrypted git repo (git crypt or something?) for my emacs.secrets
;;   that has stuff like all my github keys, other stuff. Use 2fa and/or
;;   password manager (1Pass or something)?

;;------------------------------------------------------------------------------
;; General version control settings
;;------------------------------------------------------------------------------

;; TODO: check out the documentation for these, maybe move some here?
;; from bootstrap-backups
;; (setq version-control t)
;; (setq vc-make-backup-files t)


;;------------------------------------------------------------------------------
;; TODO: something for figuring out what git(hub) user I am for current buffer?
;;------------------------------------------------------------------------------

;;---
;; Multiple git users / Multiple SSH keypairs
;;---
;; https://stackoverflow.com/questions/3860112/multiple-github-accounts-on-the-same-computer
;;
;; https://code.tutsplus.com/tutorials/quick-tip-how-to-work-with-github-and-multiple-accounts--net-22574
;;   1) SSH key per Github account
;;      - ssh-keygen -t rsa -C "your-email-address"
;;   2) Log into Github and attach new key to its account.
;;      - Note that my created keys are in one place and the key used by the
;;        GitHub app is in an entirely different place on my computer's HDD.

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


;;------------------------------------------------------------------------------
;; subversion? I do still use this...
;;------------------------------------------------------------------------------

;; todo: which should take precedence if in a svn repo that I also made a git repo.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-version-control)
