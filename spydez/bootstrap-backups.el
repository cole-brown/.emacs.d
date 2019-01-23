;; -*- emacs-lisp -*-

;;------------------------------------------------------------------------------
;; Backups, auto-saves, etc.
;;------------------------------------------------------------------------------

;; Stuff taken from:
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://emacs.stackexchange.com/questions/33/put-all-backups-into-one-backup-folder
;; and Sacha.org.

;; By default, Emacs saves backup files in the current directory. These are the files ending
;; in ~ that are cluttering up your directory lists.
;; Move where they get stored.
(setq backup-directory-alist `(("." . ,spydez/dir/backup-files)))

;; Seems to be the safest. No symlink issues?
(setq backup-by-copying t)

;; This or similar will keep the number of backups in check.
;(setq delete-old-versions t
;  kept-new-versions 6
;  kept-old-versions 2
;  version-control t)

;; ...but disk space is cheap. Let's try having all of them.
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

;; Also pull auto-saves out to their own folder.
(setq auto-save-file-name-transforms `((".*" ,spydez/dir/auto-save-files t)))

;; Ask tramp to behave as well.
(setq tramp-backup-directory-alist `((".*" ,spydez/dir/auto-save-files t)))
(setq tramp-auto-save-directory spydez/dir/auto-save-files)

;; TODO: do I want this?
;; "It is nice to have commands and their history saved so that every time
;;  you get back to work, you can just re-run stuff as you need it."
;;  - http://pages.sachachua.com/.emacs.d/Sacha.html#orgdfcb533
;; https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
(setq savehist-file spydez/file/save-history)
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-backups)
