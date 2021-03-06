;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; Backups, auto-saves, etc.
;;------------------------------------------------------------------------------

;; Some stuff taken from:
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://emacs.stackexchange.com/questions/33/put-all-backups-into-one-backup-folder
;; and Sacha.org.


;;---
;; Auto-Saves
;;---

;; By default, Emacs saves backup files in the current directory. These are the
;; files ending in ~ that are cluttering up your directory lists. Move where
;; they get stored.
(when (spydez/dir/self-policing-p)
  (setq backup-directory-alist `(("." . ,(spydez/dirky/path :emacs :backup-files)))))

;; Also pull auto-saves out to their own folder.
(setq auto-save-file-name-transforms
      (if (spydez/dir/self-policing-p)
          `((".*" ,(spydez/dirky/path :emacs :auto-save-files) t)) ;; self-police? my auto-save location
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))) ;; else no-littering's


;;---
;; Backups
;;---

;; Seems to be the safest. No symlink issues?
(setq backup-by-copying t)

;; This or similar will keep the number of backups in check.
;; (setq delete-old-versions t
;;       kept-new-versions 6
;;       kept-old-versions 2
;;       version-control t)

;; ...but disk space is cheap. Let's try having all of them.
(setq delete-old-versions -1
      version-control t
      vc-make-backup-files t)


;;---
;; Tramp
;;---

;; Ask tramp to behave as well.
(when (spydez/dir/self-policing-p)
  (setq tramp-auto-save-directory   (spydez/path/to-dir
                                     (spydez/dirky/path :default :emacs.d)
                                     "tramp" "auto-save"))
  (setq tramp-persistency-file-name (spydez/path/to-file
                                     (spydez/dirky/path :default :emacs.d)
                                     "tramp" "persistency.el")))


;;---
;; History
;;---

;; "It is nice to have commands and their history saved so that every time
;;  you get back to work, you can just re-run stuff as you need it."
;;  - http://pages.sachachua.com/.emacs.d/Sacha.html#orgdfcb533
;; https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
(when (spydez/dir/self-policing-p)
  (setq savehist-file spydez/file/save-history))
(savehist-mode 1)
(setq history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; TODO: consider this `real-auto-save' mode if needed. Don't think it's needed
;; at this time though.
;;   https://github.com/ChillarAnand/real-auto-save
;; Found in: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html#sec-10-2-3


;;---
;; Desktop
;;---
;; Seems pretty popular but I dunno.
;; Found in: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html#sec-10-2-3
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;; https://www.emacswiki.org/emacs/Desktop
;; TODO: Try this out? Not convinced.
;; (desktop-save-mode 1)
;; (setq desktop-restore-eager 10)


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-backups)
