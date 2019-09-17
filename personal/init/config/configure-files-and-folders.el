;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces


;;------------------------------------------------------------------------------
;; Better Kill-Matching-Buffer
;;------------------------------------------------------------------------------

;; I could just grub my meaty hooks into kill-matching-buffers, but... eh.
;; I'd probably just forget the function that way.
(defun spydez/buffer/kill-matching (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space (internal
buffers), unless optional prefix argument INTERNAL-TOO is
non-nil. Asks before killing each buffer if it is modified,
unless NO-ASK is non-nil."
  (interactive "sKill buffers matching regex: ")

  ;; TODO: find all matching buffers and do a "does this look about right?"
  ;; prompt?

  (let ((killed-names ()))
    ;; for all open buffers...
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        ;; (message "kill-match plz name:%s e?%s it?%s regex?%s ok?%s"
        ;;          ;; has a name
        ;;          name
        ;;          ;; not empty
        ;;          (not (string-equal name ""))
        ;;          ;; internal-too or not internal
        ;;          (or internal-too (/= (aref name 0) ?\s))
        ;;          ;; matches regex
        ;;          (string-match regexp name)
        ;;          ;; total
        ;;          (and name (not (string-equal name ""))
        ;;               (or internal-too (/= (aref name 0) ?\s))
        ;;               (string-match regexp name)))

        ;; check name obeys `internal-too' type restriction and matches regex
        (when (and name (not (string-equal name ""))
                   (or internal-too (/= (aref name 0) ?\s))
                   (string-match regexp name))
          ;; and kill it maybe
          (if no-ask
              ;; either just kill it...
              (kill-buffer buffer)
            ;; ...or probably kill it? Save the name if so.
            (let ((maybe-kill-name (spydez/buffer/kill-ask buffer)))
              (unless (null maybe-kill-name)
                (push maybe-kill-name killed-names)))))))
    ;; And finally, give some goddamn output (looking at you,
    ;; kill-matching-buffers).
    (cond
     ((null killed-names)
      (message "No buffers killed matching '%s'."
               regexp))
     ((>= (length killed-names) 10)
      (message "Killed %s buffers matching '%s'."
               (length killed-names)
               regexp))
     (t
      (message "Killed %s buffers matching '%s': %s"
               (length killed-names)
               regexp
               killed-names)))))


;; Like `kill-buffer-ask' but no confirmation for unmodified buffers.
(defun spydez/buffer/kill-ask (buffer)
  "Kill BUFFER if confirmed. No confirm given for unmodified
buffers; just kill.

Returns buffer-name on kill, nil on no kill."
  ;; so... kill?
  (if (or
         ;; just kill it if not modified
         (not (buffer-modified-p buffer))
         ;; or ask first if modded
         (yes-or-no-p (format "Buffer '%s' HAS BEEN EDITED.  Kill? "
                                 (buffer-name buffer))))
    ;; ok - kill
    (prog1
        ;; return name when killed
        (buffer-name buffer)
      (kill-buffer buffer))
    ;; ret nil when no kill
    nil))


;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------
;; TODO: note for this:
;; `C-x d' for dired
;; TODO: add a shortcut for `find-dired'?
;; `C-x C-d' maybe?

;; TODO: May need DiredSortMenu (or DiredSortMenu+) on windows.
;;   None of this shit for sorting/formatting/displaying works other than '-lah'

;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;; add human-readable flag
(setq dired-listing-switches "-lah --time-style=long-iso --group-directories-first")

;; Was `top'. Any non (top always) value means always ask.
(setq dired-recursive-deletes  +1)

;; from: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html#sec-10-3-2
;; But not in melpa so not sure I want. Probably superceded by a file explorer like treemacs.
;; ;;---
;; ;; Dired Details+
;; ;;---
;; ;;...not on melpa either.
;; (require 'dired-details+)
;; (setq-default dired-details-hidden-string "")

;; TODO: windows doesn't seem to like this `find-dired'?
;; "By default Emacs will pass -exec to find and that makes it very slow. It is
;; better to collate the matches and then use xargs to run the command. To do
;; this instead add this to your .emacs."
;;   - https://www.masteringemacs.org/article/working-multiple-files-dired
;; Trial [2019-01-29]
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; https://stackoverflow.com/questions/18823742/in-emacs-24-3-1-on-windows-7-how-can-i-group-directories-first-in-dired
;; this doesn't seem to do anything...
;; (when (string-equal system-type "windows-nt")
;;   (setq ls-lisp-dirs-first t)
;;
;;   ;; hide the link count, user, and group columns - default is '(links uid gid)
;;   (setq ls-lisp-verbosity nil)
;;
;;   ;; use ISO dates (the first is for recent dates, second for old dates)
;;   (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
;;   (setq ls-lisp-use-localized-time-format t))


;;---
;; peep-dired
;;---
;; For quick look into files/folders at point in dired buffer. Shows in
;; `other-window'. Scroll peeped file in `other-window' down/up with
;; <space>, <backspace>.
;; https://github.com/asok/peep-dired
;; Trial [2019-01-29]
;; Note [2019-02-28]: This doesn't do anything by default - you have to turn it
;; on in the buffer. Or change settings that make it do things on its own.
(use-package peep-dired
  :delight ;; TODO: my google-fu is failing me or no one uses all-the-icons for minor modes...
  ;; I just want an eyeball for peeping. :(
  ;; Would ":after all-the-icons" help any?
;;   '(:eval (propertize (all-the-icons-octicon "eye")
;;             'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
;;             'display '(raise -0.1)))
;; (all-the-icons-icon-for-mode)
;; (all-the-icons-icon-for-file)
;; (insert (all-the-icons-octicon "eye")) ;; todo: set modeline to eyeball emoji

  ;; Do we want these shortcuts or not?
  ;;   - Um... they already exist? Maybe this is to nuke them out of other
  ;;     settings?
  ;; :bind (:map peep-dired-mode-map 
  ;;        ("SPC" . nil)            ;; scroll peeped file down
  ;;        ("<backspace>" . nil))   ;; scroll peeped file up

  ;;---
  ;; Config
  ;;---
  :config
  (setq peep-dired-cleanup-eagerly t ; cleanup ASAP (just after moving to next entry
        peep-dired-cleanup-on-disable t ; cleanup when minor mode disabled 
        peep-dired-enable-on-directories t) ; peep is enabled in peeped dirs.

  ;; Ignore all these extensions. Probably need more but here's my start.
  (setq peep-dired-ignored-extensions '("iso" "deb"  ; disk images and packages
                                        "exe" "dll"  ; binaries
                                        "mp3" "mp4" "mkv")) ; music/video

  ;; If OS is opening files too often with peep, do this:
  ;;   - https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
  ;; ;; Functions
  ;; (defun turn-off-openwith-mode ()
  ;;   (make-local-variable 'openwith-mode)
  ;;   (if (not peep-dired)
  ;;       (openwith-mode 1)
  ;;     (openwith-mode -1)))
  ;;
  ;; ;; Hooks
  ;; (add-hook 'peep-dired-hook #'turn-off-openwith-mode)
  )

;;---
;; all-the-icons-dired
;;---
;; TRIAL: [2019-03-06]
(use-package all-the-icons-dired
  :after all-the-icons
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))
;; Hey, neat. Thanks to wyuenho's dotfiles repo for showing up in a search for
;; my attempts at the peep-dired delight line.
;; https://github.com/wyuenho/dotfiles/blob/master/.emacs


;;------------------------------------------------------------------------------
;; recentf for recent files
;;------------------------------------------------------------------------------

;; From http://pages.sachachua.com/.emacs.d/Sacha.html#org0676afd
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
;; Recentf and TRAMP need some peace-keeping to get along.
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2007-07/msg00019.html
(add-to-list 'recentf-keep 'file-remote-p)
(recentf-mode)

;; may want to exclude more? idk.
(unless (spydez/dir/self-policing-p)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; More config:
;;   periodically save list of files: https://www.emacswiki.org/emacs/RecentFiles#toc1
;;
;;   Helm: https://www.emacswiki.org/emacs/RecentFiles#toc11
;;     - ‘M-x helm-recentf’ will show only recently opened files (from recentf).


;;------------------------------------------------------------------------------
;; Desktop?
;;------------------------------------------------------------------------------

;; Desktop-Save mode saves the current buffer configuration on exit and
;; reloads it on restart.
;; TODO: Not 100% convinced I want this, but I do want to try it.
;; (use-package desktop
;;   :defer nil
;;   :custom
;;   (desktop-restore-eager   1   "Restore only the first buffer right away")
;;   (desktop-lazy-idle-delay 1   "Restore the rest of the buffers 1 seconds later")
;;   (desktop-lazy-verbose    nil "Be silent about lazily opening buffers")
;;   :bind
;;   ("C-M-s-k" . desktop-clear)
;;   :config
;;   (desktop-save-mode))

;; TODO: Probably pair Desktop with this:
;; midnight-mode purges buffers which haven’t been displayed in a time period.
;; (use-package midnight
;;   :defer 3
;;   :config
;;   (setq midnight-period 7200) ;; seconds (2 hours)
;;   (midnight-mode 1))


;;------------------------------------------------------------------------------
;; Copy Filename
;;------------------------------------------------------------------------------
;; This (or similar (prelude-copy-file-name-to-clipboard)) used to be in Prelude emacs.
;; https://github.com/bbatsov/prelude/issues/764
;; But they use easy-kill.el now for stuff, and I'm not sure I want all of it.
;; Trial [2019-01-30]
(defun spydez/copy-file-name-to-clipboard ()
  "Copy the current file name to the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message "Copied buffer file name '%s' to the clipboard." filename)
      )))


;; This is more complex. Can do buffer file path or dired. C-u for folder instead of file.
;; http://ergoemacs.org/emacs/emacs_copy_file_path.html
;; Trial [2019-01-30]
(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))


;;------------------------------------------------------------------------------
;; Open files externally
;;------------------------------------------------------------------------------
;; This doesn't have a use for me right now, but I might want it eventually.
;; Would have to figure out the windows case of it:
;; https://emacsredux.com/blog/2013/03/27/open-file-in-external-program/

;; There's also package `openwith'.
;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;; https://github.com/emacsmirror/openwith


;;------------------------------------------------------------------------------
;; Smartscan
;;------------------------------------------------------------------------------
;; here or configure-text? text right now.


;;------------------------------------------------------------------------------
;; Treemacs
;;------------------------------(that name tho)---------------------------------
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :defer t

  ;;:bind
  ;; bind some keys so you can get into treemacs?
  ;; M-x treemacs?
  ;; M-x treemacs-find-file?

  ;;:custom
  ;; many settings that can be set can be set here

  ;;   "The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size."
  ;;
  ;; Doesn't seem to work... maybe emacs on windows doesn't have the
  ;; functionality needed - see function documentation help.
  ;;(treemacs-resize-icons 44)
  )

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)


;;------------------------------------------------------------------------------
;; disk-usage
;;------------------------------------------------------------------------------
;; https://www.reddit.com/r/emacs/comments/as8cri/diskusageel_a_disk_usage_analyzer_for_emacs/
;; https://gitlab.com/ambrevar/emacs-disk-usage
;; Trial: [2019-04-18 Thu]
(use-package disk-usage
  :defer t
  :commands (disk-usage disk-usage-here disk-usage-dired-at-point))


;;------------------------------------------------------------------------------
;; TODO: these. Parenthesis, bells?
;;------------------------------------------------------------------------------
;; here or configure-text? text right now.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-files-and-folders)
