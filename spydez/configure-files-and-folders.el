;; -*- emacs-lisp -*-

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces

;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------
;; TODO: note for this:
;; `C-x d' for dired
;; TODO: add a shortcut for `find-dired'?
;; `C-x C-d' maybe?

;; TODO: windows doesn't seem to like this `find-dired'?
;; "By default Emacs will pass -exec to find and that makes it very slow. It is
;; better to collate the matches and then use xargs to run the command. To do
;; this instead add this to your .emacs."
;;   - https://www.masteringemacs.org/article/working-multiple-files-dired
;; Trial [2019-01-29]
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;;---
;; peep-dired
;;---
;; For quick look into files at point in dired buffer.
;; https://github.com/asok/peep-dired
;; Trial [2019-01-29]
(use-package peep-dired
  ;; TODO: some config settings maybe?
  ;;   - peep-dired-cleanup-eagerly seems good...
  ;;   - also this (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))

  ;; TODO: do we want these shortcuts or not?
  ;; :bind (:map peep-dired-mode-map 
  ;;        ("SPC" . nil)            ;; scroll peeped file down
  ;;        ("<backspace>" . nil))   ;; scroll peeped file up
  )


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

;; More config:
;;   periodically save list of files: https://www.emacswiki.org/emacs/RecentFiles#toc1
;;
;;   Helm: https://www.emacswiki.org/emacs/RecentFiles#toc11
;;     - ‘M-x helm-recentf’ will show only recently opened files (from recentf).


;;------------------------------------------------------------------------------
;; Smartscan
;;------------------------------------------------------------------------------
;; here or configure-text? text right now.


;;------------------------------------------------------------------------------
;; TODO: these. Parenthesis, bells?
;;------------------------------------------------------------------------------
;; here or configure-text? text right now.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-files-and-folders)
