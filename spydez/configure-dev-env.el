;; -*- emacs-lisp -*-

;; TODO: consume configure-text into this as most of it I'm todoing to put here vs there.
;; or find a better/cleaner split if that makes this a ridiculous chunky config file.

;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------
;; TODO: note for this:
;; `C-x d' for dired
;; TODO: add a shortcut for `find-dired'?
;; `C-x C-d' maybe?

;; "By default Emacs will pass -exec to find and that makes it very slow. It is
;; better to collate the matches and then use xargs to run the command. To do
;; this instead add this to your .emacs."
;;   - https://www.masteringemacs.org/article/working-multiple-files-dired
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
(provide 'configure-dev-env)
