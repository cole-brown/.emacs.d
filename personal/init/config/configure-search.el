;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; If emacs grep or <package>-grep are looking in dirs you don't want, try
;; adding to this list: `grep-find-ignored-directories'
;; e.g. from http://nhoffman.github.io/.emacs.d/#org9119f86
;;   - Add ".eggs" and "src" to list for python development.


;;------------------------------------------------------------------------------
;; Anzu - Show Number of Matches in Mode-Line While Searching
;;------------------------------------------------------------------------------
(use-package anzu
  ;; Force load cuz ':bind' wants to defer til one of those is called.
  ;; But I want it loaded for normal searchs too (M-s, etc).
  :demand t

  ;; TODO: mess with colors?

  ;; TODO: give it a tab or something in moody?

  ;;---
  :bind
  ;;---
  ;; Rebind to Anzu versions.
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))

  ;;---
  :config
  ;;---
  ;; and enable
  (global-anzu-mode))


;;------------------------------------------------------------------------------
;; Occur DWIM (Do What I Mean)
;;------------------------------------------------------------------------------
;; https://oremacs.com/2015/01/26/occur-dwim/
;; It will offer as the default candidate:
;;   - the current region, if it's active
;;   - the current symbol, otherwise
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; A Hydra that goes nicely with occur:
;;   Since occur, like some other functions (grep, rgrep, compile), works with
;; next-error, it's possible to use this Hydra to navigate the occur matches:
;; (hydra-create "M-g"
;;   '(("h" first-error "first")
;;     ("j" next-error "next")
;;     ("k" previous-error "prev")))
;; TODO: use this for occur and grep and such?


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

;; If using venv (virtual env), try using advice-add to dynamically put the
;; project's venv dir into the grep ignored list.
;; (Have to add it to each grep function: rgrep, <package>-grep...)


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: try out ag / silver searcher
;;   - if no, ack?

;; TODO: get grep working?
;;   M-x rgrep
;;   projectile-grep
;;   helm-projectile-grep
;;   ...

;; TODO: try out this visual help for emacs-style regex
;;   https://github.com/benma/visual-regexp.el
;; Also check out their visual-regexp-setroids for normal-style
;; (perl style? modern style?) regexs.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-search)

