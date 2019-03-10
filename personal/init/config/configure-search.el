;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; If emacs grep or <package>-grep are looking in dirs you don't want, try
;; adding to this list: `grep-find-ignored-directories'
;; e.g. from http://nhoffman.github.io/.emacs.d/#org9119f86
;;   - Add ".eggs" and "src" to list for python development.


;;------------------------------------------------------------------------------
;; Blank
;;------------------------------------------------------------------------------


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


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-search)

