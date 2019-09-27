;; -*- mode: emacs-lisp; lexical-binding: t -*-


;; NOTE: DEFAULTS file. Anything updated here may want to be mirrored into any
;; specific finalize-domain.el files as well.



;; TODO-EASY: get this likek bootstrap-this-early - talkative about being default
;; then maybe move their default talky funcs elsewhere so I can reuse them?




;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; None?


;;------------------------------------------------------------------------------
;; Auto-Open Files
;;------------------------------------------------------------------------------

;; auto-open this file list at end of emacs init/setup
;; setq to just override the whole thing
(defvar spydez/file/auto-open-list
  ;; don't do: '(
  ;; do this: (list
  ;; Need a thing that will eval the list items to strings either now or in auto-open-files.
  ;; So we choose now.
  (list
   ;;---
   ;; Work Stuff
   ;;---

   ;; Leaving this to the work-specific finalize files.

   ;;---
   ;; Emacs Stuff
   ;;---
   ;; Working on .emacs.d a lot right now so add these in.
   (spydez/path/to-file spydez/dir/personal/docs "working-on.org")
   (spydez/path/to-file spydez/dir/emacs "init.el")
   ))
;; TODO: check that these exist before opening. Complain (:warning level?) if not.


;; This becomes pretty useless if I start using emacs desktop package...
;; Until then, an easy place to make files more sticky:

;; TODO: should put this in a common place instead of here
;; TODO: split frame, open these in other-window?
(defun spydez/hook/auto-open-files ()
  (if (and window-system (bound-and-true-p spydez/file/auto-open-list))
      (dolist (file spydez/file/auto-open-list)
        (find-file file))))
(add-hook 'spydez/hook/finalize/run-the-final-hooks
          'spydez/hook/auto-open-files)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'finalize-domain)
;; TODO: This could also be easily renamed, especially since maybe the system level
;; would want to override the domain level.
;;   TODO: "finalize-dev"?
