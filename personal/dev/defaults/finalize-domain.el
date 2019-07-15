;; -*- mode: emacs-lisp; lexical-binding: t -*-






;; TODO-EASY: get this likek bootstrap-this-early - talkative about being default
;; then maybe move their default talky funcs elsewhere so I can reuse them?




;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; None?


;;------------------------------------------------------------------------------
;; Files
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

   ;; top level work org-mode file for notes & such
   (expand-file-name "2019-00_work.org" spydez/dir/org-docs)
   ;; current milestone
   (expand-file-name "2019-03_free-trial-mode.org" spydez/dir/org-docs)
   ;; current todo list
   (expand-file-name "2019-03_tasks.org" spydez/dir/org-docs)

   ;;---
   ;; Emacs Stuff
   ;;---
   ;; Working on .emacs.d a lot right now so add these in.
   (expand-file-name "working-on.org" spydez/dir/personal/docs)
   (expand-file-name "init.el" spydez/dir/emacs)
   ))
;; TODO: check that these exist before opening. Complain (:warning level?) if not.
;; TODO: potentially add to this from secrets file (maybe one with a cheaper password?)


;;------------------------------------------------------------------------------
;; Auto-Open Files
;;------------------------------------------------------------------------------
;; This becomes pretty useless if I start using emacs desktop package...
;; Until then, an easy place to make files more sticky:

;; TODO: should put this in a common place instead of here
;; TODO: split frame, open these in other-window?
;; TODO: move to a finalize probably
(defun spydez/hook/auto-open-files ()
  (if (and window-system (bound-and-true-p spydez/file/auto-open-list))
      (dolist (file spydez/file/auto-open-list)
        (find-file file))))
(add-hook 'emacs-startup-hook 'spydez/hook/auto-open-files)


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'finalize-domain)
;; TODO: This could also be easily renamed, especially since maybe the system level
;; would want to override the domain level.
;;   TODO: "finalize-dev"?
