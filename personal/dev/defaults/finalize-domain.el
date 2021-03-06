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

;; §-TODO-§ [2019-09-21]: make this an alist, with (action . path). actions:
;; 'bury, 'default/nil, 'show, 'the-one
;;   - bury: open, then call `bury-buffer'
;;   - default: open, then it's done
;;   - show: open, add to list of 'to-be-summoned-to-top'.
;;     - once all auto-open opened, bring to top in order.
;;   - the-one: Open after everything is done. Be on top, shown.
;;     - There can be only one.

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
   (spydez/path/to-file (spydez/dirky/path :emacs :docs) "working-on.org")
   (spydez/path/to-file (spydez/dirky/path :default :emacs.d) "init.el")
   ))
;; TODO: check that these exist before opening. Complain (:warning level?) if not.


;; This becomes pretty useless if I start using emacs desktop package...
;; Until then, an easy place to make files more sticky:

;; TODO: should put this in a common place instead of here
;; TODO: split frame, open these in other-window?
(spydez/hook/defun-and-hooker
    spydez/hook-runner/finalize/final-finalities nil nil
    "auto-open-files"
    nil
    "dev/defaults/finalize-domain.el"
  "Opens files defined in `spydez/file/auto-open-list'."
  (if (and window-system (bound-and-true-p spydez/file/auto-open-list))
      (dolist (file spydez/file/auto-open-list)
        (find-file file))))
;; (setq spydez/hook-runner/finalize/final-finalities nil)
;; (run-hook 'spydez/hook-runner/finalize/final-finalities)


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
