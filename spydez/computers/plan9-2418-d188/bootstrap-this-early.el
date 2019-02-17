;; -*- emacs-lisp -*-


;; a local var we can refer to while not being complete yet...
(let ((spydez/bootstrap/expected-complete 'specific))

  ;;------------------------------------------------------------------------------
  ;; General Settings
  ;;------------------------------------------------------------------------------
  ;; Vars to override, other general stuff to provide so bootstrap can finish
  ;; successfully for this computer.



  ;;------------------------------------------------------------------------------
  ;; System Setup
  ;;------------------------------------------------------------------------------

  ;;---
  ;; Directories
  ;;---

  ;; todo: move these dirs to secrets?

  ;; todo: Not sure here or dropbox...
  (defconst spydez/dir/common-doc-save "d:/vault/documents/emacs"
    "Place for auto-open files or secrets or something to be.")

  (defconst spydez/dir/dropbox "d:/Users/spydez/Dropbox"
    "Dropbox's location on local disk...")

  ;; todo: move this list to secrets?
  ;; auto-open this file list at end of emacs init/setup
  ;; setq to just override the whole thing
  (defconst spydez/auto-open-list
    ;; don't do: '(
    ;; do this: (list
    ;; Need a thing that will eval the list items to strings either now or in auto-open-files.
    ;; So we choose now.
    (list
     ;; top level work org-mode file for notes & such
     (expand-file-name "42.org" (spydez/dir-name "42" spydez/dir/dropbox))

     ;; Working on .emacs.d a lot right now so add these in.
     ;; (expand-file-name "configure-completion.el" spydez/dir/emacs/personal)
     (expand-file-name "init.el" spydez/dir/emacs)
     ))

  ;;------------------------------------------------------------------------------
  ;; TODOs
  ;;------------------------------------------------------------------------------


  ;;------------------------------------------------------------------------------
  ;; The End.
  ;;------------------------------------------------------------------------------
  (setq spydez/bootstrap/complete spydez/bootstrap/expected-complete)

  ) ;; (let ...)
(provide 'bootstrap-this-early)
