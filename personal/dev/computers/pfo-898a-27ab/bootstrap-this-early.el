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

  ;; todo: updated to c:/home/<user>/documents
  (defconst spydez/dir/common-doc-save "c:/home/documents"
    "Place for auto-open files or secrets or something to be.")

  ;; auto-open this file list at end of emacs init/setup
  ;; setq to just override the whole thing
  (defconst spydez/auto-open-list
    ;; don't do: '(
    ;; do this: (list
    ;; Need a thing that will eval the list items to strings either now or in auto-open-files.
    ;; So we choose now.
    (list
     ;; top level work org-mode file for notes & such
     (expand-file-name "work.org" spydez/dir/common-doc-save)

     ;; Working on .emacs.d a lot right now so add these in.
     (expand-file-name "working-on.org" spydez/dir/personal/docs)
     ;; (expand-file-name "performance.long-lines.org" spydez/dir/docs/notes)
     (expand-file-name "init.el" spydez/dir/emacs)
     ))
  ;; TODO: check that these exist before opening. Complain (:warning level?) if not.


  ;;------------------------------------------------------------------------------
  ;; TODOs
  ;;------------------------------------------------------------------------------



  ;;------------------------------------------------------------------------------
  ;; The End.
  ;;------------------------------------------------------------------------------
  (setq spydez/bootstrap/complete spydez/bootstrap/expected-complete)

  ) ;; (let ...)
(provide 'bootstrap-this-early)
