;; -*- mode: emacs-lisp; lexical-binding: t -*-


;; a local var we can refer to while not being complete yet...
(let ((spydez/bootstrap/expected-complete 'default))

  ;;----------------------------------------------------------------------------
  ;; General Settings
  ;;----------------------------------------------------------------------------
  ;; Vars to override, other general stuff to provide so bootstrap can finish
  ;; successfully for this computer.


  ;;----------------------------------------------------------------------------
  ;; New System Setup
  ;;----------------------------------------------------------------------------

  ;; system's folder exists but still have a default bootstrap-this-early.
  (when (and
         (eq spydez/bootstrap/expected-complete 'default) ;; default bootstrap
         (bound-and-true-p 'spydez/bootstrap/system/known-p) ;; know this system
         (file-exists-p spydez/dir/dev/system-this)) ;; system's dir exists
    (spydez/warning/message
     nil nil
     (concat "  Update this system's bootstrap-this-early "
             "(expecting 'specific return (got '%s)): %s")
     spydez/bootstrap/expected-complete
     spydez/dir/dev/system-this))

  ;; if default and system/known-p and no this-comp dir...
  ;;   - make this-comp dir
  ;;   - copy default bootstrap-this-early there?
  (when (and
         (eq spydez/bootstrap/expected-complete 'default) ;; default bootstrap
         (bound-and-true-p 'spydez/bootstrap/system/known-p) ;; know this system
         (not (file-exists-p spydez/dir/dev/system-this))) ;; system's dir DNE
    ;; Create the dir, fail if no parent.
    ;; Parent is probably spydez/dir/emacs/personal and should exist manually.
    (make-directory spydez/dir/dev/system-this)

    ;; Copy this default bootstrap-this-early into the specific dir. init will
    ;; still 'fail' as it is still a default bootstrap-this-early until you go
    ;; tweak it.
    (let* ((filename spydez/file/bootstrap/local)
           (default-path (spydez/path/to-file spydez/dir/dev/defaults filename))
           (this-path (spydez/path/to-file spydez/dir/dev/system-this filename)))
      ;; TODO: Copy an 'empty' or placeholder bootstrap-this-early.el instead of
      ;; this one that does stuff.
      (copy-file default-path this-path)
      (spydez/warning/message nil nil
                              "  Copied %s to this system's folder: %s"
                              filename this-path)))


  ;;----------------------------------------------------------------------------
  ;; New Device Instructions
  ;;----------------------------------------------------------------------------
  ;; Tell user to setup up this system.

  (unless (bound-and-true-p 'spydez/bootstrap/system/known-p)
    ;; Put point at end of (spydez/warning/message nil nil ...) then:
    ;;   C-x C-e to evaluate these sexprs.
    (spydez/warning/message
     nil nil
     "Hello there from *default* bootstrap-this-early.")

    ;; Make sure these are correct:
    (spydez/warning/message
     nil nil
     "New computer? Make sure these are correct:")
    (spydez/warning/message
     nil nil
     "  system/name: %s" spydez/setup/system/name)
    (spydez/warning/message
     nil nil
     "  system/hash: %s" spydez/setup/system/hash)

    ;; Then make sure these folders/files are correct/exist:
    (spydez/warning/message
     nil nil
     "  dir/domain: %s" spydez/dir/dev/domain-this)
    (spydez/warning/message
     nil nil
     "  dir/system: %s" spydez/dir/dev/system-this)
    (spydez/warning/message
     nil nil
     "  expecting 'specific' flag in %s (have '%s' in defaults): %s"
     "bootstrap-this-early.el"
     spydez/bootstrap/expected-complete
     (spydez/path/to-file spydez/dir/dev/system-this "bootstrap-this-early.el"))

    ;; TODO: Some messages for ading system-this to hash table/settings
    ;; in early-init.el?
    )


  ;;----------------------------------------------------------------------------
  ;; The End.
  ;;----------------------------------------------------------------------------
  (spydez/bootstrap/step-set 'spydez/bootstrap/expected-complete))

;;------------------------------------------------------------------------------
;; The Actual End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-this-early)
