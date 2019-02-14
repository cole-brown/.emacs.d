;; -*- emacs-lisp -*-


;; a local var we can refer to while not being complete yet...
(let ((spydez/bootstrap/expected-complete 'default))

  ;;------------------------------------------------------------------------------
  ;; General Settings
  ;;------------------------------------------------------------------------------
  ;; Vars to override, other general stuff to provide so bootstrap can finish
  ;; successfully for this computer.


  ;;------------------------------------------------------------------------------
  ;; New System Setup
  ;;------------------------------------------------------------------------------

  ;; system's folder exists but still have a default bootstrap-this-early.
  (when (and (eq spydez/bootstrap/expected-complete 'default) ; default bootstrap
             (and (boundp 'spydez/bootstrap/system/known-p) spydez/bootstrap/system/known-p) ; know this system
             (file-exists-p spydez/dir/personal/system-this)) ; system-this's folder exists
    (message "  Update this system's bootstrap-this-early (expecting 'specific return (got %s)): %s"
             spydez/bootstrap/expected-complete
             spydez/dir/personal/system-this))

  ;; if default and system/known-p and no this-comp dir...
  ;;   - make this-comp dir
  ;;   - copy default bootstrap-this-early there?
  (when (and (eq spydez/bootstrap/expected-complete 'default) ; default bootstrap
             (and (boundp 'spydez/bootstrap/system/known-p) spydez/bootstrap/system/known-p) ; know this system
             (not (file-exists-p spydez/dir/personal/system-this))) ; no folder for this system
    ;; Create the dir, fail if no parent.
    ;; Parent is probably spydez/dir/emacs/personal and should exist manually.
    (make-directory spydez/dir/personal/system-this)

    ;; Copy this default bootstrap-this-early into the specific dir.
    ;; init will still 'fail' as it is still a default bootstrap-this-early until you go tweak it.
    (let* ((filename spydez/file/bootstrap/local)
	   (default-path (expand-file-name filename spydez/dir/personal/defaults))
           (this-path (expand-file-name filename spydez/dir/personal/system-this)))
      ;; TODO: copy an 'empty' or placeholder bootstrap-this-early.el instead of this one that does stuff
      (copy-file default-path this-path)
      (message "  Copied %s to this system's folder: %s" filename this-path)
      ))


  ;;------------------------------------------------------------------------------
  ;; TODOs
  ;;------------------------------------------------------------------------------
  ;; The todos are: Setup up this system.

  (unless (and (boundp 'spydez/bootstrap/system/known-p) spydez/bootstrap/system/known-p)
    ;; Put point at end of (message ...) then:
    ;;   C-x C-e to evaluate these sexprs.
    (message "Hello there from *default* bootstrap-this-early.")

    ;; Make sure these are correct:
    (message "New computer? Make sure these are correct:")
    (message "  system/name: %s" spydez/setup/system/name)
    (message "  system/hash: %s" spydez/setup/system/hash)

    ;; Then make sure these folders/files are correct/exist:
    (message "  dir/domain: %s" spydez/dir/personal/domain-this)
    (message "  dir/system: %s" spydez/dir/personal/system-this)
    (message "  expecting 'specific' flag in %s (have '%s' in defaults): %s"
             "bootstrap-this-early.el"
             spydez/bootstrap/expected-complete
             (expand-file-name "bootstrap-this-early.el" spydez/dir/personal/system-this))

    ;; TODO: Some messages for ading system-this to hash table/settings in early-init?
    )


  ;;------------------------------------------------------------------------------
  ;; The End.
  ;;------------------------------------------------------------------------------
  (setq spydez/bootstrap/complete spydez/bootstrap/expected-complete)

  ) ;; (let ...)
(provide 'bootstrap-this-early)
