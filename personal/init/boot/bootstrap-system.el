;; -*- mode: emacs-lisp; lexical-binding: t -*-


(require 'seq)

;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

(defun spydez/bootstrap/step-at (expected)
  "Checks if bootstrap is at expected step of the process (exactly)."
  (if (and (boundp 'spydez/bootstrap/step-completed)
           (eq spydez/bootstrap/step-completed expected))
      ;; Ok! All's well.
      t

    ;; Nopers.
    (error (concat "Bootstrap: Sanity check failed. Expected to be at step "
                   "'%s' but we're on '%s' instead.")
           expected spydez/bootstrap/step-completed)
    nil))


(defun spydez/bootstrap/step>= (expected)
  "Checks if bootstrap is at expected step of the process (exactly)."
  ;; expected pos is either found in list, or very negative
  (let ((exp-index (or (seq-position spydez/bootstrap/steps expected)
                       -999))
        ;; current pos is either found in list, or very positive
        (curr-index (or (seq-position spydez/bootstrap/steps
                                      spydez/bootstrap/step-completed)
                        999)))
    ;; So if one of those isn't found, we should have...
    ;; -999 >=   N -> false
    ;;    N >= 999 -> false
    ;; Yeah? Ok.
    (>= curr-index exp-index)))


(defun spydez/bootstrap/step-set (current)
  "Checks if bootstrap is at expected step of the process (exactly)."
  (if (and (boundp 'spydez/bootstrap/step-completed)
           (seq-contains spydez/bootstrap/steps current))
      (setq spydez/bootstrap/step-completed current)
    (error (concat "spydez/bootstrap/step-set: Cannot set current "
                   "step to '%s'. Options: %s")
           current spydez/bootstrap/steps)))


;;--------------------------Little Bit of Sanity...----------------------------
;;--                         Early Step Completed?                           --
;;-----------------------------------------------------------------------------
(unless (spydez/bootstrap/step-at 'early)
  (spydez/bootstrap/step-set 'default))
;; step-at complains so we don't have to...


;;-----------------------------------System------------------------------------
;;--                        Find It and Check It...                          --
;;-----------------------------------------------------------------------------
(cond
 ;;-----------------------------------------------------------------------------
 ;; Known System
 ;;-----------------------------------------------------------------------------
 ((bound-and-true-p spydez/bootstrap/system/known-p)

  ;; system's dir exists or system doesn't need special setup here.
  (if (or (file-exists-p spydez/dir/dev/system-this)
          (not spydez/setup/system/additional-required))
      ;; a-ok - no need to do anything
      (spydez/bootstrap/step-set 'specific)

    ;; needs another step done?
    (spydez/warning/message
     nil nil
     "  System '%s' needs additional setup! %s %s"
     spydez/setup/system/name
     (if spydez/setup/system/additional-required
         "(more setup required?)"
       (format "(setup file missing: %s" spydez/dir/dev/system-this)))
    (spydez/bootstrap/step-set 'default)))
 ;;!!!!!
 ;; AFTER SETUP IN master-list.el ONLY!
 ;;   If you need to create the system's dir for additional-setup, put cursor
 ;; after closing parenthesis and eval with C-x C-e
 ;;---
 ;; (make-directory spydez/dir/dev/system-this)
 ;;---
 ;; Creates the dir, fails if no parent.
 ;; Parent is probably spydez/dir/emacs/personal and should exist manually.
 ;;!!!!!


 ;;---------------------------------------------------------------------------
 ;; New System Instructions
 ;;---------------------------------------------------------------------------
 ;; Tell user to setup up this system.
 ((not (bound-and-true-p spydez/bootstrap/system/known-p))
  ;; Put point at end of (spydez/warning/message nil nil ...) then:
  ;;   C-x C-e to evaluate these sexprs.
  (spydez/warning/message
   nil nil
   "Hello there from bootstrap-system. This system needs added to: '%s'"
   (spydez/path/to-file spydez/dir/personal/dev "master-list.el"))

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

  ;; And we're only done w/ default bootstrap.
  (spydez/bootstrap/step-set 'default)))


;;--------------------------Little Bit of Sanity...----------------------------
;;--                   "How was bootstrap today, Honey?"                     --
;;-----------------------------------------------------------------------------
;; Not too much sanity.
(cond ((spydez/bootstrap/step-at 'specific) t) ;; good to go

      ;; using default - should probably warn
      ((spydez/bootstrap/step-at 'default)
       (spydez/warning/message nil nil
           "Specific bootstrap does not exist for this computer: %s %s"
           spydez/bootstrap/step-completed spydez/setup/system/hash))

      ;; fallthrough cases - nothing used
      (t (error (spydez/warning/message nil nil
                    "Bootstrap: No bootstrap at all for this computer?: %s %s"
                    spydez/bootstrap/step-completed spydez/setup/system/hash))))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The Actual End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-system)
