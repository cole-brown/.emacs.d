;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;----------------------------------------------------------------------------;;
;;                                 Bootstrap.                                 ;;
;;---To pull oneself up by the bootstraps, one must first find one's boots.---;;
;;                           ...Oh, here they are.                            ;;


;; After agonizing about this for all of [2019-02-13], and various points
;; throughout from the start, I think the new early-init.el is the best place to
;; put enough of the bootstrap to find the local settings here.
;;
;; Also, it appears that some of the window settings improve startup by
;; significant fractions of a second. Other settings too.
;;
;; However, early-init is an Emacs 27 thing. So for Emacs 26.1 til then, we'll
;; just have to fudge things. For Emacs 27 and onward, the message above will
;; start being spit out, and eventually, decades from now, I'll notice and fix
;; things to how they should be.
;;
;; See: https://github.com/search?q=early-init.el&type=Code
(let ((post-type '(spydez zeroth post)))
  (if (boundp 'early-init-file)
      ;; as in 'Power-On-Self-Test', not the HTTP thing.
      (lwarn post-type :warning
             "  %s:  %s! Emacs %s uses early-init proper."
             post-type
             "Update early-init.el for actual active duty"
             emacs-version)
      ;; NOTE: Try to keep this lwarn roughly like the mis/debug/warning output.

  ;; Print out something that hopefully still conforms to nice function output
  ;; we won't have til after zeroth-debug...
  (message ">> %s: early-init.el... Pre-history." post-type)))



;;-----------------------------------zeroth-----------------------------------;;
;;                              0th Order Stuff                               ;;
;;----------------------------------------------------------------------------;;
;;              For when the first thing is too soon of a thing.              ;;


;;---
;; Load Path: The Stuff Before The Pre-Basics
;;---

;; Save off orginal. We're going to have a reduced set for bootstrapping, then
;; reset and add the full monty.
(defconst spydez/dir/load-path/orig load-path)

;; Ugly dirty hardcoded etc. But I want over-simple until we get some help
;; loaded properly.
(let ;; zeroith is all init code to get through early init.
     ((zero   (expand-file-name "zeroth"
                (expand-file-name "init"
                  (expand-file-name "personal"
                    (file-name-as-directory  user-emacs-directory)))))
      ;; master is for the master-list file need to ID this device/system
      (master (expand-file-name "dev"
                (expand-file-name "personal"
                  (file-name-as-directory  user-emacs-directory))))
      ;; mis.el is my messages/string lib, and I want it ASAP for init and
      ;; debug messages.
      (mis    (expand-file-name "mis"
                (expand-file-name "packages"
                  (expand-file-name "personal"
                    (file-name-as-directory  user-emacs-directory))))))
  ;; Toss them into load-path ASAP so we can get started.
  (add-to-list 'load-path zero)   ;; zeroth-* files
  (add-to-list 'load-path master) ;; master-list file
  (add-to-list 'load-path mis))   ;; mis.el library


;;---
;; Ok. Now we can do a few things before getting started.
;;---

;; This is, like, pre-pre-basics...
(require 'zeroth-steps)
(require 'zeroth-zero)
(spydez/init/step/set-completed 'zeroth 'zero)

;; Now we're into the pre-basics, I guess.

;; Adjust garbage collection thresholds.
(require 'zeroth-garbage)

;; Funcs, vars for my debugging/warning messages.
(require 'mis)
(require 'zeroth-debug)

;; Don't (currently) need to set up mis timeouts. Init step of emacs ignores
;; them anyways, it seems...
;;
;; ;; Setup mis for init. First hook into a final step for reverting to more
;; ;; normal settings...
;; (let ((seq-cpy (copy-sequence mis/message/echo-area-timeout)))
;;   (spydez/hook/defun-and-hooker
;;       spydez/hook-runner/finalize/final-finalities nil nil
;;       "mis/reset"
;;       nil
;;       "early-init.el"
;;     "Resets some mis settings back to a 'running interactively' state."
;;
;;     (message "mis hook resetting: %S" seq-cpy)
;;     (customize-set-variable 'mis/message/echo-area-timeout
;;                             seq-cpy)))
;; ;; And second set mis/messages to clear out quickly during init.
;; ;; §-TODO-§ [2020-02-20]: 0.1 good? Try actual 0.0?
;; (customize-set-variable 'mis/message/echo-area-timeout '(0.1 0.1))
;; ;; mis/message/echo-area-timeout

;; debug helpers now loaded - including `spydez/require'
(spydez/init/step/set-completed 'zeroth 'debug)

;;                                    ---
;;                                  -------
;; This will be the start of actual mis-init.el messages.
;;                                  -------
;;                                    ---

;; Funcs, vars for some very early strings/hashing.
(spydez/require 'zeroth-funcs)


;;------------------------------------------------------------------------------
;; Step... 0.5? Domains, Master List, Per Computer Stuff.
;;------------------------------------------------------------------------------
;; Need to know something about my boots so I can pull the right straps.
(mis/init/message 'init "early-init.el... Zeroth step.")

;;---
;; Domain & System Setup
;;---
(spydez/require 'zeroth-domains)


;;---
;; Master List & Reason I'm doing system/hash.
;;---

;; not in zeroth directory - in dev instead 
(spydez/require 'master-list)


;;---
;; Directories
;;---
(spydez/require 'zeroth-directories)

;;---
;; Load Path: The Basics
;;---

;; Bare minimum in the Load-Path for finding bootstrapping files:
;; Add in order of more-overridable/less-specific.
(add-to-list 'load-path (spydez/dirky/path :load-path :boot))         ;; "all systems" stuff
(add-to-list 'load-path (spydez/dirky/path :load-path :dev/defaults)) ;; "system defaults" stuff
;; Could add a domain level if needed?
(add-to-list 'load-path (spydez/dirky/path :load-path :dev/system))   ;; most specific

(spydez/init/step/set-completed 'zeroth 'system)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(spydez/init/step/set-completed 'zeroth 'complete)
;; (provide 'early-init)
;; early-init ends here
