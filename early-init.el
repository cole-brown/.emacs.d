;; -*- lexical-binding: t -*-


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
(when (boundp 'early-init-file)
  (let ((warn-type '(spydez post))) ;; as in power-on-self-test, not the HTTP thing.
    (lwarn warn-type :warning "  %s:  Update early-init.el for actual active duty! Emacs %s uses early-init proper." warn-type emacs-version)))


;;-----------------------------------zeroth-----------------------------------;;
;;                              0th Order Stuff                               ;;
;;----------------------------------------------------------------------------;;
;;              For when the first thing is too soon of a thing.              ;;


;;---
;; Load Path: The Pre-Basics
;;---

;; Save off orginal. We're going to have a reduced set for bootstrapping, then reset and add the full monty.
(defconst spydez/dir/load-path/orig load-path)

;; Ugly dirty hardcoded etc. But I want over-simple until we get some help loaded properly.
(let ((zero (expand-file-name "zeroth"
             (expand-file-name "init"
                (expand-file-name "personal"
                   (file-name-as-directory  user-emacs-directory)))))
      (master (expand-file-name "dev"
                (expand-file-name "personal"
                   (file-name-as-directory  user-emacs-directory)))))
  ;; Toss them into load-path ASAP so we can get started.
  (add-to-list 'load-path zero)    ;; zeroth-* files
  (add-to-list 'load-path master)) ;; master-list file


;;---
;; Ok. Now we can do a few things before getting started.
;;---

;; Adjust garbage collection thresholds.
(require 'zeroth-garbage)

;; Funcs, vars for my debugging/warning messages.
(require 'zeroth-debug)

;; Funcs, vars for some very early strings/hashing.
(require 'zeroth-strings)


;;------------------------------------------------------------------------------
;; Step... 0.5? Domains, Master List, Per Computer Stuff.
;;------------------------------------------------------------------------------
;; Need to know something about my boots so I can pull the right straps.

;;---
;; Domain & System Setup
;;---
(require 'zeroth-domains)


;;---
;; Master List & Reason I'm doing system/hash.
;;---

;; not in zeroth directory - in dev instead 
(require 'master-list)


;;---
;; Directories
;;---
(require 'zeroth-directories)

;;---
;; Load Path: The Basics
;;---

;; Bare minimum in the Load-Path for finding bootstrapping files:
(add-to-list 'load-path spydez/dir/dev/defaults) ;; defaults first so everything else overrides.
;; Could add a domain level if needed?
(add-to-list 'load-path spydez/dir/dev/system-this) ;; most specific to this computer last


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(defconst spydez/bootstrap/complete 'early
  "values: nil, 'early, 'default, 'specific
compare: (eq spydez/bootstrap/complete 'early)")

;; (provide 'early-init)
;; early-init ends here
