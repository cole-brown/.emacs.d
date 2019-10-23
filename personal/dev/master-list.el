;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Master List & Reason I'm Doing System/Hash.
;;------------------------------------------------------------------------------

;; C-u C-x C-e with point on end of next line to find out your hash:
;;   spydez/dev/system/hash
;;   e.g.: "computer-898a-27ab"
;; This is so each system can have its own overrides or piggybacks that can be
;; found with an easy (load ...) or (require ...)

(defvar spydez/bootstrap/system/known-p t)

;; Find this system, setup domain names, reform reduced hash string.
(cond ;; switch case on system name hashes
 ;;---
 ;; Next System Here:
 ;;---
 ;; ((equal spydez/dev/system/hash "<something>")
 ;;  (setq spydez/dev/domain/name "home"
 ;;        spydez/dev/domain/subname "comp"
 ;;        spydez/dev/system/hash (spydez/hash-and-reduce
 ;;                                  spydez/dev/system/name
 ;;                                  spydez/dev/domain/subname)
 ;;        spydez/bootstrap/system/known-p t
 ;;        spydez/dev/setup/additional-required nil))

 ;;---
 ;; Home Laptop/Tablet
 ;;---
 ((equal spydez/dev/system/hash "computer-d122-2b66")
  (setq spydez/dev/domain/name "home"
        spydez/dev/domain/subname "deep"
        spydez/dev/system/hash (spydez/hash-and-reduce
                                  spydez/dev/system/name
                                  spydez/dev/domain/subname)
        spydez/bootstrap/system/known-p t
        spydez/dev/setup/additional-required nil))

 ;;---
 ;; Home Desktop 2017-
 ;;---
 ((equal spydez/dev/system/hash "computer-2418-d188")
  (setq spydez/dev/domain/name "home"
        ;; I never know what to call my home stuff...
        ;;   - Name it after less popular OSes? Like Plan 9 from Bell Labs?
        ;;   - Name it an androgynous name? Like Sasha from this list?
        ;;     https://en.wikipedia.org/wiki/Unisex_name#English
        ;;   - Name it an element, like back when I was doing that? But I got
        ;;     stuck on Boron for a phone and then there were 3 borons and it
        ;;     got confusing...
        ;; Also, this is just the prefix for a hash (right now), so...
        spydez/dev/domain/subname "plan9"
        spydez/dev/system/hash (spydez/hash-and-reduce
                                  spydez/dev/system/name
                                  spydez/dev/domain/subname)
        spydez/bootstrap/system/known-p t
        spydez/dev/setup/additional-required nil))

 ;;---
 ;; PFO Desktop 2013-
 ;;---
 ((equal spydez/dev/system/hash "computer-898a-27ab")
  (setq spydez/dev/domain/name "work"
        spydez/dev/domain/subname "pfo"
        spydez/dev/system/hash (spydez/hash-and-reduce
                                  spydez/dev/system/name
                                  spydez/dev/domain/subname)
        spydez/bootstrap/system/known-p t
        spydez/dev/setup/additional-required nil))

 ;;---
 ;; Fallthrough Case - nothing specified so defaults will be used
 ;;---
 (t
  (mis/warning nil nil
        "Update 'Master List' for this system (%s) here."
        spydez/dev/system/hash)
    (setq spydez/bootstrap/system/known-p nil
          spydez/dev/setup/additional-required t)))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'master-list)
