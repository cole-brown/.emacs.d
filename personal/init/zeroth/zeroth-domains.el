;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;; These all should start as these values and then be corrected per-computer
;; in master-list.el.

(defconst spydez/dev/domain/name "invalid"
  "A domain/folder for setups similar to this. E.g. work vs home.
  Should be set in master-list.el.")

(defconst spydez/dev/domain/subname "computer"
  "A sub-domain/folder for setups similar to this. E.g. work laptop vs work PC.

This is currently one thing ('computer') for master list, and then changed
(in the master-list) to a more useful string (e.g. 'pfo')")

(defconst spydez/dev/system/name (system-name)
  "(Plain String) Intended for this specific computer's setup folder.")

(defconst spydez/dev/system/hash (spydez/hash-and-reduce
                                  spydez/dev/system/name
                                  spydez/dev/domain/subname)
  "(Hashed) Intended for this specific computer's setup folder.")

(defconst spydez/dev/setup/additional-required t
  "Whether this system should warn on bootstrap that it's missing something.")


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-domains)
