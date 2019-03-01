;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst spydez/file/bootstrap/local "bootstrap-this-early.el"
  "Definitions for how this computer is different from others, for getting bootstrap started.")

(defconst spydez/setup/domain/name "work"
  "A domain/folder for setups similar to this. E.g. work vs home.")

;; This is currently one thing ("computer") for master list, and then changed
;; (in the master-list) to a more useful string (e.g. "pfo")
(defconst spydez/setup/domain/subname "computer"
  "A sub-domain/folder for setups similar to this. E.g. work laptop vs work PC.")

(defconst spydez/setup/system/name (system-name)
  "(Plain String) Intended for this specific computer's setup folder.")

(defconst spydez/setup/system/hash (spydez/hash-and-reduce spydez/setup/system/name spydez/setup/domain/subname)
  "(Hashed) Intended for this specific computer's setup folder.")


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-domains)
