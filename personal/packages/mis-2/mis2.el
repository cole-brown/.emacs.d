;; -*- mode: emacs-lisp; lexical-binding: t -*-


;; §-TODO-§ [2019-10-23]: Make sure warnings work?

;; §-TODO-§ [2019-10-23]: 'proper' header for 'proper' emacs pkg...
;;
;;   Functions for manipulating strings, propertizing them, sending
;; them to the *Messages* buffer as such...

;;-----------------------------strings and outputs------------------------------
;;--                               Make It So!                                --
;;------------------------------------------------------------------------------


(defgroup mis2 nil
  "Functions for manipulating strings, propertizing them, sending
them to the *Messages* buffer as such..."
  :group 'editing)


;; And now some settings before we include all our files...


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Helper for optional arg/custom settings.
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Pull in our bits and pieces...
;;------------------------------------------------------------------------------

;;---
;; Doesn't require any of the others right now...
;;---
;; (require 'mis2-debug)

;;---
;; Trying to be non-circularly referential with these...
;;---

(require 'mis2-themes)
(require 'mis2-settings)
(require 'mis2-contents)
(require 'mis2-message)

;; (require 'mis2-comment)
;; (require 'mis2-nomer)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2)
