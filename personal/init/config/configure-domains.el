;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------Um...-------------------------------------
;;--                     I hope it's easier than BIND...                      --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;==============================================================================
;;------------------------------------------------------------------------------
;; REDEFINE THESE!!! IN configure-domains-secret.el OR WHEREVER!!!
;;------------------------------------------------------------------------------
;;==============================================================================
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - - - - - - - - - - - - - - - - - START - - - - - - - - - - - - - - - - - - -

(defun spydez/domains/include/home ()
  "Adds dirs to load path and requires/loads files from the `:home' domain.

Intended for this to be overridden in configure-domains-secret.
Can just define as (ignore) or something.
"
  (warn "%s: Has not been defined to do anything useful."
        "spydez/domains/include/home"))


(defun spydez/domains/include/work ()
  "Adds dirs to load path and requires/loads files from the `:work' domain.

Intended for this to be overridden in configure-domains-secret.
Can just define as (ignore) or something.
"
  (warn "%s: Has not been defined to do anything useful."
        "spydez/domains/include/work"))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - - - - - - - - - - - - - - - - - - -END- - - - - - - - - - - - - - - - - - -
;;==============================================================================
;;------------------------------------------------------------------------------
;; REDEFINE THESE!!! IN configure-domains-secret.el OR WHEREVER!!!
;;------------------------------------------------------------------------------
;;==============================================================================


;;------------------------------------------------------------------------------
;; Includes
;;------------------------------------------------------------------------------

(defun spydez/domains/include (domain-key)
  "Adds dirs to load path and requires/loads files from the DOMAIN-KEY domain.
"
  (cond ((eq domain-key :home)
         (spydez/domains/include/home))

        ((eq domain-key :work)
         (spydez/domains/include/work))

        (t
         (warn "%s: Don't know what to do with domain-key '%s'"
               "spydez/domains/include"
               domain-key))))
;; (spydez/domains/include :jeff)

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-domains)
