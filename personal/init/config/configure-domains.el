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
;; Interactives
;;------------------------------------------------------------------------------

(defvar spydez/dirky/dired/history nil
   "Var to hold separate history for `spydez/dirky/dired' command.")

(defun spydez/dirky/dired ()
  "Visit a dirky directory via dired.
"
  (interactive)
  (let* ((keybase
          ;; Keybase get.
          (intern
           (completing-read
            "Keybase:"

            ;; Keybase can be any of these keys ("columns") from the alist:
            ;; ...which we will complicate a bit so that some popular ones are
            ;; up top.
            (-distinct
             (-flatten
              (list
               ;; Some things to get preferred up towards the top...
               (-select-column 0 spydez/dev/domains)
               :default
               :secrets

               ;; And this actually has all the keys
               (-select-column 0 spydez/dirky/keys))))
            ;; Already did our list filter shenanigans, thanks.
            nil

            ;; Make 'em confirm if going off the menu.
            'confirm

            nil
            nil
            ;; Do I want a default value?
            spydez/dirky/domain)))

         ;; Dirkey get.
         (dirkey
          (intern
           (completing-read
            "Dir Key:"

            ;; Keybase can be any of these keys ("columns") from the
            ;; alist of our keybase:
            (-select-column 0 (alist-get keybase
                                         spydez/dirky/keys))
            nil

            ;; Make 'em confirm if going off the menu.
            'confirm

            nil
            ;; History for this arg of this command.
            'spydez/dirky/dired/history)))

         ;; Might as well build the path here too.
         (path (spydez/dirky/path keybase dirkey)))

    (message "Dirky<%S %S>: %S"
             keybase dirkey path)
    (dired path)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-domains)
