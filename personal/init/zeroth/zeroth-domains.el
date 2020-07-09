;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;; These all should start as these values and then be corrected per-computer
;; in master-list.el.

(defconst spydez/dev/domain/valid-list
  '("home"
    "work")
  "Valid domains.")


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
;; Functions
;;------------------------------------------------------------------------------

(defvar spydez/dev/domain/switch/history nil
  "Var to hold separate history for `spydez/dev/domain/switch' command.")


(defun spydez/dev/domain/switch (new-domain)
  "Switch between domains. If `new-domain' is nil, interactively choose from
valid options.
"
  (interactive
   (list
    (completing-read "Choose Domain: "
                     spydez/dev/domain/valid-list
                     nil
                     ;; Make them confirm if not on the list...
                     'confirm
                     ;; default input, history stuff
                     nil
                     spydez/dev/domain/switch/history
                     nil)))

  (setq spydez/dev/domain/name new-domain)

  ;; TODO: the rest of the stuff...
  )


(defmacro spydez/dev/domain/with (domain &rest body)
  "Lexically set `spydez/dev/domain/name' to DOMAIN, then run BODY in that
lexical context.
"
  (declare (indent defun))
  `(let ((spydez/dev/domain/name ,domain))
     ,@body))
;; (spydez/dev/domain/with "work"
;;   (message "%s" spydez/dev/domain/name))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-domains)
