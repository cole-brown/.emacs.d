;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;; Stuff I don't quite want in my public repo will be in these somewhere...

;; TODO-SECRETS: Change from spydez/.../secrets... to secrets/... ??

(defconst spydez/dir/secrets (spydez/path/to-dir spydez/dir/home ".secrets.d")
  "Location of secrets dir on this computer.")
(defconst spydez/file/secrets
  (spydez/path/to-file spydez/dir/secrets "emacs.secrets.el.gpg")
  "Location of emacs' elisp secrets.")

(defconst spydez/dir/secrets/dev
  (spydez/path/to-dir spydez/dir/secrets "dev")
  "Dev config outside of .emacs.d.")

(defconst spydez/dir/secrets/dev/defaults
  (spydez/path/to-dir spydez/dir/secrets/dev "defaults")
  "All of my optional/default setup elisp files...")

(defconst spydez/dir/secrets/dev/domain-all
  (spydez/path/to-dir spydez/dir/secrets/dev "domains")
  "Domains folder. For subdirs of work, home, etc.")

(defconst spydez/dir/secrets/dev/domain-this
  (spydez/path/to-dir spydez/dir/secrets/dev/domain-all
                      spydez/setup/domain/name)
  "Anything that has to be domain specific. Tab widths or whatnot.")

(defconst spydez/dir/secrets/dev/system-all
  (spydez/path/to-dir spydez/dir/secrets/dev "computers")
  "Computers folder. For subdirs of different computers.")

(defconst spydez/dir/secrets/dev/system-this
  (spydez/path/to-dir spydez/dir/secrets/dev/system-all
                      spydez/setup/system/hash)
  "Anything that has to be computer specific. Overriding tab
  widths or whatnot.")


;;------------------------------------------------------------------------------
;; Path
;;------------------------------------------------------------------------------

;; Now we can push even more dirs onto the load path.
;; Also this may be getting a skosh out of hand...

;; NOTE: these are all higher priority than any .emacs.d path, right now,
;; so take care.
;; Leaving Off: spydez/dir/secrets/dev
(add-to-list 'load-path spydez/dir/secrets/dev/defaults)
(add-to-list 'load-path spydez/dir/secrets/dev/domain-all)
(add-to-list 'load-path spydez/dir/secrets/dev/domain-this)
(add-to-list 'load-path spydez/dir/secrets/dev/system-all)
(add-to-list 'load-path spydez/dir/secrets/dev/system-this)
;; Add highest-priority/most specific to this computer last.


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'dev-directories)
