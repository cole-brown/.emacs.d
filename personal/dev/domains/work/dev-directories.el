;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst spydez/dir/common-doc-save (spydez/dir-name  "documents" spydez/dir/home)
  "Place for auto-open files or secrets or something to be.")

(defconst spydez/dir/org-docs (spydez/dir-name "org" spydez/dir/common-doc-save)
  "Generic place to put general org docs.")

;; Stuff I don't quite want in my public repo will be in these somewhere...

;; TODO: Change from spydez/.../secrets... to secrets/... ??

(defconst spydez/dir/secrets (spydez/dir-name ".secrets.d" spydez/dir/home)
  "Location of secrets dir on this computer.")
(defconst spydez/file/secrets (expand-file-name "emacs.secrets.el.gpg" spydez/dir/secrets)
  "Location of emacs' elisp secrets.")

(defconst spydez/dir/secrets/dev
  (spydez/dir-name "dev" spydez/dir/secrets)
  "Dev config outside of .emacs.d.")

(defconst spydez/dir/secrets/dev/defaults
  (spydez/dir-name "defaults" spydez/dir/secrets/dev)
  "All of my optional/default setup elisp files...")

(defconst spydez/dir/secrets/dev/domain-all
  (spydez/dir-name "domains" spydez/dir/secrets/dev)
  "Domains folder. For subdirs of work, home, etc.")

(defconst spydez/dir/secrets/dev/domain-this
  (spydez/dir-name spydez/setup/domain/name spydez/dir/secrets/dev/domain-all)
  "Anything that has to be domain specific. Tab widths or whatnot.")

(defconst spydez/dir/secrets/dev/system-all
  (spydez/dir-name "computers" spydez/dir/secrets/dev)
  "Computers folder. For subdirs of different computers.")

(defconst spydez/dir/secrets/dev/system-this
  (spydez/dir-name spydez/setup/system/hash spydez/dir/secrets/dev/system-all)
  "Anything that has to be computer specific. Overriding tab widths or whatnot.")


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
(add-to-list 'load-path spydez/dir/secrets/dev/system-this) ;; most specific to this computer last


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'dev-directories)
