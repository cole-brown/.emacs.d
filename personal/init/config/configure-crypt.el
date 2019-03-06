;; -*- emacs-lisp -*-


;;-----------------------------------uryyb--------------------------------------
;;--                   Guvf vf rapelcgrq rabhtu... evtug?                     --
;;------------------------------------------------------------------------------
;;      It was good enough for Caesar, and that guy makes great salads.


;; https://serverfault.com/questions/86048/gpg-what-do-i-need-to-backup
;;   - so... zip up that gpg dir if you think any magic blue smoke is about to escape?


;;------------------------------------------------------------------------------
;; EasyPG, GPG, Etc.
;;------------------------------------------------------------------------------
;; So much went wrong setting this up... see `spydez/dir/docs/issues' for
;; notes and solution.

;; This finally works. I can see my key; I can decrypt/encrypt my test file.
;; TODO: use vars and set 'em in an overridable manner.
;; TODO: figure out how to integrate this into use-tool
(require 'epa-file)

;; Can't use expand-file-name with hacky unixy paths...
;; So just muck it on our own and we're like a hacky onion. In muck.
(let ((spydez/hack/ugly-hacky-gpg-dir
       (concat (spydez/dir/windows-to-mingw spydez/dir/home) ".gnupg/")))
  ;; TODO: what the fuck to do with these... I don't really want these in
  ;; custom file...
  (custom-set-variables
   '(epg-gpg-home-directory spydez/hack/ugly-hacky-gpg-dir) ; unixy path instead of windowsy
   '(epg-gpg-program (executable-find "gpg"))               ; windowsy
   '(epg-gpgconf-program (executable-find "gpgconf"))       ; windowsy
   ))
;; TODO: get gpg more cross-computery via use-tool

;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/


;;------------------------------------------------------------------------------
;; Secrets.
;;------------------------------------------------------------------------------
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources

;; To make encrypted file:
;; Add to top of file: ;; -*- epa-file-encrypt-to: ("gpg2019@spydez.com") -*-
;;
;; Add your actual stuff.
;;
;; Explicitly encrypt with M-x epa-encrypt-file.

;; TODO: a way to defer the getting of secrets so we don't just hang loading?
;;   - doesn't seem to be hanging so probably ok. Leaving until home and work comps both... work.
(defconst spydez/dir/secrets (spydez/dir-name ".secrets.d" spydez/dir/home)
  "Location of secrets dir on this computer.")
(defconst spydez/file/secrets (expand-file-name "emacs.secrets.el.gpg" spydez/dir/secrets)
  "Location of emacs' elisp secrets.")


;;------------------------------------------------------------------------------
;; Auth-Source
;;------------------------------------------------------------------------------

;; https://www.gnu.org/software/emacs/manual/html_mono/auth.html

;; if we need to debug auth-source package issues, set to t:
(setq auth-source-debug nil)
;; Another useful function to call is M-x auth-source-forget-all-cached. Auth
;; source will cache your credentials in Emacs; use this command to forget all
;; the cached details.

;; TODO secrets in subfolder of .emacs.d, a single dot file outside, or what?
;; (setq auth-sources
;;       '((:source "~/.emacs.d/secrets/.authinfo.gpg")))

;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: hook up to org-mode for encrypting org files or specific
;; entries in a file.
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-crypt)
