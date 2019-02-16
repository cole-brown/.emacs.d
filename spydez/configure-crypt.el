;; -*- emacs-lisp -*-


;;-----------------------------------uryyb--------------------------------------
;;--                   Guvf vf rapelcgrq rabhtu... evtug?                     --
;;------------------------------------------------------------------------------
;;      It was good enough for Caesar, and that guy makes great salads.


;; https://serverfault.com/questions/86048/gpg-what-do-i-need-to-backup
;;   - so... zip up that gpg dir if you think any magic blue smoke is about to escape?

;;------------------------------------------------------------------------------
;; General Cursing and Gnashing of Teeth.
;;------------------------------------------------------------------------------


;; TODO: we expect GnuPG to exist, so make sure spydez/tools/external exists
;; and has "gpg" in it. Or do I already have a func for tool-exists?
;; Well it's worse than that and yes I do.


;; Followed this for making key, checking that it exists in both GPG
;; and M-x epa-list-secret-keys.
;;   https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources


;; Initial error:
;;   GnuPG: “decryption failed: secret key not available”
;;
;; NOTE: gpg version should be identical? Created a key on (a Git Bash) command line with
;;   > gpg --version
;;   gpg (GnuPG) 2.2.9-unknown
;;   libgcrypt 1.8.3
;; Then in emacs I tried to do stuff, and it encrypted it, but cannot decrypt it now?
;;   emacs
;;   M-x shell
;;   > gpg --version
;;   gpg (GnuPG) 1.4.21
;;
;; So... Got to find the uppest to datest GPG in emacs.
;;   https://stackoverflow.com/a/7319251

;; Ok. Had to chunk some old GitHub app junk in the Windows PATH, and then also
;; update my bootstrap-this-late.el to point to correct place (mingw
;; installation that's part of Git for Windows)
;;
;; Might should consider prepending external tools to paths instead of postpending.
;;
;; Now we have the correct version. We still can't do anything. :(


;; Error now:
;;   GPG error: "no usable configuration", OpenPGP

;; So... we're not working still. Internet says we need this:
;;   (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg2"))
;;
;; Or maybe this:
;;   (custom-set-variables
;;    '(epg-gpg-home-directory "c:/Users/MYUSER/AppData/Roaming/gnupg")
;;    '(epg-gpg-program "C:/Progra~2/gnupg/bin/gpg.exe")
;;    '(epg-gpgconf-program "c:/progra~2/gnupg/bin/gpgconf.exe")
;;   )
;;  https://emacs.stackexchange.com/questions/21699/how-to-configure-easypg-with-gpg4win-or-any-other-windows-gpg-version

;; So here. Does this work?
;;   (require 'epa-file)
;;   (custom-set-variables
;;    '(epg-gpg-home-directory (spydez/dir-name ".gnupg" (getenv "HOME")))
;;    )
;; No. Error now:
;;   GPG error: "no usable configuration", OpenPGP

;; Or this?!
;;   (require 'epa-file)
;;   (custom-set-variables
;;    '(epg-gpg-home-directory (spydez/dir-name ".gnupg" (getenv "HOME")))
;;    '(epg-gpg-program (executable-find "gpg"))
;;    '(epg-gpgconf-program (executable-find "gpgconf"))
;;    )
;; ...Maybe? Probably not?
;; emacs:
;;   M-x epa-list-secret-keys
;;   > just an empty buffer
;; bash:
;;   gpg -k (or gpg -K)
;;   > info about my gpg key.

;; So, no. Still doesn't work. Try to open my test encrypted gpg file and get:
;;   Error while decrypting with "c:/Program Files/Git/usr/bin/gpg.exe":
;;   gpg: Fatal: can't create directory '/c/home/cole/.emacs.d/spydez/c:/home/cole/.gnupg': No such file or directory

;; So............ We're in some unholy limbo of windows and linux... -_-
;;
;; gpgconf's output is linuxy, in Git Bash anyways. That may be throwing it off the scent.
;; Options maybe?:
;;   1) Install a GnuPG that's for windows and don't rely on Git for Windows to provide it.
;;   2) Bash on ahead like all we have is a hammer.

;; So, dumb idea:
;;   (require 'epa-file)
;;   (custom-set-variables
;;    '(epg-gpg-home-directory "/c/home/cole/.gnupg/") ;; nixy path instead of windowsy
;;    '(epg-gpg-program (executable-find "gpg"))
;;    '(epg-gpgconf-program (executable-find "gpgconf"))
;;    )

;; Dumb idea may have worked.
;; emacs:
;;   M-x epa-list-secret-keys
;;   > info about my gpg key.
;; bash:
;;   gpg -k (or gpg -K)
;;   > info about my gpg key.

;; Can I simplify down to just the home dir? That'll make it slightly less of a
;; complete abomination.
;;   (require 'epa-file)
;;   (custom-set-variables
;;    '(epg-gpg-home-directory "/c/home/cole/.gnupg/") ;; nixy path instead of windowsy
;;    )

;; emacs:
;;   M-x epa-list-secret-keys
;;   GPG error: "no usable configuration", OpenPGP
;; Nope. Gotta be NC-17 full frontal abomination.


;;------------------------------------------------------------------------------
;;                         General Middle Fingers to:
;;
;;   EPA, Emacs, GPG, Bash, Git, Windows, Git for Windows, Bash for Git for
;;                 Windows, GPG for Bash for Git for Windows,
;;      and optimism (I had so much I thought "one more quick feature")
;;------------------------------------------------------------------------------

;; So this one finally works. I can see my key; I can decrypt/encrypt my test file.
;; TODO: use vars and set 'em in an overridable manner.
(require 'epa-file)
(custom-set-variables
 '(epg-gpg-home-directory "/c/home/cole/.gnupg/")   ; unixy path instead of windowsy
 '(epg-gpg-program (executable-find "gpg"))         ; windowsy
 '(epg-gpgconf-program (executable-find "gpgconf")) ; windowsy
 )

;; Don'th think I need this:
;;;;(epa-file-enable)
;; Possibly don't need (require 'epa-file) either.


;; leaving off here:
;;   https://www.google.com/search?q=emacs%20encrypt%20decrypt
;;     https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
;;     https://github.com/syohex/forecast.el
;;       https://darksky.net/forecast/40.7127,-74.0059/us12/en
;;     http://pages.sachachua.com/.emacs.d/Sacha.html
;;     https://www.reddit.com/r/emacs/comments/aqss7l/anyone_store_password_in_emacs/
;;     https://zzamboni.org/post/my-emacs-configuration-with-commentary/


;;------------------------------------------------------------------------------
;; Secrets.
;;------------------------------------------------------------------------------
;; Next Up: An emacs.secrets file/folder/repo.


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
