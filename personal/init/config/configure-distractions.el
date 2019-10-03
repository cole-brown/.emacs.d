;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;--------------------------~♫~Distraction Force!~♫~---------------------------
;;--                                 Music.                                  --
;;-----------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Spotify
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-03]: Move these if-load -> if-bound -> use-package into a
;; helper for secrets-requiring stuff?

;; First (try to) load the auth secrets
(if (not (spydez/info/require 'spotify-auth nil 'noerror))
    (spydez/warning/message
     nil :error
     (concat "Cannot setup Spotify.el package correctly. "
             "No `spotify-auth' feature found."))
  ;; We have loaded the file and can check for our secrets now.
  (if (not (and (boundp 'spydez/secrets/spotify/client-secret)
                (boundp 'spydez/secrets/spotify/client-id)))
      (spydez/warning/message
       nil :error
       (concat "Cannot setup Spotify.el package correctly. "
               "No auth consts found. %s: %s, %s: %s")
               (symbol-name 'spydez/secrets/spotify/client-secret)
               (boundp 'spydez/secrets/spotify/client-secret)
               (symbol-name 'spydez/secrets/spotify/client-id)
               (boundp 'spydez/secrets/spotify/client-id))

    ;; secrets exist and we have done all the error checking. use-package it.
    (use-package spotify
      :load-path (lambda () (spydez/path/to-dir spydez/dir/packages/git-subtrees "spotify"))

      ;; Checked above and error/warning message, but can do here if lazy or
      ;; want quiet load/ignore.
      ;; ;; Only setup Spotify.el if we can talk to Spotify.
      ;; :when (and (boundp 'spydez/secrets/spotify/client-secret)
      ;;            (boundp 'spydez/secrets/spotify/client-id))

      ;;---
      :custom
      ;;---

      ;; Have to use "connect" (and have Spotify Premium) to use this package on
      ;; Windows because no DBUS.
      (spotify-transport 'connect)

      ;; refresh rate in seconds
      (spotify-mode-line-refresh-interval 5)

      ;; spotify-mode-line-format: default is "[%p: %a - %t ◷ %l %r%s]"
      ;;   - https://github.com/danielfm/spotify.el#customizing-the-mode-line
      ;; (setq spotify-mode-line-truncate-length 10) ;; default: 15

;;spotify-remote-mode-map
      ;;---
      :config
      ;;---

      ;; Secrets from... why do you want to know? Who are you?!
      ;; Don't set in custom. They shouldn't be defcustoms anyways so,
      ;; uh... yes. Set here in config.
      (setq spotify-oauth2-client-id     spydez/secrets/spotify/client-id)
      (setq spotify-oauth2-client-secret spydez/secrets/spotify/client-secret)

      ;; §-TODO-§ [2019-10-03]: Setup modeline? Or frame title?
      ;; §-TODO-§ [2019-10-03]: Setup keybinds?

      ;; either use spotify-remote-mode as wanted, or turn on globally:
      ;; (progn (setq debug-on-error t) (global-spotify-remote-mode))
      ;; (setq global-spotify-remote-mode nil)
      ;; Not turning auto-on as part of init, though.
      )))

;; §-TODO-§ [2019-10-03]: Hercules for the spotify mode map? Spotify's "M-p"
;;   prefix SUUUUUUUCKS and wants such an important (to me) & also default
;;   binding...


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-distractions)
