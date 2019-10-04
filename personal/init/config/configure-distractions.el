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

      ;; Refresh rate in seconds.
      ;; This has to make an API call every time, so... don't overdo it and
      ;; throttle yourself.
      (spotify-mode-line-refresh-interval 10)

      ;; Currently Playing Info:
      ;;   §-TODO-§ [2019-10-03]: Put in frame title? Modeline is full...
      ;; Take over this, maybe: `spotify-update-mode-line'

      ;; spotify-mode-line-format: default is "[%p: %a - %t ◷ %l %r%s]"
      ;;   - https://github.com/danielfm/spotify.el#customizing-the-mode-line
      ;; I don't have that nice clock symbol in my font, do I?
      ;; §-TODO-§ [2019-10-03]: Remove unicode icon if I don't have it?
      ;; §-TODO-§ [2019-10-03]: or all-the-icons it?
      (spotify-mode-line-format "[%r%s %p: %a - %t (◷%l)]")

      ;; Do I have unicode media icons?
      ;;   Play:         ▶ / ▶️
      ;;   Pause:        ⏸ / ⏸️
      ;;   Play/Pause:   ⏯ / ⏯️
      ;;   Stop:         ⏹ / ⏹️
      ;;   Skip Back:    ⏮ / ⏮️
      ;;   Skip Forward: ⏭ / ⏭️
      ;;   Shuffle:      🔀 / 🔀️
      ;;   Repeat:       🔁 / 🔁️
      ;; §-TODO-§ [2019-10-03]: Remove unicode icons if I don't have them?
      ;; §-TODO-§ [2019-10-03]: or all-the-icons them?
      (spotify-mode-line-playing-text       "▶")
      (spotify-mode-line-paused-text        "⏸")
      (spotify-mode-line-stopped-text       "⏹")
      (spotify-mode-line-repeating-text     "🔁")
      (spotify-mode-line-not-repeating-text "-")
      (spotify-mode-line-shuffling-text     "🔀")
      (spotify-mode-line-not-shuffling-text "-")

      ;; (setq spotify-mode-line-truncate-length 10) ;; default: 15

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


      ;; §-TODO-§ [2019-10-03]: bind a turn on, then rebind as hercules/hydra when on?
      ;; (bind-key "C-c m" #'global-spotify-remote-mode) ;; bind onto the global map
      ;; Bind that, hook into spotify-remote-mode. Run hercules-def there?

      ;; §-TODO-§ [2019-10-03]: Oh... comment differently like this?
      ;; (hercules-def
      ;;  :toggle-funs '(spotify-remote-mode)
      ;;  :keymap 'spotify-remote-mode-map
      ;;  :transient t)


      ;; Can't run this here as it doesn't know about spotify-remote-mode now.
      ;;
      ;; Hercules for the spotify mode map? Spotify's "M-p"
      ;; prefix SUUUUUUUCKS and wants such an important (to me) & also default
      ;; binding...
      ;;
      ;; TRIAL [2019-10-03]: Is this any good? Trying it out instead of hydra or
      ;; lots of redefined keybinds.
      ;; (hercules-def
      ;;  :toggle-funs (#'spotify-remote-mode)
      ;;  :keymap 'spotify-remote-mode-map
      ;;  :transient t)
      ;; Bind-key doesn't work with maps...
      ;; (bind-key "C-c m" #'spotify-remote-mode) ;; bind onto the global map
      ;; (define-key global-map (kbd "C-c m") #'spotify-remote-mode) ;; bind onto the global map
      )))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-distractions)
