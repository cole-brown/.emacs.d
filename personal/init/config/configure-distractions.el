;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;--------------------------~♫~Distraction Force!~♫~---------------------------
;;--                                 Music.                                  --
;;-----------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; RedTick - Pomodoro Timer
;;------------------------------------------------------------------------------

;; Launch a pomodoro by:
;;  - M-x redtick
;;  - M-x redtick-with-description RET (manual description) RET

;; Check up on your history?
;; (find-file redtick-history-file)
;; --> added to grab-bag

;; A simple pomodoro timer. No org-mode integration or any of that.
;; https://github.com/ferfebles/redtick
;; https://melpa.org/#/redtick
(use-package redtick
  :ensure t

  ;; You should install SoX (Sound eXchange http://sox.sourceforge.net) if you
  ;; want to hear the clock ticking! (be careful, in windows you should clear
  ;; the "" in the sox path to allow emacs to find the executable)

  ;;---
  :custom
  ;;---
  ;; NOTE: Placeholder! Should get overridden in <secrets>/finalize-domain.el
  ;; or somewhere.
  ;; TODO: default to home or org-docs dir? would probably need to def org-docs
  ;; in .emacs.d if defaulting to it...
  ;; (redtick-history-file (spydez/path/to-file spydez/dir/org-docs "logbook"
  (redtick-history-file (spydez/path/to-file spydez/dir/home "logbook"
                                             spydez/dev/system/hash
                                             "redtick-history.txt")
                        "Default. Overridden in finalize-domain.el?")

  ;;---
  :config
  ;;---

  ;; Stuff the redtick timer into the moody time tab.
  (when (spydez/moody/managing-time)

    (defun spydez/redtick/mode-line-misc-info ()
      "Redtick puts this into `mode-line-misc-info'... which we
ignore when moody is managing the time tab."
      (if (and redtick-mode (redtick--selected-window-p))
          redtick--current-bar))

    ;; add redtick to our time tab
    (add-to-list 'spydez/moody/mode-line-misc-info/inside-parts
                 #'spydez/redtick/mode-line-misc-info
                 t)))


;;------------------------------------------------------------------------------
;; Spotify
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-03]: Move these if-load -> if-bound -> use-package into a
;; helper for secrets-requiring stuff?

;; First (try to) load the auth secrets
(if (not (spydez/require 'spotify-auth nil 'noerror))
    (mis/warning
     nil :error
     (concat "Cannot setup Spotify.el package correctly. "
             "No `spotify-auth' feature found."))
  ;; We have loaded the file and can check for our secrets now.
  (if (not (and (boundp 'spydez/secrets/spotify/client-secret)
                (boundp 'spydez/secrets/spotify/client-id)))
      (mis/warning
       nil :error
       (concat "Cannot setup Spotify.el package correctly. "
               "No auth consts found. %s: %s, %s: %s")
               (symbol-name 'spydez/secrets/spotify/client-secret)
               (boundp 'spydez/secrets/spotify/client-secret)
               (symbol-name 'spydez/secrets/spotify/client-id)
               (boundp 'spydez/secrets/spotify/client-id))

    ;; secrets exist and we have done all the error checking. use-package it.
    (use-package spotify
      :load-path (lambda () (spydez/path/to-dir spydez/dir/packages/git-submodules "spotify"))

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
      (spotify-player-status-refresh-interval 10)

      ;; Put status in title-bar instead of modeline
      (spotify-status-location 'title-bar)

      ;; spotify-player-status-format: default is "[%p: %a - %t ◷ %l %r%s]"
      ;;   - https://github.com/danielfm/spotify.el#customizing-the-player-status
      ;; I don't have that nice clock symbol in my font, do I?
      ;; §-TODO-§ [2019-10-03]: Remove unicode icon if I don't have it?
      ;; §-TODO-§ [2019-10-03]: or all-the-icons it?
      (spotify-player-status-format "╠ <Spotify: [%r%s %p: %a - %t (T%l)]> ╣")
      (spotify-title-bar-separator "      ┅┅      ")
      ;; frame-title-format

      ;; Do I have unicode media icons? Doesn't seem so. :/
      ;;   Play:         ▶ / ▶️
      ;;   Pause:        ⏸ / ⏸️
      ;;   Play/Pause:   ⏯ / ⏯️
      ;;   Stop:         ⏹ / ⏹️
      ;;   Skip Back:    ⏮ / ⏮️
      ;;   Skip Forward: ⏭ / ⏭️
      ;;   Shuffle:      🔀 / 🔀️
      ;;   Repeat:       🔁 / 🔁️
      ;; (spotify-player-status-playing-text       "▶")
      ;; (spotify-player-status-paused-text        "⏸")
      ;; (spotify-player-status-stopped-text       "⏹")
      ;; (spotify-player-status-repeating-text     "🔁")
      ;; (spotify-player-status-not-repeating-text "-")
      ;; (spotify-player-status-shuffling-text     "🔀")
      ;; (spotify-player-status-not-shuffling-text "-")
      ;; §-TODO-§ [2019-10-03]: Does all-the-icons have right icons and work?
      (spotify-player-status-playing-text       "p")
      (spotify-player-status-paused-text        "-")
      (spotify-player-status-stopped-text       "x")
      (spotify-player-status-repeating-text     "R")
      (spotify-player-status-not-repeating-text "-")
      (spotify-player-status-shuffling-text     "S")
      (spotify-player-status-not-shuffling-text "-")

      ;; I got a lot of room in the titlebar...
      (spotify-player-status-truncate-length 30) ;; default: 15

      (spotify-player-status-cache-enabled t)

      ;;---
      :config
      ;;---

      ;; Secrets from... why do you want to know? Who are you?!
      ;; Don't set in custom. They shouldn't be defcustoms anyways so,
      ;; uh... yes. Set here in config.
      (setq spotify-oauth2-client-id     spydez/secrets/spotify/client-id)
      (setq spotify-oauth2-client-secret spydez/secrets/spotify/client-secret)

      ;;---
      ;; Status in Title Bar (before it was a feature in spotify.el)
      ;; (spydez/help/issue/visit "spotify" "title-bar-status.org")
      ;;---


      ;;---
      ;; Keybinds
      ;;---

      ;; §-TODO-§ [2019-10-25]: Move to spotify-hydra.el to show off cache?

      ;; Don't like the normal map... doing a hydra instead.
      (require 'with)
      (with-feature 'hydra
        (defun spydez/spotify/now-playing ()
          (if-let ((artist (spotify-player-status-field 'artist))
                   (track (spotify-player-status-field 'track)))
              ;; 64 is the length of the dashed line in the hydra
              (s-center 64 (format "%s - %s" artist track))
            ""))


        (defhydra spydez/hydra/spotify (:color blue ;; default exit heads
                                        :idle 0.25   ;; no help for this long
                                        :hint none)  ;; no hint - just docstr
          "
%s(spydez/spotify/now-playing)
^Track^                    ^Playlists^            ^Misc^
^-^------------------------^-^--------------------^-^-----------------
_p_: ?p?^^^^^^^^           _l m_: My Lists        _d_:   Select Device
_b_: Back a Track          _l f_: Featured Lists
_f_: Forward a Track       _l u_: User Lists      _v u_: Volume Up
_M-r_: ?M-r?^^^^^^^^^^^^   _l s_: Search List     _v d_: Volume Down
_M-s_: ?M-s?^^^^^^^^^^^^^  _l c_: Create list     _v m_: ?v m?
_C-s_: Search Track
_C-r_: Recently Played     ^   ^                  _q_:   quit"

          ;;---
          ;; Track
          ;;---
          ("p" spotify-toggle-play
           (format "%-11s" (if (spotify-player-status-field 'playing)
                               "Pause Track"
                             "Play Track")))

          ("b" spotify-previous-track
               :color red)
          ("f" spotify-next-track
               :color red)

          ;; §-TODO-§ [2019-10-17]: this is kinda more playlist than track...
          ("M-r" spotify-toggle-repeat
           (concat (if (spotify-player-status-field 'repeating)
                       "[R] " "[-] ")
                   "Toggle Repeat"))
          ;; §-TODO-§ [2019-10-17]: this is kinda more playlist than track...
          ("M-s" spotify-toggle-shuffle
           (concat (if (spotify-player-status-field 'shuffling)
                       "[S] " "[-] ")
                   "Toggle Shuffle"))
          ("C-s" spotify-track-search)
          ("C-r" spotify-recently-played)

          ;;---
          ;; Playlist
          ;;---
          ("l m" spotify-my-playlists)
          ("l f" spotify-featured-playlists)
          ("l u" spotify-user-playlists)
          ("l s" spotify-playlist-search)
          ("l c" spotify-create-playlist)

          ;;---
          ;; Volume & Misc
          ;;---
          ("v u" spotify-volume-up
                 :color red)
          ("v d" spotify-volume-down
                 :color red)
          ("v m" spotify-volume-mute-unmute
           (concat (if (spotify-player-status-field 'muted)
                       "[M] " "[-] ")
                   "Toggle Mute"))

          ("d"   spotify-select-device)

          ("q"   nil))

        (defun spydez/spotify/smart-mode-or-hydra ()
          "Enters `spotify-remote-mode' if not in it,
           else calls `spydez/hydra/spotify/body'."
          (interactive)
          (if spotify-remote-mode
              (call-interactively #'spydez/hydra/spotify/body)
            (global-spotify-remote-mode)
            (call-interactively #'spotify-select-device)))

        ;; bind onto the global map in "C-c"/user-binds section.
        ;; 'm' for music? idk...
        (bind-key "C-c m" #'spydez/spotify/smart-mode-or-hydra))

      (defun spydez/spotify/go-home ()
        "Cleans up spotify for the day so it hopefully doesn't
have 11 zombie connections to spotify api tomorrow..."
        (interactive)
        (condition-case-unless-debug err
            (progn
              (spotify-connect-player-pause)
              (global-spotify-remote-mode -1)
              ;; (spydez/buffer/kill-matching ...)?
              )
          ;; Catch signaled error 'error': downgrade to just message.
          ;; [2019-10-22]: This is just theory as spotify can get cranky if
          ;; connected but device was left paused...
          (error (message
                  "error: spydez/spotify/go-home: received error signal:" err))))

      ;; Don't like hercules for this. It wants clear enter/exit functions.
      ;; (hercules-def
      ;;  :toggle-funs '(spotify-remote-mode)
      ;;  :keymap 'spotify-remote-mode-map
      ;;  :transient t)
      ;; (bind-key "C-c m" #'spotify-remote-mode) ;; bind onto the global map
      )))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-distractions)
