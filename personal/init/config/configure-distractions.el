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
    (spydez/message/warning
     nil :error
     (concat "Cannot setup Spotify.el package correctly. "
             "No `spotify-auth' feature found."))
  ;; We have loaded the file and can check for our secrets now.
  (if (not (and (boundp 'spydez/secrets/spotify/client-secret)
                (boundp 'spydez/secrets/spotify/client-id)))
      (spydez/message/warning
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

      ;; (spotify-player-status-truncate-length 10) ;; default: 15


      ;;---
      :config
      ;;---

      ;; Secrets from... why do you want to know? Who are you?!
      ;; Don't set in custom. They shouldn't be defcustoms anyways so,
      ;; uh... yes. Set here in config.
      (setq spotify-oauth2-client-id     spydez/secrets/spotify/client-id)
      (setq spotify-oauth2-client-secret spydez/secrets/spotify/client-secret)

      ;;---
      ;; Status Cache?
      ;;---
      (defvar spydez/spotify/player-status/cache nil
        "Tuple of (current-time raw-status) where raw-status is
untouched return value of `spotify-api-get-player-status'.")

      (defun spydez/spotify/player-status (field)
        "Returns value of field in cached status, or nil."
        (if-let* ((duration-format "%m:%02s")
                  (cache spydez/spotify/player-status/cache) ;; null check
                  (status (nth 1 cache))
                  (track (gethash 'item status)))
            ;; if-let* success case
            (cond
             ;; known fields and some aliases - field is first
             ((eq field 'artist)
              (gethash 'name (car (gethash 'artists track))))

             ((or (eq field 'track-name)
                  (eq field 'track_name)
                  (eq field 'name))
              (gethash 'name track))

             ((or (eq field 'track-number)
                  (eq field 'track_number)
                  (eq field 'number))
              (gethash 'track_number track))

             ((eq field 'duration)
              (gethash 'duration_ms track))
             ((eq field 'duration-formatted)
              (format-seconds "%m:%02s" (/ (gethash 'duration_ms track) 1000)))

             ((or (eq field 'player-shuffling)
                  (eq field 'player_shuffling)
                  (eq field 'shuffling))
              (not (eq (gethash 'shuffle_state status) :json-false)))

             ((or (eq field 'player-repeating)
                  (eq field 'player_repeating)
                  (eq field 'repeating))
              (not (string= (gethash 'repeat_state status) "off")))

             ((or (eq field 'player-state)
                  (eq field 'player_state)
                  (eq field 'playing))
              ;; ...if it's not not playing, it's playing!
              (not (eq (gethash 'is_playing status) :json-false)))
             ((eq field 'paused)
              ;; ...if it's not playing, it's paused?
              ;; What about stopped or not started yet? *shrug*
              (eq (gethash 'is_playing status) :json-false))

             ((eq field 'volume)
              (gethash 'volume_percent (gethash 'device status)))
             ((or (eq field 'mute)
                  (eq field 'muted))
              (= (gethash 'volume_percent (gethash 'device status)) 0))

             ;; Well... we can try to get it anyways?
             (t
              ;; gethash returns default of nil so I'm fine with blindly
              ;; trying. Warn first though.
              (spydez/message/debug
               nil
               (concat "spydez/spotify/player-status: "
                       "unknown field get attempting: %s")
               field)
              (or (gethash field status)
                  (gethash field track))))

          ;; if-let* fail case. Can debug it, but so far just nil for normal
          ;; things like no music for a while.
          ;; (spydez/message/debug
          ;;  nil
          ;;  (concat "spydez/spotify/player-status: Something null... "
          ;;          "cache?:%s status?:%s track?:%s")
          ;;  (not (null spydez/spotify/player-status/cache))
          ;;  (not (null (nth 1 spydez/spotify/player-status/cache)))
          ;;  (not (null (if (null (nth 1 spydez/spotify/player-status/cache))
          ;;                 nil
          ;;               (gethash 'item
          ;;                        (nth 1 spydez/spotify/player-status/cache))))))
          nil))
      ;; (spydez/spotify/player-status 'artist)
      ;; (spydez/spotify/player-status 'name)
      ;; (spydez/spotify/player-status 'track-number)
      ;; (spydez/spotify/player-status 'duration)
      ;; (spydez/spotify/player-status 'duration-formatted)
      ;; (spydez/spotify/player-status 'shuffling)
      ;; (spydez/spotify/player-status 'repeating)
      ;; (spydez/spotify/player-status 'playing)
      ;; (spydez/spotify/player-status 'paused)
      ;; (spydez/spotify/player-status 'volume)
      ;; (spydez/spotify/player-status 'muted)


      (defun spydez/advice/spotify/player-status/store (callback status)
        "Timestamp and hold onto status from
`spotify-api-get-player-status' for when we want status and don't
care about how uppest to datest it is."
        ;; cache status w/ timestamp
        (let ((time (current-time)))
          (when (or (null spydez/spotify/player-status/cache)
                    (time-less-p (car spydez/spotify/player-status/cache)
                                 (current-time)))
            (setq spydez/spotify/player-status/cache (list time status))))

        ;; and return unchanged status
        (funcall callback status))


      (defun spydez/advice/spotify/player-status/glom (args)
        "Function for grabbing onto spotify-api player status
calls in order to cache results. Insert our func as the
callback (which will then call correct callback)."
        (let ((callback (nth 0 args)))
          (list (lambda (status)
                  (funcall #'spydez/advice/spotify/player-status/store
                           callback
                           status)))))
      ;; (spydez/advice/spotify/player-status/glom '(jeff))

      (advice-add 'spotify-api-get-player-status
                  :filter-args
                  #'spydez/advice/spotify/player-status/glom)
      ;; (advice-remove 'spotify-api-get-player-status
      ;;                 #'spydez/advice/spotify/player-status/glom)

      ;;---
      ;; Status in Title Bar (before it was a feature in spotify.el)
      ;;---

      ;; §-TODO-§ [2019-10-11]: Move this to an issue or something?.. it was
      ;; good learnin' and I hate to see it go.
;;       ;; Currently Playing Info:
;;       ;;   Put in frame title? Modeline is full...
;;       ;; Take over this, maybe: `spotify-update-player-status'
;;       (defun spydez/frame/spotify/update-status (str)
;;         "Sets the given str to the frame title instead of the
;; modeline. This should take over from `spotify-update-player-status'."
;;         (when (not (string= str spotify-player-status))
;;           (setq spotify-player-status str)
;;           ;; There isn't a straight-forward way to tell frame to update name
;;           ;; based on `frame-title-format', but I did find this:
;;           ;;   https://stackoverflow.com/questions/13667702/how-to-force-emacs-update-frame-title
;;           (sit-for 0)
;;           ;; This would nuke whatever `frame-title-format' last set...
;;           ;; (modify-frame-parameters (selected-frame) (list (cons 'name title)))
;;           ))
;;       (setq spydez/spotify/orig-fn/spotify-update-player-status
;;             #'spotify-update-player-status)

;;       (defvar spydez/hook/spotify/entered nil
;;         "Non-nil if `spydez/hook/spotify-mode' has been entered and setup for
;;          `spotify-remote-mode'. Nil if it hasn't been entered yet, or if it has
;;          been used to exit and tear-down `spotify-remote-mode'.")

;;       (spydez/hook/defun-and-hooker spotify-remote-mode-hook t
;;           nil nil "init/config/configure-distractions.el"
;;         "Hook to enable/disable Spotify-Mode status in Frame Title."
;;         ;; skip setup/teardown?
;;         (unless (eq spotify-remote-mode spydez/hook/spotify/entered)
;;           (setq spydez/hook/spotify/entered spotify-remote-mode)

;;           (let ((status '(:eval (spotify-player-status-text))))
;;             (if spotify-remote-mode
;;                 ;; enter mode
;;                 (progn
;;                   (spydez/message/debug/when '(spydez debug hook)
;;                                            "Entering spotify-remote-mode?")
;;                   ;; Hook into the frame title
;;                   (unless (member status frame-title-format)
;;                     ;; Push our status string to the cdr of the last element of
;;                     ;; the `frame-title-format'. i.e. append status to end of
;;                     ;; list.
;;                     (push status (cdr (last frame-title-format))))
;;                   ;; and steal `spotify-update-player-status'
;;                   (fset #'spotify-update-player-status
;;                         #'spydez/frame/spotify/update-status))
;;               ;; exit mode
;;               (spydez/message/debug/when '(spydez debug hook)
;;                                        "Exiting spotify-remote-mode?")
;;               (when (member status frame-title-format)
;;                 (setq frame-title-format (remq s frame-title-format)))
;;               ;; and un-steal `spotify-update-player-status'
;;               (fset #'spotify-update-player-status
;;                     #'spydez/spotify/orig-fn/spotify-update-player-status)))))

      ;; Either use spotify-remote-mode as wanted, or turn on globally:
      ;; (progn (setq debug-on-error t) (global-spotify-remote-mode))
      ;; (setq global-spotify-remote-mode nil)
      ;; Not turning auto-on as part of init, though.
      ;; Seems to work fine right now as not-turned-on and with hydra calls.

      ;;---
      ;; Keybinds
      ;;---

      ;; Don't like the normal map... doing a hydra instead.
      (require 'with)
      (with-feature 'hydra
        (defhydra spydez/hydra/spotify (:color blue ;; default exit heads
                                        :idle 0.25   ;; no help for this long
                                        :hint none)  ;; no hint - just docstr
          "
^Track^                    ^Playlists^            ^Misc^
^-^------------------------^-^--------------------^-^-----------------
_p_: ?p?^^^^^^^            _l m_: My Lists        _d_:   Select Device
_b_: Back a Track          _l f_: Featured Lists
_f_: Forward a Track       _l u_: User Lists      _v u_: Volume Up
_M-r_: ?M-r?^^^^^^^^^^^^   _l s_: Search List     _v d_: Volume Down
_M-s_: ?M-s?^^^^^^^^^^^^^  _l c_: Create list     _v m_: ?v m?
_C-s_: Search Track
_C-r_: Recently Played  ^   ^                  _q_:   quit"

          ;;---
          ;; Track
          ;;---
          ("p" spotify-toggle-play
           (if (spydez/spotify/player-status 'playing)
               "Play Track"
             "Pause Track"))

          ("b" spotify-previous-track
               :color red)
          ("f" spotify-next-track
               :color red)

          ;; §-TODO-§ [2019-10-17]: this is kinda more playlist than track...
          ("M-r" spotify-toggle-repeat
           (concat (if (spydez/spotify/player-status 'repeating)
                       "[R] " "[-] ")
                   "Toggle Repeat"))
          ;; §-TODO-§ [2019-10-17]: this is kinda more playlist than track...
          ("M-s" spotify-toggle-shuffle
           (concat (if (spydez/spotify/player-status 'shuffling)
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
           (concat (if (spydez/spotify/player-status 'muted)
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
