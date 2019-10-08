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

      ;; spotify-mode-line-format: default is "[%p: %a - %t ◷ %l %r%s]"
      ;;   - https://github.com/danielfm/spotify.el#customizing-the-mode-line
      ;; I don't have that nice clock symbol in my font, do I?
      ;; §-TODO-§ [2019-10-03]: Remove unicode icon if I don't have it?
      ;; §-TODO-§ [2019-10-03]: or all-the-icons it?
      (spotify-mode-line-format "    <<Spotify: [%r%s %p: %a - %t (T%l)]>>")

      ;; Do I have unicode media icons? Doesn't seem so. :/
      ;;   Play:         ▶ / ▶️
      ;;   Pause:        ⏸ / ⏸️
      ;;   Play/Pause:   ⏯ / ⏯️
      ;;   Stop:         ⏹ / ⏹️
      ;;   Skip Back:    ⏮ / ⏮️
      ;;   Skip Forward: ⏭ / ⏭️
      ;;   Shuffle:      🔀 / 🔀️
      ;;   Repeat:       🔁 / 🔁️
      ;; (spotify-mode-line-playing-text       "▶")
      ;; (spotify-mode-line-paused-text        "⏸")
      ;; (spotify-mode-line-stopped-text       "⏹")
      ;; (spotify-mode-line-repeating-text     "🔁")
      ;; (spotify-mode-line-not-repeating-text "-")
      ;; (spotify-mode-line-shuffling-text     "🔀")
      ;; (spotify-mode-line-not-shuffling-text "-")
      ;; §-TODO-§ [2019-10-03]: Does all-the-icons have right icons and work?
      (spotify-mode-line-playing-text       "p")
      (spotify-mode-line-paused-text        "-")
      (spotify-mode-line-stopped-text       "x")
      (spotify-mode-line-repeating-text     "R")
      (spotify-mode-line-not-repeating-text "-")
      (spotify-mode-line-shuffling-text     "S")
      (spotify-mode-line-not-shuffling-text "-")

      ;; (spotify-mode-line-truncate-length 10) ;; default: 15

      ;;---
      :config
      ;;---

      ;; Secrets from... why do you want to know? Who are you?!
      ;; Don't set in custom. They shouldn't be defcustoms anyways so,
      ;; uh... yes. Set here in config.
      (setq spotify-oauth2-client-id     spydez/secrets/spotify/client-id)
      (setq spotify-oauth2-client-secret spydez/secrets/spotify/client-secret)

      ;; Currently Playing Info:
      ;;   Put in frame title? Modeline is full...
      ;; Take over this, maybe: `spotify-update-mode-line'
      (defun spydez/frame/spotify/update-status (str)
        "Sets the given str to the frame title instead of the
modeline. This should take over from `spotify-update-mode-line'."
        (when (not (string= str spotify-mode-line))
          (setq spotify-mode-line str)
          ;; There isn't a straight-forward way to tell frame to update name
          ;; based on `frame-title-format', but I did find this:
          ;;   https://stackoverflow.com/questions/13667702/how-to-force-emacs-update-frame-title
          (sit-for 0)
          ;; This would nuke whatever `frame-title-format' last set...
          ;; (modify-frame-parameters (selected-frame) (list (cons 'name title)))
          ))
      (setq spydez/spotify/orig-fn/spotify-update-mode-line
            #'spotify-update-mode-line)

      (defvar spydez/hook/spotify/entered nil
        "Non-nil if `spydez/hook/spotify-mode' has been entered and setup for
         `spotify-remote-mode'. Nil if it hasn't been entered yet, or if it has
         been used to exit and tear-down `spotify-remote-mode'.")

      (defun spydez/hook/spotify-mode ()
        "Hook to enable/disable Spotify-Mode status in Frame Title."
        ;; skip setup/teardown?
        (unless (eq spotify-remote-mode spydez/hook/spotify/entered)
          (setq spydez/hook/spotify/entered spotify-remote-mode)

          (let ((status '(:eval (spotify-mode-line-text))))
            (if spotify-remote-mode
                ;; enter mode
                (progn
                  (spydez/message/debug/when '(spydez debug hook)
                                           "Entering spotify-remote-mode?")
                  ;; Hook into the frame title
                  (unless (member status frame-title-format)
                    ;; Push our status string to the cdr of the last element of
                    ;; the `frame-title-format'. i.e. append status to end of
                    ;; list.
                    (push status (cdr (last frame-title-format))))
                  ;; and steal `spotify-update-mode-line'
                  (fset #'spotify-update-mode-line
                        #'spydez/frame/spotify/update-status))
              ;; exit mode
              (spydez/message/debug/when '(spydez debug hook)
                                       "Exiting spotify-remote-mode?")
              (when (member status frame-title-format)
                (setq frame-title-format (remq s frame-title-format)))
              ;; and un-steal `spotify-update-mode-line'
              (fset #'spotify-update-mode-line
                    #'spydez/spotify/orig-fn/spotify-update-mode-line)))))

      (add-hook 'spotify-remote-mode-hook 'spydez/hook/spotify-mode)
      ;; (add-hook 'global-spotify-remote-mode-hook 'spydez/hook/spotify-mode)
      ;; global-spotify-remote-mode-hook
      ;; spotify-remote-mode-hook

      ;; Either use spotify-remote-mode as wanted, or turn on globally:
      ;; (progn (setq debug-on-error t) (global-spotify-remote-mode))
      ;; (setq global-spotify-remote-mode nil)
      ;; Not turning auto-on as part of init, though.
      ;; Seems to work fine right now as not-turned-on and with hydra calls.

      ;;---
      ;; Spotify's "M-p" prefix SUUUUUUUCKS and wants such an important (to me)
      ;; & also default binding...
      ;;---

      (require 'with)
      (with-feature 'hydra
        (defhydra spydez/hydra/spotify (:color blue ;; default exit heads
                                        :idle 0.25   ;; no help for this long
                                        :hint none)  ;; no hint - just docstr
          "
^Track^                 ^Playlists^            ^Misc^
^-^---------------------^-^--------------------^-^-----------------
_p_: ?p?^^^^^^^         _l m_: My Lists        _d_:   Select Device
_b_: Back a Track       _l f_: Featured Lists
_f_: Forward a Track    _l u_: User Lists      _v u_: Volume Up
_M-r_: ?M-r?^^^^^^^^    _l s_: Search List     _v d_: Volume Down
_M-s_: ?M-s?^^^^^^^^^   _l c_: Create list     _v m_: ?v m?
_C-s_: Search Track
_C-r_: Recently Played  ^   ^                  _q_:   quit"

          ;;---
          ;; Track
          ;;---
          ("p" spotify-toggle-play
           ;; §-TODO-§ [2019-10-07]: dynamic text for which it would toggle to
           ;; Or Unicode. Or "[X] Shuffle"/"[ ] Shuffle"
           "Play/Pause")

          ("b" spotify-previous-track
               :color red)
          ("f" spotify-next-track
               :color red)

          ("M-r" spotify-toggle-repeat
           ;; §-TODO-§ [2019-10-07]: dynamic text for which it would toggle to
           ;; Or Unicode. Or "[X] Shuffle"/"[ ] Shuffle"
           "Toggle Repeat")
          ("M-s" spotify-toggle-shuffle
           ;; §-TODO-§ [2019-10-07]: dynamic text for which it would toggle to
           ;; Or Unicode. Or "[X] Shuffle"/"[ ] Shuffle"
           "Toggle Shuffle")
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
           ;; §-TODO-§ [2019-10-07]: dynamic text for which it would toggle to
           ;; Or Unicode. Or "[X] Shuffle"/"[ ] Shuffle"
           "Toggle Mute")

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
