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
  :init
  ;;---

  (defun spydez/redtick/notification (cycles &optional dings text)
    (if (or
         ;; bad cycles?
         (not cycles)
         (not (integerp cycles))
         (not (> cycles 0))
         ;; Or... have dings and bad dings?
         (and (not (null dings))
              (or (not (integerp dings))
                  (not (> dings 0)))))
        ;; failed validation - error out
        (error "spydez/redtick/notification: Cannot notify. %S %S %S"
               cycles dings text)

      (let ((cycles (1- cycles)) ;; doing first cycle immediately
            (flash-sec (/ 1.0 5)))
        ;; First cycle on/ding
        (invert-face 'mode-line)

        (when (and dings (> dings 0))
          (ding)
          (setq dings (1- dings)))

        ;; Revert
        (run-with-timer flash-sec nil #'invert-face 'mode-line)

        ;; Cycle timers are doubled up time-wise, so take care.
        ;; Cycle 0: 0           -> f-sec*1
        ;; Cycle 1: f-sec*2     -> f-sec*3
        ;; Cycle 2: f-sec*4     -> f-sec*5
        ;; Cycle N: f-sec*(N*2) -> f-sec*(N*2+1)
        ;;
        ;; Take double care because we took one out of cycles because of the
        ;; first, immediate cycle.
        ;; So... Cycle N where N > 1:
        ;;  f-sec * ((N+1)*2) -> f-sec * ((N+1)*2 + 1)

        ;; Rest of the cycles...
        (dotimes (i cycles)
          ;; Nth cycle on/ding
          (run-with-timer (* (1+ i) 2 flash-sec)
                          nil #'invert-face 'mode-line)

          (when (and dings (> dings 0))
            (run-with-timer (* (1+ i) 2 flash-sec)
                            nil #'ding)
            (setq dings (1- dings)))

          ;; Revert
          (run-with-timer (* (1+ (* (1+ i) 2)) flash-sec)
                          nil #'invert-face 'mode-line)))

      (if text
          (message text))))
  ;; (spydez/redtick/notification 2 1 "hi 1 ding")
  ;; (spydez/redtick/notification 2 2 "hi 2 dings")
  ;; (spydez/redtick/notification 4 nil "hi 0 dings")


  (spydez/hook/defun redtick-before-work-hook t
                     nil nil "init/config/configure-distractions.el"
    "Hooks into redtick's before-work hook."
    ;; Don't think I want to do anything - I invoked the thing and this
    ;; immediately goes, right?
    (message "Redtick Start: Good luck... You're gonna need it.")
    (ignore))


  (spydez/hook/defun redtick-before-rest-hook t
                     nil nil "init/config/configure-distractions.el"
    "Hooks into redtick's before-rest hook."
    (spydez/redtick/notification 4 2
                                 "Redtick: Pomodoro is done... Stop work?")
    (ignore))


  (spydez/hook/defun redtick-after-rest-hook t
                     nil nil "init/config/configure-distractions.el"
    "Hooks into redtick's before-rest hook."
    (spydez/redtick/notification 2 1
                                 "Redtick: Downtime is done... Go again?")
    (ignore))


  ;;---
  :hook
  ;;---
  ((redtick-before-work . spydez/hook/redtick-before-work-hook)
   (redtick-before-rest . spydez/hook/redtick-before-rest-hook)
   (redtick-after-rest  . spydez/hook/redtick-after-rest-hook))


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
      :load-path (lambda () (spydez/path/to-dir
                             spydez/dir/packages/git-submodules "spotify"))

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

      ;; my modified spotify stuff...
      (spotify-cache-player-status-enabled t)
      (spotify-hydra-keybind "C-c m")
      (spotify-hydra-player-status-format "%a - %t")
      (spotify-hydra-player-status-truncate nil)
      (spotify-hydra-auto-remote-mode t)

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
      ;; Misc
      ;;---

      ;; (defun spydez/spotify/smart-mode-or-hydra ()
      ;;   "Enters `spotify-remote-mode' if not in it,
      ;;      else calls `spydez/hydra/spotify/body'."
      ;;   (interactive)
      ;;   (if spotify-remote-mode
      ;;       (call-interactively #'spydez/hydra/spotify/body)
      ;;     (global-spotify-remote-mode)
      ;;     (call-interactively #'spotify-select-device)))

      ;; ;; bind onto the global map in "C-c"/user-binds section.
      ;; ;; 'm' for music? idk...
      ;; (bind-key "C-c m" #'spydez/spotify/smart-mode-or-hydra))

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
      )))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-distractions)
