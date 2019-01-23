;; -*- emacs-lisp -*-

;;------------------------------------------------------------------------------
;; Window/GUI Setup
;;------------------------------------------------------------------------------

;; Don't show the GNU splash.
(setq inhibit-startup-screen t)

;; I like the menu bar right now... (File, Edit, etc)
(when (fboundp 'menu-bar-mode) (menu-bar-mode 1))
;; Tool bar must go. (new, open, etc buttons).
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Scroll bar useful for buffer size/position at-a-glance.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))

;; Winner-mode lets you use C-c <left> and C-c <right> to switch between window
;; configurations. This is handy when something has popped up a buffer that you
;; want to look at briefly before returning to whatever you were working
;; on. When you're done, press C-c <left>.
(when (fboundp 'winner-mode) (winner-mode 1))
;; https://www.emacswiki.org/emacs/WinnerMode
;; Some use use-package for this... http://pages.sachachua.com/.emacs.d/Sacha.html#org59481f4

;; todo: window config?
;; https://www.emacswiki.org/emacs/WindowConfiguration

;; todo: fonts here, own configure file, or custom.el?
;; See: http://pages.sachachua.com/.emacs.d/Sacha.html (find "when window") for a
;; set conditional on gui interface.


;;------------------------------------------------------------------------------
;; Color scheme: Zenburn
;;------------------------------------------------------------------------------
(use-package zenburn-theme)
;; Seems to work fine without 'load-theme


;;------------------------------------------------------------------------------
;; Mode line: Time
;;------------------------------------------------------------------------------
;; Puts a clock down in the mode line.

;; For simple 24hr time:
;; (setq display-time-24hr-format t)
;; (display-time-mode t)

;; For ISO time:
;; https://emacs.stackexchange.com/questions/7365/how-to-display-date-in-julian-in-the-mode-line
(require 'calendar)
;; Set format to: yyyy-mm-dd HH:MM
;; (trimmed down from: yy-mm-dd HH:MM:SS (Time Zone) <Mail notify>
(setq display-time-string-forms
    ;; 2 digit year: '((substring year -2) "/" month "/" day
    '(year "/" month "/" day
      " " 24-hours ":" minutes ; ":" seconds
      ; Long-ass TZ: (if time-zone " (") time-zone (if time-zone ")")
      ; Mail notice: (if mail " Mail" "")
      ))
(display-time-mode t)

;; todo: color clock if late, or approaching late?


;;------------------------------------------------------------------------------
;; Jump between Emacs windows more efficientcly.
;;------------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/WindMove
;; http://pragmaticemacs.com/emacs/whizz-between-windows-with-windmove/
;; Trial: [2019-01-23]
(use-package windmove
  ;; 2 column editing uses F2... -_- So no to this then as well.
  ;; ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Two_002dColumn.html
  ;; :bind
  ;; (("<f2> <right>" . windmove-right)
  ;;  ("<f2> <left>" . windmove-left)
  ;;  ("<f2> <up>" . windmove-up)
  ;;  ("<f2> <down>" . windmove-down)
  ;;  )

  :config
  ;; No to this.
  ;; Super + arrows on Windows is a command I use for organizing Windows' windows.
  ;; Trying Sacha's F2+arrows for now.
  ;; ;; use command key on Mac
  ;; (windmove-default-keybindings 'super)

  ;; wrap around at edges
  (setq windmove-wrap-around t)
  )
;; TODO: Not quite sure what to do with this.
;; TODO: Try out default for now - see how much my org-mode usage (not much)
;;   is affected (probably also not much).

;; TODO: try ace-window instead? Or dump this if not used much.
;;   - https://www.reddit.com/r/emacs/comments/7evidd/windmove_shortcuts/
;;     -> https://github.com/abo-abo/ace-window

;; Switch window has a lot more setup/options in github readme. For now just
;; see if we like it at all.
;; https://github.com/dimitri/switch-window
;; Trial: [2019-01-23]
(use-package switch-window
  :bind (("C-x o" . switch-window)))


;;------------------------------------------------------------------------------
;; Zoom in/out? / Repeatable Commands
;;------------------------------------------------------------------------------
;; todo: I would never use this, but it is cool...
;; from: https://oremacs.com/2015/01/14/repeatable-commands/
;;   from: http://pages.sachachua.com/.emacs.d/Sacha.html#orgfdc65f7


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-window)
