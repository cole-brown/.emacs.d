;; -*- emacs-lisp -*-

;; configure-window.el - menu bar, tool bar, color theme, time...

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
;; Time in the modeline
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

;;------------------------------------------------------------------------------
;; Mode line
;;------------------------------------------------------------------------------



(provide 'configure-window)
