;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Line and column numbers in mode line
(column-number-mode t)
(line-number-mode t)

;; Size indicator in mode line with position
;; Trial: [2019-03-15 Fri]
(size-indication-mode t) ;; "x%" ->  "x% of 6.5k"


;;------------------------------------------------------------------------------
;; Mode line: Time / Clock
;;------------------------------------------------------------------------------
;; Puts a clock down in the mode line.

;; For ISO time:
;;   https://emacs.stackexchange.com/questions/7365/how-to-display-date-in-julian-in-the-mode-line

;; Formatting:
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
;; Simple version (format: yyyy-mm-dd HH:MM):
;; (setq display-time-format "%F %H:%M")
;; (display-time-mode t)

;; More complicated version:
;;   We're not full ISO 8601, but closeish. Set format to: yyyy-mm-dd HH:MM
(setq display-time-string-forms
      '((propertize (format-time-string "%F %H:%M" now)
;;                    ))) ;; no change
;;                    'face 'mode-line-buffer-id))) ;; bold yellow/gold like buffer name
                    'face 'bold))) ;; slightly bolded
;; Faces to use to get into theme's customization from:
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;; Propertize/format-time-string from:
;;   https://emacs.stackexchange.com/questions/13227/easy-way-to-give-the-time-its-own-face-in-modeline

;; and enable
(display-time-mode t)
;; eval this when testing changes: (display-time-update)

;; todo: color clock if late, or approaching late?
;;   would need a dynamic function instead of a format list/string.


;;------------------------------------------------------------------------------
;; Smart Mode Line
;;------------------------------------------------------------------------------
;; "Display a more compact mode line."
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#orga2e2814
;;
;; "Smart Mode Line is a sexy mode-line for Emacs. It aims to be easy to read
;; from small to large monitors by using colors, a prefix feature, and smart
;; truncation."
;;   - https://github.com/Malabarba/smart-mode-line
;;
;; Well I'll try it... but right now it's not all that different. Maybe
;; when more packages are installed...
(use-package smart-mode-line)
;; Trial: [2019-01-17]

;; Could configure some regexes into sml/replacer-regexp-list when up and running.
;; See git repo readme or Google.


;;------------------------------------------------------------------------------
;; Unique buffer names
;;------------------------------------------------------------------------------
(use-package uniquify
  :ensure nil

  ;; TODO: try these instead, see if they get sucked into custom-file
  ;; (I don't want them in there...).
  ;; :custom
  ;; (uniquify-buffer-name-style 'post-forward)
  ;; (uniquify-separator ":")            ; "file.txt:path/to"
  ;; (uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  ;; (uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

  :config
  ;; "file.txt/to/path"
  ;; (setq uniquify-buffer-name-style 'forward)
  ;;       uniquify-separator "/"

  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"            ; "file.txt:path/to"
        uniquify-after-kill-buffer-p t    ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
        ))


;;------------------------------------------------------------------------------
;; Moody - Appearance
;;------------------------------------------------------------------------------
;; https://github.com/tarsius/moody
;; "Tabs" (kinda) style layout of the mode line.
(use-package moody
  :demand t
  :config
  (setq x-underline-at-descent-line t) ;; No idea why.
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  ;; TODO: Would be awesome if I could put the time/date into a 'tab'.
  ;;   2x points if can also anchor to the right-hand side.
  )


;;------------------------------------------------------------------------------
;; Minions - Better hiding of minor modes?
;;------------------------------------------------------------------------------
;; https://github.com/tarsius/minions
(use-package minions
  :demand t

  ;; TODO: switch many many things over to :custom? Doesn't look like it hits
  ;; the custom-file at all.
  :custom
  ;; options:
  ;;   minions-blacklist: never show in menu
  ;;   minions-whitelist: always show in menu, even when not enabled
  ;;   minions-direct: let these exist on actual modeline
  ;;   minions-mode-line-lighter: Text used for minions menu in mode line
  ;;   minions-mode-line-delimiters: Strings placed around mode elements
  ;;     - (does this manually format entire mode string in modeline then?)
  (minions-mode-line-lighter ":" "Smile instead of wink.")

  :config
  (minions-mode))


;;------------------------------------------------------------------------------
;; Disabled: Major Mode Icons?
;;------------------------------------------------------------------------------
;; This config does a lot, maybe try building our own?
;;   http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html#org7288b2d

;; Wish I could turn off the 'line endings' icon... and the lock/unlock one...
;; and the 'black text on white square' mode icons...
;; Trial: [2019-03-15 Fri]
;; TODO: Not sure about this - try out something else.
;; (use-package mode-icons
;;   ;; Do I need to wait for all-the-icons? Not 100% sure it uses it. It does
;;   ;; use the fonts I installed to go with all-the-icons, though.
;;   :after all-the-icons
;;
;;   :init
;;   (setq
;;         ;; Mode Icons in the 'buffer names' list (like when changing buffers)...
;;         ;; doesn't seem to behave well with Helm's buffer list.
;;         mode-icons-change-mode-name nil
;;
;;         ;; You can also change the icon to match the active mode line (disabled by default):
;;         mode-icons-desaturate-active t
;;         )
;;
;;   :config
;;   (mode-icons-mode))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; Could go full crazy and just define `mode-line-format' myself...
;; e.g. https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-mode-line.el


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-modeline)
