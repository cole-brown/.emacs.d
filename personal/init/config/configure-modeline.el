;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


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
;; Moody
;;------------------------------------------------------------------------------


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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-modeline)
