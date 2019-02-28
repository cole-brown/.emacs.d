;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; Minibuffer
;;------------------------------------------------------------------------------

;;---
;; Properties
;;---

;; Added property to prevent point from entering prompt/read-only
;; text in minibuffer.
(setq minibuffer-prompt-properties '(read-only t
                                     cursor-intangible t
                                     face minibuffer-prompt))


;;---
;; Minibuffer editting
;;---

;; from Sacha: http://pages.sachachua.com/.emacs.d/Sacha.html#org3a92988
;;
;; Sometimes you want to be able to do fancy things with the text that you're
;; entering into the minibuffer. Sometimes you just want to be able to read it,
;; especially when it comes to lots of text. This binds C-M-e in a minibuffer)
;; so that you can edit the contents of the minibuffer before submitting it.
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))
;; Trial: [2019-01-17]

;; Alternative:
;;  minibuffer often displays so much information, even temporarily, that 
;;  it is nice to give it some room to breath.
;; (setq resize-mini-windows t)
;; (setq max-mini-window-height 0.33)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-Edit.html
;; TODO: try this resize out


;;---
;; Misc
;;---
;; Don't want to bother typing out a whole 'yes'...
(fset 'yes-or-no-p 'y-or-n-p)


;;------------------------------------------------------------------------------
;; Mode line
;;------------------------------------------------------------------------------

;;---
;; Smart Mode Line
;;---
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


;;---
;; Unique buffer names
;;---
(require 'uniquify)
;; "file.txt/to/path"
;; (setq uniquify-buffer-name-style 'forward)
;;       uniquify-separator "/"
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"            ; "file.txt:path/to"
      uniquify-after-kill-buffer-p t    ; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
      )


;;---
;; Time in the modeline
;;---
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
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-minibuffer)
