;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Line and column numbers in mode line
(column-number-mode t)
(line-number-mode t)

;; Size indicator in mode line with position
;; Tried Out: [2019-03-15 Fri]
;; Ended: [2019-07-01 Mon]
;;   Good info but I have it in the titlebar already and I want more space for
;;   other things in the modeline.
;; Positive to enable, negative to disable.
;; (size-indication-mode -1) ;; 1) ;; "x%" ->  "x% of 6.5k"

;; TODO: move to a use-package bootstrap or zeroth or something?
;; TODO-TOO: make one for every package?
;; TODO-3: make a helper where you can put the package name and it'll check
;;   if bound-and-true-p? e.g.: (spydez/using-package 'moody) -> t/nil
(defconst spydez/use-package/moody t
  "True if moody should be enabled in use-package.")


;;------------------------------------------------------------------------------
;; Mode line: Time / Clock
;;------------------------------------------------------------------------------
;; Puts a clock down in the mode line.

(defconst spydez/moody/enable-time t
  "True if moody should manage a clock in the modeline. False if it should not.")

(defun spydez/moody/managing-time ()
  "True if moody should manage a clock in the modeline. False if it should not."
  (and spydez/moody/enable-time   ;; explicitly enabled
       (featurep 'moody)          ;; moody is installed
       spydez/use-package/moody ;; moody is enabled in use-package
       ))

(defconst spydez/modeline/time-format
  "%F %R"
  "We're not full ISO 8601, but closeish. Format for time in modeline is: yyyy-mm-dd HH:MM")

;;---
;; Display Time Mode
;;----
(unless (spydez/moody/managing-time)
  ;;(message "display-time-mode is managing time!")

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
        '((propertize (format-time-string spydez/modeline/time-format now)
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

  ) ;; /unless spydez/moody/managing-time


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
;; Trial: [2019-01-17]
(use-package smart-mode-line)

;; Could configure some regexes into sml/replacer-regexp-list when up and running.
;; See git repo readme or Google.


;;------------------------------------------------------------------------------
;; Unique buffer names
;;------------------------------------------------------------------------------
(use-package uniquify
  :ensure nil

  ;;---
  :custom
  ;;---
  ;; "file.txt/to/path"
  ;; (uniquify-buffer-name-style 'forward
  ;;  "Set uniquify-separator '/'. e.g. file.txt/to/path")

  (uniquify-buffer-name-style 'post-forward
   "Set uniquify names to e.g. file.txt|to/path")

  (uniquify-separator ":"
   "Set uniquify buffer/path separator to e.g. file.txt:path/to")

  (uniquify-after-kill-buffer-p t
   "Rename after killing uniquified. E.g. de-uniquify others as possible.")

  (uniquify-ignore-buffers-re "^\\*"
   "Don't muck with special buffers."))


;;------------------------------------------------------------------------------
;; Moody - Appearance
;;------------------------------------------------------------------------------
;; https://github.com/tarsius/moody
;; "Tabs" (kinda) style layout of the mode line.
(use-package moody
  :if spydez/use-package/moody
  :demand t

  ;;---
  :config
  ;;---

  ;;---
  ;; General
  ;;----

  (setq x-underline-at-descent-line t) ;; No idea why.

  ;; tabify buffer name
  (moody-replace-mode-line-buffer-identification)
  ;; TODO: Use this??? moody-replace-sml/mode-line-buffer-identification
  ;;   - I am using smart-mode-line... I think.
  ;;   - do I need: ':after smart-mode-line' ??

  ;; tabify vc-mode info
  (moody-replace-vc-mode)

  ;;---
  ;; Custom: Time tab.
  ;;---
  (when (spydez/moody/managing-time)
    (spydez/message/init 'ignore "Moody is managing time!")
    ;; Have to get the time string replaced by moody ala `(moody-replace-vc-mode)' above.

    ;; It exists in the modeline like so:
    ;;   mode-line-format
    ;;     -> ("%e" mode-line-front-space mode-line-mule-info mode-line-client
    ;;         mode-line-modified mode-line-remote mode-line-frame-identification
    ;;         moody-mode-line-buffer-identification "   " mode-line-position
    ;;         (vc-mode moody-vc-mode) "  " minions-mode-line-modes
    ;;         mode-line-misc-info mode-line-end-spaces)
    ;;
    ;;   mode-line-misc-info
    ;;     -> global-mode-string
    ;;        -> ("" display-time-string)

    ;; Could go full crazy and just define `mode-line-format' myself...
    ;; (In order to pull time out into its own definitively separate thing.
    ;; e.g. https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-mode-line.el
    ;; Or I could just steal mode-line-misc-info. (fset ...) if needed.
    ;;
    ;; Just replacing mode-line-misc-info right now.
    (defun spydez/moody/replace-mode-line-misc-info (&optional reverse)
      (interactive "P")
      (moody-replace-element 'mode-line-misc-info
                             'spydez/moody/mode-line-misc-info
                             reverse))

    ;; ;; Our non-moody-tab time wants to be different - propertized.
    ;; ;; But moody is moody or I can't grok today. So no propertize in moody?
    ;; (setq display-time-string-forms
    ;;       '(format-time-string "%F %H:%M" now))

    (defun spydez/moody/time-string ()
      "Outputs string like: '2019-07-02 10:05'"
      (format-time-string spydez/modeline/time-format))

    ;; Errors... yay:
    ;; (defvar spydez/moody/mode-line-misc-info
    ;;   '(:eval (moody-ribbon 'display-time-string nil 'up)))
    ;; Error during redisplay: (eval (moody-ribbon (quote display-time-string) nil (quote up))) signaled (wrong-type-argument sequencep display-time-string)
    ;; Ok - the error there is that display-time-string should be evaluated as
    ;; a function, not passed as a symbol.

    ;; Need to parse out mode-line-misc-info, parse out global-mode-string?,
    ;; replace display-time-string with my shit, and then I'm done?? Nonono, no.
    ;; No. Just moody-tab the whole thing and do that shit later if required.

    (defvar spydez/moody/mode-line-misc-info/inside-parts
      '(spydez/moody/time-string)
      "Items to go inside `moody-tab' of `spydez/moody/mode-line-misc-info'.")

    (defun spydez/moody/mode-line-misc-info/inside-string ()
        "Turns `spydez/moody/mode-line-misc-info/inside-parts' into a string for
         the modeline moody tab."
        (let ((result nil))
          (string-trim
           (mapconcat
            'identity
            (nreverse
             (dolist (part spydez/moody/mode-line-misc-info/inside-parts result)
               (cond
                ((stringp part)
                 (push part result))
                ((functionp part)
                 (push (funcall part) result))
                (t
                 (push (format "%s" part) result)))))
            " "))))
    ;; (spydez/moody/mode-line-misc-info/inside-string)

    (defvar spydez/moody/mode-line-misc-info
      '(:eval (moody-tab (spydez/moody/mode-line-misc-info/inside-string)
                         nil 'up))
      "Moody tab to replace mode-line-misc-info.")

    (put 'spydez/moody/mode-line-misc-info 'risky-local-variable t)
    (make-variable-buffer-local 'spydez/moody/mode-line-misc-info)

    ;; make it so
    (spydez/moody/replace-mode-line-misc-info)

    ;; TODO: Anchor time to the right-hand side (with just the left slant from moody tab?).
    ))


;;------------------------------------------------------------------------------
;; Minions - Better hiding of minor modes?
;;------------------------------------------------------------------------------
;; https://github.com/tarsius/minions
(use-package minions
  :demand t

  ;; TODO: switch many many things over to :custom? Doesn't look like it hits
  ;; the custom-file at all.

  ;;---
  :custom
  ;;---
  ;; options:
  ;;   minions-blacklist: never show in menu
  ;;   minions-whitelist: always show in menu, even when not enabled
  ;;   minions-direct: let these exist on actual modeline
  ;;   minions-mode-line-lighter: Text used for minions menu in mode line
  ;;   minions-mode-line-delimiters: Strings placed around mode elements
  ;;     - (does this manually format entire mode string in modeline then?)
  (minions-mode-line-lighter ":" "Smile instead of wink.")

  ;;---
  :config
  ;;---
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
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; Could go full crazy and just define `mode-line-format' myself...
;; e.g. https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-mode-line.el


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-modeline)
