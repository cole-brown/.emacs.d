;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: does some or all of this belong in configure-emacs? vice versa?

;; TODO: skip all or most of this:
;;    (unless window-system (mis/debug/when nil "hi"))
;; Or maybe do all/most of this:
;;    (when window-system (mis/debug/when nil "hi"))


;;------------------------------------------------------------------------------
;; Window/GUI Setup
;;------------------------------------------------------------------------------

;; Don't show the GNU splash.
(setq inhibit-startup-screen t)

;; smaller inital message in *scratch* buffer
(setq initial-scratch-message ";; Hello there.\n\n")

;; This doesn't seem to be an issue on Windows right now, but I think it was in
;; the past and this would have been nice to know for fixing how it would refuse
;; to be sane on a maximize.
;;(setq frame-resize-pixelwise t)

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
;;   Consolas is good for Windows. Or was, anyways.
;;   Inconsolata was FOSS equivalant?


;;------------------------------------------------------------------------------
;; Color scheme: Zenburn
;;------------------------------------------------------------------------------
(use-package zenburn-theme
  :init
  ;; zenburn uses "-N" for lighter and "+N" for darker in their names in the
  ;; zenburn-default-colors-alist variable.
  ;; These are some additional colors I'm testing out.
  ;;
  ;; Went to this website and plugged in zenburn-magenta and zenburn-bg
  ;; with 10 midpoints:
  ;;   https://meyerweb.com/eric/tools/color-blend/#3F3F3F:DC8CC3:10:hex
  (setq spydez/theme/color/zenburn-magenta-bg   "#4D464B"
        spydez/theme/color/zenburn-magenta-bg-1 "#5C4D57"
        spydez/theme/color/zenburn-magenta-bg-2 "#6A5463"
        spydez/theme/color/zenburn-magenta-bg-3 "#785B6F"
        spydez/theme/color/zenburn-magenta-bg-4 "#86627B"
        spydez/theme/color/zenburn-magenta-bg-5 "#956987"))
;; Seems to work fine without 'load-theme

;; Old links, but wanted to carry them over from old .emcas:
;;   Zenburn theme from: http://www.brockman.se/software/zenburn/zenburn.el
;;     - dinosaur link
;;   Zenburn for various other programs: http://www.brockman.se/software/zenburn/
;;     - dead link
;;   Zenburn for VS 05: http://www.codinghorror.com/blog/archives/000682.html
;;     - dead link


;;-----------------------------------------------------------------------------
;; Pretty Init Messages in *Messages* Buffer?
;;-----------------------------------------------------------------------------
;; (require 'with)
;; (with-feature 'zenburn
;;   (zenburn-with-color-variables
;;     (defface spydez/face/types-sep
;;       `(;; (display . plist)

;;         ;; display t matches everything...
;;         (t
;;          ;; face plist
;;          (:foreground ,zenburn-fg-1)))
;;       "my test face for finding out how to face"
;;       :group 'spydez/group
;;       )
;;     (defface spydez/face/types
;;       `(;; (display . plist)

;;         ;; display t matches everything...
;;         (t
;;          ;; face plist
;;          (:foreground ,zenburn-orange)))
;;       "my test face for finding out how to face"
;;       :group 'spydez/group
;;       )
;;     (defface spydez/face/gutter
;;       `(;; (display . plist)

;;         ;; display t matches everything...
;;         (t
;;          ;; face plist
;;          (:foreground ,zenburn-yellow)))
;;       "my test face for finding out how to face"
;;       :group 'spydez/group
;;       )
;;     (defface spydez/face/text
;;       `(;; (display . plist)

;;         ;; display t matches everything...
;;         (t
;;          ;; face plist
;;          (:foreground ,zenburn-default)))
;;       "my test face for finding out how to face"
;;       :group 'spydez/group
;;       )))

;; see `mis/message/preserve-properties'


;;------------------------------------------------------------------------------
;; Cursor: Beacon
;;------------------------------------------------------------------------------
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-beacon.el
;; https://github.com/Malabarba/beacon
(use-package beacon
  :delight

  ;;---
  :custom
  ;;---
  ;; [2019-09-24]: Most are here (but unchanged) so I know about them for
  ;; maybe tweaking...
  (beacon-blink-when-point-moves-vertically nil) ; default nil
  (beacon-blink-when-point-moves-horizontally nil) ; default nil
  (beacon-blink-when-buffer-changes t) ; default t
  (beacon-blink-when-window-scrolls t) ; default t
  (beacon-blink-when-window-changes t) ; default t
  (beacon-blink-when-focused t) ; default nil

  (beacon-blink-duration 0.3) ; default 0.3
  (beacon-blink-delay 0.3) ; default 0.3
  (beacon-size 20) ; default 40
  (beacon-color 0.25 "A bit 'brighter' on zenburn dark bg.") ; default 0.5

  ;; `beacon-color' can take names, hex strings ("#AABBCC"),
  ;; and floats (converted to 'between bg and fg colors')
  ;; (beacon-color "yellow") ; default 0.5
  ;; (beacon-color 0.5) ; default 0.5
  ;; (beacon-color spydez/theme/color/zenburn-magenta-bg-5) ; default 0.5

  ;; Experiments, not sure I like...
  ;; (setq beacon-size 10)
  ;; (setq beacon-color spydez/theme/color/zenburn-magenta-bg-5)
  ;; (zenburn-with-color-variables (setq beacon-color zenburn-red))
  ;; (zenburn-with-color-variables (setq beacon-color zenburn-magenta))
  ;; Reset:
  ;; (setq beacon-size 20)
  ;; (setq beacon-color 0.25)

  ;;---
  :config
  ;;---
  ;; Not sure exactly why term-mode is no blink...
  ;; Do we want to also add shell, eshell?
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode)
  ;; (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
  ;; (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode)

  (beacon-mode 1))


;;------------------------------------------------------------------------------
;; Cursor/Line: Highlighting?
;;------------------------------------------------------------------------------
;; I really am liking Beacon, so these must not kill that and also must
;; play nice.

;;   "Using the built-in global-hl-mode to always highlight the current line,
;; together with the col-highlight package, which highlights the column only
;; after a defined interval has passed.
;;
;;   "Using the crosshairs package, which combines both but always highlights both
;; the column and the line. It also has a “highlight crosshairs when idle” mode,
;; but I prefer to have the current line always highlighted, I’m only undecided
;; about the always-on column highlighting.
;;
;;   "Sometimes I find the always-highlighted column to be distracting, but other
;; times I find it useful. So I have both pieces of code here, I’m still
;; deciding. For now only hl-line is enabled."
;;   - https://zzamboni.org/post/my-emacs-configuration-with-commentary/#appearance-buffer-file-management-and-theming
;;
;; TODO: Do I want any of these even?
;; Trial: [2019-03-14]
(use-package hl-line
  :defer nil
  :ensure nil
  :config
  (global-hl-line-mode))
;; (use-package col-highlight
;;   :disabled
;;   :defer nil
;;   :config
;;   (col-highlight-toggle-when-idle)
;;   (col-highlight-set-interval 2))
;; (use-package crosshairs
;;   :disabled
;;   :defer nil
;;   :config
;;   (crosshairs-mode))


;;------------------------------------------------------------------------------
;; Mode line: Time / Clock
;;------------------------------------------------------------------------------
;; Moved to configure-modeline.

;;------------------------------------------------------------------------------
;; Jump between Emacs windows more efficientcly.
;;------------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/WindMove
;; http://pragmaticemacs.com/emacs/whizz-between-windows-with-windmove/
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/windmove.el
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
  ;; Well... We're still using this in a hydra, I guess.

  ;;---
  :custom
  ;;---
  (windmove-wrap-around t)

  ;; ;;---
  ;; :config
  ;; ;;---
  ;; ;; No to this.
  ;; ;; Super + arrows on Windows is a command I use for organizing Windows' windows.
  ;; ;; Trying Sacha's F2+arrows for now.
  ;; ;; ;; use command key on Mac
  ;; ;; (windmove-default-keybindings 'super)
  )

;; TODO: Not quite sure what to do with this.
;;   Maybe try org-mode's solution:
;;     - https://emacs.stackexchange.com/questions/22286/shiftarrow-to-change-window-does-not-work-in-org-mode
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
  :bind
  (("C-x o" . switch-window)))


;;------------------------------------------------------------------------------
;; Zoom in/out? / Repeatable Commands
;;------------------------------------------------------------------------------
;; todo: I would never use this, but it is cool...
;; from: https://oremacs.com/2015/01/14/repeatable-commands/
;;   from: http://pages.sachachua.com/.emacs.d/Sacha.html#orgfdc65f7


;;------------------------------------------------------------------------------
;; Lines, Buffers, Frames, Splits...
;;------------------------------------------------------------------------------

;; Some neat functions that transpose these things or swap frames or such.
;;   http://nhoffman.github.io/.emacs.d/#org1f32ab7
;;   http://nhoffman.github.io/.emacs.d/#orgaa6fa6d
;; Seem to all be from elsewhere so copying shouldn't be a problem.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-window)
