;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; Tab Settings
;;------------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/NoTabs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Just-Spaces.html
;; https://www.emacswiki.org/emacs/TabsAreEvil
;; https://www.emacswiki.org/emacs/SmartTabs

;; always replace tabs with spaces
(setq-default indent-tabs-mode nil)
;; set default tab width for all buffers
(setq-default tab-width spydez/dev-env/tab/normal)

;; TODO: do I need a tab-stop-list? What's it used in? Not prog-modes...
;; https://www.emacswiki.org/emacs/TabStopList
;; (setq tab-stop-list (spydez/range 0 120 spydez/tab-width))
;; TODO: range function is in lisp/misc-functions... load them earlier?

;; NOTE: M-x tabify and M-x untabify exist and work on regions.


;;------------------------------------------------------------------------------
;; Trailing Whitespace.
;;------------------------------------------------------------------------------

;; TODO: Trial this: ws-butler (turn off whitespace-cleanup in
;;   (use-package whitespace...))
;; https://melpa.org/#/ws-butler
;; Only removes whitespace from regions you've changed.


;;------------------------------------------------------------------------------
;; Whitespace in General.
;;------------------------------------------------------------------------------

;; Trial: [2019-07-16 Tue]
;; ...Finally. Took long enough to figure out making things look ok.
;; (set-face-attribute ...) kids. Helps a lot when your theme has
;; defaults that you can just steal and tweak.
(use-package whitespace
  :ensure nil
  :demand t

;;   ;;----------
;;   :hook
;;   ;;----------
;;   ;; This... Doesn't work here? Doesn't get called.
;;   ;; Trial: [2019-07-11 Thu]
;;   ;; https://batsov.com/articles/2011/11/25/emacs-tip-number-3-whitespace-cleanup/
;;   (before-save-hook . whitespace-cleanup)

  ;;----------
  :custom
  ;;----------
  (whitespace-style
   (quote
    ;; visualization via faces (see set-face-attribute below)
    (face
     ;; general/normal whitespace
     tabs spaces newline

     ;; the bad kind
     trailing space-before-tab space-after-tab
     empty       ;; ...lines (...at beginning/end of buffer)
     lines-tail  ;; `lines' would be whole line...
     ;; lines-tail is just whatever's past fill-column

     ;; not sure if want or bad or what.
     indentation
     ;; visualize these whitespaces with non-whitespace chars via display-table
     space-mark tab-mark newline-mark)))

  ;; (whitespace-style ;; minus `face'
  ;; '(tabs spaces trailing lines space-before-tab newline
  ;;                     indentation empty space-after-tab space-mark
  ;;                     tab-mark newline-mark))

  ;;----------
  :config
  ;;----------

  ;; Why does this not work in :hook section? -_-
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; TODO: A way to tell use-package to load after zenburn if zenburn is going to load?
  (require 'with)
  (with-feature 'zenburn-theme
    ;; Change color/faces of whitespace attributes.
    ;; Defaults are ugly/distracting/meh. Make 'em more backgroundy.
    (zenburn-with-color-variables
      ;; bg+3 looks good, I think, for "noticible but not in your face"
      ;; bg+2 might be better after I get used to things...
      (set-face-attribute 'whitespace-space nil
                          :foreground zenburn-bg+2
                          :background zenburn-bg)

      (set-face-attribute 'whitespace-hspace nil
                          :foreground zenburn-bg+2
                          :background zenburn-bg)

      (set-face-attribute 'whitespace-tab nil
                          :foreground zenburn-bg+2
                          :background zenburn-red-1)

      ;; Don't think any of these are customized right now.
      ;; (set-face-attribute 'whitespace-newline nil
      ;;                     :foreground zenburn-bg+1)
      ;;
      ;; (set-face-attribute 'whitespace-trailing nil
      ;;                     :foreground zenburn-bg+1
      ;;                     :background zenburn-red)
      ;;
      ;; (set-face-attribute 'whitespace-line nil
      ;;                     :foreground zenburn-magenta
      ;;                     :background zenburn-bg)
      ;;
      ;; (set-face-attribute 'whitespace-space-before-tab nil
      ;;                     :foreground zenburn-orange
      ;;                     :background zenburn-orange)
      ;;
      ;; (set-face-attribute 'whitespace-indentation nil
      ;;                     :foreground zenburn-red
      ;;                     :background zenburn-yellow)
      ;;
      ;; (set-face-attribute 'whitespace-empty nil
      ;;                     :background zenburn-yellow)
      ;;
      ;; (set-face-attribute 'whitespace-space-after-tab nil
      ;;                     :foreground zenburn-red
      ;;                     :background zenburn-yellow)
      ))

  (global-whitespace-mode 1) ;; positive: enable, other: disable
  )


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-whitespace)
