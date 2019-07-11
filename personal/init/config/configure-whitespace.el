;; -*- emacs-lisp -*-

;;------------------------------------------------------------------------------
;; Trailing whitespace.
;;------------------------------------------------------------------------------

;; TODO: Trial this: ws-butler
;; https://melpa.org/#/ws-butler
;; Only removes whitespace from regions you've changed.

;; Trial: [2019-07-11 Thu]
;; https://batsov.com/articles/2011/11/25/emacs-tip-number-3-whitespace-cleanup/
(add-hook 'before-save-hook 'whitespace-cleanup)

;; TODO: Not sure I want all of whitespace or what even that means...
;; ;; Has a lot more settings, but to start:
;; (use-package whitespace
;;   :ensure nil
;;
;;   :commands (whitespace-buffer
;;              whitespace-cleanup
;;              whitespace-mode)
;;
;;   :hook
;;   (before-save-hook . whitespace-cleanup))

;; different way to ban whitespace at end of lines?
;; ;; Delete trailing whitespace before save?
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Caveat: Same downside as below. Ducks up the blame.

;; Ban whitespace at end of lines, globally.
;; (add-hook 'write-file-hooks
;;           '(lambda ()
;;              (gcr/delete-trailing-whitespace)))
;; Caveat: Banning has downside of fucking up code blame when you didn't really change anything.

;; Or just show it?
;; (setq show-trailing-whitespace t)
;; TODO: Holding off on this in case a calmer whitespace display can be found.


;;------------------------------------------------------------------------------
;; Whitespace in General.
;;------------------------------------------------------------------------------

;; haven't tried:
;;   https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;;   https://github.com/jwiegley/use-package/issues/122#issuecomment-54634367
;; (use-package whitespace
;;   :commands whitespace-mode
;;   :config
;;   (modeline-remove-lighter 'whitespace-mode)
;;
;;   ;; Hooks
;;   (add-hook 'prog-mode-hook #'whitespace-mode)
;;
;;   ;; Variables
;;   (setq whitespace-line-column nil)
;;   (setq whitespace-style '(face lines-tail)))
;; and from above:
;; ;; https://batsov.com/articles/2011/11/25/emacs-tip-number-3-whitespace-cleanup/
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;; and also
;;


;; This is pretty close:
;; https://www.emacswiki.org/emacs/WhiteSpace
;; (delete 'lines whitespace-style)
;; whitespace-space face may could use minor adjusting (regular bg color?)
;; probably don't want on global. Just for programming modes maybe.
;;
;; TODO: Possibly try this for whitspace fill-column indicator?
;;   https://www.reddit.com/r/emacs/comments/also27/second_trial_for_a_weekly_tipstricksetc_thread/efzl7ft
;;   (that comment and also child comment with code snippet to try)

;; ;; Whitespace
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
;; ;; (require 'whitespace)
;; ;; (setq whitespace-style '(trailing lines tab-mark))
;; ;; (setq whitespace-line-column 80)
;; ;; (global-whitespace-mode 1)
;; ;; (eval-after-load "delight"
;; ;;   '(progn
;; ;;      (eval-after-load "whitespace"
;; ;;        '(delight 'global-whitespace-mode "ᗣ"))
;; ;;      (eval-after-load "whitespace"
;; ;;        '(delight 'whitespace-mode ""))))
;; ;; meh. No work?
;;
;; ;; TODO: have yet to find a good config. This is closest. I think everyone else maybe wants
;; ;;   just hilight colors and I want symbols or something. Most thing I've tried do nothing.
;; ;; TODO: Something like this? Maybe?
;; ;; This gets closer, but is a bit ugly. Need to tweak zenburn theme?
;; ;; http://ergoemacs.org/emacs/whitespace-mode.html
;; ;; (progn
;; ;;  ;; Make whitespace-mode with very basic background coloring for whitespaces.
;; ;;   ;; http://ergoemacs.org/emacs/whitespace-mode.html
;; ;;   (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
;; ;;
;; ;;   ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
;; ;;   (setq whitespace-display-mappings
;; ;;         ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
;; ;;         '(
;; ;;           (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;; ;;           (newline-mark 10 [182 10]) ; LINE FEED,
;; ;;           (tab-mark 9 [9655 9] [92 9]) ; tab
;; ;;           )))
;; ;;
;; ;; https://www.emacswiki.org/emacs/WhiteSpace


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-whitespace)
