;; -*- emacs-lisp -*-


;;-----------------------------------emacs--------------------------------------
;;--                  for things that change emacs itself                     --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings?
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Garbage Collection
;;------------------------------------------------------------------------------
;; Give the minibuffer more gc room:
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defconst spydez/gc-cons-threshold/minibuffer (* 100 1000 1000)
  "Give minibuffer 100MB limit temporarily.")

;;---
;; Hooks
;;---
(defun spydez/hook/gc-minibuffer/enter ()
  (setq gc-cons-threshold spydez/gc-cons-threshold/minibuffer))

(defun spydez/hook/gc-minibuffer/exit ()
  (setq gc-cons-threshold spydez/gc-cons-threshold/normal))

;; TODO: profile this - does it add/save time, does it feel better/laggier?
(add-hook 'minibuffer-setup-hook #'spydez/hook/gc-minibuffer/enter)
(add-hook 'minibuffer-exit-hook #'spydez/hook/gc-minibuffer/exit)


;;------------------------------------------------------------------------------
;; Performance: (Very) Long Lines
;;------------------------------------------------------------------------------

;; See in spydez/dir/docs/notes
;;   - performance.long-lines.org
;;   - performance.long-lines.example.log

;; The issue: Something is causing terrible slowness, laggy responsiveness in
;; files with long lines (like the example).

;;---
;; Failures
;;---
;; `So Long' package failed to be useful.
;; Disabling as many minor modes as possibel also useless.


;;---
;; Solution: Too Long Lines Mode
;;---
;; Not on MELPA, but is on GitHub.
;; https://github.com/rakete/too-long-lines-mode

;; "To use this mode just require this file, configure too-long-lines-threshold
;; and too-long-lines-show-number-of-characters to your pleasing and call
;; too-long-lines-mode to enable the mode globally."
(use-package too-long-lines-mode
  ;; have it specify that it's in the manual package archive dir
  :load-path spydez/dir/emacs/manual-packages
  ;; May want a "version" on the file?
  ;; For now... I downloaded that on [2019-02-25 Mon].

  :demand t

  :config
  (too-long-lines-mode t))

;; Ok. This /does/ help with the example file. Excellent responsiveness.


;;---
;; Misc
;;---
;; Minorly helpful? But didn't profile or anything so could be entirely wrong.

;; bi-directional display not needed for my english-only stuff...
(setq bidi-display-reordering nil)


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; Also could make buffer read-only?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-emacs)
