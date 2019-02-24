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
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-emacs)
