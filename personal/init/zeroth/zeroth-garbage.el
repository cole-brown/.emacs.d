;; -*- emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Garbage Collection
;;------------------------------------------------------------------------------
;; Adjust garbage collection thresholds.

;; Testing startup aka (emacs-init-time)
;;   with defaults:            6.5 sec (~50 GCs)
;;   with 0.5 and 100 MB:   -> 5.8 sec (4 GCs)
;;   with 0.5 and 1000 MB:  -> 5.8 sec (4 GCs)
;;   with 0.75 and 1000 MB: -> 5.7 sec (4 GCs)
;;   with biggest thresh:   -> 5.8 sec (4 GCs)
;; (setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-percentage 0.75)
;; (setq gc-cons-threshold (* 1000 1000 1000))
;; (setq gc-cons-percentage 0.5)
;; (setq gc-cons-threshold (* 100 1000 1000)) ;; up to 100 MB from default (800k)

(defconst spydez/gc-cons-threshold/normal (* 20 1000 1000)
  "Upped to 20MB for GC threshold.")

;; In init, have big gc so it doesn't get hit much.
;; In normal running, have smaller gc so you don't get affected by lag too much
;; when it does go off on you.
;; Could consider messing with gc-cons-percentage as well.
;; (default thresh is 800k, percent is 0.1)
;; https://github.com/purcell/emacs.d/commit/6d9439b74153f91f614bba48b734a43e8cacf57e

(let ((spydez/gc-cons-threshold/init (* 256 1000 1000))
      ;; TODO: mess with gc-cons-percentage?
      )
  (setq gc-cons-threshold spydez/gc-cons-threshold/init)
  (add-hook 'after-init-hook
            (lambda ()  (setq gc-cons-threshold spydez/gc-cons-threshold/normal))))

;; Note that here:
;;   https://github.com/syl20bnr/spacemacs/issues/3011#issuecomment-140536210
;; an argument against messing with this is presented, but saving most of a
;; second and dozens of gc cycles is worthwile.

;; See also configure-emacs.el.


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-garbage)
