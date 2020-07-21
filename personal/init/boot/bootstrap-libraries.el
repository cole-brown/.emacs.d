;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; My Personal Packages
;;------------------------------------------------------------------------------

;; My Key-Value Store package I want around so I can use it.
(use-package jerky
  ;; My own personal package - do not package manager it.
  :ensure nil


  ;;------------------------------
  :config
  ;;------------------------------
)


;;------------------------------------------------------------------------------
;; Packages That I Don't Use (Or Don't Use Much), But Other Packages Do.
;;------------------------------------------------------------------------------

;; library used for some list functions (by me and by some packages)
;; making it explicit now that I use it too
;; https://github.com/magnars/dash.el
(use-package dash) ;; util functions ("-flatten", etc)
(use-package f)    ;; file functions
(use-package s)    ;; string functions


;; Do use this some, but mostly use-package uses it.
(use-package bind-key)


;; use-package (can) use these.
(use-package diminish)
(use-package delight)
;; diminish vs delight... Having no sound knowledge on this subject, no real
;; interest in it, and after weakly trying to get Projectile to use delight but
;; failing and having to resort to asking Projectile nicely... I am fully
;; qualified to s-
;; I dunno... Delight maybe?
;;   The correct answer was... `minions'.


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-libraries)
