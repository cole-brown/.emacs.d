;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------hi----------------------------------------
;;--                                header?                                   --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings.
;;------------------------------------------------------------------------------

;; Hm... I actually think dying is bad... So non-existant required vars are bad.
;; Maybe def their consts, then if a thing doesn't exist, fine. Maybe throw
;; a message about it out during init. Like "can't find file in default: %s"?
;; (defun spydez/var/exists-p (var)
;;   ;(spydez/debug/message-if nil "name: %s" var) ;; name: spydez/dir/doc-save-common
;;   ;(spydez/debug/message-if nil "name: %s" (symbol-name var)) ;; name: spydez/dir/doc-save-common
;;   ;(spydez/debug/message-if nil "symbol?: %s" (symbolp var)) ;; symbol?: t
;;   ;(spydez/debug/message-if nil "value: %s" (symbol-value var)) ;; error: Symbol’s value as variable is void: spydez/dir/doc-save-common
;;   ;(spydez/debug/message-if nil "name: %s value: %s" (symbol-name var) (symbolp var) (bound-and-true-p var))
;;   ;(spydez/debug/message-if nil "bound-and-true?: %s" (bound-and-true-p var)) ; bound-and-true?: spydez/dir/doc-save-common
;;   ;(spydez/debug/message-if nil "bound?: %s" (boundp var)) ; nil
;;   (if (boundp var)
;;       t
;;     nil)
;;   )
;; (setq spydez/dir/doc-save-common "hi")
;; (setq spydez/dir/doc-save-common nil)
;; (makunbound 'spydez/dir/doc-save-common)
;; (spydez/var/exists-p 'spydez/dir/doc-save-common)


;;------------------------------------------------------------------------------
;; Blank
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: Anything in here?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-sanity-early)
