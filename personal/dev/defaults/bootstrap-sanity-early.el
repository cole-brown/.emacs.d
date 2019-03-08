;; -*- emacs-lisp -*-


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
;;   ;(spydez/debug/message-if nil "name: %s" var) ;; name: spydez/dir/common-doc-save
;;   ;(spydez/debug/message-if nil "name: %s" (symbol-name var)) ;; name: spydez/dir/common-doc-save
;;   ;(spydez/debug/message-if nil "symbol?: %s" (symbolp var)) ;; symbol?: t
;;   ;(spydez/debug/message-if nil "value: %s" (symbol-value var)) ;; error: Symbol’s value as variable is void: spydez/dir/common-doc-save
;;   ;(spydez/debug/message-if nil "name: %s value: %s" (symbol-name var) (symbolp var) (bound-and-true-p var))
;;   ;(spydez/debug/message-if nil "bound-and-true?: %s" (bound-and-true-p var)) ; bound-and-true?: spydez/dir/common-doc-save
;;   ;(spydez/debug/message-if nil "bound?: %s" (boundp var)) ; nil
;;   (if (boundp var)
;;       t
;;     nil)
;;   )
;; (setq spydez/dir/common-doc-save "hi")
;; (setq spydez/dir/common-doc-save nil)
;; (makunbound 'spydez/dir/common-doc-save)
;; (spydez/var/exists-p 'spydez/dir/common-doc-save)


;;------------------------------------------------------------------------------
;; Blank
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: Anything in here? 


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-sanity-early)
