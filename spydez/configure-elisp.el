;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; TODO: these might belong in their own file...

;;---
;; Jumping to code
;;---
;; TODO: put these in elisp hook?
;; (define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
;; (bind-key "C-c f" 'find-function) ;; TODO: is this useful in other modes?


;;------------------------------------------------------------------------------
;; Elisp
;;------------------------------------------------------------------------------

;; Don't think I have settings in hook in old .emacs...
;; Might not have any settings at all besides tab size...
(defun spydez/hook/emacs-lisp-mode ()
  ;; TODO: settings for elisp?
  )


;;------------------------------------------------------------------------------
;; ElDoc
;;------------------------------------------------------------------------------
;; TODO: Move diminish here?


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: (use-package paredit) looks neat


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-elisp)
