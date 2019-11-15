;; -*- mode: emacs-lisp; lexical-binding: t -*-


;; §-TODO-§ [2019-10-23]: Make sure warnings work?

;; §-TODO-§ [2019-10-23]: 'proper' header for 'proper' emacs pkg...
;;
;;   Functions for manipulating strings, propertizing them, sending
;; them to the *Messages* buffer as such...

;;-----------------------------strings and outputs------------------------------
;;--                               Make It So!                                --
;;------------------------------------------------------------------------------


(defgroup mis nil
  "Functions for manipulating strings, propertizing them, sending
them to the *Messages* buffer as such..."
  :group 'editing)


;; And now some settings before we include all our files...

;;------------------------------------------------------------------------------
;; Faces
;;------------------------------------------------------------------------------

;; M-x list-faces-display
(defcustom mis/type->faces
  '(;;---
    ;; General, Maybe Useful faces.
    ;;---
    ;; §-TODO-§ [2019-11-15]: Remove refs to 'default - use :default instead
    (default (;; :border    font-lock-comment-delimiter-face
              ;; :padding     font-lock-comment-face
              :title       font-lock-keyword-face      ;; gold/bold
              :attention1  font-lock-preprocessor-face ;; brightish blue
              :attention2  font-lock-constant-face     ;; brightish green
              :text        font-lock-builtin-face      ;; white/bold
              :inattention font-lock-string-face))     ;; darkish red
    (:default (;; :border    font-lock-comment-delimiter-face
              ;; :padding     font-lock-comment-face
              :title       font-lock-keyword-face      ;; gold/bold
              :attention1  font-lock-preprocessor-face ;; brightish blue
              :attention2  font-lock-constant-face     ;; brightish green
              :text        font-lock-builtin-face      ;; white/bold
              :inattention font-lock-string-face))     ;; darkish red

    ;;---
    ;; My Custom Stuff
    ;;---
    ((spydez homeward) (:border    font-lock-comment-delimiter-face
                        :padding     font-lock-comment-face
                        :text       font-lock-builtin-face
                        :highlight  font-lock-keyword-face
                        :highlight2 font-lock-constant-face
                        :title      font-lock-preprocessor-face))
    ;;---
    ;; Koans
    ;;---
    ;; text lines
    ((mis koan text) (;; :indent nil
                      :border  font-lock-comment-delimiter-face
                      :padding   font-lock-comment-face
                      :text     font-lock-keyword-face))
    ;; non-text lines
    ((mis koan presence) (;; :indent nil
                          :border  font-lock-comment-delimiter-face
                          :padding   font-lock-comment-face
                          :text     font-lock-comment-face)))
  "alist of: (types-list faces-list)
See 'M-x list-faces-display' for all defined faces."
  :group 'mis
  :type '(alist :key-type list :value-type list))


;;------------------------------------------------------------------------------
;; Helper for optional arg/custom settings.
;;------------------------------------------------------------------------------

(defun mis/setting/get-with-default (arg default)
  "Figures out actual ARG by looking at ARG and DEFAULT. It will
be ARG if ARG is non-nil and either symbolp or functionp. Else it
will look at the value of DEFAULT and use either that symbol, or
call that function to get a symbol."
  (cond
   ;; pass through - function
   ((and (not (null arg))
         (functionp arg))
    (funcall arg))
   ;; pass through - symbol
   ((and (not (null arg))
         (symbolp arg))
    ;; if it has a value, use that, else use directly
    (if (symbol-value arg)
          (symbol-value arg)
      arg))

   ;; default - function to get current
   ((and (not (null default))
         (functionp default))
    (funcall default))
   ;; default - symbol as default
   ((symbolp default)
    ;; if it has a value, use that, else use directly
    (if (and (not (null default))
             (symbol-value default))
          (symbol-value default)
      default))

   ;; fallback to something drastic-ish
   (t
    :error)))
;; (mis/setting/get-with-default nil mis/debug/type)


;;------------------------------------------------------------------------------
;; Pull in our bits and pieces...
;;------------------------------------------------------------------------------

;;---
;; Doesn't require any of the others right now...
;;---
(require 'mis-debug)

;;---
;; Trying to be non-circularly referential with these...
;;---

;; Refs mis-center functions, vars in `mis/parts/symbols-alist'.
;; Inside quoted list, so ok.
(require 'mis-parts)

;; Needs mis-parts
(require 'mis-center)

;; Needs mis-parts, mis-center
(require 'mis-comment)

;; Needs mis-parts
(require 'mis-message)

;; Needs mis-message
(require 'mis-init)

;; Needs mis-parts, mis-center, mis-message
(require 'mis-koan)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis)
