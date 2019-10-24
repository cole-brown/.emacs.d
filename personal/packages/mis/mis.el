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


;;------------------------------------------------------------------------------
;; Faces
;;------------------------------------------------------------------------------

;; M-x list-faces-display
(defcustom mis/type->faces
  '(;;---
    ;; General, Maybe Useful faces.
    ;;---
    (default (;; :padding    font-lock-comment-delimiter-face
              ;; :border     font-lock-comment-face
              :title       font-lock-keyword-face      ;; gold/bold
              :attention1  font-lock-preprocessor-face ;; brightish blue
              :attention2  font-lock-constant-face     ;; brightish green
              :text        font-lock-builtin-face      ;; white/bold
              :inattention font-lock-string-face))     ;; darkish red

    ;;---
    ;; My Custom Stuff
    ;;---
    ((spydez homeward) (:padding    font-lock-comment-delimiter-face
                        :border     font-lock-comment-face
                        :text       font-lock-builtin-face
                        :highlight  font-lock-keyword-face
                        :highlight2 font-lock-constant-face
                        :title      font-lock-preprocessor-face))
    ;;---
    ;; Koans
    ;;---
    ;; text lines
    ((mis koan text) (;; :whitespace nil
                      :padding  font-lock-comment-delimiter-face
                      :border   font-lock-comment-face
                      :text     font-lock-keyword-face))
    ;; non-text lines
    ((mis koan presence) (;; :whitespace nil
                          :padding  font-lock-comment-delimiter-face
                          :border   font-lock-comment-face
                          :text     font-lock-comment-face)))
  "alist of: (types-list faces-list)
See 'M-x list-faces-display' for all defined faces."
  :group 'mis
  :type '(alist :key-type list :value-type list))


;;------------------------------------------------------------------------------
;; Pull in our bits and pieces...
;;------------------------------------------------------------------------------

(require 'mis-parts)
(require 'mis-center)
(require 'mis-comment)
(require 'mis-message)
(require 'mis-debug)
(require 'mis-init)
(require 'mis-koan)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis)
