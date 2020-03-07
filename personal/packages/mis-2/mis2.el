;; -*- mode: emacs-lisp; lexical-binding: t -*-


;; §-TODO-§ [2019-10-23]: Make sure warnings work?

;; §-TODO-§ [2019-10-23]: 'proper' header for 'proper' emacs pkg...
;;
;;   Functions for manipulating strings, propertizing them, sending
;; them to the *Messages* buffer as such...

;;-----------------------------strings and outputs------------------------------
;;--                               Make It So!                                --
;;------------------------------------------------------------------------------


(defgroup mis2 nil
  "Functions for manipulating strings, propertizing them, sending
them to the *Messages* buffer as such..."
  :group 'editing)


;; And now some settings before we include all our files...


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defcustom mis2/mode/interactive nil
  "If nil, mis2 echo area messages will have 0 second delay so that they don't
slow down other output (using `mis2/message/echo-area-timeout/non-interactive').

If non-nil, mis2 echo area messages will stick around for a while
(using `mis2/message/echo-area-timeout/non-interactive').")


;; §-TODO-§ [2020-02-05]: Do this for passing in/around more settings?
;; Settings keyword plist:
;;    KEYS         TYPE-or-VALUES
;;
;;   :interactive    t, nil
;;      Shorthand for:
;;        :echo to t.
;;        :echo-delay to `mis2/message/echo-area-timeout/interactive'
;;
;;   :startup        t, nil
;;      Shorthand for:
;;        :echo to t.
;;        :echo-delay to `mis2/message/echo-area-timeout/non-interactive'
;;
;;   :echo           t, nil
;;   :echo-delay     nil, numberp (see `minibuffer-message-timeout')
;;
;; Yes or no?
;;   :type           mis2/type->faces alist key (keyword symbol or list)
;;   :face           symbol for desired face
(defconst mis2/settings/keys
  '(:interactive :startup :echo :echo-delay)
  "Valid keys for mis2/settings plists.")


(defun mis2/settings/put (key value list)
  "Puts VALUE into LIST under KEY, after verifying KEY is a valid mis2 setting.
"
  (if (memq key mis2/settings/keys)
      (plist-put list key value)
    (error "Key %S not a valid mis2/settings key: %S"
           key
           mis2/settings/keys)))


(defun mis2/settings/get (key user-settings &optional mis2-setting)
  "Get a mis2 setting based off KEY. Setting either comes from USER-SETTINGS
plist or from the appropriate mis2 setting const/var (passed in
as MIS2-SETTING).
"
  ;; Simply return value from user-settings or mis2-setting, preferring
  ;; user-settings. Only complication is if user-settings specifies a nil, so we
  ;; have to check that key is a member of user-settings...
  (if (plist-member user-settings key)
      (plist-get user-settings key)
    mis2-setting))


;;------------------------------------------------------------------------------
;; Faces
;;------------------------------------------------------------------------------

;; M-x list-faces-display
(defcustom mis2/type->faces
  '(;;---
    ;; General, Maybe Useful faces.
    ;;---
    (:default (;; Text Things
              :title       font-lock-preprocessor-face ;; brightish blue
              :highlight   font-lock-keyword-face      ;; gold/bold
              :highlight2  font-lock-constant-face     ;; brightish green
              :text        font-lock-builtin-face      ;; white/bold
              :inattention font-lock-string-face       ;; darkish red
              :attention1  font-lock-preprocessor-face ;; brightish blue
              :attention2  font-lock-constant-face     ;; brightish green

              ;; Non-texty Things:
              :border      font-lock-comment-delimiter-face
              :padding     font-lock-comment-face
              ))

    ;;---
    ;; My Custom Stuff
    ;;---
    ;; §-TODO-§ [2019-11-15]: change do (:spydez :homeward)?
    ((spydez homeward) (:border      font-lock-comment-delimiter-face
                        :padding     font-lock-comment-face
                        :text        font-lock-builtin-face
                        :highlight   font-lock-keyword-face
                        :highlight2  font-lock-constant-face
                        :title       font-lock-preprocessor-face
                        :inattention font-lock-string-face))

    ;;---
    ;; Koans
    ;;---
    ;; §-TODO-§ [2019-11-15]: change do (:mis2 :koan :text)?
    ;; text lines
    ((mis2 koan text) (;; :indent nil
                      :border  font-lock-comment-delimiter-face
                      :padding   font-lock-comment-face
                      :text     font-lock-keyword-face))
    ;; §-TODO-§ [2019-11-15]: change do (:mis2 :koan :presence)?
    ;; non-text lines
    ((mis2 koan presence) (;; :indent nil
                          :border  font-lock-comment-delimiter-face
                          :padding   font-lock-comment-face
                          :text     font-lock-comment-face)))
  "alist of: (types-list faces-list)
See 'M-x list-faces-display' for all defined faces."
  :group 'mis2
  :type '(alist :key-type list :value-type list))


;;------------------------------------------------------------------------------
;; Helper for optional arg/custom settings.
;;------------------------------------------------------------------------------

(defun mis2/setting/get-with-default (arg default)
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
;; (mis2/setting/get-with-default nil mis2/debug/type)


;;------------------------------------------------------------------------------
;; Pull in our bits and pieces...
;;------------------------------------------------------------------------------

;;---
;; Doesn't require any of the others right now...
;;---
(require 'mis2-debug)

;;---
;; Trying to be non-circularly referential with these...
;;---

;; Refs mis2-center functions, vars in `mis2/parts/symbols-alist'.
;; Inside quoted list, so ok.
(require 'mis2-parts)

;; Needs mis2-parts
(require 'mis2-center)

;; Needs mis2-parts, mis2-center
(require 'mis2-comment)

;; Needs mis2-parts
(require 'mis2-message)

;; Needs mis2-message
(require 'mis2-init)

;; Needs mis2-parts, mis2-center, mis2-message
(require 'mis2-koan)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2)
