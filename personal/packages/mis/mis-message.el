;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;----------------------------------...---...-----------------------------------
;;--                   We're all fine here... How are you?                    --
;;------------------------------------------------------------------------------


(require 'subr-x)

(require 'mis-parts)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defcustom mis/message/echo-area-timeout 0
  "See docs for `minibuffer-message-timeout'. This will lexically bind
`minibuffer-message-timeout' to this value. If not numberp, it seems the first
message will not clear until a non-`minibuffer-message' hits the *Messages*
buffer, at which point it and all subsequent `minibuffer-message' messages will
appear in *Messages* before the new message.

If numberp, this is the number of seconds to display the message
in the echo area. 0 is a good value for 'normal' `message'
minibuffer-echo-area functionality.

This can have performance impacts on startup."
  :group 'mis
  :type 'boolean)

;;------------------------------------------------------------------------------
;; Main Entry Point?
;;------------------------------------------------------------------------------

(defun mis/message/propertize (echo type &rest args)
  "Given TYPE, figure out a faces alist from `mis/type->faces' to use, then
build ARGS into propertized string via `mis/parts/build' and output it
to the *Messages* buffer.

If ECHO is non-nil, also echo message to the minibuffer echo area.

NOTE: Could (optionally) add TYPE to output easily enough if desired.
"
  (if-let ((faces (nth 1 (assoc type mis/type->faces)))
           ;; If args was a single list and got boxed by '&rest', unbox.
           ;; e.g. '(:text "hi") -&rest-> '((:text "hi")) -unbox-> '(:text "hi")
           (args (or (and (listp args)
                          (= (length args) 1)
                          (-flatten-n 1 args))
                     args))
           ;; Also need to check for a full unboxing...
           ;; e.g. 'newline -&rest-> '(newline) -unbox-> 'newline
           (args (or (and (listp args)
                          (= (length args) 1)
                          (nth 0 args))
                     args))
           ;; null check
           (valid-args (not (null args))))

      ;; Build propertized string and output it.
      (mis/message/preserve-properties echo (mis/parts/build args faces))

    ;; Else didn't find in type in type->faces or null args.
    ;; Complain, return nil.
    (mis/warning
     type :warning
     (concat "Null args (%S)? Or type not found in "
             "`mis/type->faces': %S -> %S")
     args type mis/type->faces)
    nil))
;; (mis/message/propertize t '(mis koan text) '(:text "hi"))
;; (mis/message/propertize nil '(mis koan text) 'newline)
;; (mis/message/propertize t '(mis koan text) :text "hi %s" "there")
;; (mis/message/propertize t '(spydez homeward) "  LSP: Killed %s servers." 1) ;; err: no type



;;------------------------------------------------------------------------------
;; Pretty Messages in *Messages* Buffer?
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-11-01]:
;;   - add a mis setting for whether to allow also-echo?

;; https://emacs.stackexchange.com/a/20178
(defun mis/message/preserve-properties (echo format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
M-x list-faces-display for all defined faces. Call with a propertized string.

If ECHO is non-nil, also echo message to the minibuffer echo area.
"
  (let ((output (apply 'format format args)))
    (with-current-buffer (get-buffer "*Messages*")
      ;;   "Manually inserts the propertized string at the end of the messages
      ;; buffer by lexically-binding inhibit-read-only to t in
      ;; the message buffer."
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          ;; Before insert, do a newline if needed.
          (unless (zerop (current-column)) (insert "\n"))
          ;; Actual propertized string put into *Messages* buffer
          (insert output)
          ;; And... insert a final newline if needed.
          (insert "\n"))))

    ;; String also goes into echo area, maybe.
    (if echo
        ;; But temp bind `message-log-max' to nil so it doesn't go into the
        ;; *Messages* buffer from here too, which would cause our first message
        ;; to lose its properties, somehow, probably due to *Messages* stacking
        ;; identical messages.
        (let ((message-log-max nil)
              (minibuffer-message-timeout mis/message/echo-area-timeout))
          (minibuffer-message output)))))

;; (mis/message/preserve-properties t (propertize "--->" 'face 'underline))
;; (mis/message/preserve-properties nil (propertize "--->" 'face 'underline))
;; (mis/message/preserve-properties t
;;  (concat
;;   (propertize "--->    " 'face 'font-lock-variable-name-face)
;;   " "
;;   (propertize "├" 'face 'font-lock-comment-delimiter-face)
;;   " "
;;   (propertize "(mis zeroth debug)" 'face 'font-lock-comment-face)
;;   " "
;;   (propertize "┤:" 'face 'font-lock-comment-delimiter-face)
;;   " "
;;   (propertize "early-init.el... Zeroth step." 'face 'default)))



;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-11]: Try font-lock mode instead of string properties?
;; Could add auto detect of my file names, maybe.
;; And other "arrorws"...
;; and 'require'...
;;
;; (mis/hook/defun example-hook t
;;     nil "simple-list" "init/config/configure-jeff.el"
;;   "Nice up simple lists - replacing hypen with a unicode middle dot."
;;   (font-lock-add-keywords
;;    nil ;; if in a derived mode, doing font lock in a hook could be easier...
;;    '(("^ *\\([-]\\) "
;;       (0 (prog1 () (compose-region (match-beginning 1)
;;                                    (match-end 1) "•")))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-message)
