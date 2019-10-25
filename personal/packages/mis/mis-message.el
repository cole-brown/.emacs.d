;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;----------------------------------...---...-----------------------------------
;;--                   We're all fine here... How are you?                    --
;;------------------------------------------------------------------------------


(require 'subr-x)

(require 'mis-parts)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Main Entry Point?
;;------------------------------------------------------------------------------

(defun mis/message/propertize (type &rest args)
  "Given TYPE, figure out a faces alist from `mis/type->faces' to use, then
build ARGS into propertized string via `mis/parts/build' and output it
to the *Messages* buffer.

NOTE: Could (optionally) add TYPE to output easily enough if desired."
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
      (mis/message/preserve-properties (mis/parts/build args faces))

    ;; Else didn't find in type in type->faces or null args.
    ;; Complain, return nil.
    (mis/warning
     type :warning
     (concat "Null args (%s)? Or type not found in "
             "`mis/type->faces': %s -> %s")
     args type mis/type->faces)
    nil))
;; (mis/message/propertize '(mis koan text) '(:text "hi"))
;; (mis/message/propertize '(mis koan text) 'newline)


;;------------------------------------------------------------------------------
;; Pretty Messages in *Messages* Buffer?
;;------------------------------------------------------------------------------

;; https://emacs.stackexchange.com/a/20178
(defun mis/message/preserve-properties (format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
M-x list-faces-display for all defined faces. Call with a propertized string."

  ;; ;; Temporarily nuke log max to let us do this Stupid Shindig.
  ;; (let ((message-log-max nil))
  ;;   ;;   "Calls message with message-log-max lexically bound to nil so it
  ;;   ;; won't log to *Messages*."
  ;;   ;; ...makes sense.
  ;;   (apply 'message format args))
  ;;
  ;; Yeah, ok. Um... One of these?
  ;;   1) The StackExchange user didn't figure out which bits
  ;;      were /actually/ needed.
  ;;   2) That whatever it did used to be required but isn't now.
  ;;   3) Windows 7 and/or
  ;;      "GNU Emacs 26.1 (build 1, x86_64-w64-mingw32) of 2018-05-30"
  ;;      don't need that step.
  ;;   4) Magic.
  ;; I'll go with... not #1. So (+ 2 (random 3)). Definitely that one.

  ;; 2 Stupid 2 Shindig.
  (with-current-buffer (get-buffer "*Messages*")
    ;;   "Manually inserts the propertized string at the end of the messages
    ;; buffer by lexically-binding inhibit-read-only to t in
    ;; the message buffer."
    ;; ...So why the call to messages then?!
    ;;   - Oh. I see. Reason number (+ 2 (random 3)).
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        ;; Before insert, do a newline if needed.
        (unless (zerop (current-column)) (insert "\n"))
        ;; Actual propertized string put into *Messages* buffer
        (insert (apply 'format format args))
        ;; And... insert a final newline if needed.
        (insert "\n"))))

  ;; §-TODO-§ [2019-10-24]: Can I get this to have its message also show up in
  ;; echo area?
  ;;   - not that I can figure out yet.
  ;; Fails:
  ;;   - Return formatted string
  ;;     -> pp-eval-expression instead
  ;;   - Do Stupid Shindig step 1 down at bottom
  ;;     -> pp-eval-expression instead
  )
;; (mis/message/preserve-properties (propertize "--->" 'face 'underline))
;; (mis/message/preserve-properties
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
