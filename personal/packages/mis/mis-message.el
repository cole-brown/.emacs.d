;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;----------------------------------...---...-----------------------------------
;;--                   We're all fine here... How are you?                    --
;;------------------------------------------------------------------------------


(require 'subr-x)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Pretty Messages in *Messages* Buffer?
;;------------------------------------------------------------------------------

;; https://emacs.stackexchange.com/a/20178
(defun mis/message/preserve-properties (format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
M-x list-faces-display for all defined faces. Call with a propertized string."
  (let ((message-log-max nil))
    (apply 'message format args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (zerop (current-column)) (insert "\n"))
        (insert (apply 'format format args))
        (insert "\n")))))
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
;; Functions
;;------------------------------------------------------------------------------

(defun mis/message/propertize (type arg)
  "Given TYPE, figure out a faces alist from `mis/type->faces' to use, then
build ARG into propertized string via `mis/parts/build' and output it
to the *Messages* buffer.

NOTE: Could (optionally) add TYPE to output easily enough if desired."
  (if-let ((faces (nth 1 (assoc type mis/type->faces)))
           ;; null check
           (arg arg))

      ;; Build propertized string and output it.
      (mis/message/preserve-properties (mis/parts/build arg faces))

    ;; Else didn't find in type in type->faces or null arg.
    ;; Complain, return nil.
    (mis/warning
     type :warning
     (concat "Null arg (%s)? Or type not found in "
             "`mis/type->faces': %s -> %s")
     arg type mis/type->faces)
    nil))
;; (mis/message/propertize '(mis koan text) '(:text "hi"))



;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-message)
