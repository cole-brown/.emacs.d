;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------Illegible Squiggle-------------------------------
;;--                       Insert John Hancock Here.                          --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;; https://unicode-table.com/en/blocks/miscellaneous-symbols/
;; Single unicode options:
;;   ʬ ʭ ҈ ҉ † ‡ ‣ ↀ ↂ  ∀ ∎ ∮ ≈ ≜ ≡ ≣ ≷ ⊫ ⋈
;;   § ▀ ▄
;;  Can't see now, but maybe in future? Watch the line height though...
;;   ⛤ ⇶ ⌦ ⏩ ⏻ ␑ ␦ ⑄ ⚶
(defcustom spydez/signature/char "§" ;; 2nd choice right now: ▀
  "Short 1-char signature for notes
(differentating my additions to someone else's text blocks)."
  :group 'spydez/group
  :type 'string)


(defcustom spydez/signature/short-pre (concat spydez/signature/char ": ")
  "Short signature for prefixing note lines."
  :group 'spydez/group
  :type 'string)


(defcustom spydez/signature/name user-full-name
  "Long signature."
  :group 'spydez/group
  :type 'string)


(defcustom spydez/signature/name-post (concat "-" spydez/signature/name)
  "Long signature for postfixing note lines or as last line of note block."
  :group 'spydez/group
  :type 'string)


(defcustom spydez/signature/todo (concat spydez/signature/char
                                         "-TODO-"
                                         spydez/signature/char)
  "Long signature for postfixing note lines or as last line of note block."
  :group 'spydez/group
  :type 'string)


(defvar spydez/signature/options
  (list spydez/signature/short-pre
        spydez/signature/name-post
        spydez/signature/todo
        spydez/signature/char
        spydez/signature/name)
  "Signatures to present to user in prompt of
`spydez/signature/insert' and `spydez/signature/search'.")
;; To update it, use eval-defun (C-M-x) like for when in the middle
;; of a function.


(defvar spydez/signature/insert/history nil)


;;------------------------------------------------------------------------------
;; Signatures - Insert, Search...
;;------------------------------------------------------------------------------


;; Org-Mode Signature: For easy marking of "this here is my inserted note"
(defun spydez/signature/insert (signature)
  "Inserts a signature. Choose from a few options."
  (interactive (list
    ;; Arg 0: signature type
    (completing-read
     ;; Prompt:
     "Insert Signature: "

     ;; Shown list.
     spydez/signature/options

     ;; No predicate to limit above (shown list).
     ;; TODO: limit to short/char if not at EOL?
     nil

     ;; Set to 'confirm if want confirmation of non-list entry.
     ;; But right now I think deny all not on list via `true'.
     t

     ;; Deprecated.
     nil

     ;; Get our own separate history for this command.
     'spydez/signature/insert/history

     ;; default user input value
     nil)))

  (unless (null signature)
    ;; now insert the chosen at point
    (insert signature)))


(require 'subr-x)
(defun spydez/signature/search (signature)
  "Choose a signature and then search for it via `isearch-forward'."
  (interactive (list
    ;; Arg 0: signature type
    (completing-read
     ;; Prompt:
     "Search for Signature: "

     ;; Shown list.
     spydez/signature/options

     ;; No predicate to limit above (shown list).
     ;; TODO: limit to short/char if not at EOL?
     nil

     ;; Set to 'confirm if want confirmation of non-list entry.
     ;; But right now I think deny all not on list via `true'.
     t

     ;; Deprecated.
     nil

     ;; Get our own separate history for this command.
     'spydez/signature/insert/history

     ;; default user input value
     nil)))

  (if (null signature)
      (message "Cannot search for nothing.")

    ;; Thank you, StackOverflow.
    ;; https://emacs.stackexchange.com/questions/2754/preset-search-isearch-string-from-command-line
    (isearch-forward nil 1)
    (isearch-yank-string (string-trim signature))))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-signatures)
