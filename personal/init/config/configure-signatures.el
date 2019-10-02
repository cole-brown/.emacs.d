;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------Illegible Squiggle-------------------------------
;;--                       Insert John Hancock Here.                          --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
(require 'subr-x)

;; https://unicode-table.com/en/blocks/miscellaneous-symbols/
;; Single unicode options:
;;   ʬ ʭ ҈ ҉ † ‡ ‣ ↀ ↂ ∀ ∎ ∮ ≈ ≜ ≡ ≣ ≷ ⊫ ⋈
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


(defvar spydez/signature/options/list
  '(spydez/signature/short-pre
    spydez/signature/name-post
    spydez/signature/todo/comment
    spydez/signature/todo
    spydez/signature/char
    spydez/signature/name)
  "Signatures to present to user in prompt of
`spydez/signature/insert' and `spydez/signature/search'. Can be
strings or functions. As the function results could change
per-buffer, I will have to figure out buffer-local type things or
erse just eval these every call.")


(defun spydez/signature/options/add (signature)
  "Figures out how to make signature a string and returns the string."
  (cond
   ;; Function? Use its output.
   ((functionp signature)
    (funcall signature))

   ;; String? Use as is.
   ((stringp signature)
    signature)

   ;; Dive deeper?
   ((symbolp signature)
    (spydez/signature/options/add (symbol-value signature)))

   ;; Uh... Here there be dragons.
   (t
    (error "Unknown type of signature... '%s' is not: %s."
             signature
             "functionp or stringp"))))
;; (spydez/signature/options/add "hi")
;; (spydez/signature/options/add 'spydez/signature/todo/comment)
;; (setq direct-var "hello there")
;; (setq indirection-hah 'direct-var)
;; (spydez/signature/options/add 'direct-var)
;; (spydez/signature/options/add 'indirection-hah)


(defun spydez/signature/options ()
  "Build list of strings from `spydez/signature/options/list'."

  ;; Translate each s/s/o/l item into a string via s/s/o/a, then add to output
  ;; list if 'valid' (non-null ATM).
  (let (signatures)
    (dolist (sig-option spydez/signature/options/list signatures)
      (let ((sig-str (spydez/signature/options/add sig-option)))
        (unless (null sig-str)
          (push sig-str signatures))))))
;; (spydez/signature/options)


(defvar spydez/signature/insert/history nil
  "Just a bucket to hold history for sig commands to keep
  segregated from general history.")


;;------------------------------------------------------------------------------
;; Signatures - Insert, Search...
;;------------------------------------------------------------------------------

(defun spydez/signature/todo/dwim ()
  "Takes `spydez/signature/todo' and uses as-is, or adds comment
characters to it, as appropriate."
  (interactive)
  (cond
   ;; just sig str if in string
   ((spydez/point/inside-string-p)
    spydez/signature/todo)

   ;; just sig str if in comment
   ((spydez/point/inside-comment-p)
    spydez/signature/todo)

   ;; empty line? insert indented comment
   ((spydez/point/current-line-empty-p)
    (comment-indent)
    (spydez/signature/todo/comment))

   ;; Default... IDK. Just sig str?
   (t
    spydez/signature/todo)))


(defun spydez/signature/todo/timestamp (&optional timestamp)
  "Optionally adds a TIMESTAMP if non-nil to spydez/signature/todo."
  (if timestamp
      (concat spydez/signature/todo
              " "
              (format-time-string
               spydez/datetime/format/org-inactive-derivative))
    spydez/signature/todo))
;; (spydez/signature/todo/timestamp)
;; (spydez/signature/todo/timestamp t)


(defun spydez/signature/todo/comment (&optional timestamp)
  "Turns spydez/signature/todo into a proper comment based on
mode (uses `comment-*' emacs functions). Optionally adds a
TIMESTAMP if non-nil."
  (spydez/prog-mode/comment/wrap
   (concat
    (spydez/signature/todo/timestamp timestamp)
    ":")))
;; (spydez/signature/todo/comment)
;; (spydez/signature/todo/comment t)


;; Org-Mode Signature: For easy marking of "this here is my inserted note"
(defun spydez/signature/insert (signature)
  "Inserts a signature. Choose from a few options."
  (interactive (list
    ;; Arg 0: signature type
    (completing-read
     ;; Prompt:
     "Insert Signature: "

     ;; Shown list.
     (spydez/signature/options)

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


(defun spydez/signature/search (signature)
  "Choose a signature and then search for it via `isearch-forward'."
  (interactive (list
    ;; Arg 0: signature type
    (completing-read
     ;; Prompt:
     "Search for Signature: "

     ;; Shown list.
     (spydez/signature/options)

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
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-signatures)
