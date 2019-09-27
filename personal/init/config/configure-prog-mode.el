;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------------mode, not rock.-------------------------------
;;--                      General Programming Mode Stuff                     --
;;-----------------------------------------------------------------------------


(require 'subr-x)

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

(defcustom spydez/prog-mode/comment/center/whitespace ?\s
  "Center with whitespace."
  :group 'spydez/group
  :type 'character)

(defcustom spydez/prog-mode/comment/center/thin ?-
  "Center with thin border."
  :group 'spydez/group
  :type 'character)

(defconst spydez/prog-mode/comment/center/placeholder (string-to-char "\uFFFD")
  "'Replacement character' seems a bit appropriate. Won't exist
outside function.")
;; https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------

(defun spydez/prog-mode/comment/wrap (arg &optional trim concat-sep)
  "Turns (string) arg into a proper comment based on
mode (uses `comment-*' emacs functions)."
  (let* ((inside (if (stringp arg) arg (format "%s" arg)))
         (concat-sep (or concat-sep " "))
         (addon   (comment-add nil))
         (prefix  (string-trim-right (comment-padright comment-start addon)))
         (postfix (comment-padleft comment-end (comment-add addon)))
         (comment (mapconcat 'identity (list prefix inside postfix) concat-sep)))
    (if trim
        (string-trim comment)
      comment)))
;; (spydez/prog-mode/comment/wrap "foo")
;; (spydez/prog-mode/comment/wrap "foo" t)
;; (spydez/prog-mode/comment/wrap "---" nil "")
;; (spydez/prog-mode/comment/wrap "")
;; (spydez/prog-mode/comment/wrap (make-string 3 ?-) t "")


(defun spydez/prog-mode/comment/center
    (comment &optional fill-comment center-fill border-fill)
  "Centers `comment' string. Wraps it in comment characters, gives it a border
  of `border-fill' characters, and fills the center arount `comment' with
  `center-fill' characters.

  A non-nil `fill-comment' will allow `comment' to be filled with
  `center-fill' as well."

  (let* ((border-fill (or border-fill spydez/prog-mode/comment/center/thin))
         (center-fill (or center-fill spydez/prog-mode/comment/center/thin))
         (comment-placeholder
          (if fill-comment
              ;; use comment as placeholder so it'll get filled
              comment
            ;; else make some junk to replace easy later
            (make-string (length comment)
                         spydez/prog-mode/comment/center/placeholder)))
         (addon   (comment-add nil))
         (prefix  (string-trim-right (comment-padright comment-start addon)))
         (postfix (comment-padleft comment-end (comment-add addon)))
         (indent (save-excursion
                   (beginning-of-line)
                   (funcall indent-line-function)
                   (current-column)))
         (border (make-string 2 border-fill))
         (pad-left (+ indent
                     (length prefix)
                     (length border)))
         (pad-right (+ (length border)
                       (length postfix)))
         ;; Pessimistically 1 less than fill column.
         (line-len (- fill-column (current-left-margin) 1))
         (fill-len (- line-len pad-left pad-right))
         ;; Center the whole line so text is correctly placed even if left/right
         ;; padding aren't same size.
         (centered-line (s-replace " " (string center-fill)
                                   (s-center line-len comment-placeholder)))
         ;; Then trim out space for padding.
         (centered-text (substring
                         (substring centered-line pad-left nil)
                         0 (- pad-right)))
         ;; Join it all together.
         (line-template (mapconcat 'identity
                                   (list
                                    ;; "/*--"
                                    prefix
                                    border
                                    ;; "---centered---"
                                    centered-text
                                    ;; "--*/"
                                    border
                                    postfix)
                                   "")))
    ;; Now we either have to replace a placeholder, or be done.
    (if fill-comment
        ;; No replacement; line-template is actual centered line.
        line-template
      (s-replace comment-placeholder comment line-template))))

;; (spydez/prog-mode/comment/center "Hello there.")
;; (spydez/prog-mode/comment/center "Hello there." t)
;; (spydez/prog-mode/comment/center "Hello there." nil ?- ?-)
;; (spydez/prog-mode/comment/center "Hello there." nil ?\s ?-)
;; (spydez/prog-mode/comment/center "Hello there." t ?\s ?-)
;; (spydez/prog-mode/comment/center "")
;; (spydez/prog-mode/comment/center "" t)
;; (spydez/prog-mode/comment/center "test" t)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-prog-mode)
