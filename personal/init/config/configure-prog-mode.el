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

;;(setq debug-on-error t)
;; §-TODO-§ [2019-10-15]: recombine with spydez/prog-mode/comment/center after
;; splitting into parts...
(defun spydez/string/center
    (string &optional fill-string center-fill
            border-fill border-width
            indent return-list)
  "§-TODO-§ [2019-10-15]: better docstr

  Centers STRING. Wraps it in a border of BORDER-FILL characters (length
  BORDER-WIDTH or 2), and fills the center arount STRING with CENTER-FILL
  characters.

  A non-nil FILL-MESSAGE will allow spaces in STRING to be filled with
  CENTER-FILL as well."

  (let* ((border-fill  (or border-fill spydez/prog-mode/comment/center/thin))
         (border-width (or border-width 2))
         (center-fill  (or center-fill spydez/prog-mode/comment/center/thin))
         (string-placeholder
          (if fill-string
              ;; use string as placeholder so it'll get filled
              string
            ;; else make some junk to replace easy later
            (make-string (length string)
                         spydez/prog-mode/comment/center/placeholder)))
         (prefix  (string-trim-right (make-string 2 ?|)))
         (postfix (reverse prefix))
         (indent (or indent 0))
         (border (make-string border-width border-fill))
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
                                   (s-center line-len string-placeholder)))
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

    ;; (message "bf: '%c', bw: %s, cf: '%c'"
    ;;          border-fill border-width center-fill)
    ;; (message "cp: %s"  string-placeholder)
    ;; (message "ao: %s, pre: %s, post: %s, indent: %s, border: %s"
    ;;          addon prefix postfix indent border)
    ;; (message "pad-l: %s, pad-r: %s, line-l: %s, fill-l: %s"
    ;;          pad-left pad-right line-len fill-len)
    ;; (message "centered-l: %s, centered-t: %s, line-temp: %s"
    ;;          centered-line centered-text line-template)
    ;; (message "%s? %s %s -> %s"
    ;;          fill-string
    ;;          line-template
    ;;          (s-replace string-placeholder string line-template)
    ;;          (if fill-string
    ;;              line-template
    ;;            (s-replace string-placeholder string line-template)))

    (if (null return-list)
        ;; Now we either have to replace a placeholder, or be done.
        (if fill-string
            ;; No replacement; line-template is actual centered line.
            line-template
          (s-replace string-placeholder string line-template))

      (list
       (mapconcat 'identity (list prefix border) "")

       (if fill-string
            ;; No replacement; centered-text is actual centered line.
            centered-text
          (s-replace string-placeholder string centered-text))

       (mapconcat 'identity (list border postfix) "")
       ))))
;; (spydez/string/center "" t nil nil nil nil nil)


;; §-TODO-§ [2019-10-15]: split into subfunctions
(defun spydez/prog-mode/comment/center
    (comment &optional fill-comment center-fill border-fill border-width
             force-mirror indent return-list)
  "Centers COMMENT string. Wraps it in comment characters, gives it a border of
  BORDER-FILL characters (length FORCE-MIRROR or 2), and fills the center arount
  COMMENT with CENTER-FILL characters.

  A non-nil FILL-COMMENT will allow COMMENT to be filled with
  CENTER-FILL as well.

  A non-nil FORCE-MIRROR will append comment prefix characters to end of line,
  but only if there are no comment postfix characters."

  (let* ((border-fill  (or border-fill spydez/prog-mode/comment/center/thin))
         (border-width (or border-width 2))
         (center-fill  (or center-fill spydez/prog-mode/comment/center/thin))
         (comment-placeholder
          (if fill-comment
              ;; use comment as placeholder so it'll get filled
              comment
            ;; else make some junk to replace easy later
            (make-string (length comment)
                         spydez/prog-mode/comment/center/placeholder)))
         (addon   (comment-add nil))
         (prefix  (string-trim-right (comment-padright comment-start addon)))
         ;; postfix of comment's suggestion, or reversed prefix if FORCE-MIRROR,
         ;; or nil.
         (postfix (or (comment-padleft comment-end (comment-add addon))
                      (and force-mirror (reverse prefix))))
         (indent (or indent
                     (save-excursion
                       (beginning-of-line)
                       (funcall indent-line-function)
                       (current-column))))
         (border (make-string border-width border-fill))
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

    ;; (message "bf: '%c', bw: %s, cf: '%c'"
    ;;          border-fill border-width center-fill)
    ;; (message "cp: %s"  comment-placeholder)
    ;; (message "ao: %s, pre: %s, post: %s, indent: %s, border: %s"
    ;;          addon prefix postfix indent border)
    ;; (message "pad-l: %s, pad-r: %s, line-l: %s, fill-l: %s"
    ;;          pad-left pad-right line-len fill-len)
    ;; (message "centered-l: %s, centered-t: %s, line-temp: %s"
    ;;          centered-line centered-text line-template)
    ;; (message "%s? %s %s -> %s"
    ;;          fill-comment
    ;;          line-template
    ;;          (s-replace comment-placeholder comment line-template)
    ;;          (if fill-comment
    ;;              line-template
    ;;            (s-replace comment-placeholder comment line-template)))

    (if (null return-list)
        ;; Now we either have to replace a placeholder, or be done.
        (if fill-comment
            ;; No replacement; line-template is actual centered line.
            line-template
          (s-replace comment-placeholder comment line-template))

      (list
       (mapconcat 'identity (list prefix border) "")

       (if fill-comment
            ;; No replacement; centered-text is actual centered line.
            centered-text
          (s-replace comment-placeholder comment centered-text))

       (mapconcat 'identity (list border postfix) "")
      ))))
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
