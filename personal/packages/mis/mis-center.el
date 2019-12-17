;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------Hello there.------------------------------------
;;--                          (will center on exit)                           --
;;------------------------------------------------------------------------------

;; Need s.el for `s-center', but don't want to require as we use other
;; parts of mis very early in init before packages.
;; (require 's)

(require 'mis-parts)
(require 'subr-x)

;;------------------------------------------------------------------------------
;; Notes
;;------------------------------------------------------------------------------

;; This uses the "CSS Box Model" naming conventions, namely:
;;   - Margin - whitespace around outside
;;   - Border - left/right separators (e.g. comment character(s) in mis-comment)
;;   - Padding - filler for text, with some minimum usually
;;   - Content/Center/Text - the actual text

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defcustom mis/center/char/whitespace ?\s
  "Center with whitespace."
  :group 'mis
  :type 'character)


(defcustom mis/center/char/margin ?\s
  "Margin character."
  :group 'mis
  :type 'character)


(defcustom mis/center/char/padding ?-
  "Center with thin padding."
  :group 'mis
  :type 'character)


(defcustom mis/center/char/border ?|
  "Padding for centering strings."
  :group 'mis
  :type 'character)


(defcustom mis/center/regexp/recenter
  (rx line-start
      (0+ whitespace)

      (group graphic)
      (+? (backref 1))
      (*? printing)

      (group graphic)
      (>= 2 (backref 2))
      (*? printing)

      ;; Main Match (center) Group!
      (group (>= 3 printing))

      ;; The rest don't actually help...
      ;; ...with my levels of rx-fu, anyways.

      ;; ;; back to yyy
      ;; (*? printing)
      ;; (>= 3 (backref 2))

      ;; ;; back to xx
      ;; (??
      ;;  (minimal-match
      ;;   (0+ printing))
      ;;  (>= 2 (backref 1)))

      ;; (*? printing)
      ;; line-end
      )
  "Regexp for finding the region we care about for a recenter.")


;; §-TODO-§ [2019-12-13]: Finish this, move down to a functions/code section.
(defun mis/center/recenter ()
  "Tries to recenter a line that is centered/formatted according
to mis/center's usual layout."
  (interactive)

  (let* ((origin (point))
        (pieces (mis/center/decompose origin)))

    (message "%S" pieces)
    ))
;;---------------------------------test-----------------------------------
;;--                               test                                 --
;;---------------------------testing-hi.hello-----------------------------

;; §-TODO-§ [2019-12-16]: stopped here:
;; Top and bottom test line work.
;; Middle does not - regex doesn't match everything.

(defun mis/center/decompose (point)
  "Tries to break apart a line into padding char, margin char, and
centered text.
"
  (prog1
      (progn
        ;; Go to start of this line. Will then save this point and also start
        ;; from here, so dual duty.
        (beginning-of-line)
        (let ((from (point))
              ;; (from (save-excursion (beginning-of-line (point))))
              (to   (save-excursion (end-of-line)      (point))))

          ;; Do search, populate match data.
          (re-search-forward mis/center/regexp/recenter to t)
          (message "match-data: %S, 1: %S, 2: %S, 3: %S, line: %S" (match-data)
                   (match-string-no-properties 1)
                   (match-string-no-properties 2)
                   (match-string-no-properties 3)
                   (match-string-no-properties 0)
                   )

          ;; Ok; my regexp is... suck, but seems to do the first half good, so
          ;; we use that to chop down the target center text. It'll start where
          ;; the regexp says it does and go to the midpoint and then that number
          ;; of characters past it.
          ;;
          ;; line-*: related to start of line, line length
          ;; text-*: related to centered text, position in buffer
          ;; line-text-*: related to centered text, position in line
          (let* ((line-len        (- to from))
                 (line-mid        (/ line-len 2))
                 (text-start      (match-beginning 3))
                 (line-text-start (- text-start from))
                 (line-text-end   (+ line-text-start
                                     (* 2 (- line-mid line-text-start))))
                 (text-end (+ text-start line-text-end))
                 (padding (buffer-substring-no-properties
                           (match-beginning 1)
                           (match-end 1)))
                 (margin (buffer-substring-no-properties
                          (match-beginning 2)
                          (match-end 2))))

            (message "padding: %S, margin: %S" padding margin)

            ;; Now we have the data to re-build our line...
            (list :padding padding
                  :margin  margin
                           ;; trim margin
                  :text    (string-trim-right
                            ;; end-of-line: whitespace and padding
                            (string-trim-right
                             (buffer-substring-no-properties
                              text-start
                              text-end)
                             (rx-to-string `(one-or-more
                                             (or whitespace ,padding))))
                            (rx-to-string `(one-or-more ,margin)))))))

    ;; go back to where we started, but return above value
    (goto-char point)))


(defconst mis/center/char/placeholder (string-to-char "\uFFFD")
  "'Replacement character' seems a bit appropriate. Won't exist
outside function.")
;; https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character


;;------------------------------------------------------------------------------
;; Main Entry Point
;;------------------------------------------------------------------------------

(defun mis/center (string-or-parts &optional faces-plist)
  "Centers STRING-OR-PARTS with defaults via `mis/center/parts',
  or takes output of that function and returns the centered
  string.

  If faces-plist is non-nil, propertizes string parts as per plist."

  ;; build string from parts sections
  (mis/parts/build
   ;; if we have a list, assume it's a properly formatted one for build-section
   (if (listp string-or-parts)
       string-or-parts
     (mis/center/parts string-or-parts))
   faces-plist))
;; (mis/center "jeff")
;; (mis/center "")


;;------------------------------------------------------------------------------
;; Centering Functions
;;------------------------------------------------------------------------------

(defun mis/center/margins (&optional width fill-char)
  "Returns a 2-tuple of left and right margin. Can be nil if no margin.
Defaults to WIDTH of nil and FILL-CHAR of `mis/center/char/padding'
(this nil WIDTH default means the default is no margins)."
  (if-let ((fill-char (or fill-char mis/center/char/margin))
           ;; default is no margins, so if-let and this or for nil return.
           (width (or width nil)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (mis/center/margins)


(defun mis/center/borders (&optional width fill-char)
  "Returns a 2-tuple of left and right borders. Can be nil if no borders.
Defaults to WIDTH of 2 and FILL-CHAR of `mis/center/char/border'."
  (if-let ((fill-char (or fill-char mis/center/char/border))
           (width (or width 2)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (mis/center/borders)


(defun mis/center/paddings (&optional width fill-char)
  "Returns a 2-tuple of left and right paddings. Can be nil if no paddings.
Defaults to WIDTH of 2 and FILL-CHAR of `mis/center/char/padding'."
  (if-let ((fill-char (or fill-char mis/center/char/padding))
           (width (or width 2)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (mis/center/paddings)

(defun mis/center/parts
    (string &optional string/fill-spaces? string/fill-char
            margins borders paddings indent)
  "Figures out how to center STRING with INDENT (default 0), PADDINGS (default
`mis/center/paddings') and BORDERS (default
`mis/center/borders').

Uses `fill-column' for line width, so let-bind that if a
different line width is desired.

If STRING/FILL-SPACES? is non-nil, STRING will be filled with STRING/FILL-CHAR
(default `mis/center/char/padding'). characters.

Returns a plist with tags as shown:
(:prefix
 (:indent \"    \" :margin \"xx\" :border \"+++\" :padding \"===\")
 :center
 (:text \"-------------------------center plz-----------------------------\")
 :postfix
 (:padding \"===\" :border \"+++\" :margin \"xx\"))"

  (let* ((fill-char (or string/fill-char mis/center/char/whitespace))
         (margins   (or margins (mis/center/margins)))
         (borders  (or borders (mis/center/borders)))
         (paddings   (or paddings (mis/center/paddings)))
         (indent    (or indent 0))
         (indent-text (make-string indent mis/center/char/whitespace))
         (string-placeholder
          (if string/fill-spaces?
              ;; use string as placeholder so it'll get filled
              string
            ;; else make some junk to replace easy later
            (make-string (length string)
                         mis/center/char/placeholder)))
         (len-left (+ indent
                      (length (nth 0 margins))
                      (length (nth 0 borders))
                      (length (nth 0 paddings))))
         (len-right (+ (length (nth 0 margins))
                       (length (nth 1 borders))
                       (length (nth 1 paddings))))
         ;; Pessimistically 1 less than fill column?
         (len-line (- fill-column (current-left-margin)));; be optimistic! 1))
         (len-fill (- len-line len-left len-right))
         ;; Center the whole line (to len-line) so text is correctly placed even
         ;; if len-left/right aren't same size.
         (centered-line (s-replace " " (string fill-char)
                                   (s-center len-line string-placeholder)))
         ;; Now carve out space for left/right strings
         (centered-text (substring
                         ;; Left Side: substring len-left from left side to
                         ;; carve out place for left border/padding
                         (substring centered-line len-left nil)
                         ;; Right Side: save everything except truncate off a
                         ;; len-right string for right border/padding
                         0 (- len-right))))

    ;; return the parts list
    (list
     ;; Left prefix:
     :prefix
     (list
      :indent
      indent-text         ;; "    "
      :margin
      (nth 0 margins)    ;; "xx"
      :border
      (nth 0 borders)    ;; "||"
      :padding
      (nth 0 paddings))    ;; "--"

     ;; centered string:  ;; "---centered---"
     :center
     (list
      :text
      (if string/fill-spaces?
          ;; No replacement; centered-text is actual text.
          centered-text
        ;; Need to replace placeholder w/ actual text.
        (s-replace string-placeholder string centered-text)))

     ;; Right postfix:
     :postfix
     (list
      :padding
      (nth 1 paddings)  ;; "--"
      :border
      (nth 1 borders)   ;; "||"
      :margin
      (nth 0 margins))  ;; "xx"
     )))
;; (mis/center/parts "center plz")
;; (mis/center/parts "center plz" t)
;; (mis/center/parts "center plz" t ?X)
;; (mis/center/parts "center plz" nil nil
;;                   (mis/center/margins 3 ?.))
;; (mis/center/parts "center plz" nil nil
;;                   (mis/center/margins 3 ?.)
;;                   (mis/center/paddings 3 ?+))
;; (mis/center/parts "center plz" nil nil
;;                   (mis/center/margins 3 ?.)
;;                   (mis/center/paddings 3 ?+)
;;                   (mis/center/borders 3 ?=))
;; (mis/center/parts "center plz" nil nil
;;                   (mis/center/margins 3 ?.)
;;                   (mis/center/borders 3 ?+)
;;                   (mis/center/paddings 3 ?=) 4)
;; (mis/center/parts "")
;; (mis/center/parts "" nil)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-center)
