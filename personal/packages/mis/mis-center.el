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
      (nth 1 margins))  ;; "xx"
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
;; Recentering Functions
;;------------------------------------------------------------------------------

(defun mis/recenter ()
  "Tries to recenter a line that is centered/formatted according
to mis/center's usual layout."
  (interactive)

  (let* ((origin (point))
         (decomposed (mis/center/decompose origin)))

    ;; need to kill current line and replace with new centered
    (kill-whole-line)
    (insert (mis/center
             (mis/center/parts
              (plist-get (plist-get decomposed :center) :text)
              nil nil ;; fill? and fill-char
              (list (plist-get (plist-get decomposed :prefix) :margin)
                    (plist-get (plist-get decomposed :postfix) :margin))
              (list (plist-get (plist-get decomposed :prefix) :border)
                    (plist-get (plist-get decomposed :postfix) :border))
              (list (plist-get (plist-get decomposed :prefix) :padding)
                    (plist-get (plist-get decomposed :postfix) :padding))
              (length (plist-get (plist-get decomposed :prefix) :indent))))
            ;; and a newline, cuz it's a line
            "\n")))
  ;;---------------------------------test-----------------------------------
  ;;---------------------------------test-----------------------------------
;;--                               test                                 --
;;--                               test                                 --
;;---------------------------testing-hi.hello-----------------------------
;;---------------------------testing-hi.hello-----------------------------


(defun mis/center/decompose (origin)
  "Tries to break apart a line into padding char, margin char, and
centered text.
"
  ;; Regexps have broken my resolve. Let's be old fashioned...
  ;; ...by using regexps. >.>
  ;; But at least, differently.

  ;; Go to start of this line. Will then save this point and also start
  ;; from here, so dual duty.
  (beginning-of-line)
  (let* ((from (point))
         (to   (save-excursion (end-of-line)      (point)))

         ;; Get line as string.
         (line (buffer-substring-no-properties from to))

         ;; Will for holding whatever regex we're using at the moment.
         (regexp "[ \\t\\n\\r]+") ;; 'whitespace' trim rx from string-trim

         ;; Output vars
         whitespace
         char-border
         char-padding
         text)

    ;; §-TODO-§ [2020-01-07]: Fill character for:
    ;;   ;;--+++Hello, there.+++--;;
    ;; versus:
    ;;   ;;--   Hello, there.   --;;

    ;; Trim whitespace both ends / note whitespace on left
    (re-search-forward regexp to t)
    (if (= (match-beginning 0) from)
        (setq whitespace (match-string-no-properties 0)))
    (setq line (string-trim line))

    ;; Note border char.
    ;; Trim border both ends / note border both ends.
    (setq char-border (string-to-char line))
    (setq regexp (rx-to-string `(one-or-more ,char-border)))
    (setq line (string-trim line regexp regexp))

    ;; Note padding char.
    ;; Trim padding both ends / note padding both ends.
    (setq char-padding (string-to-char line))
    (setq regexp (rx-to-string `(one-or-more ,char-padding)))
    (setq line (string-trim line regexp regexp))

    ;; End up with middle text.
    ;; Trim middle text of whitespace both sides.
    (setq text (string-trim line))

    ;; return start whitespace, padding, border, text.
    (list :prefix  (list :indent whitespace
                         :margin nil
                         :border char-border
                         :padding char-padding)
          :center  (list :text text)
          :postfix (list :padding char-padding
                         :border char-border
                         :margin nil))))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-center)
