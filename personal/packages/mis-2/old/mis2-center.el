;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------Hello there.------------------------------------
;;--                          (will center on exit)                           --
;;------------------------------------------------------------------------------

;; Need s.el for `s-center', but don't want to require as we use other
;; parts of mis2 very early in init before packages.
;; (require 's)

(require 'mis2-parts)
(require 'subr-x)

;;------------------------------------------------------------------------------
;; Notes
;;------------------------------------------------------------------------------

;; This uses the "CSS Box Model" naming conventions, namely:
;;   - Margin - whitespace around outside
;;   - Border - left/right separators (e.g. comment character(s) in mis2-comment)
;;   - Padding - filler for text, with some minimum usually
;;   - Content/Center/Text - the actual text

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defcustom mis2/center/char/whitespace ?\s
  "Center with whitespace."
  :group 'mis2
  :type 'character)


(defcustom mis2/center/char/margin ?\s
  "Margin character."
  :group 'mis2
  :type 'character)


(defcustom mis2/center/char/padding ?-
  "Center with thin padding."
  :group 'mis2
  :type 'character)


(defcustom mis2/center/char/border ?|
  "Padding for centering strings."
  :group 'mis2
  :type 'character)


(defconst mis2/center/char/placeholder (string-to-char "\uFFFD")
  "'Replacement character' seems a bit appropriate. Won't exist
outside function.")
;; https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character


;;------------------------------------------------------------------------------
;; Main Entry Point
;;------------------------------------------------------------------------------

(defun mis2/center (string-or-parts &optional faces-plist)
  "Centers STRING-OR-PARTS with defaults via `mis2/center/parts',
  or takes output of that function and returns the centered
  string.

  If faces-plist is non-nil, propertizes string parts as per plist."

  ;; build string from parts sections
  (mis2/parts/build
   ;; if we have a list, assume it's a properly formatted one for build-section
   (if (listp string-or-parts)
       string-or-parts
     (mis2/center/parts string-or-parts))
   faces-plist))
;; (mis2/center "jeff")
;; (mis2/center "")


;;------------------------------------------------------------------------------
;; Centering Functions
;;------------------------------------------------------------------------------

(defun mis2/center/margins (&optional width fill-char)
  "Returns a 2-tuple of left and right margin. Can be nil if no margin.
Defaults to WIDTH of nil and FILL-CHAR of `mis2/center/char/padding'
(this nil WIDTH default means the default is no margins)."
  (if-let ((fill-char (or fill-char mis2/center/char/margin))
           ;; default is no margins, so if-let and this or for nil return.
           (width (or width nil)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (mis2/center/margins)


(defun mis2/center/borders (&optional width fill-char)
  "Returns a 2-tuple of left and right borders. Can be nil if no borders.
Defaults to WIDTH of 2 and FILL-CHAR of `mis2/center/char/border'."
  (if-let ((fill-char (or fill-char mis2/center/char/border))
           (width (or width 2)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (mis2/center/borders)


(defun mis2/center/paddings (&optional width fill-char)
  "Returns a 2-tuple of left and right paddings. Can be nil if no paddings.
Defaults to WIDTH of 2 and FILL-CHAR of `mis2/center/char/padding'."
  (if-let ((fill-char (or fill-char mis2/center/char/padding))
           (width (or width 2)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (mis2/center/paddings)

(defun mis2/center/parts
    (string &optional string/fill-spaces? string/fill-char
            margins borders paddings indent)
  "Figures out how to center STRING with INDENT (default 0), PADDINGS (default
`mis2/center/paddings') and BORDERS (default
`mis2/center/borders').

Uses `fill-column' for line width, so let-bind that if a
different line width is desired.

If STRING/FILL-SPACES? is non-nil, STRING will be filled with STRING/FILL-CHAR
(default `mis2/center/char/padding'). characters.

Returns a plist with tags as shown:
(:prefix
 (:indent \"    \" :margin \"xx\" :border \"+++\" :padding \"===\")
 :center
 (:text \"-------------------------center plz-----------------------------\")
 :postfix
 (:padding \"===\" :border \"+++\" :margin \"xx\"))"

  (let* ((fill-char (or string/fill-char mis2/center/char/whitespace))
         (margins   (or margins (mis2/center/margins)))
         (borders  (or borders (mis2/center/borders)))
         (paddings   (or paddings (mis2/center/paddings)))
         (indent    (or indent 0))
         (indent-text (make-string indent mis2/center/char/whitespace))
         (string-placeholder
          (if string/fill-spaces?
              ;; use string as placeholder so it'll get filled
              string
            ;; else make some junk to replace easy later
            (make-string (length string)
                         mis2/center/char/placeholder)))
         (len-left (+ indent
                      (length (nth 0 margins))
                      (length (nth 0 borders))
                      (length (nth 0 paddings))))
         (len-right (+ (length (nth 0 margins))
                       (length (nth 1 borders))
                       (length (nth 1 paddings))))
         ;; Pessimis2tically 1 less than fill column?
         (len-line (- fill-column (current-left-margin)));; be optimis2tic! 1))
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
;; (mis2/center/parts "center plz")
;; (mis2/center/parts "center plz" t)
;; (mis2/center/parts "center plz" t ?X)
;; (mis2/center/parts "center plz" nil nil
;;                   (mis2/center/margins 3 ?.))
;; (mis2/center/parts "center plz" nil nil
;;                   (mis2/center/margins 3 ?.)
;;                   (mis2/center/paddings 3 ?+))
;; (mis2/center/parts "center plz" nil nil
;;                   (mis2/center/margins 3 ?.)
;;                   (mis2/center/paddings 3 ?+)
;;                   (mis2/center/borders 3 ?=))
;; (mis2/center/parts "center plz" nil nil
;;                   (mis2/center/margins 3 ?.)
;;                   (mis2/center/borders 3 ?+)
;;                   (mis2/center/paddings 3 ?=) 4)
;; (mis2/center/parts "")
;; (mis2/center/parts "" nil)


;;------------------------------------------------------------------------------
;; Recentering Functions
;;------------------------------------------------------------------------------

(defun mis2/recenter ()
  "Tries to recenter a line that is centered/formatted according
to mis2/center's usual layout."
  (interactive)

  (let* ((origin (point))
         (decomposed (mis2/center/decompose origin)))

    ;; need to kill current line and replace with new centered
    (kill-whole-line)
    (insert (mis2/center
             (mis2/center/parts
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


(defun mis2/center/decompose (origin)
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
(provide 'mis2-center)
