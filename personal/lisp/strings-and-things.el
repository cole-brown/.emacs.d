;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------(not c-strings...)------------------------------
;;--                          Strings and Things!                            --
;;-----------------------------------------------------------------------------


(require 'subr-x)

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
(defcustom spydez/char/center/whitespace ?\s
  "Center with whitespace."
  :group 'spydez/group
  :type 'character)

(defcustom spydez/char/center/border ?-
  "Center with thin border."
  :group 'spydez/group
  :type 'character)

(defcustom spydez/char/center/padding ?|
  "Border for centering strings."
  :group 'spydez/group
  :type 'character)

(defconst spydez/char/center/placeholder (string-to-char "\uFFFD")
  "'Replacement character' seems a bit appropriate. Won't exist
outside function.")
;; https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character


;;------------------------------------------------------------------------------
;; String Functions
;;------------------------------------------------------------------------------


(defun spydez/string/center/paddings (&optional width fill-char)
  "Returns a 2-tuple of left and right paddings. Can be nil if no borders."
  (let ((fill-char (or fill-char spydez/char/center/padding))
        (width (or width 2)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (spydez/string/center/paddings)


(defun spydez/string/center/borders (&optional width fill-char)
  "Returns a 2-tuple of left and right borders. Can be nil if no borders."
  (let ((fill-char (or fill-char spydez/char/center/border))
        (width (or width 2)))
    (list
     ;; left:
     (make-string width fill-char)
     ;; right:
     (make-string width fill-char))))
;; (spydez/string/center/borders)


(defun spydez/string/center/parts
    (string &optional string/fill-spaces? string/fill-char
            paddings borders indent)
  "Figures out how to center STRING with INDENT (default 0), BORDERS (default
  `spydez/string/center/borders') and PADDINGS (default
  `spydez/string/center/paddings').

 If STRING/FILL-SPACES? is non-nil, STRING will be filled with STRING/FILL-CHAR
  (default `spydez/char/center/border'). characters.

Returns a plist with tags as shown:
(:prefix
 (:whitespace \"    \" :padding \"+++\" :border \"===\")
 :center
 (:text \"-------------------------center plz-----------------------------\")
 :postfix
 (:border \"===\" :padding \"+++\"))"

  (let* ((fill-char (or string/fill-char spydez/char/center/whitespace))
         (paddings  (or paddings (spydez/string/center/paddings)))
         (borders   (or borders (spydez/string/center/borders)))
         (indent    (or indent 0))
         (indent-text (make-string indent spydez/char/center/whitespace))
         (string-placeholder
          (if string/fill-spaces?
              ;; use string as placeholder so it'll get filled
              string
            ;; else make some junk to replace easy later
            (make-string (length string)
                         spydez/char/center/placeholder)))
         (len-left (+ indent
                     (length (nth 0 paddings))
                     (length (nth 0 borders))))
         (len-right (+ (length (nth 1 paddings))
                       (length (nth 1 borders))))
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
                         ;; carve out place for left padding/border
                         (substring centered-line len-left nil)
                         ;; Right Side: save everything except truncate off a
                         ;; len-right string for right padding/border
                         0 (- len-right))))

    ;; return the parts list
    (list
     ;; Left prefix:
     :prefix
     (list
      :whitespace
      indent-text         ;; "    "
      :padding
      (nth 0 paddings)    ;; "||"
      :border
      (nth 0 borders))    ;; "--"

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
      :border
      (nth 1 borders)     ;; "--"
      :padding
      (nth 1 paddings))   ;; "||"
     )))
;; (spydez/string/center/parts "center plz")
;; (spydez/string/center/parts "center plz" t)
;; (spydez/string/center/parts "center plz" t ?X)
;; (spydez/string/center/parts "center plz" nil nil
;;                             (spydez/string/center/paddings 3 ?X))
;; (spydez/string/center/parts "center plz" nil nil
;;                             (spydez/string/center/paddings 3 ?+)
;;                             (spydez/string/center/borders 3 ?=))
;; (spydez/string/center/parts "center plz" nil nil
;;                             (spydez/string/center/paddings 3 ?+)
;;                             (spydez/string/center/borders 3 ?=) 4)


(defun spydez/string/parts/build-string (string prop &optional faces-plist)
  "Builds a STRING. Propertizes from FACES-PLIST if PROP is found there."
  (let ((face-val (plist-get faces-plist prop)))
    (if face-val
        (propertize string 'face face-val)
      string)))
;; (spydez/string/parts/build-string "    " :whitespace nil)


(defun spydez/string/parts/build-section (section &optional faces-plist)
  "Builds a string from a parts-list SECTION,
propertized with face from FACES-PLIST if non-nil and can find
matching properties in both plists."
  ;; sanity check...
  (if (or
       ;; no param
       (null section)
       ;; or wrong type
       (not (listp section))
       ;; or uneven if list
       (= (% (length section) 2) 1))
      (unless (null section)
        ;; think nil should be allowed and just ignored/returned.
        (error "Cannot build string from section: '%s'." section))

      ;; Get property and section, see if we have matching face
      (let ((accum nil))
        ;; build each sub-section, push to accum
        (while section
          (let* ((prop     (nth 0 section))
                 (value    (nth 1 section))
                 (face-val (plist-get faces-plist prop)))

            ;; it'll be a string...
            (push (apply #'spydez/string/parts/build-string
                         ;; either now...
                         (if (stringp value)
                             (list value prop faces-plist)
                           ;; ...or after going deeper.
                           (list
                            (spydez/string/parts/build-section value
                                                         faces-plist)
                            prop
                            faces-plist)))
                  accum)
            ;; update loop conditional
            (setq section (cddr section))))
        ;; put 'em back in correct order and stringify 'em.
        (mapconcat #'identity (nreverse accum) ""))))
;; (setq test-parts
;;       '(:prefix
;;         (:whitespace "    " :padding "+++" :border "===")
;;         :center
;;         (:text "---------------------center plz-------------------------")
;;         :postfix
;;         (:border "===" :padding "+++")))
;; (spydez/string/parts/build-section
;;  (plist-get test-parts :prefix)
;;  '(:padding 'font-lock-comment-delimiter-face
;;    :border  'font-lock-comment-face))


(defun spydez/string/center (string-or-parts &optional faces-plist)
  "Centers STRING-OR-PARTS with defaults via `spydez/string/center/parts',
  or takes output of that function and returns the centered
  string.

  If faces-plist is non-nil, propertizes string parts as per plist."

  ;; build string from parts sections
  (spydez/string/parts/build-section
   ;; if we have a list, assume it's a properly formatted one for build-section
   (if (listp string-or-parts)
       string-or-parts
     (spydez/string/center/parts string-or-parts))
   faces-plist))
;; (spydez/string/center "foo")
;; (spydez/string/center "")


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'strings-and-things)
