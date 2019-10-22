;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------(not c-strings...)------------------------------
;;--                          Strings and Things!                            --
;;-----------------------------------------------------------------------------


(require 'subr-x)

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;;---
;; "Themed" Messages Consts & Vars
;;---

;; M-x list-faces-display
(defcustom spydez/string/type->faces
  '(((spydez homeward) (:padding    font-lock-comment-delimiter-face
                        :border     font-lock-comment-face
                        :text       font-lock-builtin-face
                        :highlight  font-lock-keyword-face
                        :highlight2 font-lock-constant-face
                        :title      font-lock-preprocessor-face))
    ;;---
    ;; Koans
    ;;---
    ;; text lines
    ((spydez koan text) (;; :whitespace nil
                         :padding  font-lock-comment-delimiter-face
                         :border   font-lock-comment-face
                         :text     font-lock-keyword-face))
    ;; non-text lines
    ((spydez koan presence) (;; :whitespace nil
                             :padding  font-lock-comment-delimiter-face
                             :border   font-lock-comment-face
                             :text     font-lock-comment-face)))
  "alist of: (types-list faces-list)
See 'M-x list-faces-display' for all defined faces."
  :group 'spydez/group
  :type '(alist :key-type list :value-type list))


;;---
;; Centering Consts & Vars
;;---

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


;;---
;; Koan Consts & Vars
;;---

(defvar spydez/koan/list nil
  "Koans loaded up and ready to go.")


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
;; (spydez/string/center/parts "")
;; (spydez/string/center/parts "" nil)


(defconst spydez/string/parts/type-list
  '(invalid
    symbol
    string
    pairs
    format)
  "Known and valid parts types.")


(defconst spydez/string/parts/symbols-alist
  `(;;---
    ;; strings, simple substitution
    ;;---
    (newline      nil "\n")
    (line-empty   nil "\n")
    (string-empty nil "")

    ;;---
    ;; strings, less simple
    ;;---
    ;; these should fill a line as defined by `fill-column'.
    (line-full   spydez/string/center/parts
                 "" nil ,spydez/char/center/border)
    (string-full spydez/string/center/parts
                 "" nil ,spydez/char/center/border))
  "Known and valid symbols alist. Format is:
  (symbol-name func arg0 arg1 ...)
or
  (symbol-name nil string).

A func will get called with args; nil func will return the string instead.")
;; (alist-get 'newline spydez/string/parts/symbols-alist)
;; (alist-get 'string-full spydez/string/parts/symbols-alist)


(defun spydez/string/parts/process/symbol (symbol)
  "Processes a SYMBOL into its parts."
  (if-let ((value (alist-get symbol spydez/string/parts/symbols-alist)))
      (cond ((and (null (nth 0 value))
                  (stringp (nth 1 value)))
             ;; Simple string - just return its value.
             (nth 1 value))

            ((functionp (nth 0 value))
             ;; More complex; call function w/ args and return that.
             (apply (car value) (cdr value)))

            (t
             ;; idk
             (spydez/message/warning
              nil :warning
              "Don't know how to translate symbol to part: sym: '%s' elt: '%s'"
              symbol value)
             nil))

    (spydez/message/warning
     nil :warning
     "No symbol in `spydez/string/parts/symbols-alist' for: '%s' '%s'"
     symbol spydez/string/parts/symbols-alist)))
;; (spydez/string/parts/process/symbol 'newline)
;; (spydez/string/parts/process/symbol 'line-full)


(defun spydez/string/parts/type (part)
  "Figures out part type. Types are:
 - symbol: 'newline
 - string: \"Hello, World!\"
 - pairs: (:whitespace \"    \" :padding \"+++\" :border \"===\")
 - format: (:title \"Test v%s.%s\" ver-major ver-minor)
 - invalid: unknown type. Fix caller or update code to deal with new type.

See `spydez/string/parts/type-list' for list.

Returns an element from `spydez/string/parts/type-list'."
  (cond
   ((symbolp part)
    'symbol)

   ((stringp part)
    'string)

   ;; list - more than 1 keyword, so assume 'pairs'.
   ;;   (:whitespace "    " :padding "+++" :border "===")
   ((and (listp part)
         (> (-count #'keywordp part) 1))
    'pairs)

   ;; list - 1 keyword, multiple other things. Assume format.
   ;;  (:text "Hello, %s %s %s" "World!" "And Mars..." "......(and jeff.)")
   ((and (listp part)
         (> (length part) 2) ;; (:keyword "fmt-str" extra stuff)
         (= (-count #'keywordp part) 1)) ;; only the one
    'format)

   ;; list - prop and str? Assume pairs.
   ;; (:border "--")
   ((and (listp part)
         (= (length part) 2)
         (= (-count #'keywordp part) 1))
    'pairs)

   ;; Dunno. invalid
   (t
    (spydez/message/warning nil :warning
                            "Don't know how to classify type of part: '%s'"
                            part)
    'invalid)))


;; Want all these types correctly dealt with...
;; Plist:
;; (:whitespace "    " :padding "+++" :border "===")
;;   -> pass-through
;; List of prop w/ format list:
;; (:title "Test v%s.%s" ver-major ver-minor)
;;   -> process to: (:title "Test v5.11")
;; Special Case Symbols:
;; 'line-empty
;;   -> process to: "\n"
;; String:
;; "hello there"
;;   -> pass-through
(defun spydez/string/parts/process (part)
  "Checks for special processing for PART. Handles a few types:

PART is:
  symbol: special string type e.g. 'string-empty, 'string-full
  string: un-special string type - will not get propertized or anything
  plist tuple: string that will probably get propertized

Returns for:
  symbol: string or plist tuple representing the string now
  string: string (no change)
  plist tuple: plist tuple
    - If more than one key, unchanged. E.g.
      (:whitespace \"    \" :padding \"+++\" :border \"===\")
    - if only one key, use other args to format string
      (:title \"Test v%s.%s\" ver-major ver-minor)
      -> (:title \"Test v5.11\")"

  (let ((result nil)
        (type (spydez/string/parts/type part)))
    (cond
     ;;---
     ;; type == invalid
     ;;---
     ((eq type 'invalid)
      ;; `spydez/string/parts/type' already complained - just pass nil as part.
      nil)

     ;;---
     ;; type == symbol
     ;;---
     ((eq type 'symbol)
      (setq result (spydez/string/parts/process/symbol part)))

     ;;---
     ;; type == string
     ;;---
     ;; string is just passed through.
     ((stringp part)
      (setq result part))

     ;;---
     ;; type == pairs, part 1
     ;;---
     ;; list - more than 1 keyword, so pass it through. e.g.
     ;; (:whitespace "    " :padding "+++" :border "===")
     ((and (listp part)
           (> (-count #'keywordp part) 1))
      (setq result part))

     ;;---
     ;; type == format
     ;;---
     ;; list - 1 keyword, multiple other things. Try to format it.
     ;; This expects a list like:
     ;;  (:text "Hello, %s %s %s" "World!" "And Mars..." "......(and jeff.)")
     ;; And returns:
     ;;  (:text "Hello, World! And Mars... ......(and jeff.)")
     ((and (listp part)
           (> (length part) 2) ;; (:keyword "fmt-str" extra stuff)
           (= (-count #'keywordp part) 1)) ;; only the one
      (setq result
            (list (car part)
                  (apply #'format (cdr part)))))

     ;;---
     ;; type == pairs, part 2
     ;;---
     ;; list - prop and str?
     ;; No formatting needed?.. Just pass through?
     ;; (:border "--")
     ((listp part)
      (setq result part))

     ;;---
     ;; type... should have been caught as invalid.
     ;;---
     ;; No other special cases, no other sanity checks... just pass through.
     (t
      (spydez/message/warning
       nil :warning
       "Type '%s' wasn't caught by specific condition: '%s'"
       type part)
      (setq result part)))

    ;; (message "processed: '%s' -> '%s'" part result)
    result))
;; (spydez/string/parts/process 'line-empty)
;; (spydez/string/parts/process 'string-empty)
;; (spydez/string/parts/process 'string-full)
;; (spydez/string/parts/process "hello")
;; (spydez/string/parts/process '(:border "----"))
;; (spydez/string/parts/process '(:text "Hello, %s %s %s" "a" "b" "c!"))
;; (spydez/string/parts/process '(:whitespace "  " :padding "++" :border "=="))


(defun spydez/string/parts/next (section)
  "Returns section sans this part. i.e. section set up for next
iteration of parts processing/building."

  (let ((type (spydez/string/parts/type section)))
    (cond
     ((eq type 'invalid)
      ;; type complained, so... just return nil?
      nil)

     ((eq type 'symbol)
      ;; No more parts to this section.
      nil)

     ((eq type 'string)
      ;; No more parts to this section.
      nil)

     ((eq type 'pairs)
      ;; Jump over this pair.
      (cddr section))

     ((eq type 'format)
      ;; No more parts to this section.
      nil))))
;; (spydez/string/parts/next '(:whitespace "  " :padding "++" :border "=="))


(defun spydez/strings/parts/property (type part)
  "Returns property from PART based on TYPE."
  ;; Simple enough now that we don't really need to rely on type yet.
  (if (and (listp part)
           (keywordp (car part)))
      (car part)
    nil))


(defun spydez/strings/parts/value (type part)
  "Returns value from PART based on TYPE."
  (cond
   ((eq type 'invalid)
    ;; type complained, so... just return nil?
    nil)

   ((or (eq type 'symbol)
        (eq type 'string))
    ;; value is the part itself
    part)

   ((eq type 'pairs)
    ;; value is 2nd element. Extract with nth.
    (nth 1 part))

   ((eq type 'format)
    ;; value is everything except property, which should be 1st.
    ;; Exclude prop and return "the rest".
    (cdr part))))


(defun spydez/string/parts/propertize (string prop &optional faces-plist)
  "Builds a STRING. Propertizes from FACES-PLIST if PROP is found there."
  (let ((face-val (plist-get faces-plist prop)))
    (if face-val
        (propertize string 'face face-val)
      string)))
;; (spydez/string/parts/propertize "    " :whitespace nil)


(defun spydez/string/parts/section (section &optional faces-plist)
  "Builds a SECTION of a string parts list."
  (let* ((type     (spydez/string/parts/type section))
         (prop     (spydez/strings/parts/property type section))
         (value    (spydez/strings/parts/value type section))
         (face-val (plist-get faces-plist prop)))

    ;; (message (concat "type: '%s', prop: '%s', value: '%s'"
    ;;                  ", face: '%s'... <-section: %s")
    ;;          type prop value face-val
    ;;          section)

    ;; it'll be a string...
    (apply #'spydez/string/parts/propertize
           ;; either now...
           (if (stringp value)
               (list value prop faces-plist)
             ;; ...or after going deeper.
             (list
              (spydez/string/parts/build value
                                         faces-plist)
              prop
              faces-plist)))))


(defun spydez/string/parts/build (arg &optional faces-plist)
  "Builds a string from ARG, propertized with face from
FACES-PLIST if non-nil and can find matching properties in both
plists.

§-TODO-§ [2019-10-22]: section here about what all arg can be."
  ;; sanity check...
  (if (or
       ;; no param
       (null arg)
       ;; or wrong type
       (not (or (listp arg)
                (stringp arg)
                (symbolp arg))))
      (unless (null arg)
        ;; think nil should be allowed and just ignored/returned.
        (error "Cannot build string from arg: '%s'." arg))

    ;; Process and build each item in section. Importantly, build off of
    ;; processed input, not raw. Need to allow expansion of symbols into big
    ;; sections. >.>
    (let ((accum nil)
          (input (spydez/string/parts/process arg)))
      ;; push to accum until no more sections
      (while input
        ;; process first section of input
        (push (spydez/string/parts/section input
                                           faces-plist)
              accum)

        ;; update loop conditional
        (setq input (spydez/string/parts/next input)))

      ;; put 'em back in correct order and stringify 'em.
      (mapconcat #'identity (nreverse accum) ""))))
;; (spydez/string/parts/build '(:padding "||" :border "----"))
;; (spydez/string/parts/build '(:prefix
;;                              (:whitespace "" :padding "||" :border "--")
;;                              :center
;;                              (:text "xx")
;;                              :postfix
;;                              (:border "==" :padding "!!")))
;; (spydez/string/parts/build 'string-empty)
;; (spydez/string/parts/build 'string-full)
;; (spydez/string/parts/build "hello")
;; (spydez/string/parts/build '(:text "Hello, %s %s %s" "a" "b" "c!"))
;; (spydez/string/parts/build '(:whitespace "  " :padding "++" :border "=="))


(defun spydez/string/center (string-or-parts &optional faces-plist)
  "Centers STRING-OR-PARTS with defaults via `spydez/string/center/parts',
  or takes output of that function and returns the centered
  string.

  If faces-plist is non-nil, propertizes string parts as per plist."

  ;; build string from parts sections
  (spydez/string/parts/build
   ;; if we have a list, assume it's a properly formatted one for build-section
   (if (listp string-or-parts)
       string-or-parts
     (spydez/string/center/parts string-or-parts))
   faces-plist))
;; (spydez/string/center "jeff")
;; (spydez/string/center "")


(defun spydez/message/propertize (type alist)
  "Acts like `spydez/message/preserve-properties' but takes a
ALIST of '((:symbol string) ...)'. This symbol should be defined in
`spydez/string/type->faces' for TYPE."
  (if-let ((faces (nth 1 (assoc type spydez/string/type->faces)))
           ;; null check
           (alist alist)
           ;; '(:prop "text") -> '((:prop "text))
           ;; (i.e. one element of alist into proper alist)
           (alist (if (listp (nth 0 alist)) alist (list alist))))

      ;; Build propertized string from alist.
      (spydez/message/preserve-properties
       (mapconcat 'identity
                  (let (result)
                    (dolist (entry alist result)
                      (push (spydez/string/parts/build entry faces)
                            result)))
                  ""))

    ;; Else didn't find in type->faces. Complain, return nil.
    (spydez/message/warning
     type :warning
     (concat "Null alist (%s)? Or type not found in "
             "`spydez/string/type->faces': %s -> %s")
     type alist spydez/string/type->faces)
    nil))
;; (spydez/message/propertize '(spydez homeward) '((:text "hi")))
;; (spydez/message/propertize '(spydez homeward) '(:text "hi"))


;;-----------------------------There is no spoon.-------------------------------
;;--                             Koan Functions                               --
;;------------------------------------------------------------------------------

(defun spydez/koan/process-line (line)
  "Returns a propertized line of a koan."
  (let ((faces-text (nth 1 (assoc '(spydez koan text)
                                  spydez/string/type->faces)))
        (faces-other (nth 1 (assoc '(spydez koan presence)
                                   spydez/string/type->faces))))

    (cond
     ;; 0) specials
     ((and (symbolp line)
           (alist-get line spydez/string/parts/symbols-alist))
      (spydez/string/parts/build line faces-other))

     ;; 1) general
     ;; a) Just a string
     ((stringp line)
      (spydez/string/center line faces-text))

     ;; b) ("string" border-width-int)
     ((and (listp line)
           (= (length line) 2)
           (stringp (nth 0 line))
           (numberp (nth 1 line)))
      ;; center, with parts built from string and border-width
      (spydez/string/center
       (spydez/string/center/parts
        (nth 0 line)
        nil nil nil
        (spydez/string/center/borders (nth 1 line)))
       faces-text))

     ;; 3) errors
     (t
      (error "unknown line type: %s" line)
      nil))))
;; (spydez/koan)


(defun spydez/koan/message (lines)
  "Prints a pretty little message to the *Messages* buffer."
  (dolist (line lines)
    (spydez/message/preserve-properties (spydez/koan/process-line line))))


(defun spydez/koan/add (koan)
  "Pushes KOAN into `spydez/koan/list'."
  (push koan spydez/koan/list))


(defun spydez/koan ()
  "Prints a random koan, or the one indicated by WHICH if non-nil."
  (when (and spydez/koan/list
             (listp spydez/koan/list))
    (spydez/koan/message (nth (random (length spydez/koan/list))
                              spydez/koan/list))))


;;---------------------No, really. Go get your own spoon.-----------------------
;;--                             Koan Collection                              --
;;------------------------------------------------------------------------------

;; General Kenobi!
(spydez/koan/add
  '(line-empty
    line-full
    "Hello there."
    line-full
    line-empty))


;; Such language...
(spydez/koan/add
  '(line-empty
    line-full
    "You can't say 'fuck' in the source code..."
    line-full
    line-empty))


;; §-TODO-§ [2019-10-15]: get more from here? http://www.catb.org/~esr/writings/unix-koans/
;; http://www.catb.org/~esr/writings/unix-koans/
(spydez/koan/add
 '(line-empty
   line-full
                        ("“Even the hacker who works alone" 13)
                           ("collaborates with others," 9)
                ("and must constantly communicate clearly to them," 7)
                    ("lest his work become confused and lost,”" 9)
                                ("said the Master." 13)
   line-full

               "“Of what others do you speak?” the Prodigy demanded."
   line-full
                   ("Master Foo said: “All your future selves.”" 13)
   line-full
   line-empty))


;; (theme song)
(spydez/koan/add
 '(line-empty
   line-full
   "Standby for Reincarnation..."
   "  - Futurama"
   line-full
   line-empty))


;; Spybreak! - Short One
(spydez/koan/add
  '(line-empty
    line-full
                        ("“Do not try and bend the spoon," 13)
                               ("that’s impossible." 9)
                   ("Instead, only try to realize the truth..." 7)
    line-full
                                "There is no spoon."
    line-full
              ("Then you’ll see that it is not the spoon that bends," 7)
                             ("it is only yourself.”" 9)
                               ("    - Spoon Boy to Neo" 13)
    line-full
    line-empty))


;; (spydez/koan)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-21]: Refactor?
;; - Move zeroith-debug.el string/message functions into here.
;; - Break here up into:
;;   - strings?  (strings-and-things.el?)
;;   - messages? (messages-and-misc.el?)
;;   - koans?    (koans-and-spoons.el?)
;; - `require' what's needed (or all 3?) in zeroth-debug.el
;;
;; So we have (in here right now) roughly:
;; - string/parts
;; - type->faces
;; - string/center
;; - propertize
;; - koan

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'strings-and-things)
