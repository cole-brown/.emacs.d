;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;--------------------------They'll grow up some day.---------------------------
;;--               Strings That Aren't, like... Strings. Yet?                 --
;;------------------------------------------------------------------------------

;; These should/must all deal in strings.


(require 'subr-x)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defcustom mis/parts/type-list
  ;; §-TODO-§ [2019-10-23]: more types, possibly a meta-type? e.g. koans have
  ;;   '(string int) for centered string w/ padding width though that could be
  ;;   converted to... something fancier, but we only look at keywords for faces
  ;;   right now.
  '(:invalid
    :symbol
    :string
    :pairs
    :format)
  "Known and valid parts types."
  :group 'mis
  :type 'sexp)


(defcustom mis/parts/symbols-alist
  '(;;---
    ;; strings, simple substitution
    ;;---
    ;; (SYMBOL NIL STRING-SUBSTITUTION)
    (:newline      nil "\n")
    (:line-empty   nil "\n")
    (:string-empty nil "")

    ;;---
    ;; strings, less simple
    ;;---
    ;; (SYMBOL FUNCTION FUNC-ARG-0 FUNC-ARG-1 ...)
    ;; these should fill a line as defined by `fill-column'.
    (:line-full   mis/center/parts
                  "" nil mis/center/char/padding)
    (:string-full mis/center/parts
                  "" nil mis/center/char/padding))
  "Known and valid symbols alist. Format is:
  (symbol-name func arg0 arg1 ...)
or
  (symbol-name nil string).

A func will get called with args; nil func will return the string instead."
  :group 'mis
  :type '(alist :key-type symbol :value-type sexp))
;; (alist-get :newline mis/parts/symbols-alist)
;; (alist-get :string-full mis/parts/symbols-alist)


;;------------------------------------------------------------------------------
;; Info/Meta-Data Functions
;;------------------------------------------------------------------------------

(defun mis/parts/type (part)
  "Figures out part type. Types are:
 - symbol: :newline
 - string: \"Hello, World!\"
 - pairs: (:indent \"    \" :border \"+++\" :padding \"===\")
 - format: (:title \"Test v%s.%s\" ver-major ver-minor)
 - invalid: unknown type. Fix caller or update code to deal with new type.

See `mis/parts/type-list' for list.

Returns an element from `mis/parts/type-list'."
  (cond
   ((symbolp part)
    :symbol)

   ((stringp part)
    :string)

   ;; list - more than 1 keyword, so assume 'pairs'.
   ;;   (:indent "    " :border "+++" :padding "===")
   ((and (listp part)
         (> (-count #'keywordp part) 1))
    :pairs)

   ;; list - 1 keyword, multiple other things. Assume format.
   ;;  (:text "Hello, %s %s %s" "World!" "And Mars..." "......(and jeff.)")
   ((and (listp part)
         (> (length part) 2) ;; (:keyword "fmt-str" extra stuff)
         (= (-count #'keywordp part) 1)) ;; only the one
    :format)

   ;; list - prop and str? Assume pairs.
   ;; (:padding "--")
   ((and (listp part)
         (= (length part) 2)
         (= (-count #'keywordp part) 1))
    :pairs)

   ;; Dunno. invalid
   (t
    (mis/warning nil :warning
                 "Don't know how to classify type of part: '%S'"
                 part)
    :invalid)))


(defun mis/parts/property (type part)
  "Returns property from PART based on TYPE."
  ;; Simple enough now that we don't really need to rely on type yet.
  (if (and (listp part)
           (keywordp (car part)))
      (car part)
    nil))


(defun mis/parts/value (type part)
  "Returns value from PART based on TYPE."
  (cond
   ((eq type :invalid)
    ;; type complained, so... just return nil?
    nil)

   ((or (eq type :symbol)
        (eq type :string))
    ;; value is the part itself
    part)

   ((eq type :pairs)
    ;; value is 2nd element. Extract with nth.
    (nth 1 part))

   ((eq type :format)
    ;; value is everything except property, which should be 1st.
    ;; Exclude prop and return "the rest".
    (cdr part))))


(defun mis/parts/next (section)
  "Returns section sans this part. i.e. section set up for next
iteration of parts processing/building."

  (let ((type (mis/parts/type section)))
    (cond
     ((eq type :invalid)
      ;; type complained, so... just return nil?
      nil)

     ((eq type :symbol)
      ;; No more parts to this section.
      nil)

     ((eq type :string)
      ;; No more parts to this section.
      nil)

     ((eq type :pairs)
      ;; Jump over this pair.
      (cddr section))

     ((eq type :format)
      ;; No more parts to this section.
      nil))))
;; (mis/parts/next '(:indent "  " :border "++" :padding "=="))


;;------------------------------------------------------------------------------
;; Processing
;;------------------------------------------------------------------------------

(defun mis/parts/process/symbol (symbol)
  "Processes a SYMBOL into its parts."
  (if-let ((value (alist-get symbol mis/parts/symbols-alist)))
      (cond
       ((and (null (nth 0 value))
             (stringp (nth 1 value)))
        ;; Simple string - just return its value.
        (nth 1 value))

       ((functionp (nth 0 value))
        ;; More complex; call function w/ args and return that. But I
        ;; didn't want to eval symbols in `mis/parts/symbols-alist' then,
        ;; so I need to eval them now.
        (let ((accum nil))
          (dolist (element (cdr value) accum)
            (push (if (symbolp element)
                      (symbol-value element)
                    element)
                  accum))
          (apply (car value) (nreverse accum))))

       (t
        ;; idk
        (mis/warning
         nil :warning
         "Don't know how to translate symbol to part: sym: '%S' elt: '%S'"
         symbol value)
        nil))

    (mis/warning
     nil :warning
     "No symbol in `mis/parts/symbols-alist' for: '%S' '%S'"
     symbol mis/parts/symbols-alist)))
;; (mis/parts/process/symbol :newline)
;; (mis/parts/process/symbol :line-full)


;; Want all these types correctly dealt with...
;; Plist:
;; (:indent "    " :border "+++" :padding "===")
;;   -> pass-through
;; List of prop w/ format list:
;; (:title "Test v%s.%s" ver-major ver-minor)
;;   -> process to: (:title "Test v5.11")
;; Special Case Symbols:
;; :line-empty
;;   -> process to: "\n"
;; String:
;; "hello there"
;;   -> pass-through
(defun mis/parts/process (part)
  "Checks for special processing for PART. Handles a few types:

PART is:
  symbol: special string type e.g. :string-empty, :string-full
  string: un-special string type - will not get propertized or anything
  plist tuple: string that will probably get propertized

Returns for:
  symbol: string or plist tuple representing the string now
  string: string (no change)
  plist tuple: plist tuple
    - If more than one key, unchanged. E.g.
      (:indent \"    \" :border \"+++\" :padding \"===\")
    - if only one key, use other args to format string
      (:title \"Test v%s.%s\" ver-major ver-minor)
      -> (:title \"Test v5.11\")"

  (let ((result nil)
        (type (mis/parts/type part)))
    (cond
     ;;---
     ;; type == invalid
     ;;---
     ((eq type :invalid)
      ;; `mis/parts/type' already complained - just pass nil as part.
      nil)

     ;;---
     ;; type == symbol
     ;;---
     ((eq type :symbol)
      (setq result (mis/parts/process/symbol part)))

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
     ;; (:indent "    " :border "+++" :padding "===")
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
     ;; (:padding "--")
     ((listp part)
      (setq result part))

     ;;---
     ;; type... should have been caught as invalid.
     ;;---
     ;; No other special cases, no other sanity checks... just pass through.
     (t
      (mis/warning
       nil :warning
       "Type '%S' wasn't caught by specific condition: '%S'"
       type part)
      (setq result part)))

    ;; (message "processed: '%S' -> '%S'" part result)
    result))
;; (mis/parts/process :line-empty)
;; (mis/parts/process :string-empty)
;; (mis/parts/process :string-full)
;; (mis/parts/process "hello")
;; (mis/parts/process '(:padding "----"))
;; (mis/parts/process '(:text "Hello, %s %s %s" "a" "b" "c!"))
;; (mis/parts/process '(:indent "  " :border "++" :padding "=="))


(defun mis/parts/propertize (string prop &optional faces-plist)
  "Builds a STRING. Propertizes from FACES-PLIST if PROP is found there."
  (let ((face-val (plist-get faces-plist prop)))
    (if face-val
        (propertize string 'face face-val)
      string)))
;; (mis/parts/propertize "    " :indent nil)


(defun mis/parts/section (section &optional faces-plist)
  "Builds a SECTION of a string parts list."
  (let* ((type     (mis/parts/type section))
         (prop     (mis/parts/property type section))
         (value    (mis/parts/value type section))
         (face-val (plist-get faces-plist prop)))

    ;; (message (concat "type: '%S', prop: '%S', value: '%S'"
    ;;                  ", face: '%S'... <-section: %S")
    ;;          type prop value face-val
    ;;          section)

    ;; it'll be a string...
    (apply #'mis/parts/propertize
           ;; either now...
           (if (stringp value)
               (list value prop faces-plist)
             ;; ...or after going deeper.
             (list
              (mis/parts/build value
                               faces-plist)
              prop
              faces-plist)))))


;;------------------------------------------------------------------------------
;; Main Entry Point?
;;------------------------------------------------------------------------------

(defun mis/parts/build (arg &optional faces-plist)
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
        (error "Cannot build string from arg: '%S'." arg))

    ;; Process and build each item in section. Importantly, build off of
    ;; processed input, not raw. Need to allow expansion of symbols into big
    ;; sections. >.>
    (let ((accum nil)
          (input (mis/parts/process arg)))
      ;; push to accum until no more sections
      (while input
        ;; process first section of input
        (push (mis/parts/section input
                                 faces-plist)
              accum)

        ;; update loop conditional
        (setq input (mis/parts/next input)))

      ;; put 'em back in correct order and stringify 'em.
      (mapconcat #'identity (nreverse accum) ""))))
;; (mis/parts/build '(:border "||" :padding "----"))
;; (mis/parts/build '(:prefix
;;                    (:indent "" :border "||" :padding "--")
;;                    :center
;;                    (:text "xx")
;;                    :postfix
;;                    (:padding "==" :border "!!")))
;; (mis/parts/build :string-empty)
;; (mis/parts/build :string-full)
;; (mis/parts/build "hello")
;; (mis/parts/build '(:text "Hello, %s %s %s" "a" "b" "c!"))
;; (mis/parts/build '(:indent "  " :border "++" :padding "=="))
;; (mis/parts/build '(:john (:title "Test?") :text (:title " %s %s %s" "a" "b" "c!")))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-parts)
