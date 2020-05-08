;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------------mis the builder--------------------------------
;;--               Build :mis2//contents into :mis2//message.                 --
;;------------------------------------------------------------------------------

(require 'dash)
(require 's)

(require 'mis2-utils)
(require 'mis2-themes)
(require 'mis2-settings)


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defconst mis2//align/keys '(:center :right :left :skip)
  "Alignment keywords. Will be in the :mis2//style list if specified. Defaults
to left-aligned.")


(defconst mis2//line/keys '(:indent :padding)
  "Line-related keywords. Will be in the :mis2//line list if specified.")


(defconst mis2//box/keys '(:margins :borders :padding :skip)
  "Box-related keywords. Will be in the :mis2//box list if specified.")


(defconst mis2//format/keys '(:column)
  "Format-related keywords. Will be in the :mis2//format list if specified.
Not included: the base `:format' key.")


(defconst mis2//line/full-lines
  '((:newline "")  ;; "\n"
    (:empty   "")  ;; "\n"
    (:full    ?-)) ;; "<line-width padded with something like '-'>\n"
  "Alist of keywords for entire lines and data for help building them.")


;;------------------------------------------------------------------------------
;; Build... boss?
;;------------------------------------------------------------------------------

(defun mis2//contents (plist)
  "Build mis2 PLIST contents into a propertized message ready for output.

Pipeline for sink of:
  - all style keys.
"
  ;; Error out if bad inputs.
  (if (not (mis2//data/plist? plist))
      (error "mis2//contents: not a mis2 plist: %S" plist)
    (if-let ((contents (mis2//data/get :mis2//contents plist)))

        ;; Call function to do actual building. We just check and pass the buck.
        (plist-put plist :mis2//message
                   (mis2//contents/build plist))

      (error "mis2//contents: `:mis2//contents' key not in plist: %S" plist))))


;;------------------------------------------------------------------------------
;; Build... supervisor?
;;------------------------------------------------------------------------------

(defun mis2//contents/build (plist)
  "Build :mis2//contents from PLIST into a final output message suitable for
putting into the data as :mis2//message.

Final sink for settings:
  :face

Pipeline for sink of:
  - all other style keys.
"
  ;; Style and contents for this level of the message. Could have more levels of
  ;; style/content to recurse into? ...but this is what we're dealing with now.
  (-let* (;; Contents from plist.
          (contents (plist-get plist :mis2//contents))
          ;; Prep: Pull any style overrides out of contents now.
          ((overrides contents) (mis2//style/overrides contents))
          ;; Prep Work: Convert contents to a string.
          (string (mis2//contents/section contents plist overrides)))

    ;; (message "mis2//contents/build: contents: %S, overrides: %S -> string: %S"
    ;;          contents overrides string)

    ;; Prep Work: Boxing: Deal with any box styling.
    (mis2//contents/box/parts string plist overrides)

    (-> string
        ;; Prep/String Work: Alignment.
        (mis2//contents/align plist overrides)

        ;; Penultimate:
        ;;   (Reserved for the ultimate pen.)

        ;; Ultimately: Build parts into propertized strings and combine into
        ;; final output string.
        (mis2//contents/finalize plist))))


(defun mis2//contents/finalize (string plist)
  "Build final, propertized string from input STRING and PLIST string
parts (:mis2//line, :mis2//box, etc).
"
  ;; Backwardsly (so no nreverse (last (left-most) first)), build list of
  ;; strings for box parts, line parts, string, etc. then concat together.

  ;; (message "mis2//contents/finalize: string: %S" string)

  ;; A boxed line is, in backwardsly:
  ;;   - Box, Right:
  ;;     - margin, right
  ;;     - border, right
  ;;     - padding, right
  ;;     - (possible auto-padding inserted, depending on string, to get to
  ;;        box edge when left-aligned)
  ;;   - String
  ;;   - Box, Left:
  ;;     - (possible auto-padding char inserted)
  ;;     - padding, left
  ;;     - border, left
  ;;     - margin, left
  ;;   - Indentation, Leading
  (let (deredro-postfix
        deredro-prefix) ;; ordered stack, backwardsly

    ;;-------------
    ;; Box Parts
    ;;-------------
    (-setq (deredro-prefix deredro-postfix)
      (mis2//contents/box/finalize string
                                   plist
                                   deredro-prefix
                                   deredro-postfix))
    ;; (message "%1$s %2$s: pre:  %4$S\n%3$s %2$s: post: %5$S"
    ;;          "mis2//contents/finalize"
    ;;          "boxed"
    ;;          (make-string (length "mis2//contents/finalize") ?\s)
    ;;          ;; print args 4+:
    ;;          deredro-prefix deredro-postfix)

    ;;-------------
    ;; Indent Parts
    ;;-------------
    (-setq (deredro-prefix deredro-postfix)
      (mis2//contents/indent/finalize string
                                      plist
                                      deredro-prefix
                                      deredro-postfix))
    ;; (message "%1$s %2$s: pre:  %4$S\n%3$s %2$s: post: %5$S\n%3$s %2$s: string: %6$S"
    ;;          "mis2//contents/finalize"
    ;;          "indented"
    ;;          (make-string (length "mis2//contents/finalize") ?\s)
    ;;          ;; print args 4+:
    ;;          deredro-prefix deredro-postfix string)

    ;;-------------
    ;; Finalized
    ;;-------------

    ;; Finish the full thing.
    ;; Parts are propertized properly; can just concatenate carelessly.
    (concat
     (apply #'concat deredro-prefix)
     string
     (apply #'concat deredro-postfix))))


(defun mis2//contents/box/finalize (string plist stack-prefixes stack-postfixes)
  "Build final, propertized box parts from STRING and PLIST string
parts (:mis2//line, :mis2//box, etc).

Pushes to the stacks based on `:margins', `:borders', and `:padding'
in `:mis2//box'.

Currently only capable of free-hand ASCII style boxes like:
  +----+   +----+   ;;----;;
  + hi +   | hi |   ;;-hi-;;
  +----+   +----+   ;;----;;

Currently not capable of figuring out how to interconnect the
ASCII box drawing characters on its own - caller would have to
figure it out on their own. So this can be done, you just have to
do the hard work yourself:
  ┌┬┬┬──────┬┬┬┐
  ├┼┼┤  hi  ├┼┼┤
  └┴┴┴──────┴┴┴┘

Pushes parts into STACK-PREFIXES and STACK-PREFIXES 'ordered'
stacks (last-first). Returns (stack-prefixes stack-postfixes).
"
  ;; Our box, if we have one, has been built in parts in `:mis2//box'
  ;; in PLIST.
  (if-let ((box (mis2//data/get :mis2//box plist)))
      (let* ((margins (plist-get box :margins))
             (borders (plist-get box :borders))
             (padding (plist-get box :padding)))

        ;; (message "mis2//contents/box/finalize: boxing: m: %S, b: %S, p: %S"
        ;;          margins borders padding)

        ;; Propertize this stuff as each individual piece gets made:
        ;; (mis2//contents/propertize str-in-question :margins/whatever plist)

        ;;-----
        ;; - margin, right
        ;; - border, right
        ;;---
        (push (mis2//contents/propertize (mis2//second margins)
                                         :margins plist)
              stack-postfixes)
        (push (mis2//contents/propertize (mis2//second borders)
                                         :borders plist)
              stack-postfixes)
        ;;-----
        ;; (message "mis2//contents/box/finalize: margins & borders, right")
        ;; (message "%1$s %2$s: pre:  %4$S\n%3$s %2$s: post: %5$S"
        ;;          "mis2//contents/box/finalize"
        ;;          "00"
        ;;          (make-string (length "mis2//contents/box/finalize") ?\s)
        ;;          ;; print args 4+:
        ;;          stack-prefixes stack-postfixes)

        ;;-----
        ;; - padding, right
        ;; - (possible auto-padding inserted, depending on string, to get to
        ;;    box edge when left-aligned)
        ;;---
        ;; Add fixed-size padding elements to final pieces.
        (when padding
          (let (pad-left-adj-char pad-right-adj-char
                                  ;; static pad strings still need added to output
                                  ;; if they are non-nil
                                  pad-left-static   pad-right-static)
            ;; (message "%s: paddings start: %S (1st: %S (char? %S))"
            ;;          "mis2//contents/box/finalize"
            ;;          padding
            ;;          (mis2//first padding)
            ;;          (characterp (mis2//first padding)))

            ;;---
            ;; Inner or outer fixed padding?
            ;;---
            ;; padding: (char string string char)
            (if (characterp (mis2//first padding))
                (progn
                  ;; Adjustable padding is on outsides.

                  ;; Right comes first because backwardsly.
                  (setq pad-right-adj-char (mis2//fourth padding))
                  ;; Right static has to be saved (adjustable has to be
                  ;; pushed in first).
                  (setq pad-right-static (mis2//contents/propertize
                                          (mis2//third padding)
                                          :padding plist))

                  ;; Left paddings. Static can be pushed now since it
                  ;; comes last.
                  (push (mis2//contents/propertize (mis2//second padding)
                                                   :padding plist)
                        stack-prefixes)
                  (setq pad-left-adj-char (mis2//first padding)))

              ;; Else:
              ;;   padding: (string char char string)

              ;; Right comes first because backwardsly.
              ;; Right static can be pushed now as it comes after adjustable.
              (push (mis2//contents/propertize (mis2//fourth padding)
                                               :padding plist)
                    stack-postfixes)
              ;; Adjustable padding is on insides.
              (setq pad-right-adj-char (mis2//third padding))

              ;; Adjustable padding is on insides.
              (setq pad-left-adj-char (mis2//second padding))
              ;; Static padding. Save for after adjustable
              (setq pad-left-static (mis2//contents/propertize
                                     (mis2//first padding)
                                     :padding plist)))
            ;; (message "%s: padding (types, static outsides)"
            ;;          "mis2//contents/box/finalize")
            ;; (message "%1$s %2$s: pre:  %4$S\n%3$s %2$s: post: %5$S"
            ;;          "mis2//contents/box/finalize"
            ;;          "01"
            ;;          (make-string (length "mis2//contents/box/finalize") ?\s)
            ;;          ;; print args 4+:
            ;;          stack-prefixes stack-postfixes)

            ;;-----
            ;; Padding parse done; figure out adjustable padding strings.
            ;;-----
            (when (or pad-right-adj-char pad-left-adj-char)
              (let* ((width-line (mis2//contents/line/width plist))
                     (width-reserved (mis2//contents/line/reserved-amount
                                      plist))
                     (width-remaining (- width-line
                                         (mis2//first width-reserved)
                                         (mis2//string/length-safe string)
                                         (mis2//second width-reserved)))
                     pad-left-str pad-right-str)
                ;; Static/fixed string parts are all accounted for. Create our
                ;; dynamic padding strings and then build final output.

                ;; Ensure minimal padding on left & right.
                (cond ((= width-remaining 1)
                       ;; Only one char to work with; put it on the left?
                       (setq pad-left-str (make-string 1 pad-left-adj-char)
                             pad-right-str nil))

                      ((> width-remaining 1)
                       ;; Space to work in; put one on left and rest on
                       ;; right-pad duty.
                       (setq pad-left-str (make-string 1 pad-left-adj-char)
                             pad-right-str (make-string (1- width-remaining)
                                                        pad-left-adj-char)))

                      (t  ;; Zero or negative - no more padding available.
                       (setq pad-left-str nil
                             pad-right-str nil)))

                ;; Push into their final resting places...
                (when pad-left-str
                  (push (mis2//contents/propertize pad-left-str
                                                   :padding plist)
                        stack-prefixes))
                (when pad-right-str
                  (push (mis2//contents/propertize pad-right-str
                                                   :padding plist)
                        stack-postfixes))))

            ;;---
            ;; Need to push static paddings onto lists now?
            ;;---
            (when pad-right-static
              (push (mis2//contents/propertize pad-right-static
                                               :padding plist)
                    stack-postfixes))
            (when pad-left-static
              (push (mis2//contents/propertize pad-left-static
                                               :padding plist)
                    stack-prefixes))
            ;; (message "%s: padding (adjustable & static)"
            ;;          "mis2//contents/box/finalize")
            ;; (message "%1$s %2$s: pre:  %4$S\n%3$s %2$s: post: %5$S"
            ;;          "mis2//contents/box/finalize"
            ;;          "03"
            ;;          (make-string (length "mis2//contents/box/finalize") ?\s)
            ;;          ;; print args 4+:
            ;;          stack-prefixes stack-postfixes)
            ))

        ;;-----
        ;; - border, left
        ;; - margin, left
        ;;---
        (push  (mis2//contents/propertize (mis2//first borders)
                                          :borders plist)
               stack-prefixes)
        (push  (mis2//contents/propertize (mis2//first margins)
                                          :margins plist)
               stack-prefixes)
        ;;-----
        ;; (message "mis2//contents/box/finalize: borders & margin, left")
        ;; (message "%1$s %2$s: pre:  %4$S\n%3$s %2$s: post: %5$S"
        ;;          "mis2//contents/box/finalize"
        ;;          "04"
        ;;          (make-string (length "mis2//contents/box/finalize") ?\s)
        ;;          ;; print args 4+:
        ;;          stack-prefixes stack-postfixes)
        ))

  (list stack-prefixes stack-postfixes))


(defun mis2//contents/indent/finalize (string plist stack-prefixes stack-postfixes)
  "Build final, propertized indent parts from STRING and PLIST string
parts (:mis2//line, :mis2//box, etc).

Pushes parts into STACK-PREFIXES and STACK-PREFIXES 'ordered'
stacks (last-first). Returns (stack-prefixes stack-postfixes).
"
  (if-let ((indent (plist-get (plist-get plist :mis2//line) :indent)))
      (push (mis2//contents/propertize indent
                                       :indent plist)
            stack-prefixes))
  ;; (message "mis2//contents/indent/finalize: indent")
  ;; (message "%1$s %2$s: pre:  %4$S\n%3$s %2$s: post: %5$S"
  ;;          "mis2//contents/indent/finalize"
  ;;          "05"
  ;;          (make-string (length "mis2//contents/indent/finalize") ?\s)
  ;;          ;; print args 4+:
  ;;          stack-prefixes stack-postfixes)

  (list stack-prefixes stack-postfixes))


;;------------------------------------------------------------------------------
;; Content Sections
;;------------------------------------------------------------------------------

(defun mis2//contents/section/type (contents)
  "Determines what section type the CONTENTS are:
  Basic sections:
    - :mis2//section/text   - Just a string,
                              a format string and some data,
                              or just some data.
    - :mis2//section/format - Contents start with :format keyword.
    - :mis2//section/line   - Content is just a
                              `mis2//line/full-lines' keyword.

  Complex sections:
    - :mis2//section/div    - A division with style overrides.
                              It will have contents of its own with their
                              own content section.
    - :mis2//section/multi  - Contents is more than one content section.
                              Format each on its own, concat, and return.
"
  (let ((check (mis2//first contents)))
    (cond
     ;; format: easy - it'll have `:format' as the check.
     ((and (keywordp check) (eq check :format))
      :mis2//section/format)

     ;; line: also easy - line section will be the check.
     ((and (keywordp check)
           (alist-get check mis2//line/full-lines))
      :mis2//section/line)

     ;; div: we need to look at check and see if it's a style keyword
     ((and (keywordp check)
           (alist-get check mis2/style/keys))
      :mis2//section/div)

     ;; text: Either we have just a string, format string and stuff, or we just
     ;; have one thing to print. Use 'contents' instead of 'check' for
     ;; just-a-string.
     ((or (stringp contents) ;; 1) Just a string.
          ;; 2) Or list with format string as first element.
          (and (mis2//list-exists? contents)
               (stringp check)
               (s-contains? "%" check))
          ;; 3) Just one thing.
          (mis2//length-safe/= contents 1))
      :mis2//section/text)

     ;; multi: If it's not the rest of the sections, assume it's
     ;; some combination?
     (t
      ;; (message "mis2//contents/section/type: failed text check. string?(contents) %S, string?(check) %S, list? %S, len: %S, len==1: %S, has'%%'? %S => %S"
      ;;          (stringp contents)
      ;;          (stringp check)
      ;;          (mis2//list-exists? contents)
      ;;          (mis2//length-safe contents)
      ;;          (mis2//length-safe/= contents 1)
      ;;          (s-contains? "%" check)
      ;;          (or (stringp contents) ;; 1) Just a string.
      ;;              ;; 2) Or list with format string as first element.
      ;;              (and (mis2//list-exists? contents)
      ;;                   (stringp check)
      ;;                   (s-contains? "%" check))
      ;;              ;; 3) Just one thing.
      ;;              (mis2//length-safe/= contents 1)))

      :mis2//section/multi))))


(defun mis2//contents/section (contents plist &optional overrides)
  "Determines what section type the CONTENTS are and builds the
propertized output string from it using PLIST and OVERRIDES.
"
  (let ((section (mis2//contents/section/type contents)))
    ;; (message "mis2//contents/section: type: %S, contents: %S, plist: %S"
    ;;          section contents plist)
    (setq plist (plist-put plist :mis2//section section))
    (cond ((eq section :mis2//section/text)
           (mis2//contents/section/text contents plist overrides))

          ((eq section :mis2//section/format)
           (mis2//contents/section/format contents plist overrides))

          ((eq section :mis2//section/line)
           (mis2//contents/section/line contents plist overrides))

          ((eq section :mis2//section/div)
           (mis2//contents/section/div contents plist overrides))

          ((eq section :mis2//section/multi)
           (mis2//contents/section/multi contents plist overrides)))))


(defun mis2//contents/section/multi (contents plist &optional overrides)
  "Builds CONTENTS of section `:mis2//section/multi' into a
propertized string based on settings/style in PLIST and
CONTENTS.

CONTENTS should be:
  - strings
  - lists of contents (of any section type)

E.g. this is a :mis2//section/multi message:
  (mis2/message :settings settings :style style
                \"Unbelieveable! You, \"
                (:face :highlight \"SUBJECT NAME HERE\")
                \", must be the pride of \"
                (:face :title \"SUBJECT HOMETOWN HERE\")
                \".\")
"
  (let (accum)
    (dolist (section contents)
      ;; (message "mis2//contents/section/multi: section: %S, contents: %S"
      ;;          section contents)
      ;; Recurse back up to our parent to process this section.
      (push (mis2//contents/section section plist overrides) accum))

    ;; Done with sub-sections - put it all together.
    (apply #'concat (nreverse accum))))


(defun mis2//contents/section/text (contents plist &optional overrides)
  "Builds CONTENTS of section `:mis2//section/text' into a
propertized string based on settings/style in PLIST and
CONTENTS.

CONTENTS should just be:
  - a string
  - a formatting string and its arguments
  - one argument to be formatted by \"%s\"
"
  ;; (message "mis2//contents/section/text: %S => %S"
  ;;          contents (mis2//format/text contents plist))
  (mis2//format/text contents plist))


(defun mis2//contents/section/line (contents plist &optional overrides)
  "Builds CONTENTS of section `:mis2//section/div' into a
propertized string based on settings/style in PLIST and
CONTENTS (which should just be instructions for building a
certain type of line).

CONTENTS should jus be a line type. Will deal with all the
different line types; see `mis2//line/full-lines'.
"
  (let* ((line-type (mis2//first contents))
         (build-data (alist-get line-type mis2//line/full-lines)))

    ;; (message "mis2//contents/line: type: %S, data: %S" line-type build-data)

    ;; These mean we'll be printing out nothing.
    (cond
     ((or (eq line-type :newline)
          (eq line-type :empty))
      ;; Skip alignment and boxing parts for these line types;
      ;; just want empty.
      (mis2//contents/box/update plist :skip t)
      (mis2//contents/align/update plist :skip t)

      (mis2//contents/propertize (mis2//first build-data) :line plist))

     ;; "-" or something padded out to line length.
     ((eq line-type :full)
      ;; Skip alignment part for full lines;
      ;; they should be too full to be aligned...
      (mis2//contents/align/update plist :skip t)

      ;; (message "mis2//contents/line: full. reserved: %S (sum: %S), build-data: %S (first: %S), string: %S"
      ;;          (mis2//contents/line/reserved-peek plist overrides)
      ;;          (-sum (mis2//contents/line/reserved-peek plist overrides))
      ;;          build-data
      ;;          (mis2//first build-data)
      ;;          (make-string (- (mis2//contents/line/width plist)
      ;;                          (-sum (mis2//contents/line/reserved-peek plist overrides)))
      ;;                       (mis2//first build-data)))

      ;; Make full line (with room for reserved amount).
      (mis2//contents/propertize
       (make-string (- (mis2//contents/line/width plist)
                       (-sum (mis2//contents/line/reserved-peek plist overrides)))
                    (mis2//first build-data))
       :line plist))

     (t
      (error "mis2//contents/line: unknown line type '%S' for contents: %S"
             line-type contents)))))


(defun mis2//contents/section/format (contents plist &optional overrides)
  "Builds CONTENTS of section `:mis2//section/format' into a
propertized string based on settings/style in PLIST and
CONTENTS.

CONTENTS should be:
  - A list starting with :format <format-type>. E.g.:
    '(:format :each ...)
"
  ;; Get the type keyword and then drop both :format and type keywords from
  ;; front of contents.
  (-let* (((format type . contents) contents)
          (plist-fmt (list format type))
          (i 0)
          (len (mis2//length-safe contents))
          element
          value)

    ;; Look for other formatting options like ":column :auto".
    (while (< i len)
      (setq element (mis2//nth i contents))

      ;; We require our keys to go first, so just check the elements
      ;; until not our key.
      (if (memq element mis2//format/keys)
          (progn
            ;; Get keyword and val; save to plist-fmt.
            ;; Keyword is element (and verified);
            ;; just need to get value.
            (setq value (mis2//nth (1+ i) contents))
            (setq plist-fmt (plist-put plist-fmt element value))

            ;; update the loop var for this key and value
            (setq i (+ i 2)))

        ;; Else we're past the style overrides and the rest is the contents now.
        (setq contents (-drop i contents)
              i len))) ;; Set index past end of contents so loop will end.

    (mis2//data/update plist :mis2//format plist-fmt)

    (mis2//format/by-format-type type
                                 contents
                                 plist)))


(defun mis2//contents/section/div (contents plist &optional overrides)
  "Builds CONTENTS of section `:mis2//section/div' into a
propertized string based on settings/style in PLIST and
CONTENTS.

CONTENTS should be:
  - A list starting with a mis2 style keyword. E.g.:
    '(:face :title ...)
"
  (mis2//format/div contents plist))


;;------------------------------------------------------------------------------
;; Content, uh... minions.
;;------------------------------------------------------------------------------
(defun mis2//contents/propertize (string type plist &optional style-overrides)
  "Adds a 'face property to STRING from PLIST given content TYPE.
If no appropriate face for TYPE found in PLIST, return string as-is.

Faces are in PLIST in `:mis2//style' value, in `:faces' value, in TYPE value.
So we're... 3 plists deep.
  E.g.: '(:mis2//settings (...)
          ...
          :mis2//style (...
                        :faces (:text    'face-name-0
                                :padding 'face-name-1
                                ...)))

STYLE-OVERRIDES is a secondary :mis2//style plist. Any styles in
STYLE-OVERRIDES will be favored over styles in PLIST.

Final sink for:
  :mis2//settings -- :theme
  :mis2//style    -- :faces
"
  ;; (message "prop: %S %S %S &o %S" string type plist style-overrides)
  ;; (message "prop: (or emface override: %S, emface type: %S) -> %S"
  ;;          (mis2//themes/emface/from-style style-overrides plist)
  ;;          (mis2//themes/emface type plist)
  ;;          (or (mis2//themes/emface/from-style style-overrides plist)
  ;;                       (mis2//themes/emface type plist)))

  ;; Propertize it if we have a string and find a face.
  (if-let* ((string string) ;; null check
            ;; Actual emacs face by way of override, type, theme, etc...
            ;; Favor override over plist.
            (emface (or (mis2//themes/emface/from-style style-overrides plist)
                        (mis2//themes/emface type plist))))

      ;; Found a defined face, so set `face' property of string to our
      ;; face and return that.
      (propertize string 'face emface)

    ;; No face; return the unchanged string.
    string))


(defun mis2//format/by-format-type (type contents plist)
  "Format a sub-section of the contents list (CONTENTS) by format TYPE (e.g.
`:each' for formatting a results accum).
"
  (cond ((eq type :each)
         ;; `:each' implies CONTENTS is in this layout:
         ;;   '(format-list contents-list)
         (mis2//format/each (mis2//first contents)
                            (mis2//second contents)
                            plist))

        (t
         (error "%S: unknown formatting type %S. contents: %S"
                "mis2//format/by-format-type"
                type contents))))


(defun mis2//format/div (contents plist)
  "Format a sub-section of the contents list (CONTENTS).
"
  (-let [(style-overrides contents) (mis2//style/overrides contents)]

    ;; (message "mis2//format/div: overrides: %S, string: %S"
    ;;          style-overrides
    ;;          (mis2//format/string contents))
    (mis2//contents/propertize (mis2//format/string contents)
                               :text plist style-overrides)))


(defun mis2//format/each (formats inputs plist)
  "Formats INPUTS list with FORMATS list overrides and mis2 PLIST.

FORMATS must mirror layout of an INPUTS element/item. E.g.
   INPUTS:  '((\"/path/to/test0\" \"a.txt, b.txt, c.txt\")
             (\"/path/to/test1\" \"b.jpg, o.jpg, o.png\")))
   FORMATS: '((:face :highlight \"%s: \") (:face :title))
 - INPUTS is a list of 2-tuple lists.
 - FORMATS is a 2-tuple list.

Each INPUTS element will be effectively appended to its place in a
FORMATS element, so for this example FORMATS and INPUTS are effectively:
  '((:face :highlight \"%s: \" \"/path/to/test0\")
    (:face :title              \"a.txt, b.txt, c.txt\")
    (:face :highlight \"%s: \" \"/path/to/test1\")
    (:face :title              \"b.jpg, o.jpg, o.png\")))

Which will then be processed down to a propertized string and returned.
"
  ;; (message "mis2//format/each: formats: %S, inputs: %S, plist: %S"
  ;;          formats inputs plist)

  ;; (message "mis2//format/each: formats: %S, inputs: %S" formats inputs)

  ;;---
  ;; Style the Substance:
  ;;   Format each row of the inputs to user's formats.
  ;;---
  (let (each-input
        style
        substance
        accum
        ;; `accum' is just-a-list, but inputs are in tuples like:
        ;;   2-tuple inputs = '(("abc" "xyz") ...)
        ;; `column-stride' will be 2 in that case, for knowing how to stride
        ;; across same-entries-same-elements in `accum'.
        (column-stride (mis2//length-safe (mis2//first inputs))))
    ;; Loop over our inputs...
    (dolist (each-input inputs)
      ;; get our input element and it's mate from the formats.
      (dotimes (each-i (mis2//length-safe each-input))
        (setq style     (mis2//nth each-i formats)
              substance (mis2//nth each-i each-input))
        ;; (message "mis2//format/each: style: %S, substance: %S" style substance)

        ;; And now we can combine style & substance into a list to pass on to be
        ;; formatted as a div of these contents.
        (push (mis2//format/div (-snoc style substance) plist)
              accum)))

    ;;---
    ;; Robo-Stylist:
    ;;   Check for more formatting options.
    ;; NOTE: OUTPUT (`accum') IS BACKWARDS!
    ;;---
    (let ((column-fmt (mis2//data/get/from-data :column
                                                :mis2//format
                                                plist mis2//format/keys))
          column-max
          column-index)
      (when (eq column-fmt :auto)
        ;; Walk styled output list, gathering data about it.
        ;; NOTE: IT IS BACKWARDS RIGHT NOW!
        (dotimes (i (mis2//length-safe accum))
          (setq column-index (% i column-stride))
          ;; When not at the "end" of a column (which is 0 now because backwards
          ;; accum), gather formatting info.
          (when (not (= column-index 0))
            (let ((cell (mis2//nth i accum))
                  (cell-max (alist-get column-index column-max 0 nil #'=)))
              ;; Set the generalized variable to the new max.
              ;; I.e. set the new max into alist.
              (setf (alist-get column-index column-max 0 nil #'=)
                    (max cell-max (mis2//length-safe cell))))))

        ;; Auto-format styled output list.
        ;; NOTE: STILL BACKWARDS!
        (let (auto-accum
              min-length)
          (dotimes (i (mis2//length-safe accum))
            (setq column-index (% i column-stride))
            (if (= column-index 0)
                ;; End-of-row cell; just insert as-is.
                (push (mis2//nth i accum) auto-accum)

              ;; Else, try to pad before pushing.
              (setq min-length (alist-get column-index column-max 0 nil #'=))
              (push (s-pad-right min-length " " (mis2//nth i accum))
                    auto-accum)))
          (setq accum (nreverse auto-accum)))))

    ;; (message "mis2//format/each: accum: %S" accum)

    (apply #'concat (nreverse accum))))


(defun mis2//format/text (contents plist)
  "Builds a formatted string from CONTENTS (which is a list) and mis2 PLIST.

If the mis2 plist is:
 '(:mis2//style style :mis2//contents (\"Hello, %S\" \"World\"))

We should be passed CONTENTS of:
 '(\"Hello, %S\" \"World\")

Final sink for:
  :mis2//contents
"
  ;; (message "mis2//format/text: contents: %S => str: %S"
  ;;          contents
  ;;          (mis2//format/string contents))

  ;; propertize whatever we figure out the string is...
  (mis2//contents/propertize

   ;; We'll either format the thing into a string (whatever it is), or format
   ;; all the things with the assumption that the first one is a formatting
   ;; string.
   (mis2//format/string contents)

   ;; Propertize as text.
   :text plist))


;;------------------------------------------------------------------------------
;; String Helpers
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-04-13]: 'format' vs 'propertize' vs 'format and propertize'
;; naming scheme for funcs.
;; §-TODO-§ [2020-04-13]: 'elements' is meh.
(defun mis2//format/string (elements)
  "Builds a formatted string from elements (which is a list or single thing).
Does not propertize.
"
  ;; Just passed a string - return it.
  (cond ((stringp elements)
         elements)

        ;; Passed a list of length > 1. Assume first is a format string and rest
        ;; are formatting args.
        ((and (mis2//list-exists? elements)
              (> (mis2//length-safe elements) 1))
         (apply 'format elements))

        ;; Default - print out whatever it is formatted as "%s".
        (t
         (format "%s" (mis2//first elements)))))


;;------------------------------------------------------------------------------
;; Line Helpers
;;------------------------------------------------------------------------------


(defun mis2//contents/line/reserved-peek (plist overrides)
  "Look in PLIST for any settings/style/boxing/what-not that
would reduce the amount of characters available to be used for
filling.

This looks at user's style and tries to guess what the final
sizes of line/box parts will be. Use
`mis2//contents/line/reserved-amount' if those are already built.

E.g. if we're aligning to center on 80 columns, but need 10 for
indentation, margins, etc, then we should center the string on 80
still, but reserve 6 on the right and 4 on the left (to make up
numbers), so a 70 width string should be returned.

This function just says that those 6 and 4 are reserved.

Returns tuple of '(left-reserved-chars right-reserved-chars).
For our example: '(6 4)

Pass-through sink for:
  :mis2//line -- :indent
  :mis2//box  -- :margins
              -- :borders
              -- :padding
"
  ;; Get the final line and box data for reserved characters.
  (let ((indent-amt (or (mis2//style/get/from-data :indent  plist overrides)
                        0))
        (margins    (mis2//style/get/from-data :margins plist overrides))
        (borders    (mis2//style/get/from-data :borders plist overrides))
        ;; Either '(str str) or '(x y int):
        (padding    (or (mis2//style/get/from-data :padding plist overrides)
                        '(nil nil 0))))

    ;; Reserved tuple of (left right) amounts. Some of these are expected to be
    ;; nil, but `first', `second', and `length' all cope correctly with it.
    (list
     (+ indent-amt
        (mis2//string/length-safe (mis2//first margins))
        (mis2//string/length-safe (mis2//first borders))
        ;; Either '(str str) or '(x y int):
        (if (stringp (mis2//first padding))
            (mis2//string/length-safe (mis2//first padding))
          (mis2//third padding)))
     (+ (mis2//string/length-safe (mis2//second margins))
        (mis2//string/length-safe (mis2//second borders))
        ;; Either '(str str) or '(x y int):
        (if (stringp (mis2//first padding))
            (mis2//string/length-safe (mis2//second padding))
          (mis2//third padding))))))


(defun mis2//contents/line/reserved-amount (plist)
  "Look in PLIST for any settings/style/boxing/what-not that
would reduce the amount of characters available to be used by
alignment.

E.g. if we're aligning to center on 80 columns, but need 10 for
indentation, margins, etc, then we should center the string on 80
still, but reserve 6 on the right and 4 on the left (to make up
numbers), so a 70 width string should be returned.

This function just says that those 6 and 4 are reserved.

Returns tuple of '(left-reserved-chars right-reserved-chars).
For our example: '(6 4)

Pass-through sink for:
  :mis2//line -- :indent
  :mis2//box  -- :margins
              -- :borders
              -- :padding
"
  ;; Get the final line and box data for reserved characters.
  (let ((indent  (mis2//contents/line/get/from-data :indent  plist))
        (margins (mis2//contents/box/get/from-data  :margins plist))
        (borders (mis2//contents/box/get/from-data  :borders plist))
        (padding (mis2//contents/box/get/from-data  :padding plist)))

    ;; (message "mis2//contents/line/reserved-amount: indent: %S, margins: %S, borders: %S, padding: %S"
    ;;          indent margins borders padding)

    ;; Reserved tuple of (left right) amounts. Some of these are expected to be
    ;; nil, but `first', `second', and `length' all cope correctly with it.
    (list
     (+ (mis2//string/length-safe indent)
        (mis2//string/length-safe (mis2//first margins))
        (mis2//string/length-safe (mis2//first borders))
        (mis2//string/sum (mis2//first padding) (mis2//second padding)))
     (+ (mis2//string/length-safe (mis2//second margins))
        (mis2//string/length-safe (mis2//second borders))
        (mis2//string/sum (mis2//third padding) (mis2//fourth padding))))))


(defun mis2//contents/line/width (plist)
  "Returns either the :line-width explicit setting, or the `fill-column' for the
current buffer.
"
  (or (mis2//settings/get/from-data :line-width plist)
      ;; fill-column or (current-fill-column)?
      fill-column))


(defun mis2//contents/line/indent (string plist &optional overrides)
  "Makes indent string for STRING based on `:indent' style setting in PLIST.
"
  (when-let ((indent (mis2//style/get/from-data :indent plist overrides)))
    ;; Have indent - add that to front of str.
    (mis2//contents/line/update plist
                                :indent (make-string indent ?\s)))
  string)


;;---
;; Mis2 PLIST Getter/Setter
;;---

;; §-TODO-§ [2020-04-01]: use this in here
(defun mis2//contents/line/get/from-data (key plist)
  "Get line data from a mis2 data PLIST, then get KEY from that.
"
  (mis2//data/get/from-data key
                            :mis2//line plist
                            mis2//line/keys
                            nil
                            nil))


(defmacro mis2//contents/line/update (plist key value)
  "Given mis2 PLIST, get `:mis2//line' key, and update /that/ plist with
KEY & VALUE.
"
  `(let (;;(,temp-plist ,plist)
         ;; Get out our line plist from mis2 plist.
         (line (mis2//data/get :mis2//line ,plist)))
     ;; Update mis2 plist with newest line plist.
     (setq ,plist
           (plist-put ,plist
                      :mis2//line
                      ;; Add our key/value to line plist.
                      (plist-put line ,key ,value)))))


;;------------------------------------------------------------------------------
;;                                                                  Alignment!
;;------------------------------------------------------------------------------

(defun mis2//contents/align (string plist &optional overrides)
  "Given STRING and mis2 PLIST, and assuming there are both a
`:mis2//style' keyword in PLIST and an alignment keyword in the
style list, align the STRING based on line width.

Boxing and line data must be created already for align to calculate proper size
and position of the string in the line.
Requires / Passthrough sink for:
  :mis2//box
  :mis2//line

Final sink for:
  :mis2//style    -- :center
                  -- :left
                  -- :right

Shared 'final' sink for:
  :mis2//settings -- :line-width
"
  ;; (message "mis2//contents/align: input: %S" string)
  (if (mis2//contents/align/get/from-data :skip plist)
      ;; Bail out on boxing if we've been asked to skip.
      string

    ;; Look for width (optional) and an alignment keyword.
    (let ((align-left   (mis2//style/get/from-data :left   plist overrides))
          (align-center (mis2//style/get/from-data :center plist overrides))
          (align-right  (mis2//style/get/from-data :right  plist overrides))
          (line-width   (mis2//contents/line/width plist))
          ;; `line-width' is the full width desired - but we may not be allowed to
          ;; use all that. We might need to reserve some space for e.g. boxing to
          ;; draw characters before/after our aligned string.
          (reserved (mis2//contents/line/reserved-amount plist)))

      ;; Call a specific aligner or no-op.
      (cond (align-center
             (mis2//contents/align/center string line-width reserved plist))
            (align-right
             (mis2//contents/align/right string line-width reserved plist))
            (t
             ;; If we don't need to center or right align, then we're left
             ;; aligning; it's left aligned already, but may need to do more
             ;; anyways for e.g. boxing help.
             (mis2//contents/align/left string line-width reserved plist))))))


(defun mis2//contents/align/left (string width reserved plist)
  "Align STRING to the left of WIDTH. WIDTH should be what was specified in
:mis2//settings or the default for the buffer.

RESERVED should be returned value from `mis2//contents/line/reserved-amount'.
"
  ;; Left-align. Align only to left-most space we're allow in, so remove
  ;; left-side reserved amount first. While we're at it, remove right side
  ;; reserved too - won't affect looks to do the math first then build the
  ;; string (unlike aligning to center).
  (let ((len (- width
                (mis2//first reserved)
                (mis2//second reserved)
                (mis2//string/length-safe string))))

    ;; For left align, we do not build any padding string at this time - we may
    ;; not need one at all. We'll still put into the plist just how much padding
    ;; should be used.
    (mis2//contents/line/update plist :padding (list 0 len)))

  ;; ...And return unaltered string as it's already "aligned".
  ;; (message "mis2//contents/align/left: in/out: %S" string)
  string)


(defun mis2//contents/align/center (string width reserved plist)
  "Align STRING to center of WIDTH. WIDTH should be what was specified in
:mis2//settings or the default for the buffer.

RESERVED should be returned value from `mis2//contents/line/reserved-amount'.
"
  ;; Center on the full width for true center.
  (let* ((centered (s-center width string))
         (len (mis2//string/length-safe centered)))

    ;; (message "%1$s %2$s:    input: %4$S\n%3$s %2$s: centered: %5$S"
    ;;          "mis2//contents/align/center"
    ;;          ""
    ;;          (make-string (length "mis2//contents/align/center") ?\s)
    ;;          ;; print args 4+:
    ;;          string
    ;;          (substring centered
    ;;            (mis2//first reserved)
    ;;            (- len (mis2//second reserved))))

    ;; Chop down string starting after left-reserved chars, and ending before
    ;; right-reserved chars.
    (substring centered
               (mis2//first reserved)
               (- len (mis2//second reserved)))))


(defun mis2//contents/align/right (string width reserved plist)
  "Align STRING to the right of WIDTH. WIDTH should be what was specified in
:mis2//settings or the default for the buffer.

RESERVED should be returned value from `mis2//contents/line/reserved-amount'.
"
  ;; Right-align. Align only to right-most space we're allow in, so remove
  ;; right-side reserved amount first. While we're at it, remove left side
  ;; reserved too - won't affect looks to do the math first then build the
  ;; string (unlike aligning to center).
  (let ((len (- width
                (mis2//first reserved)
                (mis2//second reserved))))
    ;; Now all we have to do is pad out by our calculated length.
    (s-pad-left len " " string)))


;;---
;; Mis2 PLIST Getter/Setter
;;---

;; §-TODO-§ [2020-04-01]: use this in here
(defun mis2//contents/align/get/from-data (key plist)
  "Get align data from a mis2 data PLIST, then get KEY from that.
"
  (mis2//data/get/from-data key
                            :mis2//align plist
                            mis2//align/keys
                            nil
                            nil))


(defmacro mis2//contents/align/update (plist key value)
  "Given mis2 PLIST, get `:mis2//align' key, and update /that/ plist with
KEY & VALUE.
"
  `(let (;;(,temp-plist ,plist)
         ;; Get out our align plist from mis2 plist.
         (align (mis2//data/get :mis2//align ,plist)))
     ;; Update mis2 plist with newest align plist.
     (setq ,plist
           (plist-put ,plist
                      :mis2//align
                      ;; Add our key/value to align plist.
                      (plist-put align ,key ,value)))))


;;----------------------------------------------------------------------------;;
;;--                                 Boxes!                                 --;;
;;----------------------------------------------------------------------------;;

(defun mis2//contents/box/parts (string plist &optional overrides)
  "Takes a STRING, and assumes:
  - It is the minimal string buildable from contents (i.e. not aligned).
  - It has its box styling parts in PLIST..
  - It may have box stylings in OVERRIDES plist foo.

Creates the boxing parts for `mis2//contents/box/finalize' to complete later.
Puts the parts into PLIST under key `:mis2//box'.

Currently only capable of free-hand ASCII style boxes like:
  +----+   +----+   ;;----;;    ┌┬┬┬──────┬┬┬┐
  + hi +   | hi |   ;;-hi-;;    ├┼┼┤  hi  ├┼┼┤
  +----+   +----+   ;;----;;    └┴┴┴──────┴┴┴┘

Currently not capable of figuring out how to interconnect the box
drawing characters on its own - caller would have to figure it
out on their own.

Pipeline for sinks of:
  :mis2//style    -- :margins
                  -- :borders
                  -- :padding
  :mis2//settings -- :line-width

Pipeline for sources of:
  :mis2//box -- :left
  :mis2//box -- :right
  :mis2//box -- :fill
  :mis2//box -- :pad
"
  ;; Bail out on boxing if we've been asked to skip.
  (unless (mis2//contents/box/get/from-data :skip plist)

    ;; A boxed line is, in order:
    ;;   - indentation
    ;;   - margin, left
    ;;   - border, left
    ;;   - padding, left
    ;;   - string
    ;;   - (possible auto-padding inserted, depending on string, to get to
    ;;      box edge when left-aligned)
    ;;   - padding, right
    ;;   - border, right
    ;;   - margin, right
    ;;
    ;; But we don't really care at this level - we just call each part and have
    ;; them build into the output key (:mis2//box) under the plist.

    (mis2//contents/box/padding string plist overrides)
    (mis2//contents/box/borders string plist overrides)
    (mis2//contents/box/margins string plist overrides)
    (mis2//contents/line/indent string plist overrides)))


(defun mis2//contents/box/padding (string plist &optional overrides)
  "Takes STRING and adds left/right padding (if defined in PLIST) to
`:mis2//box' in PLIST.

Final sink of:
  :mis2//style -- :padding

Pipeline for source of:
  :mis2//box   -- :pad

`:pad' is a 4-tuple of 2 strings and 2 characters.
Examples:
  '(\"---\" ?\s ?\s \"---\")
    - padding is three dashes on outside; fill inside before/after final aligned
      string with spaces.
  '(?\s \"---\" \"---\" ?\s)
    - padding is three dashes on inside; fill outside before/after final aligned
      (and dash-padded) string with spaces.

Strings can be asymmetrical.
"
  ;; They're optional, so only do it if we have 'em.
  (if-let ((padding (mis2//style/get/from-data :padding plist overrides)))
      ;; Padding is a weirder one as it can be a list of 2 string, or a list of
      ;; how to build the padding.
      (if (eq (mis2//style/check-value :padding padding
                                       '(:list (:string :string)))
              :*bad-value-error*)
          ;; Not the string/string list, so the other one.
          (mis2//contents/box/padding/build string padding plist)
        ;; The easy string/string path.
        (mis2//contents/box/padding/strings string padding plist))

    ;; No padding; no-op.
    string))


(defun mis2//contents/box/padding/strings (string padding plist)
  "Takes STRING and makes left/right PADDING for it. PADDING is
required to be a list of two strings - it is not verified by this
function. Adds results to `:padding' key in `:mis2//box' key in
PLIST.

A source of:
  :mis2//box -- :padding

`:padding' is a 4-tuple of 2 strings and 2 characters.
Examples:
  '(\"---\" ?\s ?\s \"---\")
    - padding is three dashes on outside; fill inside before/after final aligned
      string with spaces.
  '(?\s \"---\" \"---\" ?\s)
    - padding is three dashes on inside; fill outside before/after final aligned
      (and dash-padded) string with spaces.

Strings can be asymmetrical.
"
  ;; padding strings are always the outer pieces with inner fill of spaces.
  (mis2//contents/box/update plist
                             :padding
                             (list (mis2//first padding)
                                   ?\s
                                   ?\s
                                   (mis2//second padding))))


(defun mis2//contents/box/padding/build (string padding plist)
  "Takes STRING and makes left/right PADDING for it. PADDING is required to be a
list of three elements:
  (:char (:const (:empty :fill)) :integer)

That is:
  - a character to use
  - `:empty' or `:fill' indicator keyword
  - number of spaces on either side to either:
    - leave empty (fill rest w/ padding char)
    - fill with padding character (leave rest empty)

A source of:
  :mis2//box -- :padding

`:padding' is a 4-tuple of 2 strings and 2 characters.
Examples:
  '(\"---\" ?\s ?\s \"---\")
    - padding is three dashes on outside; fill inside before/after final aligned
      string with spaces.
  '(?\s \"---\" \"---\" ?\s)
    - padding is three dashes on inside; fill outside before/after final aligned
      (and dash-padded) string with spaces.

Strings can be asymmetrical.
"
  (cond
   ;; Given an amount to fill up.
   ((eq (mis2//second padding) :fill)
    (let ((pad (make-string (mis2//third padding) (mis2//first padding))))
      ;; Made outer pad based on fill char/amount. Inner pad will be
      ;; space characters.
      (mis2//contents/box/update plist
                                 :padding
                                 (list pad
                                       ?\s
                                       ?\s
                                       pad))))

   ;; Given an amount to leave empty.
   ((eq (mis2//second padding) :empty)
    (let ((pad (make-string (mis2//third padding) ?\s)))
      ;; Made inner pad of spaces based on fill amount. Inner pad will be
      ;; provided character.
      (mis2//contents/box/update plist
                                 :padding
                                 (list (mis2//first padding)
                                       pad
                                       pad
                                       (mis2//first padding)))))

   ;; Not sure... error out?
   (t
    (error "Unknown padding type: %S in padding data: %S"
           (mis2//second padding) padding))))


(defun mis2//contents/box/borders (string plist &optional overrides)
  "Takes STRING and adds left/right borders to it if defined in the mis2 PLIST.
"
  ;; They're optional, so only do it if we have 'em.
  (if-let ((borders (mis2//style/get/from-data :borders plist overrides)))
      ;; We have them - stuff 'em straight into the box.
      (mis2//contents/box/update plist
                                 :borders borders)))


(defun mis2//contents/box/margins (string plist &optional overrides)
  "If margins are defined in the mis2 PLIST, this takes STRING and margins
inputs and creates final margins in `:mis2//box' in PLIST.
"
  ;; They're optional, so only do it if we have 'em.
  (if-let ((margins (mis2//style/get/from-data :margins plist overrides)))
      ;; We have them - stuff 'em straight into the box.
      (mis2//contents/box/update plist
                                 :margins margins)))


;;---
;; Mis2 PLIST Getter/Setter
;;---

;; §-TODO-§ [2020-04-01]: use this in here
(defun mis2//contents/box/get/from-data (key plist)
  "Get box data from a mis2 data PLIST, then get KEY from that.
"
  (mis2//data/get/from-data key
                            :mis2//box plist
                            mis2//box/keys
                            nil
                            nil))


(defmacro mis2//contents/box/update (plist key value)
  "Given mis2 PLIST, get `:mis2//box' key, and update /that/ plist with
KEY & VALUE.
"
  `(let (;;(,temp-plist ,plist)
         ;; Get out our box plist from mis2 plist.
         (box (mis2//data/get :mis2//box ,plist)))
     ;; Update mis2 plist with newest box plist.
     (setq ,plist
           (plist-put ,plist
                      :mis2//box
                      ;; Add our key/value to box plist.
                      (plist-put box ,key ,value)))))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-contents)
