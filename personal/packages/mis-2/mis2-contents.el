;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------------mis the builder--------------------------------
;;--               Build :mis2//contents into :mis2//message.                 --
;;------------------------------------------------------------------------------


(require 'dash)
(require 's)

(require 'mis2-themes)
(require 'mis2-settings)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defconst mis2//align/keys '(:center :right :left)
  "Alignment keywords. Will be in the :mis2//style list if specified. Defaults
to left-aligned.")


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
  ;; style/content to recurse into, but this is what we're dealing with now.
  (let ((contents (plist-get plist :mis2//contents))
        message)

    ;; Second: Propertize the string with the styled face. If there is one.
    (setq message (mis2//contents/string/propertize
                   ;; First up: Build contents string.
                   (mis2//contents/string/build contents)))

    ;; Third thing: Format the string in the line? e.g. :center
    ;; §-TODO-§ [2020-03-25]: this
    ;; §-TODO-§ [2020-03-26]: thread together with 1st and 2st?

    ;; Fourth thing: Return it.
    message))


;;------------------------------------------------------------------------------
;; Build... specific parts of it. And be careful. The super and boss are here...
;;------------------------------------------------------------------------------

(defun mis2//contents/string/build (contents)
  "Builds a formatted string from CONTENTS (which is a list).

If the mis2 plist is:
 '(:mis2//style style :mis2//contents (\"Hello, %S\" \"World\"))

We should be passed CONTENTS of:
 '(\"Hello, %S\" \"World\")

Final sink for:
  :mis2//contents
"
  ;; We'll either format the thing into a string (whatever it is), or format
  ;; all the things with the assumption that the first one is a formatting
  ;; string.
  (if (not (and contents
                (listp contents)
                (> (length contents) 1)))
      ;; Simple case. Only one thing in contents (or nothing).
      ;; It is the message.
      (format "%s" (first contents))

    ;; Complex case. More than one thing! Oh no...
    ;; ...Well in that case assume the first thing is a string formatter.
    (apply 'format contents)
    ;; §-TODO-§ [2020-03-25]: More Complexer:
    ;;   - Split contents into parts we can do vs parts we recurse?
    ;;   - do each?
    ;;   - Concat results together.
    ))


(defun mis2//contents/string/propertize (string plist)
  "Given STRING and mis2 PLIST, and assuming there are both a `:mis2//style'
keyword in PLIST and a `:face' key in the style list, propertize the STRING
with the face.

Final sink for:
  :mis2//settings -- :theme
  :mis2//style    -- :face
"
  ;; Propertize it if we find a face. So let's look for that face. First, check
  ;; what theme we're using. Default to... :default if none supplied.
  (if-let* ((mis2-theme (or (mis2//settings/get/from-data :theme plist)
                            :default))
            ;; Get mis2/theme's faces based on our theme.
            (theme-faces (plist-get mis2/themes mis2-theme))
            ;; mis2's face's keyword e.g. :title, :highlight, :attention
            (mis2-face (mis2//style/get/from-data :face plist))
            ;; Get actual emacs face from the theme faces.
            (face-val (plist-get theme-faces mis2-face)))

      ;; Found a defined face, so set `face' property of string to our
      ;; face and return that.
      (propertize string 'face face-val)
    ;; No face; return the unchanged string.
    string))


;;------------------------------------------------------------------------------
;; Line helpers
;;------------------------------------------------------------------------------


(defun mis2//contents/line/reserved-amount (plist)
  "Look in PLIST for any settings/style that would reduce the
amount of characters available to be used by alignment.

E.g. if we're aligning to center on 80 columns, but need 10 for
indentation, margins, etc, then we should center the string on 80
still, but reserve 6 on the right and 4 on the left (to make up
numbers), so a 70 width string should be returned.

This function just says that those 6 and 4 are reserved.

Returns tuple of '(left-reserved-chars right-reserved-chars).
For our example: '(6 4)

Pass-through sink for style:
  :indent
  :margins
  :borders
"
  (let ((indent  (or (mis2//style/get/from-data :indent plist)
                     0))
        (margins (mis2//style/get/from-data :margins plist))
        (borders (mis2//style/get/from-data :borders plist)))

    ;; Reserved tuple of (left right) amounts.
    (list
     (+ indent
        (length (first  margins))
        (length (first  borders)))
     (+ (length (second margins))
        (length (second borders))))))


(defun mis2//contents/line/width (plist)
  "Returns either the :line-width explicit setting, or the `fill-column' for the
current buffer.
"
  (or (mis2//settings/get/from-data :line-width plist)
      ;; fill-column or (current-fill-column)?
      fill-column))


(defun mis2//contents/line/indent (string plist)
  "Makes indent string for STRING based on `:indent' style setting in PLIST.
"
  (when-let ((indent (mis2//style/get/from-data :indent plist)))
      ;; Have indent - add that to front of str.
      (mis2//contents/line/update plist
                                  :indent (make-string indent ?\s)))

  string)


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

(defun mis2//contents/align (string plist)
  "Given STRING and mis2 PLIST, and assuming there are both a
`:mis2//style' keyword in PLIST and an alignment keyword in the
style list, align the STRING based on line width.

Final sink for:
  :mis2//style    -- :center
                  -- :left
                  -- :right

Shared 'final' sink for:
  :mis2//settings -- :line-width
"
  ;; Look for width (optional) and an alignment keyword.
  (let ((align-center (mis2//style/get/from-data :center plist))
        (align-right  (mis2//style/get/from-data :right  plist))
        (line-width   (mis2//contents/line/width plist))
        ;; `line-width' is the full width desired - but we may not be allowed to
        ;; use all that. We might need to reserve some space for e.g. boxing to
        ;; draw characters before/after our aligned string.
        (reserved (mis2//contents/line/reserved-amount plist)))

    ;; Call a specific aligner or no-op.
    (cond (align-center
           (mis2//contents/align/center string line-width reserved))
          (align-right
           (mis2//contents/align/right string line-width reserved))
          (t
           ;; If we don't need to center or right align, we're done; it's left
           ;; aligned already.
           string))))


(defun mis2//contents/align/center (string width reserved)
  "Align STRING to center of WIDTH. WIDTH should be what was specified in
:mis2//settings or the default for the buffer.

RESERVED should be returned value from `mis2//contents/line/reserved-amount'.
"
  ;; Center on the full width for true center.
  (let* ((centered (s-center width string))
         (len (length centered)))

    ;; Chop down string starting after left-reserved chars, and ending before
    ;; right-reserved chars.
    (substring centered
               (first reserved)
               (- len (second reserved)))))


(defun mis2//contents/align/right (string width reserved)
  "Align STRING to the right of WIDTH. WIDTH should be what was specified in
:mis2//settings or the default for the buffer.

RESERVED should be returned value from `mis2//contents/line/reserved-amount'.
"
  ;; Right-align. Align only to right-most space we're allow in, so remove
  ;; right-side reserved amount first. While we're at it, remove left side
  ;; reserved too - won't affect looks to do the math first then build the
  ;; string (unlike aligning to center).
  (let ((len (- width
                (first reserved)
                (second reserved))))
    ;; Now all we have to do is pad out by our calculated length.
    (s-pad-left len " " string)))


;;----------------------------------------------------------------------------;;
;;--                                 Boxes!                                 --;;
;;----------------------------------------------------------------------------;;

(defun mis2//contents/box/parts (string plist)
  "Takes a STRING, and assumes:
  - It is the minimal string buildable from contents (i.e. not aligned).
  - It has its box parts in PLIST under key `:mis2//box'.

Creates the boxing parts for `mis2//contents/box/build' to complete later.

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

  ;; We'll follow that order inside-out, but ignore the right-side auto-padding.
  ;; Our padding step right now is just to build `:pad' and `:fill' for easy
  ;; completion of line later.

  (mis2//contents/box/padding string plist)
  (mis2//contents/box/borders string plist)
  (mis2//contents/box/margins string plist)
  (mis2//contents/line/indent string plist))


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


(defun mis2//contents/box/padding (string plist)
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
  (if-let ((padding (mis2//style/get/from-data :padding plist)))
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
                             (list (first padding)
                                   ?\s
                                   ?\s
                                   (second padding))))


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
   ((eq (second padding) :fill)
    (let ((pad (make-string (third padding) (first padding))))
      ;; Made outer pad based on fill char/amount. Inner pad will be
      ;; space characters.
      (mis2//contents/box/update plist
                                 :padding
                                 (list pad
                                       ?\s
                                       ?\s
                                       pad))))

   ;; Given an amount to leave empty.
   ((eq (second padding) :empty)
    (let ((pad (make-string (third padding) ?\s)))
      ;; Made inner pad of spaces based on fill amount. Inner pad will be
      ;; provided character.
      (mis2//contents/box/update plist
                                 :padding
                                 (list (first padding)
                                       pad
                                       pad
                                       (first padding)))))

   ;; Not sure... error out?
   (t
    (error "Unknown padding type: %S in padding data: %S"
           (second padding) padding))))


;; (defun mis2//contents/box/padding/trim (string left right)
;;   "Trim STRING by amount LEFT and RIGHT on each end.

;; Attempts to trim exactly as requested. If not enough whitespace
;; on left or right, will attempt to make up the difference on the
;; other side.

;; Returns: '(trimmed-string left-trimmed-amount right-trimmed-amount)
;; "
;;   ;; (s-match (rx (group string-start (repeat 0 7 whitespace))
;;   ;;              (+? printing)
;;   ;;              (group (repeat 0 7 whitespace) string-end))
;;   ;;          "    hello there   ")

;;   (let ((matches (s-match (rx
;;                            ;; first group:
;;                            ;;  - anchored at beginning-of-string
;;                            ;;  - take as many whitespaces as possible
;;                            ;;    up to total needed
;;                            (group string-start
;;                                   (repeat 0 (+ left right) whitespace))
;;                            ;; Middle should be... something, right?
;;                            (+? printing)
;;                            ;; first group:
;;                            ;;  - take as many whitespaces as possible
;;                            ;;    up to total needed
;;                            ;;  - anchored at end-of-string
;;                            (group (repeat 0 (+ left right) whitespace)
;;                                   string-end))
;;                           string))
;;         (max-left (length (second matches)))
;;         (max-right (length (third matches)))
;;         take-left
;;         take-right)

;;     ;; Now we know what's available to take from each side... Figure out what
;;     ;; we're actually going to take.
;;     (setq take-left (min max-left left))
;;     (setq take-right (min max-right right))
;;     ;; If we want more taken from left, but can't, and have more available on
;;     ;; right... Take it from there.
;;     (when (and (< take-left left)
;;                (> max-right take-right))
;;       (setq take-right
;;             (min
;;              ;; new desired
;;              (+ take-right (- left take-left))
;;              ;; or max we can do
;;              max-right)))
;;     ;; If we want more taken from right, but can't, and have more available on
;;     ;; left... Take it from there.
;;     (when (and (< take-right right)
;;                (> max-left take-left))
;;       (setq take-left
;;             (min
;;              ;; new desired
;;              (+ take-left (- right take-right))
;;              ;; or max we can do
;;              max-left)))

;;     ;; Now chop out our substring
;;     (substring string take-left (- (length string) take-right))))


(defun mis2//contents/box/borders (string plist)
  "Takes STRING and adds left/right borders to it if defined in the mis2 PLIST.
"
  ;; They're optional, so only do it if we have 'em.
  (if-let ((borders (mis2//style/get/from-data :borders plist)))
      ;; We have them - stuff 'em straight into the box.
      (mis2//contents/box/update plist
                                 :borders borders)))


(defun mis2//contents/box/margins (string plist)
  "If margins are defined in the mis2 PLIST, this takes STRING and margins
inputs and creates final margins in `:mis2//box' in PLIST.
"
  ;; They're optional, so only do it if we have 'em.
  (if-let ((margins (mis2//style/get/from-data :margins plist)))
      ;; We have them - stuff 'em straight into the box.
      (mis2//contents/box/update plist
                                 :margins margins)))


;; (defun mis2//contents/box/right-fill (string plist)
;;   "Takes STRING and mis2 PLIST. Adds right padding if needed to
;; get it to line-width sans margins & borders. Required mainly just
;; for left-aligned strings since we'll be putting a box's right
;; side on and need it to be located at the proper column.
;; "
;;   ;; need all this junk so I know how wide the inner padding/string section is.
;;   (let* ((indent     (mis2//style/get/from-data :indent  plist))
;;          (margins    (mis2//style/get/from-data :margins plist))
;;          (borders    (mis2//style/get/from-data :borders plist))
;;          (width-line (mis2//contents/line/width plist))
;;          (width-inner (- width-line
;;                          ;; avoid 'nil'
;;                          (or indent 0)
;;                          ;; reduce margins/borders down to their length
;;                          (-sum (mapcar #'length margins))
;;                          (-sum (mapcar #'length borders)))))

;;     (if (and string
;;              (< (length string) width-inner))
;;         ;; Need more - pad out string.
;;         (s-pad-right width-inner ?\s string)
;;       ;; Don't  need more - just return as-is.
;;       string)))


(defun mis2//contents/box/build (string plist)
  "Takes a STRING, and assumes it is already aligned properly (left, right, or
center). Builds around/in it on the one line based on `:margins', `:borders',
and `:padding'.

Builds the box around STRING on the one line based on boxing strings in PLIST.

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
"
  ;; A line is, in order:
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

  ;; Our box, if we have one, has been built in parts in `:mis2//box' in PLIST.
  (if-let ((box (mis2//data/get :mis2//box plist)))
      (let* ((indent     (plist-get (plist-get plist :mis2//line) :indent))
             (margins    (plist-get box :margins))
             (borders    (plist-get box :borders))
             (padding    (plist-get box :padding))
             prefix
             postfix
             pad-left-char
             pad-right-char
             pad-left-str
             pad-right-str
             (width-line (mis2//contents/line/width plist))
             width-remaining)

        (setq prefix (concat indent
                             (first margins)
                             (first borders)))

        (setq postfix (concat (second borders)
                              (second margins)))

        ;; Add fixed-size padding elements to final pieces.
        (when padding
          ;; padding: (char string string char)
          ;;  - concat to STRING, leave fill chars for final step
          (if (characterp (first padding))
              (progn
                (setq pad-left-char (first padding))
                (setq string (concat (second padding)
                                     string
                                     (third padding)))
                (setq pad-right-char (fourth padding)))

            ;; padding: (string char char string)
            ;;  - concat to prefix/postfix, leave fill chars for final step
            (setq prefix (concat prefix (first padding)))
            (setq pad-left-char (second padding))
            (setq pad-right-char (third padding))
            (setq postfix (concat (fourth padding) postfix))))

        (setq width-remaining (- width-line
                                 (-sum (mapcar #'length
                                               (list prefix string postfix)))))

        ;; Ensure minimal padding on left & right.
        (cond ((= width-remaining 1)
               ;; Only one char to work with; put it on the left?
               (setq pad-left-str (make-string 1 pad-left-char)
                     pad-right-str nil))

              ((> width-remaining 1)
               ;; Space to work in; put one on left and rest on right-pad duty.
               (setq pad-left-str (make-string 1 pad-left-char)
                     pad-right-str (make-string (1- width-remaining)
                                                pad-left-char)))

              (t  ;; zero or negative remaining - no more padding available.
               (setq pad-left-str nil
                     pad-right-str nil)))

        ;; Build full inner string. Only thing prefix is to fill in with
        ;; the pad characters.
        (concat prefix
                pad-left-str
                string
                pad-right-str
                postfix))))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-contents)
