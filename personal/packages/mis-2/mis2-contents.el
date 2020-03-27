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
    (setq message (mis2//contents/build/propertize
                   ;; First up: Build contents string.
                   (mis2//contents/build/string contents)))

    ;; Third thing: Format the string in the line? e.g. :center
    ;; §-TODO-§ [2020-03-25]: this
    ;; §-TODO-§ [2020-03-26]: thread together with 1st and 2st?

    ;; Fourth thing: Return it.
    message))


;;------------------------------------------------------------------------------
;; Build... specific parts of it. And be careful. The super and boss are here...
;;------------------------------------------------------------------------------

(defun mis2//contents/build/string (contents)
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


(defun mis2//contents/build/propertize (string plist)
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
;;                                                                  Alignment!
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


(defun mis2//contents/align (string plist)
  "Given STRING and mis2 PLIST, and assuming there are both a
`:mis2//style' keyword in PLIST and an alignment keyword in the
style list, align the STRING based on line width.

Final sink for:
  :mis2//settings -- :line-width
  :mis2//style    -- :center
                  -- :left
                  -- :right
"
  ;; Look for width (optional) and an alignment keyword.
  (let ((align-center (mis2//style/get/from-data :center plist))
        (align-right  (mis2//style/get/from-data :right  plist))
        (line-width   (or (mis2//settings/get/from-data :line-width plist)
                          ;; fill-column or (current-fill-column)?
                          fill-column))
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


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-contents)
