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


(defun mis2//contents/build/propertize (message plist)
  "Given MESSAGE and mis2 PLIST, and assuming there are both a `:mis2//style'
keyword in PLIST and a `:face' key in the style list, propertize the MESSAGE
with the face.
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

      ;; Found a defined face, so set `face' property of message string to our
      ;; face and return that.
      (propertize message 'face face-val)
    ;; No face; return the unchanged message string.
    message))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-contents)
