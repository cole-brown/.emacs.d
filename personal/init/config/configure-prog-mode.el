;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------------mode, not rock.-------------------------------
;;--                      General Programming Mode Stuff                     --
;;-----------------------------------------------------------------------------


(require 'subr-x)

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

(defcustom spydez/prog-mode/comment/padding-adjustments
  ;; pycodestyle E265: block comments should start with '# '.
  '((python-mode " ")) ;; "# " as padding for e.g. "# ---" instead of "#---".
  "Adjustments to `spydez/prog-mode/comment/paddings' per mode.
e.g. python-mode can ask for a space after it's comment to ensure
pylint is happier w/ wrapped or centered comments generated by
these functions."
  :group 'spydez/group
  :type '(alist :key-type symbol :value-type string))


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------

(defun spydez/prog-mode/comment/paddings ()
  "Gets comment prefix/postfix appropriate for mode. Returns a
  parts list like e.g. elisp-mode: (\";;\" nil)"
  (let* ((pad-more   (comment-add nil))
         (adjustment (nth 1 (assoc major-mode
                                 spydez/prog-mode/comment/padding-adjustments)))
         (prefix  (string-trim-right (comment-padright comment-start pad-more)))
         (postfix (comment-padleft comment-end (comment-add pad-more)))
         ;; if we have an adjustment, add it onto insides of paddings
         (prefix (concat prefix (if (and prefix adjustment) adjustment)))
         (postfix (concat adjustment (if (and postfix adjustment) postfix))))

    ;; and return
    (list prefix postfix)))
;; (spydez/prog-mode/comment/paddings)
;; (nth 0 (spydez/prog-mode/comment/paddings))
;; (nth 1 (spydez/prog-mode/comment/paddings))
;; (length (nth 1 (spydez/prog-mode/comment/paddings)))


(defun spydez/prog-mode/comment/wrap (arg &optional trim concat-sep)
  "Turns ARG into a string and then into a proper comment based
on mode (uses `comment-*' emacs functions)."
  (let* ((string (if (stringp arg) arg (format "%s" arg)))
         (concat-sep (or concat-sep " "))
         (comment-parts (spydez/prog-mode/comment/paddings))
         (prefix (nth 0 comment-parts))
         (postfix (nth 1 comment-parts))
         (comment (mapconcat 'identity
                             (list prefix string postfix) concat-sep)))
    (if trim
        (string-trim comment)
      comment)))
;; (spydez/prog-mode/comment/wrap "foo")
;; (spydez/prog-mode/comment/wrap "foo" t)
;; (spydez/prog-mode/comment/wrap "---" nil "")
;; (spydez/prog-mode/comment/wrap "")
;; (spydez/prog-mode/comment/wrap (make-string 3 ?-) t "")


(defun spydez/prog-mode/comment/center/parts
    (comment &optional comment/fill-spaces? comment/fill-char
             indent borders paddings)
  "Returns a centered-comment parts list, ala
`spydez/string/center/parts', but with prog-mode-specific
paddings (comment delimiters) assuming nil PADDINGS."
  (let* ((paddings (or paddings (spydez/prog-mode/comment/paddings))))
    (spydez/string/center/parts comment
                                comment/fill-spaces?
                                comment/fill-char
                                paddings
                                borders
                                indent)))
;; (spydez/prog-mode/comment/center/parts "hello?")
;; (spydez/prog-mode/comment/center/parts "hello there" t)
;; (spydez/prog-mode/comment/center/parts "hello there" nil nil 4)


(defun spydez/prog-mode/comment/center
    (comment &optional comment/fill-spaces? comment/fill-char
             indent borders paddings)
  "Centers COMMENT string. Wraps it in comment characters (or
  PADDINGS if non-nil), gives it a border of BORDERS (or
  'spydez/string/center' default if nil), and fills the center
  arount COMMENT with COMMENT/FILL-CHAR characters if
  COMMENT/FILL-SPACES? is non-nil."

  ;; build string from prog-mode parts section
  (spydez/string/parts/build-section
   (spydez/prog-mode/comment/center/parts comment
                                          comment/fill-spaces? comment/fill-char
                                          indent borders paddings)))
;; (spydez/prog-mode/comment/center "Hello there.")
;; (spydez/prog-mode/comment/center "Hello there." t)
;; (spydez/prog-mode/comment/center "Hello there." t ?x)
;; (spydez/prog-mode/comment/center "Hello there." nil ?x)
;; (spydez/prog-mode/comment/center "")
;; (spydez/prog-mode/comment/center "" t)
;; (spydez/prog-mode/comment/center "test" t)
;; (spydez/prog-mode/comment/center "foo" nil ?- 4)
;; (spydez/prog-mode/comment/center "Hello, World!" nil nil nil '("-x" "y="))
;; (spydez/prog-mode/comment/center "Hello, World!" nil nil nil '("-" "=") '(";;m" "m;;"))
;; (spydez/prog-mode/comment/center "(will center on exit)")
;; (spydez/prog-mode/comment/center "(will center on exit)" nil spydez/char/center/border)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-prog-mode)
