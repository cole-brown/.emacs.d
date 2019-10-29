;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------HELP!-------------------------------------
;;--                 My fancy header generator is broken...                   --
;;------------------------------------------------------------------------------


(require 'subr-x)
(require 's)

(require 'mis-parts)
(require 'mis-center)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defcustom mis/comment/padding-adjustments
  ;; pycodestyle E265: block comments should start with '# '.
  '((python-mode " ")) ;; "# " as padding for e.g. "# ---" instead of "#---".
  "Adjustments to `mis/comment/paddings' per mode.
e.g. python-mode can ask for a space after it's comment to ensure
pylint is happier w/ wrapped or centered comments generated by
these functions."
  :group 'mis
  :type '(alist :key-type symbol :value-type string))


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

;; May want "no padding" instead of "no trim" for interactive?..
(defun mis/comment/center (from to &optional trim)
  "Centers region marked by FROM and TO.

Interactively, a prefix arg means \"do not trim\".

Non-interactively, if TRIM is non-nil, lines will be trimmed just before
re-centering, otherwise they will only be recentered."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (region-beginning)
                       (region-end)
                       ;; No trim if yes prefix arg
                       (null current-prefix-arg))))

  (let ((start-point (point-marker))
        (start-bol (line-beginning-position))
        (start-eol (line-end-position))
        (start (min from to))
        (end (max from to)))

    ;; Mark end.
    (goto-char end)
    ;; We don't want a line with no actual characters selected.
    (when (= (line-beginning-position) (point))
      (forward-line -1)
      ;; But where to go now? Gotta make sure region is valid...
      (goto-char (max (line-end-position) start)))
    ;; Whatever line we're on now... Sure. This is the end.
    (end-of-line)
    (setq end (copy-marker (point) t))

    ;; Go back to start to get ready for loop.
    (goto-char start)
    (beginning-of-line)

    ;; Center region marked by current point & end.
    (while (< (point) end)
      ;; strip comment chars...
      (uncomment-region (line-beginning-position) (line-end-position))

      ;; trim line and center
      (let* ((line (delete-and-extract-region (line-beginning-position)
                                              (line-end-position)))
             (line (if trim (s-trim line) line))
             (centered (mis/comment/center/build line)))

        ;; Re-insert.
        ;; `delete-and-extract-region' puts at at start of region so
        ;; just `insert'.
        (insert centered)

        ;; Update loop condition.
        (forward-line 1)))

    ;; go back to where we were
    (goto-char start-point)
    (set-marker start-point nil)))
;;test
;;    test


;;------------------------------------------------------------------------------
;; Main Entry Point?
;;------------------------------------------------------------------------------

(defun mis/comment/center/build
    (comment &optional comment/fill-spaces? comment/fill-char
             indent borders paddings)
  "Centers COMMENT string. Wraps it in comment characters (or
  PADDINGS if non-nil), gives it a border of BORDERS (or
  'mis/center' default if nil), and fills the center
  arount COMMENT with COMMENT/FILL-CHAR characters if
  COMMENT/FILL-SPACES? is non-nil."

  (mis/comment/unless
    ;; build string from prog-mode parts section
    (mis/parts/build
     (mis/comment/center/parts comment
                               comment/fill-spaces? comment/fill-char
                               indent borders paddings))))
;; (mis/comment/center/build "Hello there.")
;; (mis/comment/center/build "Hello there." t)
;; (mis/comment/center/build "Hello there." t ?x)
;; (mis/comment/center/build "Hello there." nil ?x)
;; (mis/comment/center/build "")
;; (mis/comment/center/build "" t)
;; (mis/comment/center/build "test" t)
;; (mis/comment/center/build "foo" nil ?- 4)
;; (mis/comment/center/build "Hello, World!" nil nil nil '("-x" "y="))
;; (mis/comment/center/build "Hello, World!" nil nil nil '("-" "=") '(";;m" "m;;"))
;; (mis/comment/center/build "(will center on exit)")
;; (mis/comment/center/build "(will center on exit)" nil mis/center/char/border)


(defun mis/comment/wrap (arg &optional trim concat-sep padding-adj)
  "Turns ARG into a string and then into a proper comment based
on mode (uses `comment-*' emacs functions).

If CONCAT-SEP is non-nil, use it instead of a space.

Passes PADDING-ADJ to `mis/comment/paddings' as
WITH-ADJUSTMENTS arg.

If TRIM is non-nil, trims resultant string before returning."
  (mis/comment/unless
    (let* ((string (if (stringp arg) arg (format "%s" arg)))
           (concat-sep (or concat-sep " "))
           (comment-parts (mis/comment/paddings padding-adj))
           (prefix (nth 0 comment-parts))
           (postfix (nth 1 comment-parts))
           (comment (mapconcat 'identity
                               (list prefix string postfix) concat-sep)))
      (if trim
          (string-trim comment)
        comment))))
;; (mis/comment/wrap "foo")
;; (mis/comment/wrap "foo" t)
;; (mis/comment/wrap "---" nil "")
;; (mis/comment/wrap "")
;; (mis/comment/wrap (make-string 3 ?-) t "")
;; (mis/comment/wrap (make-string 3 ?-) t "" t)


(defun mis/comment/ignore ()
  "Guesses whether to ignore everything based on Emacs' comment-* vars/funcs."
  ;; first try: if comment-start is null, ignore.
  (null comment-start))


(defmacro mis/comment/unless (&rest body)
  "Runs BODY forms if `mis/comment/ignore' is non-nil."
  (declare (indent defun))
  `(unless (mis/comment/ignore)
     ,@body))


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun mis/comment/paddings (&optional with-adjustments)
  "Gets comment prefix/postfix appropriate for mode. Returns a
  parts list like e.g. elisp-mode: (\";;\" nil)"
  (mis/comment/unless
    (let* ((adjustment
            (if with-adjustments
                (nth 1 (assoc major-mode
                              mis/comment/padding-adjustments))
              nil))
           (pad-more (comment-add nil))
           (prefix   (string-trim-right (comment-padright comment-start
                                                          pad-more)))
           (postfix  (comment-padleft comment-end (comment-add pad-more)))
           ;; if we have an adjustment, add it onto insides of paddings
           (prefix   (concat prefix (if (and prefix adjustment) adjustment)))
           (postfix  (concat adjustment (if (and postfix adjustment) postfix))))

      ;; and return
      (list prefix postfix))))
;; (mis/comment/paddings)
;; (nth 0 (mis/comment/paddings))
;; (nth 1 (mis/comment/paddings))
;; (length (nth 1 (mis/comment/paddings)))


(defun mis/comment/center/parts
    (comment &optional comment/fill-spaces? comment/fill-char
             indent borders paddings)
  "Returns a centered-comment parts list, ala
`mis/center/parts', but with prog-mode-specific
paddings (comment delimiters) assuming nil PADDINGS."
  (mis/comment/unless
    (let* ((paddings (or paddings (mis/comment/paddings t))))
      (mis/center/parts comment
                        comment/fill-spaces?
                        comment/fill-char
                        paddings
                        borders
                        indent))))
;; (mis/comment/center/parts "hello?")
;; (mis/comment/center/parts "hello there" t)
;; (mis/comment/center/parts "hello there" nil nil 4)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-comment)
