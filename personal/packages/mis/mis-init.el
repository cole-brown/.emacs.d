;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------Please Press the Power Button.-------------------------
;;--                     Can't Do Anything Til You Do...                      --
;;------------------------------------------------------------------------------


(require 'mis-message)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

;; just more pretty
(defcustom mis/init/indents
  '((none    0)
    (default 0)
    (nil     0)

    ;; init sequence slightly indented
    (init 1)

    ;; require more indented, piggybacks double(ish) that
    (require (3 5 7)))
  "Alist for various indent levels."
  :group 'mis
  :type 'alist)


(defcustom mis/init/indent-gutter 8
  "Amount of space to use for indents, indent arrows in init messages."
  :group 'mis
  :type 'integer)


;; fine: - ascii hypen
;; tried: ─ unicode box drawing light horizontal
;;   - doesn't jive w/ < > as arrow heads though
(defcustom mis/init/indent-arrow/tail ?-
  "Character to use to draw indent arrow's tail."
  :group 'mis
  :type 'character)


(defcustom mis/init/indent-arrow/head
  '(;; rightwards
    (nil   . ">")
    (right . ">")
    ;; leftwards
    (t     . "<")
    (left  . "<"))
  "Character to use to draw indent arrow's tail."
  :group 'mis
  :type 'alist)
;; (alist-get nil mis/init/indent-arrow/head)


(defcustom mis/init/type '(mis default)
  "Default type used if none provided. Types are e.g.:
'(spydez bootstrap (system specific))
'(spydez running none)
etc...

They will show up in *Messages* output for `mis/init/sequence'."
  :group 'mis
  :type '(choice (sexp :tag "list of symbols - most important first")
                 (function :tag "function to call to get list of symbols")))


;;------------------------------------------------------------------------------
;; Main Entry Points?
;;------------------------------------------------------------------------------

;; separators?:
;;   | - pipe,
;;   │ - box drawing vertical,
;;   ┊ - box drawing quadruple dash vertical,
;;   ├ ┤ - box drawing light vertical and right/left,
(defun mis/init/sequence (indent arrow msg-fmt &rest args)
  "Print helpful debug message (if spydez/debugging-p) with init
sequence formatting. Indented to INDENT level/number with ARROW
'left or 'right. MSG-FMT should be a `format' style string which
ARGS will fill in."
  (when (spydez/debugging-p)
    (let* ((indent-gutter (mis/init/indent-arrow indent arrow))
           (inject-msg-fmt
            (concat
             (propertize indent-gutter 'face 'font-lock-string-face)
             " "
             (propertize "├" 'face 'font-lock-comment-delimiter-face)
             " "
             ;; current init type via nil
             (propertize (format "%s"
                                 (mis/setting/get-with-default
                                  nil
                                  mis/init/type))
                         'face 'font-lock-comment-face)
             " "
             (propertize "┤:" 'face 'font-lock-comment-delimiter-face)
             " "
             (propertize msg-fmt 'face 'default))))
      (apply 'mis/message/preserve-properties inject-msg-fmt args))))
;; (mis/init/sequence nil nil "Test: %s %s" nil 'symbol)
;; (mis/init/sequence 4 nil "Test: %s %s" '(a b c) 'symbol)
;; (mis/init/sequence 'init 'right "Test: %s" 'jeff)
;; (mis/init/sequence 'init 'left "Test: %s" 'jeff)
;; (mis/init/sequence 'require nil "Test: %s" 'jeff)
;; (mis/init/sequence 'require 'left "Test: %s" 'jeff)
;; (mis/init/sequence 'require nil "Test: %s" 'jeff)
;; (mis/init/sequence 'require 'left "Test: %s" 'jeff)


(defun mis/init/message (indent msg-fmt &rest args)
  "Print helpful mis/init/sequence message (if
spydez/debugging-p) at 'init indent. Really just a way to not
bother with INDENT/TYPE."
  (let ((indent (or indent 'init)))
    (apply #'mis/init/sequence indent nil msg-fmt args)))
;; (mis/init/message 'init "start init: %s %s" '(testing list) 'test-symbol)
;; (mis/init/message 'ignore "hi?")


(defun mis/init/step-done (prev curr &rest args)
  "Info about init step changes. See zeroth-steps.el."
  (when (spydez/debugging-p)
    (apply 'mis/init/sequence 'init 'left
           "step completed: %s <-from- %s"
           ;; Need this nil here to prevent:
           ;;   '((curr list things) prev list things)
           ;; With nil we have:
           ;;   '((curr list things) (prev list things))
           ;; Yay lisp.
           curr prev nil)))
;; (mis/init/step-done '(bootstrap tie aglets) '(boot adjust tongue))


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun mis/init/get-indent (level &optional sublevel)
  "Get an indent level. If LEVEL is a symbol, looks in
`mis/init/indents'; will return `default' from alist if not
found. If LEVEL is a numberp, returns LEVEL.

If SUBLEVEL is non-nil and indent level found in `mis/init/indents' returns a
list, will try to get nth element in list for the indent level. E.g. if LEVEL
becomes '(3 5 17 4), then sublevel nil/0 is 3, 1 is 5, 3 is 17, etc."

  ;; early out for int level
  (if (integerp level)
      level

    ;; Use level if exists, else 'default.
    (let* ((indents (or (nth 1 (assoc level mis/init/indents))
                        (nth 1 (assoc 'default mis/init/indents)))))

      ;; no sublevel asked for?
      (if (null sublevel)
          ;; just first of the indents then
          (or (and (listp indents)
                   (nth 0 indents))
              indents)

        ;; They asked for a sublevel - can we find one?
        (if (or (not (listp indents))
                (not (integerp sublevel))
                (< (length indents) sublevel))
            (prog1
                0 ;; give indent 0 in hopes it'll get noticed and fixed?
              ;; also maybe they'll notice this...
              (mis/warning
               nil :warning
               "Cannot get sublevel %s of '%s' - %s: '%s'"
               sublevel level
               "bad args or not enough options"
               indents))
          ;; give the correct sublevel
          (nth sublevel indents))))))
;; (mis/init/get-indent 'foo)
;; (mis/init/get-indent 'require)
;; (mis/init/get-indent 'require 1)


;; Current arrows:
;;->      ;; init
;; <-     ;; step-done
;;--->    ;; require
;;   <--- ;; (FUTURE?) something/done @ require level?
;;----->  ;; require-piggyback
;;01234567;; So 8 space "gutter" to work with ATM?
(defun mis/init/indent-arrow (level &optional direction)
  "Returns a string which is an ascii arrow with a tail of length
defined by LEVEL in `mis/init/indents'. Will be left
pointing if DIRECTION is 'left, else right. All strings returned
should be of length `mis/init/indent-gutter'.

Returns empty string if LEVEL or DIRECTION is `ignore'."
  (let ((gutter-format (format "%%-%ds" mis/init/indent-gutter)))
    (if (or (eq level 'ignore)
            (eq direction 'ignore))
        (format gutter-format "")
      ;; actual arrow
      (let* ((indent (mis/init/get-indent level))
             (left (eq direction 'left)) ;; defaults to right
             (prefix-str (make-string indent
                                      (if left
                                          ?\s
                                        mis/init/indent-arrow/tail)))
             (postfix-str (make-string indent
                                       (if left
                                           mis/init/indent-arrow/tail
                                         ?\s)))
             (arrow-head (or (alist-get direction
                                        mis/init/indent-arrow/head)
                             (alist-get 'right
                                        mis/init/indent-arrow/head))))
        ;; make sure we don't go over length
        (truncate-string-to-width
         ;; format the whole gutter for correct right space str pad
         ;; (make sure we don't go under length)
         (format gutter-format
                 ;; format the left/right arrow
                 (concat prefix-str arrow-head postfix-str))
         mis/init/indent-gutter)))))
;; (length (mis/init/indent-arrow 'init) )
;; (length (mis/init/indent-arrow 'init 'left) )
;; (length (mis/init/indent-arrow 'require) )
;; (length (mis/init/indent-arrow 'require 'left) )
;; (length (mis/init/indent-arrow 'require-piggyback) )
;; (length (mis/init/indent-arrow 'require-piggyback 'left) )
;; (length (mis/init/indent-arrow 'ignore) )
;; (length (mis/init/indent-arrow 'require 'ignore) )


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-init)
