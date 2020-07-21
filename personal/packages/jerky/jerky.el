;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------Like spydez/dirky, but, like, general.---------------------
;;--                      So... Generlky? Genky?... Hm.                       --
;;---------------------------------(or-maybe)-----------------------------------

(require 's)


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defvar spydez//jerky/kvs '()
  "A recursive key-value store, basically. An alist of alists (of et ceteras).
  Maybe some values thrown in there somewhere; I don't know.

Each alist entry is a list (not a cons). There are three cells like so:
'(key value docstr)

KEY should be a symbol, probably?

VALUE can be:
  - Another alist in a layout jerky understands.
  - A string.
  - A symbol that will evaluate to a string.
  - A function that will return a string.
  - Something else? But strings get special treatment.

DOCSTR should be short.
Or not.

I'm not your mother.
")


;;------------------------------------------------------------------------------
;; Keys Functions
;;------------------------------------------------------------------------------

(defun spydez//jerky/keys/path-field-cons (&rest args)
  "Turn args into a cons of key path and final key.

(spydez/jerky/keys \"a.b\" \"c\")
  -> ((a b) . c)
"
  (let ((keys '())
        (strings '())
        last-key)
    (dolist (arg args)
      ;; Push string args to strings, turn non-strings into strings.
      (cond ((stringp arg)
             (push arg strings))

            ;; symbol->string: drop keyword prefix if exists.
            ((symbolp arg)
             (push (s-chop-prefix ":" (symbol-name arg)) strings))

            ;; function->string:
            ((functionp arg)
             (push (funcall arg) strings))

            ;; fail
            (t
             (error (concat "%s: Can't convert '%S' to string for conversion "
                            "of keys into key list.")
                    "spydez/jerky/keys"
                    arg))))

    ;; Now we have strings. They are in backwards order. They need to be turned
    ;; into a final dotted string so we can then break it apart into final key
    ;; list... One of the args could've been a compond key - e.g. "jeff.key".
    (dolist (name
             (s-split "[.]" (s-join "." (nreverse strings))))

      ;; Make symbols out of the key strings.
      (push (intern name) keys))

    ;; Now we have a backwards list... again.
    ;; Turn it into a cons of:
    ;;   - path: forwards list of all but last key
    ;;   - key: the last key
    (setq last-key (car keys))
    (cons (nreverse (cdr keys))
          last-key)))
;; (spydez/jerky/keys "a.b" "c")
;; (spydez/jerky/keys :base "a.b" "c")


(defun spydez//jerky/keys/normalize (&rest args)
  "Turn args into a list of key symbols.

(spydez//jerky/keys/normalize \"a.b\" \"c\")
  -> (a b c)
(spydez//jerky/keys/normalize \"a.b\" c)
  -> (a b c)
(spydez//jerky/keys/normalize \"a\" b c)
  -> (a b c)
"
  (let ((keys '())
        (strings '()))
    (dolist (arg args)
      ;; Push string args to strings, turn non-strings into strings.
      (cond ((stringp arg)
             (push arg strings))

            ;; symbol->string: drop keyword prefix if exists.
            ((symbolp arg)
             (push (s-chop-prefix ":" (symbol-name arg)) strings))

            ;; function->string:
            ((functionp arg)
             (push (funcall arg) strings))

            ;; fail
            (t
             (error (concat "%s: Can't convert '%S' to string for conversion "
                            "of keys into key list.")
                    "spydez/jerky/keys"
                    arg))))

    ;; Now we have strings. They are in backwards order. They need to be turned
    ;; into a final dotted string so we can then break it apart into final key
    ;; list... One of the args could've been a compond key - e.g. "jeff.key".
    (dolist (name (s-split "[.]" (s-join "." (nreverse strings))))

      ;; Make symbols out of the key strings.
      (push (intern name) keys))

    ;; Now we have a backwards list... again.
    ;; Turn it into a forwards list.
    (nreverse keys)))
;; (spydez//jerky/keys/normalize "a.b" "c")
;; (spydez//jerky/keys/normalize :base "a.b" "c")


(defun spydez//jerky/keys/desired? (key)
  "Predicate for filtering list. Returns nil if it encounters one of these:
  `:value'
  `:docstr'

For all others, this returns t to indicate KEY is a valid/desired key.
"
  (not (or (eq key :value)
           (eq key :docstr))))
;; (spydez//jerky/keys/desired? :jeff)
;; (spydez//jerky/keys/desired? 'jeff)
;; (spydez//jerky/keys/desired? "jeff")
;; (spydez//jerky/keys/desired? "value")
;; (spydez//jerky/keys/desired? 'value)
;; (spydez//jerky/keys/desired? :value)
;; (-take-while 'spydez//jerky/keys/desired? '(path to thing :value "hello there"))
;; (-drop-while 'spydez//jerky/keys/desired? '(path to thing :value "hello there"))


;;------------------------------------------------------------------------------
;; Writing to Key-Value Store
;;------------------------------------------------------------------------------


(defun spydez//jerky/write/update (kvs keys &optional value docstr)
  "Looks in KVS-SYMBOL for KEYS. Updates final key in KEYS to:
(list KEY VALUE DOCSTR)

KEYS should be a list in forwards/correct order. E.g.: '(path to the thing)

KVS-SYMBOL should be the (quoted) symbol. E.g.: 'spydez//jerky/kvs

VALUE can be whatever.

DOCSTR can be nil or a string, but is not verified.
"
  (let ((key (car keys))
        (rest (cdr keys)))

    (let ((entry (assoc key kvs)))
      (if (null entry)
          ;; Entry doesn't exist; push a new one into the list.
          (if (null rest)
              ;; This is the end; we know what to do.
              (push (list key value docstr) kvs)

            ;; Have `spydez//jerky/write/tree' take care of the rest when
            ;; this isn't the end...
            (push ;; (list ;; The alist...
                   ;; The alist's key's entry... Put `key' and `rest' back
                   ;; together so it can deal with as much as possible.
                   (spydez//jerky/write/tree (cons key rest) value docstr) ;;)
                  kvs))

        ;; Entry does exist; update cdr slot with new list.
        (if (null rest)
            ;; This is the end; we know what to do.
            (setf (cdr entry) (cons value docstr))

          ;; We don't know what to do; but maybe if we ask ourself?..
          (setf (nth 1 entry) (spydez//jerky/write/update (nth 1 entry)
                                                          rest
                                                          value
                                                          docstr))))))
  ;; Always return the (modified) key-value store.
  kvs)
;; (setq spydez//test/jerky nil)
;; (symbol-value 'spydez//test/jerky)
;; (spydez//jerky/write/update spydez//test/jerky '(path) "test" "docstr")
;; (setq spydez//test/jerky (spydez//jerky/write/update spydez//test/jerky '(path to thing one) "test" "docstr"))
;; (setq spydez//test/jerky (spydez//jerky/write/update spydez//test/jerky '(path to glory) "glory?!" "*shrug*"))
;; (setq spydez//test/jerky (spydez//jerky/write/update spydez//test/jerky '(path to thing) "hello there"))


(defun spydez//jerky/write/tree (keys value docstr)
  "Uses the list of KEYS to make a tree of empty branches out to the final leaf
of (key VALUE DOCSTR).
"
  (if (cdr keys)
      ;; This is an alist of alists, so we need the cdr to be that 'of alist'...
      ;; So we put the list we get back from ourself into a list. It'll end up
      ;; as an alist with one entry, like:
      ;;   '((:solo-key "solo value" "solo docstr"))
      (list (car keys) (list (spydez//jerky/write/tree (cdr keys)
                                                       value
                                                       docstr)))
    ;; Final key. Return the '(field value docstr) entry.
    (list (car keys) value docstr)))
;; (defvar spydez//test/jerky '())
;; (spydez//jerky/write/tree '(path to thing one) "test" "docstr")


(defun spydez/jerky/set (&rest args)
  "Overwrite an existing entry or add new entry to `spydez//jerky/keys'.

KEYS: The '&rest' come first!
  All ARGS before jerky's keyword args are considered the keys. They will be
processed by `spydez/jerky/keys' to get a key path to follow.

TODO: Flatten lists here? So if only arg is list, becomes key list?

This uses two keyword ARGS after the KEYS. The keywords are:
  `:value'
  `:docstr'

`:docstr'
  The argument after :docstr will be evaluated and stored as the
  documentation string.

`:value'
  The argument after :value will be stored as the value.

If not provided, they will be nil.
"
  ;; Some shenanigans to do to turn 'args' into keys args and plist args.
  (-let ((keys (apply #'spydez//jerky/keys/normalize
                      (-take-while #'spydez//jerky/keys/desired? args)))
         ;; dash-let's plist match pattern to non-keys in ARGS.
         ((&plist :docstr docstr :value value)
          (-drop-while #'spydez//jerky/keys/desired? args)))

    (setq spydez//jerky/kvs (spydez//jerky/write/update spydez//jerky/kvs
                                                        keys
                                                        value
                                                        docstr))))
;; spydez//jerky/kvs
;; (setq spydez//jerky/kvs nil)
;; (spydez/jerky/set 'path 'to 'thing :value "hello there")
;; (spydez/jerky/set :test :jeff :value "jeffe" :docstr "I am a comment.")
;; (spydez/jerky/set :test :jeff :value "jeffe")
;; (spydez/jerky/set :test :jill :value "jill")


;;------------------------------------------------------------------------------
;; Reading from Key-Value Store
;;------------------------------------------------------------------------------

(defun spydez/jerky/get (&rest keys)
  "Gets an entry from `spydez//jerky/keys'.

KEYS: Key path to walk down to find value to return.

TODO: Flatten lists here? So if only arg is list, becomes key list?

If nothing found at KEYS, return will be nil.
"
  (let ((keys (apply #'spydez//jerky/keys/normalize keys))
        (kvs spydez//jerky/kvs))

    (dolist (key keys)
      (setq kvs (nth 1 (assoc key kvs))))
    kvs))
;; (setq spydez//jerky/kvs nil)
;; (setq spydez//jerky/kvs '((test ((jill "jill" nil) (jeff "jeffe" "I am a comment."))) (path ((to ((thing "hello there" nil))))))
;; spydez//jerky/kvs
;; (spydez/jerky/get 'path 'to 'thing)
;; (spydez/jerky/get :test :jeff)
;; (spydez/jerky/get :test :jill)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'jerky) ;; I have given you tasty jerky. Enjoy.
