;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------...Well Fancy Simple Messages Aren't All That Simple So...-----------
;;--                                Settings                                  --
;;------------------------------------------------------------------------------


(require 'dash)
(require 's)

;; §-TODO-§ [2020-03-18]: Rename this mis2-configuration.el? mis2-config.el?
;; Break into 2 files? Keep all here?


;;------------------------------------------------------------------------------
;; Customization: General Settings
;;------------------------------------------------------------------------------

;; Tested 0 and 2 second timeouts for my init (for Emacs on Windows) and...
;; No difference. But it does make a difference for interactive commands which
;; do a lot of messages in sequence.
(defcustom mis2/custom/echo-area-timeout '(0.1 2)
  "List of 2 numbers for minibuffer echo area timeout.

First element: timeout for batch type commands.
Second element: timeout during for interactive type commands.

See docs for `minibuffer-message-timeout'. This will lexically bind
`minibuffer-message-timeout' to this value. If not numberp, it seems the first
message will not clear until a non-`minibuffer-message' hits the *Messages*
buffer, at which point it and all subsequent `minibuffer-message' messages will
appear in *Messages* before the new message.

If numberp, this is the number of seconds to display the message
in the echo area. 0 is a good value for 'normal' `message'
minibuffer-echo-area functionality."
  :group 'mis2
  :type 'boolean)


(defcustom mis2/custom/keywords '(:settings :mis2//settings
                                  :style    :mis2//style)
  "Settings and style keyword symbol names -> mis2 private symbols.
"
  :group 'mis2
  :type '(list symbol symbol))


;;------------------------------------------------------------------------------
;; Customization, Consts, Vars, et ceteras...
;;------------------------------------------------------------------------------

;;---
;; Customization
;;---
;; Consts that we want available to the user to change.


;;---
;; General Consts
;;---

(defconst mis2/validity/types
  '((:number  numberp)
    (:integer integerp)
    (:float   floatp)
    (:string  stringp)
    (:char    characterp)
    (:list    listp))
  "Validity checkers for basic types and anything else that doesn't change
based on context.

E.g. a :float is always a float, but a :range could be 0 to 100 or -0.5 to 0.5.
")


;;---
;; Settings Consts
;;---

;; §-TODO-§ [2020-02-05]: Do this for passing in/around more settings?
;; Settings keyword plist:
;;    KEYS         TYPE-or-VALUES
;;
;;   :interactive    t, nil
;;      Shorthand for:
;;        :echo to t.
;;        :echo-delay to `mis2/message/echo-area-timeout/interactive'
;;
;;   :batch          t, nil
;;      Shorthand for:
;;        :echo to t.
;;        :echo-delay to `mis2/message/echo-area-timeout/non-interactive'
;;
;;   :echo           t, nil
;;   :echo-delay     nil, numberp (see `minibuffer-message-timeout')
;;
;; Yes or no?
;;   :type           mis2/type->faces alist key (keyword symbol or list)
;;   :face           symbol for desired face


(defconst mis2/settings/meta/keys
  '((:interactive :const nil)
    (:batch       :const nil))
  "10,000 foot view: calls are either intended to be in an interactive manner,
or as automatic output flung out as soon as it's reached. The main difference is
how long the echo area timeout is changes between the two.

Settings:
;; §-TODO-§ [2020-03-06]: explain each of these
  :interactive
  :batch
")


(defconst mis2/settings/keys
  '((:echo       :const     (t nil))
    (:echo-delay :range     (0.0 100.0))
    (:type       :key-alist mis2/type->faces)
    (:buffer     :string))
  "10,000 foot view: calls are either intended to be in an interactive manner,
or as automatic output flung out as soon as it's reached. The main difference is
how long the echo area timeout is changes between the two.

Settings:
;; §-TODO-§ [2020-03-06]: explain each of these
  :echo
  :echo-delay
  :type
  :buffer
")


;;---
;; Style Consts
;;---

(defconst mis2/style/keys
  '(;; Alignment
    (:center :const (t nil))
    (:left   :const (t nil))
    (:right  :const (t nil))

    ;; Text
    (:face :key-alist ':type) ;; indirect... got to get :type alist, then get
                              ;; :face's key out of that.

    ;; Box Model...ish Thing.
    (:margins :list (:string :string))
    (:borders :list (:string :string))
    ;; Can be either, e.g.:
    ;;   :padding '(">>" "<<")   ;; use the strings exactly
    ;;   :padding '(?- :empty 3) ;; Build from char, leaving 3 empties.
    ;;   :padding '(?- :fill 3)  ;; Build from char, to max of 3 long.
    (:padding :or ((:list (:string :string))
                   (:list (:char (:const (:empty :fill)) :integer)))))
  "Style for a part of a mis2 message. Alignment, text properties, border,
margin, padding...

;; §-TODO-§ [2020-03-06]: explain each of these
Styles:

  Alignment:
    :center
      (mis2/settings/style/update style :center nil)
    :left
    :right


  Text:
    :face
      (mis2/settings/style/update style :face :title)


  Box Model:
    :margins
      (mis2/settings/style/update style :margins '(\">>\" \"<<\"))
    :borders
      (mis2/settings/style/update style :borders '(\"|\" \"|\"))
    :padding
      (mis2/settings/style/update style :padding '(?- :empty 3))
      (mis2/settings/style/update style :padding '(\"---\" \"---\"))
")


;;------------------------------------------------------------------------------
;; Private Keywords
;;------------------------------------------------------------------------------

(defconst mis2//data/keywords
  (list :mis2//settings
        :mis2//style

        :mis2//contents
        :mis2//message
        :mis2//buffers

        :mis2//testing)
  "Full list of keywords:

Public -> Private (see `mis2/custom/keywords'):
  :settings -> :mis2//settings
  :style    -> :mis2//style

Private-Only:
  :mis2//contents - contents of message, unformatted
  :mis2//message  - final, propertized, and formatted message
  :mis2//buffers  - list of buffer(s) to send output to
  :mis2//testing  - valid only in mis2 tests
")


;;------------------------------------------------------------------------------
;; Data - Getters
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-03-24]: move data things to own file?
(defun mis2//data/plist? (plist)
  "Returns non-nil if PLIST is a mis2 plist.
"
  ;; Check for contents or message key/value - they're required.
  ;; or for mere existance of :mis2//testing keyword.
  (or (plist-get plist :mis2//contents)
      (plist-get plist :mis2//message)
      (memq :mis2//testing plist)))
;; (mis2//data/plist? '(:mis2//message "hi" :mis2//settings (:echo t :echo-delay 2)))
;; (mis2//data/plist? '(:mis2//testing))


(defun mis2//data/get (key plist)
  "Get a piece of a mis2 data plist (settings, styles, contents, etc).
"
  (if (memq key mis2//data/keywords)
      (plist-get plist key)
    (error "Key '%s' is not a known mis2//data key: %S"
           key mis2//data/keywords)))
;; (mis2//settings/get/from-data :echo '(:echo t :echo-delay 2)


;;------------------------------------------------------------------------------
;; Settings - Getters
;;------------------------------------------------------------------------------

(defun mis2//settings/get/from-data (key plist)
  "Get settings from a mis2 data PLIST, then get KEY from settings.
"
  (if (mis2//data/plist? plist)
      (if (or (alist-get key mis2/settings/keys)
              (alist-get key mis2/settings/meta/keys))
          ;; get settings plist from data plist, then get specific setting.
          (plist-get (mis2//data/get :mis2//settings plist) key)

        (error "Key '%s' is not a known mis2/settings key: %S or %S"
               key mis2/settings/keys mis2/settings/meta/keys))

    (error "Plist is not a mis2 plist. %S %S"
           "Might be a mis2 settings, style, etc sub-list?"
           plist)))


;; (defun mis2/settings/get (key plist)
;;   "Get a mis2 setting based off KEY. Setting either comes from USER-SETTINGS
;; plist or from the appropriate mis2 setting const/var (passed in
;; as MIS2-SETTING).
;; "
;;   ;; Simply return value from user-settings or mis2-setting, preferring
;;   ;; user-settings. Only complication is if user-settings specifies a nil, so we
;;   ;; have to check that key is a member of user-settings...
;;   (if (plist-member user-settings key)
;;       (plist-get user-settings key)
;;     mis2-setting))

;; (defun mis2/setting/get-with-default (arg default)
;;   "Figures out actual ARG by looking at ARG and DEFAULT. It will
;; be ARG if ARG is non-nil and either symbolp or functionp. Else it
;; will look at the value of DEFAULT and use either that symbol, or
;; call that function to get a symbol."
;;   (cond
;;    ;; pass through - function
;;    ((and (not (null arg))
;;          (functionp arg))
;;     (funcall arg))
;;    ;; pass through - symbol
;;    ((and (not (null arg))
;;          (symbolp arg))
;;     ;; if it has a value, use that, else use directly
;;     (if (symbol-value arg)
;;           (symbol-value arg)
;;       arg))

;;    ;; default - function to get current
;;    ((and (not (null default))
;;          (functionp default))
;;     (funcall default))
;;    ;; default - symbol as default
;;    ((symbolp default)
;;     ;; if it has a value, use that, else use directly
;;     (if (and (not (null default))
;;              (symbol-value default))
;;           (symbol-value default)
;;       default))

;;    ;; fallback to something drastic-ish
;;    (t
;;     :error)))
;; ;; (mis2/setting/get-with-default nil mis2/debug/type)


;;------------------------------------------------------------------------------
;; Settings - Setters
;;------------------------------------------------------------------------------

(defun mis2/settings/set (plist &rest args)
  "Add one or more mis settings in ARGS to whatever is already in PLIST.
Returns new list (PLIST will be unchanged).

Expects ARGS to be: key0 value0 key1 value1 ...

Takes ARGS, checks them as best as possible for validity, and returns a
mis/settings plist.

If a keyword exists twice, the later one 'wins'.
Examples:
  - If a setting already in PLIST is also in ARGS, the ARGS one will overwrite
    the PLIST one for the return value.
  - If a setting is in ARGS twice, only the last one will be in the returned
    value.
  - If `mis2/settings/set' is called multiple times to build settings, only
    the latest key/value pair will be used for the duplicated setting key.
"
  ;; Copy the list, feed into `update' for all the logic, then return and done.
  ;; Can't use `apply' or `funcall' because update is a macro...
  (let ((new-plist (copy-sequence plist)))
    (mis2/settings/update new-plist (-flatten-n 1 args))))
;; (let (settings) (mis2/settings/set settings :jeff "jill"))
;; (let (settings) (mis2/settings/set settings :echo t))
;; (mis2/settings/set nil :echo t)
;; (let (settings) (mis2/settings/update settings :echo t))


(defmacro mis2/settings/update (plist &rest args)
  "Add one or more mis settings in ARGS to whatever is already in PLIST.
Updates PLIST in place.

Expects ARGS to be: key0 value0 key1 value1 ...

Takes ARGS, checks them as best as possible for validity, and returns a
mis/settings plist.

If a keyword exists twice, the later one 'wins'.
Examples:
  - If a setting already in PLIST is also in ARGS, the ARGS one will overwrite
    the PLIST one for the return value.
  - If a setting is in ARGS twice, only the last one will be in the returned
    value.
  - If `mis2/settings/set' is called multiple times to build settings, only
    the latest key/value pair will be used for the duplicated setting key.
"
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Surprising-Local-Vars.html#Surprising-Local-Vars
  (let (;;(temp-plist (make-symbol "settings-list"))
        (temp-args (make-symbol "settings-args")))
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Argument-Evaluation.html#Argument-Evaluation
    `(let (;;(,temp-plist ,plist)
           (,temp-args (list ,@args)))

       ;; If &rest is a list of 1 element, strip it out of the list. Probably
       ;; was something like:
       ;;   (mis2/settings/update settings :interactive)
       ;; Or something that evaluated out to be...
       ;;   (mis2/settings/update settings nil)
       ;; Which we want to check for here.
       (when (and (not (null ,temp-args))
                  (listp ,temp-args)
                  (= 1 (length ,temp-args)))
         (setq ,temp-args (first ,temp-args)))

       ;; Now look at the args and do the key and value checking.
       (cond
        ;; No args: No settings; no worries.
        ((or (null ,temp-args)
             (and (= 1 (length ,temp-args))
                  (null (first ,temp-args))))
         ;; Give them back their list.
         ;; No update to it because no change.
         ,plist)

        ;; Normal Case: check keys and values, add to copy of list, return copy.
        ((listp ,temp-args)
         ;; Our return is our input plist. Need to make sure we set it;
         ;; `plist-put' may or may not update in place.
         ;; Loop over list while we have at least 2 elements left.
         (while (and ,temp-args
                     (listp (cdr ,temp-args)))
           ;; Pop a pair off to process.
           (let* ((key (pop ,temp-args))
                  (value (pop ,temp-args))
                  key-valid
                  value-valid)

             ;; Check our key! This will either return the key or an error
             ;; symbol, so always save.
             (setq key-valid (mis2//settings/check-key key))

             ;; Check our value! This will either return the value or an error
             ;; symbol, so always save.
             (setq value-valid (mis2//settings/check-value key value))

             ;; And now check for errors.
             (cond
              ;; Complain about the bad thing.
              ((eq key-valid :*bad-key-error*)
               (error "Bad key. %S is not a known mis/settings key." key))

              ;; Complain about the bad thing.
              ((eq value-valid :*bad-value-error*)
               (error "Bad value. %S is not a valid value for %S." value key))

              (t
               ;; Use plist-put so we only have one key for this in the plist.
               (setq ,plist (plist-put ,plist key value))
               ))))

         ;; Still have ,temp-args? Didn't end up with a pair - complain?
         (when ,temp-args
           (error "Uneven list; no value for last element: %s" (first ,temp-args)))

         ;; ...and return the list.
         ,plist)

        ;; Otherwise, uh... *shrug*
        (t
         (error
          "Don't know how to process args into settings - not a list or nil.")))
       )))
;; (let ((settings '(:test "an test str"))) (macroexpand '(mis2/settings/update settings :jeff "jill")))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings nil))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :jeff "jill"))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo "jill"))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo t))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo t) (message "%S" settings))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo nil))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo-delay 0))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo-delay -1))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo-delay 101))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo-delay "jeff"))
;; (let ((settings '(:test "an test str"))) (mis2/settings/update settings :echo t :type :default))
;; (let (settings) (mis2/settings/update settings :echo t :type :default) (message "%S" settings))
;; (let (settings) (macroexpand '(mis2/settings/update settings :echo t :type :default)))


(defun mis2//settings/check-key (key)
  "Checks that KEY is a known mis/settings keyword.
"
  (mis2//private/check-key key mis2/settings/keys mis2/settings/meta/keys))


(defun mis2//settings/check-value (key value)
  "Checks that VALUE is a valid value for the mis/settings keyword KEY.
"
  (mis2//private/check-value key value #'mis2//settings/check-key))


;;------------------------------------------------------------------------------
;; Style
;;------------------------------------------------------------------------------


(defun mis2/style/set (plist &rest args)
  "Add one or more mis styles in ARGS to whatever is already in PLIST.
Returns new list (PLIST will be unchanged).

Expects ARGS to be: key0 value0 key1 value1 ...

Takes ARGS, checks them as best as possible for validity, and returns a
mis/style plist.

If a keyword exists twice, the later one 'wins'.
Examples:
  - If a style already in PLIST is also in ARGS, the ARGS one will overwrite
    the PLIST one for the return value.
  - If a style is in ARGS twice, only the last one will be in the returned
    value.
  - If `mis2/style/set' is called multiple times to build a style, only
    the latest key/value pair will be used for the duplicated style key.
"
  ;; Copy the list, feed into `update' for all the logic, then return and done.
  ;; Can't use `apply' or `funcall' because update is a macro...
  (let ((new-plist (copy-sequence plist)))
    (mis2/style/update new-plist args)))
    ;; (mis2/style/update new-plist (-flatten-n 1 args))))



(defmacro mis2/style/update (plist &rest args)
  "Add one or more mis style in ARGS to whatever is already in PLIST.
Updates PLIST in place.

Expects ARGS to be: key0 value0 key1 value1 ...

Takes ARGS, checks them as best as possible for validity, and returns a
mis/style plist.

If a keyword exists twice, the later one 'wins'.
Examples:
  - If a setting already in PLIST is also in ARGS, the ARGS one will overwrite
    the PLIST one for the return value.
  - If a setting is in ARGS twice, only the last one will be in the returned
    value.
  - If `mis2/style/set' is called multiple times to build style, only
    the latest key/value pair will be used for the duplicated setting key.
"
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Surprising-Local-Vars.html#Surprising-Local-Vars
  (let (;;(temp-plist (make-symbol "style-list"))
        (temp-args (make-symbol "style-args")))
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Argument-Evaluation.html#Argument-Evaluation
    `(let (;;(,temp-plist ,plist)
           (,temp-args (list ,@args)))

       ;; §-TODO-§ [2020-03-18]: allow solo keys, like ':center'?
       ;; If &rest is a list of 1 element, strip it out of the list. Probably
       ;; was something like:
       ;;   (mis2/style/update style :interactive)
       ;; Or something that evaluated out to be...
       ;;   (mis2/style/update style nil)
       ;; Which we want to check for here.
       (when (and (not (null ,temp-args))
                  (listp ,temp-args)
                  (= 1 (length ,temp-args)))
         (setq ,temp-args (first ,temp-args)))

       ;; Now look at the args and do the key and value checking.
       (cond
        ;; No args: No style; no worries.
        ((or (null ,temp-args)
             (and (= 1 (length ,temp-args))
                  (null (first ,temp-args))))
         ;; Give them back their list.
         ;; No update to it because no change.
         ,plist)

        ;; Normal Case: check keys and values, add to copy of list, return copy.
        ((listp ,temp-args)
         ;; Our return is our input plist. Need to make sure we set it;
         ;; `plist-put' may or may not update in place.
         ;; Loop over list while we have at least 2 elements left.
         (while (and ,temp-args
                     (listp (cdr ,temp-args)))
           ;; Pop a pair off to process.
           (let* ((key (pop ,temp-args))
                  (value (pop ,temp-args))
                  key-valid
                  value-valid)

             ;; Check our key! This will either return the key or an error
             ;; symbol, so always save.
             (setq key-valid (mis2//style/check-key key))

             ;; Check our value! This will either return the value or an error
             ;; symbol, so always save.
             (setq value-valid (mis2//style/check-value key value))

             ;; And now check for errors.
             (cond
              ;; Complain about the bad thing.
              ((eq key-valid :*bad-key-error*)
               (error "Bad key. %S is not a known mis/style key." key))

              ;; Complain about the bad thing.
              ((eq value-valid :*bad-value-error*)
               (error "Bad value. %S is not a valid value for %S." value key))

              (t
               ;; Use plist-put so we only have one key for this in the plist.
               (setq ,plist (plist-put ,plist key value))
               ))))

         ;; Still have ,temp-args? Didn't end up with a pair - complain?
         (when ,temp-args
           (error "Uneven list; no value for last element: %s" (first ,temp-args)))

         ;; ...and return the list.
         ,plist)

        ;; Otherwise, uh... *shrug*
        (t
         (error
          "Don't know how to process args into style - not a list or nil.")))
       )))


(defun mis2//style/check-key (key)
  "Checks that KEY is a known mis/style keyword.
"
  (mis2//private/check-key key mis2/style/keys nil))
;; (mis2//style/check-key :string)
;; (mis2//style/check-key :center)
;; (mis2//style/check-key nil)


;; '(;; Alignment
;;   (:center :const (t nil))
;;   (:left   :const (t nil))
;;   (:right  :const (t nil))
;;
;;   ;; Text
;;   (:face :key-alist :type) ;; indirect... got to get :type alist, then get
;;                            ;; :face's key out of that.
;;
;;   ;; Box Model...ish Thing.
;;   (:margins (:list (:string :string)))
;;   (:borders (:list (:string :string)))
;;   ;; Can be either, e.g.:
;;   ;;   :padding '(">>" "<<")   ;; use the strings exactly
;;   ;;   :padding '(?- :empty 3) ;; Build from char, leaving 3 empties.
;;   ;;   :padding '(?- :fill 3)  ;; Build from char, to max of 3 long.
;;   (:padding (:or (:list (:string :string))
;;                  (:list (:char (:const (:empty :fill)) :integer)))))

(defun mis2//style/check-value (key value &optional key-info)
  "Checks that VALUE is a valid value for the mis/style keyword KEY.
"
  (mis2//private/check-value key value #'mis2//style/check-key))


;;------------------------------------------------------------------------------
;; Settings / Style Helpers
;;------------------------------------------------------------------------------

(defun mis2//private/check-value (key value check-key-fn &optional key-info)
  "Checks that VALUE is a valid value for the keyword KEY.
"
  ;; sometimes need to override key-info in a recursion (e.g. :or case)
  (let* ((key-info (or key-info (funcall check-key-fn key)))
         (type (and (listp key-info) (first key-info)))
         (value-checker (and (listp key-info) (second key-info)))
         (basic-validity (or (first (alist-get key mis2/validity/types))
                             (first (alist-get type mis2/validity/types))))
         success)

    ;; This may be a bug in check-key rather than a special edge case here...
    (when (eq :const key)
      (setq type :const
            value-checker key-info))

    (cond
     ;; Easy out: is it a bad key?
     ((eq key-info :*bad-key-error*)
           ;; Return bad value if we have a bad key.
      :*bad-value-error*)

     ;; Is it an :or? Got to check each option in value-checker and see
     ;; if any pass.
     ((eq type :or)
      ;; Check each recursively in the value-checker against this value.
      ;; Init to false/failure, then any true/success will pass 'or' test.
      (setq success :*bad-value-error*)
      (dotimes (i (length value-checker))
        ;; Ask sub-us to check this sub-key-info/sub-value.
        (when (not (eq :*bad-value-error*
                       (mis2//private/check-value
                        ;; Pass key and whole value in;
                        ;; only change value-checker.
                        key value check-key-fn
                        ;; override key-info with the or's key-info
                        (nth i value-checker))))
          ;; Any success - passes 'or' test.
          (setq success value)))
      success)

     ;; Is it a basic type? Basic check, or some possible shenanigans needed in
     ;; the :list case(s).
     ((and (not (null basic-validity))
           (functionp basic-validity))
      (setq success (funcall basic-validity value))
      ;; Failed already or don't need to recurse to check members; done.
      (cond ((not success)
             :*bad-value-error*)

            ;; Do we have a list? We need to make sure it conforms
            ;; to its validity requirements.
            ((eq :list type)
             (if (not (= (length value) (length value-checker)))
                 :*bad-value-error*
               ;; basic sanity passed - check each in list
               (setq success value)
               (dotimes (i (length value))
                 ;; If (mis2//style/check-value :margins '("hi" "hello")):
                 ;;  (nth i value)         - "hi" (sub-value)
                 ;;  (nth i value-checker) - :string (sub-key)
                 ;; So ask sub-us to check this sub-key/sub-value.
                 (when (eq :*bad-value-error*
                           (if (and (listp (nth i value-checker))
                                    (keywordp (first (nth i value-checker))))
                               ;; e.g. (:const (:empty :fill)) as a
                               ;; value-checker for a list item.
                               (mis2//private/check-value
                                (first (nth i value-checker))
                                (nth i value)
                                check-key-fn
                                (second (nth i value-checker)))

                             ;; e.g. :string as a value-checker for a list item.
                             (mis2//private/check-value (nth i value-checker)
                                                         (nth i value)
                                                         check-key-fn)))
                   ;; failure - set success to a big nope
                   (setq success :*bad-value-error*)))
               ;; Done checking. `success' is either still the value, or
               ;; it's been replaced with :*bad-value-error*, so return it.
               success))

            ;; Not a list, so no need to loop/recurse... But did succeed,
            ;; so return value
            (t
             value)))

     ;; Otherwise, use key-info to check our value.
     ;; We'll have a cdr list like...:
     ;;   '(:const     '(t nil))
     ;;   '(:key-alist 'mis2/type->faces)
     ;;   '(:range     '(0.0 100.0))
     ;;   '(:or (<option> ...))
     ;; and we have to do the check for the type.
     ((eq :const type)
      (if (memq value value-checker)
          value
        :*bad-value-error*))

     ((eq :range type)
      ;; Range must be a number (float or int) and must be in the range
      ;; specified (inclusive).
      (if (and (numberp value)
               (<= (first value-checker) value)
               (>= (second value-checker) value))
          value
        :*bad-value-error*))

     ;; Find it in the alist? Valid.
     ((eq :key-alist type)
      (if (and (not (null value))
               (not (null value-checker))
               (alist-get value (symbol-value value-checker)))
          value
        :*bad-value-error*))

     ;; *shrugs* Bad key or someone added a key type and didn't code in the
     ;; *handling yet.
     (t
      :*bad-value-error*))))

;; (mis2//style/check-value :string "string-value")
;; (mis2//style/check-value "string-value" :string)
;; (mis2//style/check-value :margins '("hi" "hello"))
;;   padding is:
;;     (:or (:list (:string :string))
;;          (:list (:char (:const (:empty :fill)) :integer)))
;; (mis2//style/check-value :padding '("hi" "hello"))
;; (mis2//style/check-value :padding '(?- :empty 3))


(defun mis2//private/check-key (key valid-key-info valid-meta-key-info)
  "Private helper function for checking keys for mis2/settings and mis2/style.
"
  (let ((info (or (alist-get key valid-key-info)
                  (alist-get key valid-meta-key-info)))
        (basic-validity (first (alist-get key mis2/validity/types))))

    ;; Check basic validity first - it takes care of str, int, etc.
    (cond ((and (not (null basic-validity))
                (functionp basic-validity))
           basic-validity)

          ;; Next, do we have info on the style element (since it
          ;; isn't a basic type)?
          ((null info)
           ;; Already bad - can just return.
           :*bad-key-error*)

           ;; We do have info, so return it...
           ;; We can use the truthiness of the info for a yes/no, and the info
           ;; itself for `check-value', so there's that little sillyness of the
           ;; return.
           (t
            info))))

;;         ------------------------------------------------------------
;;   ------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;                     Version 2, Rough Draft 0 is below.
;;------------------------------------------------------------------------------
;;   ------------------------------------------------------------------------
;;         ------------------------------------------------------------


;; (defun mis2/settings/get (key user-settings &optional mis2-setting)
;;   "Get a mis2 setting based off KEY. Setting either comes from USER-SETTINGS
;; plist or from the appropriate mis2 setting const/var (passed in
;; as MIS2-SETTING).
;; "
;;   ;; Simply return value from user-settings or mis2-setting, preferring
;;   ;; user-settings. Only complication is if user-settings specifies a nil, so we
;;   ;; have to check that key is a member of user-settings...
;;   (if (plist-member user-settings key)
;;       (plist-get user-settings key)
;;     mis2-setting))

;; (defun mis2/setting/get-with-default (arg default)
;;   "Figures out actual ARG by looking at ARG and DEFAULT. It will
;; be ARG if ARG is non-nil and either symbolp or functionp. Else it
;; will look at the value of DEFAULT and use either that symbol, or
;; call that function to get a symbol."
;;   (cond
;;    ;; pass through - function
;;    ((and (not (null arg))
;;          (functionp arg))
;;     (funcall arg))
;;    ;; pass through - symbol
;;    ((and (not (null arg))
;;          (symbolp arg))
;;     ;; if it has a value, use that, else use directly
;;     (if (symbol-value arg)
;;           (symbol-value arg)
;;       arg))

;;    ;; default - function to get current
;;    ((and (not (null default))
;;          (functionp default))
;;     (funcall default))
;;    ;; default - symbol as default
;;    ((symbolp default)
;;     ;; if it has a value, use that, else use directly
;;     (if (and (not (null default))
;;              (symbol-value default))
;;           (symbol-value default)
;;       default))

;;    ;; fallback to something drastic-ish
;;    (t
;;     :error)))
;; ;; (mis2/setting/get-with-default nil mis2/debug/type)

;; (defun mis2/settings/put (key value list)
;;   "Puts VALUE into LIST under KEY, after verifying KEY is a valid mis2 setting.
;; "
;;   (if (memq key mis2/settings/keys)
;;       (plist-put list key value)
;;     (error "Key %S not a valid mis2/settings key: %S"
;;            key
;;            mis2/settings/keys)))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-settings)
