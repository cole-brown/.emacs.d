;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------...Well Fancy Simple Messages Aren't All That Simple So...-----------
;;--                                Settings                                  --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Customization, Consts, Vars, et ceteras...
;;------------------------------------------------------------------------------

;;---
;; Customization
;;---
;; Consts that we want available to the user to change.



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


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defun mis2/settings/set (plist &rest args)
  "Add one or more mis settings in ARGS to whatever is already in PLIST.
Returns new list (PLIST will be unchanged).

Expects ARGS to be: key0 value0 key1 value1 ...
E.G.
  (mis2/settings/set set-list :type value0 :echo value2 ...)
  args == (:type value0 :echo value2 ...)

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
  (apply #'mis2/settings/update (copy-sequence plist) args))
;; (let (settings) (mis2/settings/set settings :jeff "jill"))
;; (let (settings) (mis2/settings/set settings :echo t))
;; (mis2/settings/set nil :echo t)


(defmacro mis2/settings/update (plist &rest args)
  "Add one or more mis settings in ARGS to whatever is already in PLIST.
Updates PLIST in place.

Expects ARGS to be: key0 value0 key1 value1 ...
E.G.
  (mis2/settings/set set-list :type value0 :echo value2 ...)
  args == (:type value0 :echo value2 ...)

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
  (message "u2 inputs: %S %S null?%S" plist args (null plist))
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Surprising-Local-Vars.html#Surprising-Local-Vars
  (let (;;(temp-plist (make-symbol "settings-list"))
        (temp-args (make-symbol "settings-args")))
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Argument-Evaluation.html#Argument-Evaluation
    `(let (;;(,temp-plist ,plist)
           (,temp-args (list ,@args)))
       (message "u2 plist: %S" ,plist)
       (message "u2 temp-args: %S" ,temp-args)

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
             (setq key-valid (mis2/settings/check-key key))

             ;; Check our value! This will either return the value or an error
             ;; symbol, so always save.
             (setq value-valid (mis2/settings/check-value key value))

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
               (message "settings: %S = %S => %S" key value ,plist)
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



(defun mis2/settings/check-key (key)
  "Checks that KEY is a known mis/settings keyword.
"
  (let ((info (or (alist-get key mis2/settings/meta/keys)
                  (alist-get key mis2/settings/keys))))
    (if (null info)
        ;; Already bad - can just return.
        :*bad-key-error*

      ;; Have info for checking validity... of value. Nothing left to check, I
      ;; think, for validity of key. It exists, so ok; go. However... we can use
      ;; the truthiness of the info for a yes/no, and the info itself for
      ;; `check-value', so there's that little sillyness of the return.
      info)))


(defun mis2/settings/check-value (key value)
  "Checks that VALUE is a valid value for the mis/settings keyword KEY.
"
  (let* ((key-info (mis2/settings/check-key key))
         (type (and (listp key-info) (first key-info)))
         (value-checker (and (listp key-info) (second key-info))))

    (if (eq key-info :*bad-key-error*)
        ;; Return bad value if we have a bad key.
        :*bad-value-error*

      ;; Otherwise, use key-info to check our value.
      ;; We'll have a cdr list like...:
      ;;   '(:const     '(t nil))
      ;;   '(:range     '(0.0 100.0))
      ;;   '(:number    #'numberp)
      ;;   '(:integer   #'floatp)
      ;;   '(:float     #'integerp)
      ;;   '(:string    #'stringp)
      ;;   '(:key-alist 'mis2/type->faces)
      (cond ((eq :const type)
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

            ;; Find it in the list? Valid.
            ((eq :key-alist type)
             (if (and (not (null value))
                      (not (null value-checker))
                      (alist-get value (symbol-value value-checker)))
                 value
               :*bad-value-error*))

            ;; If get more that're just funcs, just add to the list.
            ((memq type '(:number :integer :float :string))
             ;; Check value by using predicate function.
             (if (and (functionp value-checker)
                      (apply value-checker value))
                 value
               :*bad-value-error*))

            ;; *shrugs*
            (t
             :*bad-value-error*)))))


;;------------------------------------------------------------------------------
;; Style
;;------------------------------------------------------------------------------

(defun mis2/style/set (plist &rest args)
  "Add one or more mis styles in ARGS to whatever is already in PLIST.
Returns new list (PLIST will be unchanged).
"

  )


(defun mis2/style/update (plist &rest args)
  "Add one or more mis styles in ARGS to whatever is already in PLIST.
Updates PLIST in place.
"

  )


;;         ------------------------------------------------------------
;;   ------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;                     Version 2, Rough Draft 0 is below.
;;------------------------------------------------------------------------------
;;   ------------------------------------------------------------------------
;;         ------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-03-09]: Is this true? Or should they also be here for less
;; confusion?
;;
;; Package-level, or overall, mis settings are elsewhere (e.g. mis.el). This is
;; for the more fine-grain stuff. Setting up what font, echo setting, what have
;; you, then passing into a few messages during whatever function you're doing.


;; mis per-output settings are a plist passed into the functions. Build them
;; yourself or use these functions to help.


;;------------------------------------------------------------------------------
;; Customization
;;------------------------------------------------------------------------------

;; Consts that we want available to the user to change.
;; (defcustom mis2/settings
;;   '((:interactive (


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Settings Layout
;;------------------------------------------------------------------------------

;; '(:echo t :echo-delay 0.1 :type :default)

;; More examples? already explained well enough in doc strings?
;; Actual runnable code snippets?
;;   - This sounds good...


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------


(defun mis2/settings/get (key user-settings &optional mis2-setting)
  "Get a mis2 setting based off KEY. Setting either comes from USER-SETTINGS
plist or from the appropriate mis2 setting const/var (passed in
as MIS2-SETTING).
"
  ;; Simply return value from user-settings or mis2-setting, preferring
  ;; user-settings. Only complication is if user-settings specifies a nil, so we
  ;; have to check that key is a member of user-settings...
  (if (plist-member user-settings key)
      (plist-get user-settings key)
    mis2-setting))

(defun mis2/setting/get-with-default (arg default)
  "Figures out actual ARG by looking at ARG and DEFAULT. It will
be ARG if ARG is non-nil and either symbolp or functionp. Else it
will look at the value of DEFAULT and use either that symbol, or
call that function to get a symbol."
  (cond
   ;; pass through - function
   ((and (not (null arg))
         (functionp arg))
    (funcall arg))
   ;; pass through - symbol
   ((and (not (null arg))
         (symbolp arg))
    ;; if it has a value, use that, else use directly
    (if (symbol-value arg)
          (symbol-value arg)
      arg))

   ;; default - function to get current
   ((and (not (null default))
         (functionp default))
    (funcall default))
   ;; default - symbol as default
   ((symbolp default)
    ;; if it has a value, use that, else use directly
    (if (and (not (null default))
             (symbol-value default))
          (symbol-value default)
      default))

   ;; fallback to something drastic-ish
   (t
    :error)))
;; (mis2/setting/get-with-default nil mis2/debug/type)

(defun mis2/settings/put (key value list)
  "Puts VALUE into LIST under KEY, after verifying KEY is a valid mis2 setting.
"
  (if (memq key mis2/settings/keys)
      (plist-put list key value)
    (error "Key %S not a valid mis2/settings key: %S"
           key
           mis2/settings/keys)))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-settings)
