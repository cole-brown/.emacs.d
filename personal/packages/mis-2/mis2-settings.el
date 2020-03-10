;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------...Well Fancy Simple Messages Aren't All That Simple So...-----------
;;--                                Settings                                  --
;;------------------------------------------------------------------------------



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
    (:batch :const nil))
  "10,000 foot view: calls are either intended to be in an interactive manner,
or as automatic output flung out as soon as it's reached. The main difference is
how long the echo area timeout is changes between the two.

Settings:
;; §-TODO-§ [2020-03-06]: explain each of these
  :interactive
  :batch
")


(defconst mis2/settings/keys
  '((:echo       :const     '(t nil))
    (:echo-delay :float     '(0.0 100.0))
    (:type       :key-alist 'mis2/type->faces)
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
;; Settings Layout
;;------------------------------------------------------------------------------

;; '(:echo t :echo-delay 0.1 :type :default)

;; More examples? already explained well enough in doc strings?
;; Actual runnable code snippets?
;;   - This sounds good...


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun mis2/settings (&rest args)
  "Expects args to be a plist of:
  (:setting-keyword value :setting-keyword2 value2 ...)

Takes ARGS, checks them as best as possible for validity, and returns a
mis/settings plist.

If a keyword exists twice, the later one in the list wins.
"
  (cond
   ;; No args: No settings; no worries.
   ((null args)
    nil)

   ;; Normal Case: Check out this list.
   ((listp args)
    (let (settings) ;; our return value
      ;; Loop over list while we have at least 2 elements left.
      (while (and args
                  (listp (cdr args)))
        ;; Pop a pair off to process.
        (let* ((key (pop args))
               (value (pop args)))

          ;; Check our key! This will either return the key or an error
          ;; symbol, so always save.
          (setq key (mis2/settings/check-key key))

          ;; Check our value! This will either return the value or an error
          ;; symbol, so always save.
          (setq value (mis2/settings/check-value value))

          ;; And now check for errors.
          (cond
           ;; Complain about the bad thing.
           ((eq key :*bad-key-error*)
            (error "Bad key. %S is not a known mis/settings key." key))

           ;; Complain about the bad thing.
           ((eq key :*bad-value-error*)
            (error "Bad value. %S is not a valid value for %S." value key))

          (t
            ;; Use plist-put so we only have one key for this in the plist.
            (setq settings (plist-put settings key value))))))

      ;; Still have args? Didn't end up with a pair - complain?
      (when args
        (error "Uneven list; no value for last element: %s" (first args)))))

   ;; Otherwise, uh... *shrug*
   (t
    (error
     "Don't know how to process args into settings - not a list or nil."))))
;; (mis2/settings :jeff "jill")


(defun mis2/setings/check-key (key)
  "Checks that KEY is a known mis/settings keyword.
"
  (cond ((memq key mis2/settings/meta/keys)
         (alist-get key mis2/settings/meta/keys))

        ((memq key mis2/settings/keys)
         (alist-get key mis2/settings/keys))

        (t
         :*bad-key-error*))
  :*bad-key-error*)

(defun mis2/setings/check-value (key value)
  "Checks that VALUE is a valid value for the mis/settings keyword KEY.
"
  (let ((key-info (mis2/settings/check-key key)))
    (if (eq key-info :*bad-key-error*)
        ;; Return bad value if we have a bad key.
        :*bad-value-error*

      ;; Otherwise, use key-info to check our value.
      ;; We'll have a cdr list like...:
      ;;   '(:const     '(t nil))
      ;;   '(:float     '(0.0 100.0))
      ;;   '(:key-alist 'mis2/type->faces)
      ;;   '(:string)
      (cond ((eq :const (first key-info))
             )
            ((eq :float (first key-info))
             )
            ((eq :key-alist (first key-info))
             )
            ((eq :string (first key-info))
             )
      ))
  :*bad-value-error*)


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



;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-settings)
