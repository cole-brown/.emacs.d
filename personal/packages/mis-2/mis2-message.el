;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Mis2 Naming:
;;   namespace name:   "mis2"
;;   public:           mis2/...
;;   private/internal: mis2//...
;;
;; In a function, 'mis2-' vs 'mis2--' refers to a var holding a public-related
;; symbol-name or value vs one holding a private/internal related symbol-name
;; or value.

;;----------------------------------...---...-----------------------------------
;;--                   We're all fine here... How are you?                    --
;;------------------------------------------------------------------------------


(require 'dash)
(require 's)

(require 'mis2-parts)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Formatting?
;;------------------------------------------------------------------------------

;; Should I keep what I had in mis1?
;;  - No. That was one of the reasons for mis2.
;; Try to do something super smart?
;; Imitate another markup language?
;; Be lispy when trying to imitate another markup language?


;; §-TODO-§ [2020-03-23]: move this to a comment or docstring...

;; Formatting ideas?
(while nil
  ;; LispyML:
  (let ((symbol0 "test")
        (symbol1 '(thing1 thing2))
        (settings '(:echo t :type :default)))
    (mis2/message/propertize
     settings
     '(("test %S" symbol0 :face :title) ;; format w/ face from :default :title.
       :center  nil             ;; center (nil == whatever fill-column is)
       :margins '(">>" "<<")    ;; Use these strings for center's margins
       :borders '("|" "|")      ;; Use these strings for center's borders
       :padding '(?- :empty 3)  ;; Use '-' character for filling padding,
                                ;; leave 3 empty spaces before/after
                                ;; centered text.
       )))
  ;; Would have to be smart to tell what's message, what's formatting args,
  ;; what's settings, what's styling, etc...

  ;; Html-ish?
  ;; Terrible on the auto-indenting... :|
  (let ((symbol0 "test")
        (symbol1 '(thing1 thing2))
        (settings '(:echo t :type :default)))
    (mis2/message/propertize
     settings

     '(:center nil             ;; center (nil == whatever fill-column is)
               (:margins '(">>" "<<")    ;; Use these strings for center's margins
                         (:borders '("|" "|")      ;; Use these strings for center's borders
                                   (:padding '(?- :empty 3)  ;; Use '-' character for filling padding,
                                             ;; leave 3 empty spaces before/after
                                             ;; centered text.
                                             ("test %S" symbol0 :face :title) ;; format w/ face from :default :title.
                                             ))))))

  ;; Html-ish v2
  (let ((symbol0 "test")
        (symbol1 '(thing1 thing2))
        (settings '(:echo t :type :default))
        (message "test %S"))

    ;; Put '(:center nil) into mis/settings plist on symbol 'message.
    (mis2/message/style message
                        :center nil)
    (mis2/message/style message
                        :margins '(">>" "<<"))
    (mis2/message/style message
                        :borders '("|" "|"))
    (mis2/message/style message
                        :padding '(?- :empty 3))
    (mis2/message/style message
                        :face :title)
    ;; Output message now that it has property list all set up on it?
    (mis2/message settings message symbol0))

  ;; Html-ish v3?
  (let ((symbol0 "test")
        (symbol1 '(thing1 thing2))
        (settings (mis2/settings :echo t :type :default))
        style
        (message "test %S"))

    ;; Put '(:center nil) into mis/settings plist on symbol 'message.
    (mis2/settings/style style
                         :center nil)
    (mis2/settings/style style
                        :margins '(">>" "<<"))
    (mis2/settings/style style
                        :borders '("|" "|"))
    (mis2/settings/style style
                        :padding '(?- :empty 3))
    (mis2/settings/style style
                        :face :title)
    ;; Output messages with settings and style lists.
    (mis2/message :settings settings :style style message symbol0)
    (mis2/message :settings settings
                  :style style
                  "test 2: %S" symbol1))

  ;; But I need to be able to do multiple styles on one output line...

  ;; Html-ish v3?
  (let ((symbol0 "test")
        (symbol1 '(thing1 thing2))
        (settings (mis2/settings :echo t :type :default))
        style
        (message "test %S"))

    ;; Put '(:center nil) into mis/settings plist on symbol 'message.
    (mis2/settings/style/update style
                                :center nil)
    (mis2/settings/style/update style
                                :margins '(">>" "<<"))
    (mis2/settings/style/update style
                                :borders '("|" "|"))
    (mis2/settings/style/update style
                                :padding '(?- :empty 3))
    (mis2/settings/style/update style
                                :face :title)
    ;; Output messages with settings and style lists.
    (mis2/message :settings settings :style style message symbol0)
    (mis2/message :settings settings
                  :style style
                  "test 2: "
                  ;; Think I need a tag to separate a recursion level from, say,
                  ;; format args. So... `:r'? `:recurse'? `:mis'?
                  :mis '(:style (mis2/settings/style style :face :attention)
                                "%S" symbol1))
    ;; So... I want a non-destructive `mis2/settings/style' for this one.

    ;; This version would also let me hang on to settings/styles and just use
    ;; them all over.

    ;; lazy version?
    (mis2/message :settings (mis2/settings :echo t :type :default)
                  :style (mis2/settings/style
                  message symbol0))))


;;------------------------------------------------------------------------------
;; Pop settings and style off front of list.
;;------------------------------------------------------------------------------

(defun mis2//message/parse (&rest args)
  "Parses args, looking for `:settings' and `:style' keys.
 - If `:settings' key is in ARGS, the next element is mis2 settings.
 - If `:style' key is in ARGS, the next element is mis2 style.

These keys must be at the front of the list - before any message args.
  Examples:
    Valid:
      (mis2/message \"hello there\")
      (mis2/message \"%S\" 'symbol)
      (mis2/message :settings 'sets :style 'styles \"hello there\")
      (mis2/message :style 'styles :settings 'sets  \"hello there\")
      (mis2/message :settings nil :style 'styles \"hello there\")
      (mis2/message :settings nil \"hello there\")
      etc.
    Invalid:
      (mis2/message \"hello there\" :settings nil)
      (mis2/message \"%S %S\" :settings 'symbol)
      etc.

Returns: '(:mis2//settings settings-element
           :mis2//style    style-element
           :mis2//parts    remaining-args)
"
  ;; Use 'parts' in this function - we'll be changing it and want to look at the
  ;; latest, not at original args.
  (let ((parts args)
        settings style done)
    (while (not done)
      ;; We require our keys to go first, so just check the first element.
      (if (and (keyword (first parts))
               (memq (first parts) mis2/custom/keywords))
          ;; Get keyword (if it's our keyword) and val; save to settings/style.
          (-if-let (mis2--kwd (plist-get (first parts) mis2/custom/keywords))
              (let ((mis2-val (plist-get (first parts) parts)))
                ;; settings and style should update their lists, and drop
                ;; key/value from parts.
                (cond ((eq mis2--kwd :mis2//settings)
                       (setq settings mis2-val
                             parts (-drop 2 parts)))
                      ((eq mis2--kwd :mis2//style)
                       (setq style mis2-val
                             parts (-drop 2 parts)))))
            ;; Else, didn't find /our/ keyword at front of list. Done parsing.
            (setq done t))

        ;; Else, didn't find a keyword at front of list. Done parsing.
        (setq done t)))

    ;; and now return a tuple of parsed args.
    '(:mis2//settings settings
      :mis2//style style
      :mis2//parts parts)))


;;------------------------------------------------------------------------------
;; Pretty Messages in *Messages* Buffer.
;;------------------------------------------------------------------------------

(defun mis2/message (&rest args)
  "Build ARGS into a propertized string and output it to the *Messages* buffer.

If `:settings' key is in ARGS, the next element is mis2 settings.
If `:style' key is in ARGS, the next element is mis2 style.
These keys must be at the front of the list - before any message args.
  Examples:
    Valid:
      (mis2/message \"hello there\")
      (mis2/message \"%S\" 'symbol)
      (mis2/message :settings 'sets :style 'styles \"hello there\")
      (mis2/message :style 'styles :settings 'sets  \"hello there\")
      (mis2/message :settings nil :style 'styles \"hello there\")
      (mis2/message :settings nil \"hello there\")
      etc.
    Invalid:
      (mis2/message \"hello there\" :settings nil)
      (mis2/message \"%S %S\" :settings 'symbol)
      etc.
"

    )


;;------------------------------------------------------------------------------
;; Main Entry Point?
;;------------------------------------------------------------------------------

(defun mis2/message/propertize (echo type &rest args)
  "Given TYPE, figure out a faces alist from `mis2/type->faces' to use, then
build ARGS into propertized string via `mis2/parts/build' and output it
to the *Messages* buffer.

If ECHO is non-nil, also echo message to the minibuffer echo area.

NOTE: Could (optionally) add TYPE to output easily enough if desired.
"
  (if-let ((faces (nth 1 (assoc type mis2/type->faces)))
           ;; If args was a single list and got boxed by '&rest', unbox.
           ;; e.g. '(:text "hi") -&rest-> '((:text "hi")) -unbox-> '(:text "hi")
           (args (or (and (listp args)
                          (= (length args) 1)
                          (-flatten-n 1 args))
                     args))
           ;; Also need to check for a full unboxing...
           ;; e.g. :newline -&rest-> '(:newline) -unbox-> :newline
           (args (or (and (listp args)
                          (= (length args) 1)
                          (nth 0 args))
                     args))
           ;; null check
           (valid-args (not (null args))))

      ;; Build propertized string and output it.
      (mis2/message/preserve-properties echo (mis2/parts/build args faces))

    ;; Else didn't find in type in type->faces or null args.
    ;; Complain, return nil.
    (mis2/warning
     type :warning
     (concat "Null args (%S)? Or type not found in "
             "`mis2/type->faces': %S -> %S")
     args type mis2/type->faces)
    nil))
;; (mis2/message/propertize t '(mis2 koan text) '(:text "hi"))
;; (mis2/message/propertize nil '(mis2 koan text) :newline)
;; (mis2/message/propertize t '(mis2 koan text) :text "hi %s" "there")
;; (mis2/message/propertize t '(spydez homeward) "  LSP: Killed %s servers." 1) ;; err: no type



;;------------------------------------------------------------------------------
;; Pretty Messages in *Messages* Buffer?
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-11-01]:
;;   - add a mis2 setting for whether to allow also-echo?

;; §-TODO-§ [2020-02-05]: Need a dynamically adjustable timeout, I think...
;;   - 0 is good for non-interactive startup and funcs that have output?
;;     - But last output would be nice to not lose?
;;   - 2 is good for normal (interactive) usage?

;; https://emacs.stackexchange.com/a/20178
(defun mis2/message/preserve-properties (echo format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
M-x list-faces-display for all defined faces. Call with a propertized string.

If ECHO is non-nil, also echo message to the minibuffer echo area.
"
  (let ((output (apply 'format format args)))
    (with-current-buffer (get-buffer "*Messages*")
      ;;   "Manually inserts the propertized string at the end of the messages
      ;; buffer by lexically-binding inhibit-read-only to t in
      ;; the message buffer."
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          ;; Before insert, do a newline if needed.
          (unless (zerop (current-column)) (insert "\n"))
          ;; Actual propertized string put into *Messages* buffer
          (insert output)
          ;; And... insert a final newline if needed.
          (insert "\n"))))

    ;; String also goes into echo area, maybe.
    (if echo
        ;; But temp bind `message-log-max' to nil so it doesn't go into the
        ;; *Messages* buffer from here too, which would cause our first message
        ;; to lose its properties, somehow, probably due to *Messages* stacking
        ;; identical messages.
        (let ((message-log-max nil)
              (minibuffer-message-timeout
               (first mis2/message/echo-area-timeout)))
          (minibuffer-message output)))))

(dotimes (i 10)
  (minibuffer-message ["hi %s"] i))
;; (mis2/message/preserve-properties t (propertize "--->" 'face 'underline))
;; (mis2/message/preserve-properties nil (propertize "--->" 'face 'underline))
;; (mis2/message/preserve-properties t
;;  (concat
;;   (propertize "--->    " 'face 'font-lock-variable-name-face)
;;   " "
;;   (propertize "├" 'face 'font-lock-comment-delimiter-face)
;;   " "
;;   (propertize "(mis2 zeroth debug)" 'face 'font-lock-comment-face)
;;   " "
;;   (propertize "┤:" 'face 'font-lock-comment-delimiter-face)
;;   " "
;;   (propertize "early-init.el... Zeroth step." 'face 'default)))



;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-11]: Try font-lock mode instead of string properties?
;; Could add auto detect of my file names, maybe.
;; And other "arrorws"...
;; and 'require'...
;;
;; (mis2/hook/defun example-hook t
;;     nil "simple-list" "init/config/configure-jeff.el"
;;   "Nice up simple lists - replacing hypen with a unicode middle dot."
;;   (font-lock-add-keywords
;;    nil ;; if in a derived mode, doing font lock in a hook could be easier...
;;    '(("^ *\\([-]\\) "
;;       (0 (prog1 () (compose-region (match-beginning 1)
;;                                    (match-end 1) "•")))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-message)
