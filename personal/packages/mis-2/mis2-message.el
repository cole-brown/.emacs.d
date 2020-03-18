;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;----------------------------------...---...-----------------------------------
;;--                   We're all fine here... How are you?                    --
;;------------------------------------------------------------------------------


(require 'subr-x)

(require 'mis2-parts)

;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


;; 0: Emacs ready in 13.52 seconds with 8 garbage collections.
;; 2: Emacs ready in 13.34 seconds with 8 garbage collections.
(defcustom mis2/message/echo-area-timeout '(2 2)
  "List of 2 numbers for minibuffer echo area timeout.

First element: timeout during initialization or batch type commands.
Second element: timeout during normal running or for interactive type commands.

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


;;------------------------------------------------------------------------------
;; Formatting?
;;------------------------------------------------------------------------------

;; Should I keep what I had?
;; Try to do something super smart?
;; Imitate another markup language?
;; Be lispy when trying to imitate another markup language?

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
                  message symbol0)
    )
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
              (minibuffer-message-timeout (first mis2/message/echo-area-timeout)))
          (minibuffer-message output)))))

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
