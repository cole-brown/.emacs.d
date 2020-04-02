;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Mis2 Naming:
;;   namespace name:   "mis2"
;;   public:           mis2/...
;;   private:          mis2//...
;;
;; In a function, 'mis2-' vs 'mis2--' refers to a var holding a public-related
;; symbol-name or value vs one holding a private symbol-name
;; or value.


;; §-TODO-§ [2020-03-25]: move these somewhere better?
;; Simplest Example:
;;   (mis2/message "hello there")
;;
;;
;; "Full" Example:
;;   (mis2/message :settings (mis2/settings :echo t :theme :default)
;;                 :style (mis2/settings/style
;;                 message symbol0)))
;;
;;
;; Complex Example:
;;   (let ((symbol0 "test")
;;         (symbol1 '(thing1 thing2))
;;         (settings (mis2/settings :echo t :theme :default))
;;         style
;;         (message "test %S"))
;;
;;     ;; Put '(:center nil) into mis/settings plist on symbol 'message.
;;     (mis2/settings/style/update style
;;                                 :center nil)
;;     (mis2/settings/style/update style
;;                                 :margins '(">>" "<<"))
;;     (mis2/settings/style/update style
;;                                 :borders '("|" "|"))
;;     (mis2/settings/style/update style
;;                                 :padding '(?- :empty 3))
;;     (mis2/settings/style/update style
;;                                 :face :title)
;;     ;; Output messages with settings and style lists.
;;     (mis2/message :settings settings :style style message symbol0)
;;     (mis2/message :settings settings
;;                   :style style
;;                   "test 2: "
;;                   ;; Think I need a tag to separate a recursion level from,
;;                   ;; say, format args. So... `:r'? `:recurse'? `:mis'?
;;                   :mis '(:style (mis2/settings/style style :face :attention)
;;                                 "%S" symbol1)))
;; §-TODO-§ [2020-03-25]: that last one is maybe probably wrong


;;----------------------------------...---...-----------------------------------
;;--                   We're all fine here... How are you?                    --
;;------------------------------------------------------------------------------


(require 'dash)
(require 's)

(require 'mis2-settings)
(require 'mis2-contents)


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


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
           :mis2//contents remaining-args)
"
  ;; Use 'contents' in this function - we'll be changing it and want to look at
  ;; the latest, not at original args.
  (let ((contents args)
        settings style done)
    (while (not done)
      ;; We require our keys to go first, so just check the first element.
      (if (and (keywordp (first contents))
               (memq (first contents) mis2/custom/keywords))
          ;; Get keyword (if it's our keyword) and val; save to settings/style.
          (-if-let (mis2--kwd (plist-get mis2/custom/keywords (first contents)))
              (let ((mis2-val (plist-get contents (first contents))))
                ;; settings and style should update their lists, and drop
                ;; key/value from contents.
                (cond ((eq mis2--kwd :mis2//settings)
                       (setq settings mis2-val
                             contents (-drop 2 contents)))
                      ((eq mis2--kwd :mis2//style)
                       (setq style mis2-val
                             contents (-drop 2 contents)))))
            ;; Else, didn't find /our/ keyword at front of list. Done parsing.
            (setq done t))

        ;; Else, didn't find a keyword at front of list. Done parsing.
        (setq done t)))

    ;; and now return a tuple of parsed args.
    (list :mis2//settings settings
          :mis2//style style
          :mis2//contents contents)))


;;------------------------------------------------------------------------------
;; Send mis2 message to outputs.
;;------------------------------------------------------------------------------

(defun mis2//message/output (plist)
  "Output message described by mis2 PLIST. Key `:mis2//message' must
exist in PLIST.

Pipeline for sink of settings:
  :buffer, :echo, :echo-delay
"
  (if-let ((message (mis2//data/get :mis2//message plist)))
      ;; Found a message so let's try to output it.
      (progn
        (mis2//message/output/to-buffer message plist)
        (mis2//message/output/to-minibuffer message plist))

    ;; Error out if not found.
    (error "No finalized mis2 message supplied. %S" plist)))


;; Some ideas from https://emacs.stackexchange.com/a/20178
(defun mis2//message/output/to-buffer (mis2-msg plist)
  "Outputs MIS2-MSG to buffer. Will look in PLIST for any
mis2 settings, style, or data stuff it needs (that is, PLIST
is expected to be null or a mis2 plist).

Final sink for settings:
  :buffer
"
  ;; §-TODO-§ [2020-03-24]: check for buffers keyword, use that if exists.
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
        (insert mis2-msg)
        ;; Get off our line and ready for next message into the buffer.
        (insert "\n")))))
;; (mis2//message/output/to-buffer "hi" '(:mis2//settings (:echo t :echo-delay 2)))


;; Some ideas from https://emacs.stackexchange.com/a/20178
(defun mis2//message/output/to-minibuffer (mis2-msg plist)
  "Outputs MIS2-MSG to mini-buffer/echo-area. Will look in PLIST
for any mis2 settings, style, or data stuff it needs (that is,
PLIST is expected to be null or a mis2 plist).

Final sink for settings:
  :echo
  :echo-delay
"
  ;; Message required; echo setting required.
  ;; Message string goes into echo area if both these are non-nil.
  (when (and mis2-msg
             (mis2//settings/get/from-data :echo plist))

      ;; Temporarily bind `message-log-max' to nil so it doesn't go into the
      ;; *Messages* buffer from here too, which would cause our first message
      ;; to lose its properties, somehow, probably due to *Messages* stacking
      ;; identical messages.
      (let ((message-log-max nil)
            (minibuffer-message-timeout
             (mis2//settings/get/from-data :echo-delay plist)))
        (minibuffer-message mis2-msg))))
;; (mis2//message/output/to-minibuffer "hi" '(:mis2//settings (:echo t :echo-delay 2)))


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
  (let ((plist (apply #'mis2//message/parse args)))
    (mis2//message/output (mis2//contents plist))))


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
