;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;xx  ;;------------------------------
;;xx  ;; Look/Feel?
;;xx  ;;------------------------------
;;xx
;;xx  So how do we want a message to look?
;;xx  If:
;;xx    symbol0 == "s1"
;;xx    symbol1 == 42
;;xx    value0 == "a string"
;;xx    current-init-step == '(jeff config gravity)
;;xx
;;xx  ;; Super basic is super easy.
;;xx  (mis3/message "hello")
;;xx    --> "hello"
;;xx
;;xx  ;; Easy to throw together something rudimentary.
;;xx  (mis3/message "debugging: " 'symbol1 'symbol2 vaule)
;;xx    --> "debugging: symbol1 symbol2 a string"
;;xx
;;xx  ;; Like python f-strings?
;;xx  (mis3/message '(:scope-fmt "debugging: a: {symbol1}, b: {symbol2}, c: {value}"))
;;xx    or
;;xx  (mis3/message '(:s "debugging: a: {symbol1}, b: {symbol2}, c: {value}"))
;;xx    --> "debugging: a: s1, b: 42, c: a string"
;;xx
;;xx  ;; Able to output multiple separately propertized sections per
;;xx  ;; line/message/whatever.
;;xx  (mis3/message '((:face 'arrows "--->")
;;xx                  (:face 'step current-init-step)
;;xx                  (:face 'text "Doing a thing...")))
;;xx    :: propertized:
;;xx    --> "---> (jeff config gravity) Doing a thing..."
;;xx
;;xx  ;; Like python string.format()?
;;xx  (mis3/message '(:var-fmt "{} |{}|: {}"
;;xx                     (:face 'arrows "--->")
;;xx                     (:face 'step current-init-step)
;;xx                     (:face 'text "Doing a thing...")))
;;xx    or
;;xx  (mis3/message '(:f "{} |{}|: {}"
;;xx                     (:face 'arrows "--->")
;;xx                     (:face 'step current-init-step)
;;xx                     (:face 'text "Doing a thing...")))
;;xx    :: propertized:
;;xx    --> "---> |(jeff config gravity)|: Doing a thing..."
;;xx
;;xx  ;; Some easy way to columize output lines?
;;xx  (mis3/message '(:tabbed "{}: {}"  ;; :t?
;;xx                          ("Your stuff")
;;xx                          (symbol0 symbol1 value0 current-init-step)))
;;xx    or
;;xx  (mis3/message '(:tabbed '("{}" :tab ": {}")
;;xx                          ("Your stuff")
;;xx                          (symbol0 symbol1 value0 current-init-step)))
;;xx    --> "Your stuff: s1"
;;xx        "          : 42"
;;xx        "          : a string"
;;xx        "          : (jeff config gravity)"
;;xx
;;xx  ;; Some easy way to make prettier things using something similar to ye old CSS
;;xx  ;; box model (by specifying margin, border, padding, etc)?
;;xx  (mis3/message '(:boxed "hi"))  ;; :b?
;;xx    --> "+----+"
;;xx        "| hi |"
;;xx        "+----+"
;;xx
;;xx  ;; Helpers built off of previous stuff!
;;xx
;;xx  ;; debugging...
;;xx  (mis3/debug ...)
;;xx    -> Can I prepend file name, function name?
;;xx
;;xx  ;; comments...
;;xx  ;; ...
;;xx
;;xx  ;; koans...
;;xx  ;; ...
;;xx
;;xx  ;;------------------------------
;;xx  ;; Basically...
;;xx  ;;------------------------------
;;xx  ;;
;;xx  ;;  - Take a list.
;;xx  ;;    - If not given a list, turn all given into one list (we'll assume it was a
;;xx  ;;      simpler type of thing?).
;;xx  ;;  - List may or may not start with a mis3 keyword.
;;xx  ;;  - We may have to recurse.
;;xx  ;;    - e.g. format-this-list may have lists of propertize-these-things
;;xx
;;xx  ;;------------------------------
;;xx  ;; mis2 vs mis3
;;xx  ;;------------------------------
;;xx  ;;
;;xx  ;; mis2 was built to be as accepting of inputs as possible.
;;xx  ;; That was probably a mistake because I have no idea how to use it and it's not
;;xx  ;; even done...
;;xx  ;;
;;xx  ;; Restrict this (much?) more.
;;xx  ;;
;;xx  ;; Maybe force using some helper functions to format things before sending
;;xx  ;; to mis3/message?
;;xx  ;;
;;xx  ;; And maybe don't force picking up a new string formatting language?
;;xx  ;;   - Bonus: Don't have to write it.
;;xx
;;xx  ;; So... complex one?
;;xx  (mis3/message :settings m3/sett
;;xx                (mis3/boxed :style m3/box
;;xx                            "hi")
;;xx                :newline
;;xx                (mis3/column :style m3/style
;;xx                             '("%s" :tab ": %s")
;;xx                             '("Your stuff")
;;xx                             '(symbol0 symbol1 value0 current-init-step)))
;;xx    --> "+----+"
;;xx        "| hi |"
;;xx        "+----+"
;;xx        ""
;;xx        "Your stuff: s1"
;;xx        "          : 42"
;;xx        "          : a string"
;;xx        "          : (jeff config gravity)"
;;xx
;;xx  ;; Annoyingly, that looks like a macro...
;;xx  ;; Never had success with complicated macros yet...
;;xx
;;xx  top level:
;;xx    mis3/message
;;xx    -> Outputs to buffer.
;;xx
;;xx  second level:
;;xx    mis3/simple
;;xx    mis3/format
;;xx    mis3/boxed
;;xx    mis3/column
;;xx    :keywords (e.g. :newline)
;;xx    -> Return list/alist/plist/whatever for processing by/in mis3/message.
;;xx
;;xx  customization?:
;;xx    mis3/settings
;;xx    mis3/style
;;xx    mis3/theme
;;xx    :keywords (e.g. :settings)
;;xx
;;xx
;;xx  --------------------------------------------------------------------------
;;xx  --------------------------------------------------------------------------
;;xx  --------------------------------------------------------------------------
;;xx  --------------------------------------------------------------------------
;;xx  --------------------------------------------------------------------------


;; §-TODO-§ [2019-10-23]: 'proper' header for 'proper' emacs pkg...
;;
;;   Functions for manipulating strings, propertizing them, sending
;; them to the *Messages* buffer as such...

;;-----------------------------strings and outputs------------------------------
;;--                               Make It So!                                --
;;------------------------------------------------------------------------------


(defgroup mis3 nil
  "Functions for manipulating strings, propertizing them, sending
them to the *Messages* buffer as such..."
  :group 'editing)


;; And now some settings before we include all our files...


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Helper for optional arg/custom settings.
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Pull in our bits and pieces...
;;------------------------------------------------------------------------------

;;---
;; Doesn't require any of the others right now...
;;---
;; (require 'mis3-debug)

;;---
;; Trying to be non-circularly referential with these...
;;---

;; (require 'mis3-themes)
;; (require 'mis3-settings)
;; (require 'mis3-contents)
;; (require 'mis3-message)

;; (require 'mis3-comment)
;; (require 'mis3-nomer)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis3)
