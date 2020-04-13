;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------misut-------------------------------------
;;--                         Make It So Unit Tests!                           --
;;----------------------------(will center on exit)-----------------------------

;; Test Helpers
(require 'mis2-ert-mock-stub)
(require 'mis2-ert-setup)

;; Test Data
;; None right now...

;; Test Requirements
(require 'mis2-themes)
(require 'mis2-settings)
(require 'mis2-message)


;;---
;; Runner Shortcuts
;;---
;; (ert "mis2-ert/message/.*")


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defvar-local mis2-ert/mock/output/to-buffer nil)
(defvar-local mis2-ert/mock/output/to-minibuffer nil)


;;------------------------------------------------------------------------------
;; Setup & Teardown Helpers
;;------------------------------------------------------------------------------

(defun mis2-ert/mis2-message/setup ()
  "Calls `mis2-ert/setup' for general setup then does setup/reset
specific to this test suite."
  (mis2-ert/setup/setup)
  (mis2-ert/mis2-message/reset))


(defun mis2-ert/mis2-message/teardown ()
  "Calls `mis2-ert/teardown' for general setup then does setup
specific to this test suite."
  (mis2-ert/setup/teardown))


(defun mis2-ert/mis2-message/reset ()
  "Calls `mis2-ert/reset' for general reset then does reset
specific to this test suite."
  (mis2-ert/setup/reset)

  (setq mis2-ert/mock/output/to-buffer     nil
        mis2-ert/mock/output/to-minibuffer nil))


;;------------------------------------------------------------------------------
;; Mocks
;;------------------------------------------------------------------------------

(defun mis2-ert/mock/mis2//message/output/to-buffer (mis2-msg plist)
  "Save mis2-msg to `mis2-ert/mock/output/to-minibuffer' instead of normal
functionality."
  (setq mis2-ert/mock/output/to-buffer mis2-msg))


(defun mis2-ert/mock/mis2//message/output/to-minibuffer (mis2-msg plist)
  "Save mis2-msg to `mis2-ert/mock/output/to-buffer' instead of normal
functionality."
  (setq mis2-ert/mock/output/to-minibuffer mis2-msg))


;;------------------------------------------------------------------------------
;; Test: mis2//message/parse
;;------------------------------------------------------------------------------
;; (defun mis2//message/parse (&rest args)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/message/parse/valid/contents-only ()
  "Test that `mis2//message/parse' pulls inputs out into settings/style/contents
correctly. These tests are for cases:
  - that are valid
  - that are without settings or style
"
  (mis2-ert/mis2-message/setup)

  ;; Test that nothing gives back proper parsed list of nothings.
  (let ((parsed (mis2//message/parse nil)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and nil (no settings).
    (should (eq       (first  parsed) :mis2//settings))
    (should (null     (second parsed)))

    ;; Second pair should be style key and nil (no style).
    (should (eq       (third  parsed) :mis2//style))
    (should (null     (fourth parsed)))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    ;; ...or (nil) in this specific case (list with nil as only member).
    (should (eq       (fifth  parsed) :mis2//contents))
    (should-not (null (sixth  parsed)))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 1))

    ;; and in our list should be our nil.
    (should (null (first (sixth parsed)))))


  ;; Test that simple string gives back proper parsed list.
  (let ((parsed (mis2//message/parse "hello there")))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and nil (no settings).
    (should (eq       (first  parsed) :mis2//settings))
    (should (null     (second parsed)))

    ;; Second pair should be style key and nil (no style).
    (should (eq       (third  parsed) :mis2//style))
    (should (null     (fourth parsed)))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    (should (eq       (fifth  parsed) :mis2//contents))
    (should-not (null (sixth  parsed)))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 1))

    ;; and in our list should be our string
    (should (string= (first (sixth parsed)) "hello there")))


  ;; Test that all contents gives back in proper parsed list.
  (let ((parsed (mis2//message/parse "hello %S" "there" 'symbol0)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and nil (no settings).
    (should (eq       (first  parsed) :mis2//settings))
    (should (null     (second parsed)))

    ;; Second pair should be style key and nil (no style).
    (should (eq       (third  parsed) :mis2//style))
    (should (null     (fourth parsed)))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    (should (eq       (fifth  parsed) :mis2//contents))
    (should-not (null (sixth  parsed)))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 3))

    ;; and in our list should be our strings and symbol
    (should (string= (first (sixth parsed)) "hello %S"))
    (should (string= (second (sixth parsed)) "there"))
    (should (eq (third (sixth parsed)) 'symbol0)))

  (mis2-ert/mis2-message/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/message/parse/valid/all-inputs ()
  "Test that `mis2//message/parse' pulls inputs out into settings/style/contents
correctly. These tests are for cases:
  - that are valid
  - that have settings and/or style
"
  (mis2-ert/mis2-message/setup)

  ;; Test that nothing gives back proper parsed list when no contents.
  (let ((parsed (mis2//message/parse :settings 'symbol0 :style 'symbol1)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and our "settings".
    (should (eq       (first  parsed) :mis2//settings))
    (should (eq       (second parsed) 'symbol0))

    ;; Second pair should be style key and our "style".
    (should (eq       (third  parsed) :mis2//style))
    (should (eq       (fourth parsed) 'symbol1))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    ;; ...nil in this specific case.
    (should (eq       (fifth  parsed) :mis2//contents))
    (should (null (sixth  parsed))))


  ;; Test that simple string gives back proper parsed list.
  (let ((parsed (mis2//message/parse :settings 'symbol0 :style 'symbol1 "hello there")))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and our "settings".
    (should (eq       (first  parsed) :mis2//settings))
    (should (eq       (second parsed) 'symbol0))

    ;; Second pair should be style key and our "style".
    (should (eq       (third  parsed) :mis2//style))
    (should (eq       (fourth parsed) 'symbol1))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    (should (eq       (fifth  parsed) :mis2//contents))
    (should-not (null (sixth  parsed)))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 1))

    ;; and in our list should be our string
    (should (string= (first (sixth parsed)) "hello there")))


  ;; Test that all contents gives back in proper parsed list.
  (let ((parsed (mis2//message/parse :settings 'symbol0
                                     :style 'symbol1
                                     "hello %S" "there" 'symbol2)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and our "settings".
    (should (eq       (first  parsed) :mis2//settings))
    (should (eq       (second parsed) 'symbol0))

    ;; Second pair should be style key and our "style".
    (should (eq       (third  parsed) :mis2//style))
    (should (eq       (fourth parsed) 'symbol1))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    (should (eq       (fifth  parsed) :mis2//contents))
    (should-not (null (sixth  parsed)))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 3))

    ;; and in our list should be our strings and symbol
    (should (string= (first (sixth parsed)) "hello %S"))
    (should (string= (second (sixth parsed)) "there"))
    (should (eq (third (sixth parsed)) 'symbol2)))

  (mis2-ert/mis2-message/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/message/parse/valid/settings-or-style ()
  "Test that `mis2//message/parse' pulls inputs out into settings/style/contents
correctly. These tests are for cases:
  - that are valid
  - that have settings and/or style
"
  (mis2-ert/mis2-message/setup)

  ;; Test that nothing gives back proper parsed list when no contents.
  (let ((parsed (mis2//message/parse :settings 'symbol0)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and our "settings".
    (should (eq       (first  parsed) :mis2//settings))
    (should (eq       (second parsed) 'symbol0))

    ;; Second pair should be style key and our "style".
    (should (eq       (third  parsed) :mis2//style))
    (should (null     (fourth parsed)))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    ;; ...nil in this specific case.
    (should (eq       (fifth  parsed) :mis2//contents))
    (should (null     (sixth  parsed))))


  ;; Test that nothing gives back proper parsed list when no contents.
  (let ((parsed (mis2//message/parse :style 'symbol1)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and our "settings".
    (should (eq       (first  parsed) :mis2//settings))
    (should (null     (second parsed)))

    ;; Second pair should be style key and our "style".
    (should (eq       (third  parsed) :mis2//style))
    (should (eq       (fourth parsed) 'symbol1))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    ;; ...nil in this specific case.
    (should (eq       (fifth  parsed) :mis2//contents))
    (should (null     (sixth  parsed))))


  ;; Test that parse gives back proper parsed list when no style.
  (let ((parsed (mis2//message/parse :settings 'symbol0 "hello there")))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and our "settings".
    (should (eq       (first  parsed) :mis2//settings))
    (should (eq       (second parsed) 'symbol0))

    ;; Second pair should be style key and our "style".
    (should (eq       (third  parsed) :mis2//style))
    (should (null     (fourth parsed)))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    (should (eq       (fifth  parsed) :mis2//contents))
    (should-not (null (sixth  parsed)))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 1))

    ;; and in our list should be our string
    (should (string= (first (sixth parsed)) "hello there")))


  ;; Test that nothing gives back proper parsed list when no contents.
  (let ((parsed (mis2//message/parse :style 'symbol1 "hello there")))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and our "settings".
    (should (eq       (first  parsed) :mis2//settings))
    (should (null     (second parsed)))

    ;; Second pair should be style key and our "style".
    (should (eq       (third  parsed) :mis2//style))
    (should (eq       (fourth parsed) 'symbol1))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    (should (eq       (fifth  parsed) :mis2//contents))
    (should-not (null (sixth  parsed)))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 1))

    ;; and in our list should be our string
    (should (string= (first (sixth parsed)) "hello there")))

  (mis2-ert/mis2-message/teardown))


;;---
;; Test Case 003
;;---
(ert-deftest mis2-ert/message/parse/invalid/mis-keys-not-first ()
  "Test that `mis2//message/parse' pulls inputs out into settings/style/contents
correctly. These tests are for cases:
  - that are 'invalid' (as far as getting settings/style parsed out)
  - that 'have' settings and/or style

By 'invalid' and 'have', I mean these are valid mis2 messages, but they have the
':settings' and/or ':style' keywords in the wrong place(s) and won't get parsed
out into mis2 settings/style.
"
  (mis2-ert/mis2-message/setup)

    ;; Invalid:
    ;;   (mis2/message \"%S %S\" :settings 'symbol)

  ;; Test that nothing gives back proper parsed list when no contents.
  (let ((parsed (mis2//message/parse "hello there" :settings 'symbol0)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and nil because ':settings' is not in
    ;; front of 'contents'.
    (should (eq       (first  parsed) :mis2//settings))
    (should (eq       (second parsed) nil))

    ;; Second pair should be style key and nil.
    (should (eq       (third  parsed) :mis2//style))
    (should (null     (fourth parsed)))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    ;; ...Everything passed in, in this case.
    (should (eq       (fifth  parsed) :mis2//contents))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 3))

    ;; and in our list should be our string
    (should (string= (first (sixth parsed)) "hello there"))
    (should (eq (second (sixth parsed)) :settings))
    (should (eq (third (sixth parsed)) 'symbol0)))

  ;; Test that nothing gives back proper parsed list when no contents.
  (let ((parsed (mis2//message/parse "%S %S" :style 'symbol0)))
    ;; Should have a resulting list that is 6 elements.
    (should-not (null parsed))
    (should (listp parsed))
    (should (= (length parsed) 6))

    ;; Elements should be:
    ;;   (:mis2//settings settings
    ;;    :mis2//style style
    ;;    :mis2//contents contents)

    ;; First pair should be settings key and nil because ':settings' is not in
    ;; front of 'contents'.
    (should (eq       (first  parsed) :mis2//settings))
    (should (null     (second parsed)))

    ;; Second pair should be style key and nil.
    (should (eq       (third  parsed) :mis2//style))
    (should (null     (fourth parsed)))

    ;; Third pair should be contents key and a list
    ;; (of the non-settings/styles given to parse).
    ;; ...Everything passed in, in this case.
    (should (eq       (fifth  parsed) :mis2//contents))
    (should (listp    (sixth  parsed)))
    (should (= (length (sixth  parsed)) 3))

    ;; and in our list should be our string
    (should (string= (first (sixth parsed))  "%S %S"))
    (should (eq (second (sixth parsed)) :style))
    (should (eq (third (sixth parsed)) 'symbol0)))

  (mis2-ert/mis2-message/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2/message
;;------------------------------------------------------------------------------
;; (defun mis2/message (&rest args)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/message/simplest ()
  "Test that `mis2/message' outputs a properly formatted message.
"
  (mis2-ert/mis2-message/setup)

  ;; Mock actual outptus so I can capture/check message output.
  (mis2-ert/mock 'mis2//message/output/to-buffer nil
    (mis2-ert/mock 'mis2//message/output/to-minibuffer nil
      ;; No style/settings - just do the simplest message possible.
      (mis2/message "hello there")

      (should (string= mis2-ert/mock/output/to-buffer
                       "hello there"))
      ;; Our mock doesn't bother checking settings, so it'll have it too.
      (should (string= mis2-ert/mock/output/to-buffer
                       "hello there"))))

  ;; Clear out mock vars for next test.
  ;; (mis2-ert/mis2-message/reset)

  (mis2-ert/mis2-message/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/message/styled ()
  "Test that `mis2/message' outputs a properly formatted message.
"
  (mis2-ert/mis2-message/setup)

  ;; Setup for a message to test.
  (let ((message "hello, %s")
        (symbol0 "there")
        (symbol1 '(thing1 thing2))
        (settings (mis2/settings/set nil :echo t :theme :default))
        (style '(;; face: default if no specific in :faces
                 :face :text
                 ;; face: set specifics for everything this time
                 :faces (:text    :title
                         :indent  :inattention
                         :margins :highlight
                         :borders :borders
                         :padding :padding)))
        (expected-str "hello, there"))

    ;; Put more style in afterwards... just cause.
    (mis2/style/update style
                       :center nil)
    (mis2/style/update style
                       :margins '(">>" "<<"))
    (mis2/style/update style
                       :borders '("|" "|"))
    (mis2/style/update style
                       :padding '("--" "--"))
                       ;; :padding '(?- :empty 3))

    (mis2-ert/mock 'mis2//message/output/to-buffer nil
      (mis2-ert/mock 'mis2//message/output/to-minibuffer nil
        ;; Output message 0 with settings and style lists.
        (mis2/message :settings settings :style style message symbol0)

        ;; correct characters?
        (should
         (string= mis2-ert/mock/output/to-buffer
                  (concat
                   ">>"
                   "|"
                   "--"
                   " "
                   "hello, there"
                   "                                                         "
                   "--"
                   "|"
                   "<<")))

        ;; correctly propertized?
        (cl-flet ((ms #'make-string))
          (should
           (equal-including-properties
            mis2-ert/mock/output/to-buffer
            (concat
             (propertize ">>"         'face font-lock-constant-face)
             (propertize "|"          'face font-lock-comment-delimiter-face)
             (propertize "--"         'face font-lock-string-face)
             (propertize (ms  1 ?\s)  'face font-lock-string-face)
             (propertize expected-str 'face font-lock-builtin-face)
             (propertize (ms 57 ?\s)  'face font-lock-string-face)
             (propertize "--"         'face font-lock-string-face)
             (propertize "|"          'face font-lock-comment-delimiter-face)
             (propertize "<<"         'face font-lock-constant-face))))))))

  (mis2-ert/mis2-message/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/message/alt-buffer ()
  "Test that `mis2/message' outputs a properly formatted message to
specified buffer.
"
  (mis2-ert/mis2-message/setup)

  ;; A temp buffer we'll output to:
  (with-temp-buffer
    (let ((buffer-name-tmp (buffer-name (current-buffer))))

      ;; Mock to-minibuffer; actual to-buffer, but use our temp buffer instead
      ;; of default.
      (mis2-ert/mock 'mis2//message/output/to-minibuffer nil
        ;; No style/settings - just do the simplest message possible.
        (mis2/message :settings (list :buffer buffer-name-tmp) "hello there")

        (should (string= (buffer-string)
                         "hello there\n")))))

  ;; Again, but using buffer object instead of buffer name.
  (with-temp-buffer
    (let ((buffer-obj-tmp (current-buffer)))

      ;; Mock to-minibuffer; actual to-buffer, but use our temp buffer instead
      ;; of default.
      (mis2-ert/mock 'mis2//message/output/to-minibuffer nil
        ;; No style/settings - just do the simplest message possible.
        (mis2/message :settings (list :buffer buffer-obj-tmp) "hello there")

        (should (string= (buffer-string)
                         "hello there\n")))))

  (mis2-ert/mis2-message/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2/message multi-formatting!
;;------------------------------------------------------------------------------
;; (defun mis2/message (&rest args)

(ert-deftest mis2-ert/message/multi-formatted ()
  "Test that `mis2/message' outputs a properly formatted message when input
contains several styled subsections.
"
  (mis2-ert/mis2-message/setup)

  ;; Setup for a message to test.
  (let ((settings nil)
        (style '(:face :text)))

    (mis2-ert/mock 'mis2//message/output/to-buffer nil
      (mis2-ert/mock 'mis2//message/output/to-minibuffer nil

        ;; Output message with multiple differently styled user-input sections.
        (mis2/message :settings settings :style style
                      "Unbelieveable! You, "
                      '(:face :highlight "SUBJECT NAME HERE")
                      ", must be the pride of "
                      '(:face :title "SUBJECT HOMETOWN HERE")
                      ".")

        ;; correctly propertized?
        (cl-flet ((ms #'make-string))
          (should
           (equal-including-properties
            mis2-ert/mock/output/to-buffer
            (concat

             (propertize "Unbelieveable! You, "    'face font-lock-warning-face)
             (propertize "SUBJECT NAME HERE"       'face font-lock-constant-face)
             (propertize ", must be the pride of " 'face font-lock-warning-face)
             (propertize "SUBJECT HOMETOWN HERE"   'face font-lock-builtin-face)
             (propertize "."                       'face font-lock-warning-face))))))))

  (mis2-ert/mis2-message/teardown))


;;------------------------------------------------------------------------------
;; Local Test Data
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-message-ert)
