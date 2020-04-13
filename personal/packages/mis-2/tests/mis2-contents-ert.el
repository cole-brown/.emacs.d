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
(require 'mis2-contents)


;;---
;; Runner Shortcuts
;;---
;; (ert "mis2-ert/contents/.*")


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defvar-local mis2-ert/contents/fill-column/storage fill-column
  "Backup and restore helpers store/retrieve a copy of `fill-column' here.")



;;------------------------------------------------------------------------------
;; Setup & Teardown Helpers
;;------------------------------------------------------------------------------

(defun mis2-ert/mis2-contents/setup ()
  "Calls `mis2-ert/setup' for general setup then does setup/reset
specific to this test suite."
  ;; save fill column
  (setq mis2-ert/contents/fill-column/storage fill-column)
  (mis2-ert/setup/setup))


(defun mis2-ert/mis2-contents/teardown ()
  "Calls `mis2-ert/teardown' for general setup then does setup
specific to this test suite."
  ;; reset fill column
  (setq fill-column mis2-ert/contents/fill-column/storage)
  (mis2-ert/setup/teardown))


;;------------------------------------------------------------------------------
;; Mocks
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Test: mis2//contents/text/format/emacs
;;------------------------------------------------------------------------------
;; (defun mis2//contents/text/format/emacs (contents)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/contents/build/string/simple ()
  "Test that `mis2//contents/text/format/emacs' can build & return a simple string
from contents.
"
  (mis2-ert/mis2-contents/setup)

  (let ((plist '(:mis2//testing t)))
    ;; Simple string just gets returned as-is.
    (should (string= (mis2//contents/text/format/emacs '("Hello, World.") plist)
                     "Hello, World."))

    ;; Single other thing gets formatted.
    (should (string= (mis2//contents/text/format/emacs '(:face) plist)
                     ":face"))

    ;; Nothing in; nothing out (but it's a string now!).
    (should (string= (mis2//contents/text/format/emacs nil plist)
                     "nil"))

    ;; Single other thing gets formatted.
    (should (string= (mis2//contents/text/format/emacs '((1 2 3 4 5)) plist)
                     "(1 2 3 4 5)")))

  (mis2-ert/mis2-contents/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/contents/build/string/formatter ()
  "Test that `mis2//contents/text/format/emacs' can build & return a simple string
from contents.
"
  (mis2-ert/mis2-contents/setup)

  (let ((plist '(:mis2//testing t)))
    ;; More than one thing in contents means first thing is formatting string.
    (should (string= (mis2//contents/text/format/emacs '("Hello, %s" "World.")
                                                       plist)
                     "Hello, World."))

    ;; Bad contents? No formatter string...
    (should-error (mis2//contents/text/format/emacs '(:keyword valueword)
                                                    plist))

    ;; Null formatter - also bad.
    (should-error (mis2//contents/text/format/emacs '(nil "hello")
                                                    plist))

    ;; more extra args than percents in formatter
    (should (string= (mis2//contents/text/format/emacs
                      '("Hello, %s" "World" "," "my name is...") plist)
                     ;; means we just don't get the rest.
                     "Hello, World")))

  (mis2-ert/mis2-contents/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//contents/string/propertize
;;------------------------------------------------------------------------------
;; (defun mis2//contents/string/propertize (message plist)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/contents/build/propertize/nothing ()
  "Test that `mis2//contents/propertize' can return an unaltered string
when no properties are there to add.
"
  (mis2-ert/mis2-contents/setup)

  ;; Let higher level func do this.
  ;; ;; No mis2 plist at all is an error.
  ;; (should-error (mis2//contents/propertize "Hello, World." :text nil))

  ;; Simple string just gets returned as-is.
  ;; Using simplest 'valid' mis2 plist we can...
  (should (string= (mis2//contents/propertize "Hello, World."
                                              :text '(:mis2//testing t))
                   "Hello, World."))

  ;; Have a mis2 plist without `:face' in style.
  (let ((plist '(:mis2//settings (:theme :default)
                 :mis2//style (:margins (">" "<"))
                 :mis2//testing t)))
    (should (string= (mis2//contents/propertize "Hello, World."
                                                :text plist)
                     "Hello, World.")))

  ;; Have a mis2 plist without `:theme' or `:face' in style.
  (let ((plist '(:mis2//settings nil
                 :mis2//style (:margins (">" "<"))
                 :mis2//testing t)))
    (should (string= (mis2//contents/propertize "Hello, World."
                                                :text plist)
                     "Hello, World.")))

  (mis2-ert/mis2-contents/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/contents/build/propertize/face ()
  "Test that `mis2//contents/propertize' can return a propertized string
when there is a face to use.
"
  (mis2-ert/mis2-contents/setup)

  (setq mis2/themes '((:default (:test-face0 font-lock-keyword-face
                                 :test-face1 font-lock-comment-face))
                      (:fancy   (:test-face0 font-lock-string-face
                                 :test-face1 font-lock-type-face))))

  ;; Have a mis2 plist with `:face' in style, but no theme in settings.
  ;; Should use `:default' theme.
  (let ((plist '(:mis2//settings nil
                 :mis2//style (:face :test-face0)
                 :mis2//testing t))
        (expected-output "Hello, World.")
        message)
    (setq message (mis2//contents/propertize "Hello, World."
                                             :test-face0 plist))

    ;; Should have our string as expected.
    (should (string= message expected-output))

    ;; And each element in it should be propertized with the face.
    (dotimes (i (length expected-output))
      (should (eq (get-text-property 0 'face message)
                  'font-lock-keyword-face))))

  ;; Same thing, but :fancy theme
  (let ((plist '(:mis2//settings (:theme :fancy)
                 :mis2//style (:face :test-face0)
                 :mis2//testing t))
        (expected-output "Hello, World.")
        message)
    (setq message (mis2//contents/propertize "Hello, World."
                                             :test-face0 plist))
    ;; Should have our string as expected.
    (should (string= message expected-output))

    ;; And each element in it should be propertized with the face.
    (dotimes (i (length expected-output))
      (should (eq (get-text-property 0 'face message)
                  'font-lock-string-face))))

  ;; :fancy & :test-face1
  (let ((plist '(:mis2//settings (:theme :fancy)
                 :mis2//style (:face :test-face1)
                 :mis2//testing t))
        (expected-output "Hello, World.")
        message)
    (setq message (mis2//contents/propertize "Hello, World."
                                             :test-face1 plist))
    ;; Should have our string as expected.
    (should (string= message expected-output))

    ;; And each element in it should be propertized with the face.
    (dotimes (i (length expected-output))
      (should (eq (get-text-property 0 'face message)
                  'font-lock-type-face))))

  ;; Done; set mis2/themes back and check that that worked too.
  (mis2-ert/setup/themes/restore)

  (should (seq-set-equal-p mis2/themes mis2-ert/setup/themes/storage))

  (mis2-ert/mis2-contents/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//contents/align
;;------------------------------------------------------------------------------
;; (defun mis2//contents/align (string plist)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/contents/align/left ()
  "Test that `mis2//contents/align' can return a properly aligned string
when no or `:left' alignment is supplied.
"
  (mis2-ert/mis2-contents/setup)

  ;; No mis2 plist at all is an error.
  (should-error (mis2//contents/align "Hello, World." nil))

  ;; Simple string just gets returned as-is.
  ;; Using simplest 'valid' mis2 plist we can...
  (should (string= (mis2//contents/align "Hello, World."
                                         '(:mis2//testing t))
                   "Hello, World."))

  ;; Using simplest 'valid' mis2 plist we can, but as a var so we can check left
  ;; align's padding line info.
  (let ((plist '(:mis2//testing t))
        (string "Hello, World."))
    (mis2//contents/box/parts string plist)

    ;; Simple string just gets returned as-is.
    (should (string= (mis2//contents/align string plist)
                     string))

    (should (seq-set-equal-p (mis2//contents/line/get/from-data :padding plist)
                             (list 0
                                   ;; Width minus string amount in this case
                                   ;; for the right padding amount.
                                   (- fill-column (length string))))))

  ;; With :line-width. Shouldn't affect left-aligned, but is related to
  ;; alignment.
  (let ((plist '(:mis2//settings (:line-width 40)
                 :mis2//testing  t))
        (string "Hello, World."))
    (mis2//contents/box/parts string plist)

    ;; Simple string just gets returned as-is.
    (should (string= (mis2//contents/align string plist)
                     string))

    (should (seq-set-equal-p (mis2//contents/line/get/from-data :padding plist)
                             (list 0
                                   ;; Width minus string amount in this case
                                   ;; for the right padding amount.
                                   ;; Width of 40 in this case.
                                   (- 40 (length string))))))

  ;; With :left, no :line-width.
  (let ((plist '(:mis2//settings (:left t) :mis2//testing t))
        (string "Hello, World."))
    (mis2//contents/box/parts string plist)

    ;; Simple string just gets returned as-is.
    (should (string= (mis2//contents/align string plist)
                     string))

    (should (seq-set-equal-p (mis2//contents/line/get/from-data :padding plist)
                             (list 0
                                   ;; Width minus string amount in this case
                                   ;; for the right padding amount.
                                   (- fill-column (length string))))))

  ;; With :left and :line-width.
  (let ((plist '(:mis2//settings (:left t :line-width 40)
                 :mis2//testing  t))
        (string "Hello, World."))
    (mis2//contents/box/parts string plist)

    ;; Simple string just gets returned as-is.
    (should (string= (mis2//contents/align string plist)
                     string))

    (should (seq-set-equal-p (mis2//contents/line/get/from-data :padding plist)
                             (list 0
                                   ;; Width minus string amount in this case
                                   ;; for the right padding amount.
                                   ;; Width of 40 in this case.
                                   (- 40 (length string))))))

   (mis2-ert/mis2-contents/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/contents/align/center/no-reserve ()
  "Test that `mis2//contents/align' can return a properly aligned string
when `:center' alignment is supplied.
"
  (mis2-ert/mis2-contents/setup)

  ;; Give ourselves a known line-width.
  (let ((fill-column 80)
        (line-width fill-column)
        (string-odd "Hello, World.") ;; odd number of chararcters to center
        (string-even "Hello, Jeff.")) ;; even number of chararcters to center

    ;; :center? Nil means no.
    (should (string= (mis2//contents/align string-odd
                                           '(:mis2//style (:center nil)
                                             :mis2//testing t))
                     string-odd))

    ;; Actually centered. No left/right reserved.
    (should (string= (mis2//contents/align string-odd
                                           '(:mis2//style (:center t)
                                             :mis2//testing t))
                     ;; extra space is put in front of string
                     (concat
                      (make-string 34 ?\s)
                      string-odd
                      (make-string 33 ?\s))))
    (should (string= (mis2//contents/align string-even
                                           '(:mis2//style (:center t)
                                             :mis2//testing t))
                     ;; extra space is put in front of string
                     (concat
                      (make-string 34 ?\s)
                      string-even
                      (make-string 34 ?\s))))

    ;; Actually centered. No left/right reserved. Line width override.
    (should (string= (mis2//contents/align string-odd
                                           '(:mis2//settings (:line-width 40)
                                             :mis2//style (:center t)
                                             :mis2//testing t))
                     ;; extra space is put in front of string
                     (concat
                      (make-string 14 ?\s)
                      string-odd
                      (make-string 13 ?\s))))
    (should (string= (mis2//contents/align string-even
                                           '(:mis2//settings (:line-width 40)
                                             :mis2//style (:center t)
                                             :mis2//testing t))
                     ;; extra space is put in front of string
                     (concat
                      (make-string 14 ?\s)
                      string-even
                      (make-string 14 ?\s)))))

  (mis2-ert/mis2-contents/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/contents/align/center/with-reserve ()
  "Test that `mis2//contents/align' can return a properly aligned string
when `:center' alignment is supplied and a reserve exists.
"
  (mis2-ert/mis2-contents/setup)

  ;; Give ourselves a known line-width.
  (let ((fill-column 80)
        (line-width fill-column)
        (style '(:center t
                 :margins ("xx" "xx")))
        (plist '(:mis2//testing t))
        (string-odd "Hello, World.")  ;; odd number of chararcters to center
        (string-even "Hello, Jeff.")) ;; even number of chararcters to center

    (mis2//data/update plist :mis2//style style)
    (mis2//contents/box/parts string-odd plist)
    (should (string= (mis2//contents/align string-odd plist)
                     ;; Extra space is put in front of string. 2 chars knocked
                     ;; off front & back compared to no-reserved case.
                     (concat
                      (make-string 32 ?\s)
                      string-odd
                      (make-string 31 ?\s))))

    (mis2//contents/box/parts string-even plist)
    (should (string= (mis2//contents/align string-even plist)
                     (concat
                      (make-string 32 ?\s)
                      string-even
                      (make-string 32 ?\s))))

    ;; Asymmetrical reserved.
    (mis2/style/update style :margins '(nil "xx") :borders '("yy" "yy"))
    (mis2//data/update plist :mis2//style style)
    (mis2//contents/box/parts string-odd plist)
    (should (string= (mis2//contents/align string-even plist)
                     ;; Less at end of string due to asymmetry.
                     (concat
                      (make-string 32 ?\s)
                      string-even
                      (make-string 30 ?\s))))

    ;; Asymmetrical reserved.
    (mis2/style/update style :margins '("xx" nil) :borders '("yy" "yy"))
    (mis2//data/update plist :mis2//style style)
    (mis2//contents/box/parts string-even plist)
    (should (string= (mis2//contents/align string-even plist)
                     ;; Less at beginning of string due to asymmetry.
                     (concat
                      (make-string 30 ?\s)
                      string-even
                      (make-string 32 ?\s)))))

  (mis2-ert/mis2-contents/teardown))


;;---
;; Test Case 003
;;---
(ert-deftest mis2-ert/contents/align/right/no-reserve ()
  "Test that `mis2//contents/align' can return a properly aligned string
when `:right' alignment is supplied.
"
  (mis2-ert/mis2-contents/setup)

  ;; Give ourselves a known line-width.
  (let ((fill-column 80)
        (line-width fill-column)
        (string-odd "Hello, World.") ;; odd number of chararcters
        (string-even "Hello, Jeff.")) ;; even number of chararcters

    ;; :right? Nil means no.
    (should (string= (mis2//contents/align string-odd
                                           '(:mis2//style (:right nil)
                                             :mis2//testing t))
                     string-odd))

    ;; Actually right-aligned. No left/right reserved.
    (should (string= (mis2//contents/align string-odd
                                           '(:mis2//style (:right t)
                                             :mis2//testing t))
                     ;; extra spaces are all in front of string
                     (concat
                      (make-string (- line-width (length string-odd)) ?\s)
                      string-odd)))

    (should (string= (mis2//contents/align string-even
                                           '(:mis2//style (:right t)
                                             :mis2//testing t))
                     ;; extra space is put in front of string
                     (concat
                      (make-string (- line-width (length string-even)) ?\s)
                      string-even)))

    ;; Actually right-aligned. No left/right reserved. Line width override.
    (should (string= (mis2//contents/align string-odd
                                           '(:mis2//settings (:line-width 40)
                                             :mis2//style (:right t)
                                             :mis2//testing t))
                     ;; extra spaces are all in front of string
                     (concat
                      (make-string (- 40 (length string-odd)) ?\s)
                      string-odd)))

    (should (string= (mis2//contents/align string-even
                                           '(:mis2//settings (:line-width 40)
                                             :mis2//style (:right t)
                                             :mis2//testing t))
                     ;; extra space is put in front of string
                     (concat
                      (make-string (- 40 (length string-even)) ?\s)
                      string-even))))

  (mis2-ert/mis2-contents/teardown))


;;---
;; Test Case 004
;;---
(ert-deftest mis2-ert/contents/align/right/with-reserve ()
  "Test that `mis2//contents/align' can return a properly aligned string
when `:right' alignment is supplied and a reserve exists.
"
  (mis2-ert/mis2-contents/setup)

  ;; Give ourselves a known line-width.
  (let ((fill-column 80)
        (line-width fill-column)
        (style '(:right t
                 :margins ("xx" "xx")))
        (plist '(:mis2//testing t))
        (string-odd "Hello, World.") ;; odd number of chararcters to right
        (string-even "Hello, Jeff.")) ;; even number of chararcters to right

    ;; Actually right-aligned. With reserved.
    (mis2//data/update plist :mis2//style style)
    (mis2//contents/box/parts string-odd plist)
    (should (string= (mis2//contents/align string-odd plist)
                     (concat
                      (make-string (- line-width (length string-odd)
                                      ;; minus both margins too
                                      2 2)
                                   ?\s)
                      string-odd)))

    (mis2//contents/box/parts string-even plist)
    (should (string= (mis2//contents/align string-even plist)
                     (concat
                      (make-string (- line-width (length string-even)
                                      ;; minus both margins too
                                      2 2)
                                   ?\s)
                      string-even)))

    ;; Asymmetrical reserved.
    (mis2/style/update style :margins '(nil "xx") :borders '("yy" "yy"))
    (mis2//data/update plist :mis2//style style)
    (mis2//contents/box/parts string-even plist)
    (should (string= (mis2//contents/align string-even plist)
                     (concat
                      (make-string (- line-width (length string-even)
                                      ;; Minus asymmetrical margins.
                                      0 2
                                      ;; Minus borders.
                                      2 2)
                                   ?\s)
                      string-even)))

    ;; Asymmetrical reserved.
    (mis2/style/update plist :margins '("xx" nil) :borders '("yy" "yy"))
    (mis2//data/update plist :mis2//style style)
    (mis2//contents/box/parts string-even plist)
    (should (string= (mis2//contents/align string-even plist)
                     (concat
                      (make-string (- line-width (length string-even)
                                      ;; Minus asymmetrical margins.
                                      2 0
                                      ;; Minus borders.
                                      2 2)
                                   ?\s)
                      string-even))))

  (mis2-ert/mis2-contents/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//contents/box/parts
;;------------------------------------------------------------------------------
;; (defun mis2//contents/box/parts (string plist)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/contents/box/parts ()
  "Test that `mis2//contents/box/parts' can create a proper :mis2//box plist
in the mis2 plist based on mis2//settings and mis2//style.
"
  (mis2-ert/mis2-contents/setup)

  ;; No mis2 plist at all is an error.
  (should-error (mis2//contents/box/parts "Hello, World." nil))

  ;; No box; yes indent
  (let ((plist '(:mis2//settings (:line-width 80)
                 :mis2//style    (:indent 4
                                  ;; :margins ("left" "right")
                                  ;; :borders ("|" "|")
                                  ;; :padding ("--" "--")
                                  ;; :padding (?- :fill 2)
                                  ;; :padding (?- :empty 3)
                                          )
                 :mis2//testing t))
        (string "Hello, World.")
        line
        box)

    ;; Don't care about output... important things go into plist.
    (mis2//contents/box/parts string
                              plist)

    ;; Only have indent - so no :mis2//box and :mis2//line should contain a
    ;; correct indent.
    (setq box (plist-get plist :mis2//box))
    (setq line (plist-get plist :mis2//line))
    (should (null box))
    (should line)
    (should (eq (first line) :indent))
    (should (string= (second line) "    ")))

  ;; Box pieces, no indent.
  (let ((plist '(:mis2//settings (:line-width 80)
                 :mis2//style    (;; :indent 4
                                  :margins ("left" "right")
                                  :borders ("|" "|")
                                  :padding ("--" "--")
                                  ;; :padding (?- :fill 2)
                                  ;; :padding (?- :empty 3)
                                          )
                 :mis2//testing t))
        (string "Hello, World.")
        line
        box
        element)

    ;; Don't care about output... important things go into plist.
    (mis2//contents/box/parts string
                              plist)

    ;; Should have box but no line components.
    (setq box (plist-get plist :mis2//box))
    (setq line (plist-get plist :mis2//line))
    (should (null line))
    (should box)

    ;; Check margins.
    (setq element (plist-get box :margins))
    (should element)
    (should (listp element))
    (should (= (length element) 2))
    (should (string= (first element) "left"))
    (should (string= (second element) "right"))

    ;; Check borders.
    (setq element (plist-get box :borders))
    (should element)
    (should (listp element))
    (should (= (length element) 2))
    (should (string= (first element) "|"))
    (should (string= (second element) "|"))

    ;; Check padding.
    (setq element (plist-get box :padding))
    (should element)
    (should (listp element))
    (should (= (length element) 4))
    (should (string= (first element) "--"))
    (should (= (second element) ?\s))
    (should (= (third element) ?\s))
    (should (string= (fourth element) "--")))

  ;; Paddings type 2
  (let ((plist '(:mis2//settings (:line-width 80)
                 :mis2//style    (;; :indent 4
                                  ;; :margins ("left" "right")
                                  ;; :borders ("|" "|")
                                  ;; :padding ("--" "--")
                                  :padding (?- :fill 2)
                                  ;; :padding (?- :empty 3)
                                           )
                 :mis2//testing t))
        (string "Hello, World.")
        line
        box
        element)

    ;; Don't care about output... important things go into plist.
    (mis2//contents/box/parts string
                              plist)

    ;; Should have box but no line components.
    (setq box (plist-get plist :mis2//box))
    (setq line (plist-get plist :mis2//line))
    (should (null line))
    (should box)

    ;; Should not have margins.
    (setq element (plist-get box :margins))
    (should-not element)

    ;; Should not have borders.
    (setq element (plist-get box :borders))
    (should-not element)

    ;; Check padding.
    (setq element (plist-get box :padding))
    (should element)
    (should (listp element))
    (should (= (length element) 4))
    (should (string= (first element) "--"))
    (should (= (second element) ?\s))
    (should (= (third element) ?\s))
    (should (string= (fourth element) "--")))

  ;; Paddings type 3
  (let ((plist '(:mis2//settings (:line-width 80)
                 :mis2//style    (;; :indent 4
                                  ;; :margins ("left" "right")
                                  ;; :borders ("|" "|")
                                  ;; :padding ("--" "--")
                                  ;; :padding (?- :fill 2)
                                  :padding (?- :empty 3)
                                           )
                 :mis2//testing t))
        (string "Hello, World.")
        line
        box
        element)

    ;; Don't care about output... important things go into plist.
    (mis2//contents/box/parts string
                              plist)

    ;; Should have box but no line components.
    (setq box (plist-get plist :mis2//box))
    (setq line (plist-get plist :mis2//line))
    (should (null line))
    (should box)

    ;; Should not have margins.
    (setq element (plist-get box :margins))
    (should-not element)

    ;; Should not have borders.
    (setq element (plist-get box :borders))
    (should-not element)

    ;; Check padding.
    (setq element (plist-get box :padding))
    (should element)
    (should (listp element))
    (should (= (length element) 4))
    (should (= (first element) ?-))
    (should (string= (second element) "   "))
    (should (string= (third element) "   "))
    (should (= (fourth element) ?-)))

  (mis2-ert/mis2-contents/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//contents/box/build
;;------------------------------------------------------------------------------
;; (defun mis2//contents/box/build (string plist)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/contents/box/build ()
  "Test that `mis2//contents/box/bulid' can build the line based on parts in
plist: :mis2//box, :mis2//line, and :mis2//contents.
"
  (mis2-ert/mis2-contents/setup)

  ;; mis2 plist has initial stuff (settings, style) and
  ;; derived stuff (box, line). We'll only actually use derived.
  (let ((plist '(:mis2//settings (:line-width 80)
                 :mis2//style (:indent 4
                               :margins ("left" "right")
                               :borders ("|" "|")
                               :padding ("--" "--"))
                 :mis2//box (:padding ("--" ?\s ?\s "--")
                             :borders ("|" "|")
                             :margins (">>>" "<<<<<"))
                 :mis2//line (:indent "    ")
                 :mis2//testing t))

        (string "Hello, World.")
        line
        box)

    (should (string= (mis2//contents/box/build string
                                               plist)
                     (concat "    " ;; indent
                             ">>>"  ;; margin, left
                             "|"    ;; border, left
                             "--"   ;; static pad, left
                             " "    ;; dynamic pad, left
                             string
                             ;; dynamic pad, right
                             "                                                "
                             "--"    ;; static pad, right
                             "|"     ;; border, right
                             "<<<<<" ;; margin, right
                             ))))

  (mis2-ert/mis2-contents/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//contents
;;------------------------------------------------------------------------------
;; (defun mis2//contents (plist)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/contents/output ()
  "Test that `mis2//contents' can build the parts and then the
line based on inputs in plist: :mis2//settings, :mis2//style, and
:mis2//contents.
"
  (mis2-ert/mis2-contents/setup)

  ;; Bit of everything in the plist...
  (let* ((plist '(:mis2//contents ("Hello, %s." "World")
                  :mis2//settings (:line-width 80)
                  :mis2//style (;; face: default if no specific in :faces
                                :face :text
                                ;; face: set specifics for everything this time
                                :faces (:text    :title
                                        :indent  :inattention
                                        :margins :highlight
                                        :borders :borders
                                        :padding :padding)
                                ;; alignment
                                :left t
                                ;; boxing
                                :indent 4
                                :margins (">>>" "<<<<<")
                                :borders ("|" "|")
                                :padding ("--" "--"))
                  ;; This should be a proper, complete mis2 list so
                  ;; leave off testing flag:
                  ;; :mis2//testing t
                  ))
         (expected-str "Hello, World.")
         output)

    ;; Output will be in our plist under :mis2//message; plist will have updates
    ;; from intermediate steps.
    (should (mis2//data/plist? (mis2//contents plist)))
    (setq output (plist-get plist :mis2//message))
    (should output)
    (should (stringp output))

    (should (string= output
                     (concat "    " ;; indent
                             ">>>"  ;; margin, left
                             "|"    ;; border, left
                             "--"   ;; static pad, left
                             (make-string 1 ?\s) ;; dynamic pad, left
                             expected-str
                             (make-string 48 ?\s) ;; dynamic pad, right
                             "--"    ;; static pad, right
                             "|"     ;; border, right
                             "<<<<<" ;; margin, right
                             )))

    (cl-flet ((ms #'make-string))
      (should
       (equal-including-properties
        output
        (concat
         (propertize "    "       'face font-lock-comment-face)
         (propertize ">>>"        'face font-lock-constant-face)
         (propertize "|"          'face font-lock-comment-delimiter-face)
         (propertize "--"         'face font-lock-string-face)
         (propertize (ms 1 ?\s)   'face font-lock-string-face)
         (propertize expected-str 'face font-lock-builtin-face)
         (propertize (ms 48 ?\s)  'face font-lock-string-face)
         (propertize "--"         'face font-lock-string-face)
         (propertize "|"          'face font-lock-comment-delimiter-face)
         (propertize "<<<<<"      'face font-lock-constant-face))))))

  ;; Done; set mis2/themes back and check that that worked too.
  (mis2-ert/setup/themes/restore)

  (should (seq-set-equal-p mis2/themes mis2-ert/setup/themes/storage))

  (mis2-ert/mis2-contents/teardown))


;;------------------------------------------------------------------------------
;; Local Test Data
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-contents-ert)
