;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------misut-------------------------------------
;;--                         Make It So Unit Tests!                           --
;;----------------------------(will center on exit)-----------------------------



;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; BELONGS ELSEWHERE!!!
;;------------------------------------------------------------------------------

(defun mis2-ert/setup ()
  "Does general setup for all mis2 test suites."

  )


(defun mis2-ert/teardown ()
  "Does general teardown for all mis2 test suites."

  )


;;------------------------------------------------------------------------------
;; Setup & Teardown Helpers
;;------------------------------------------------------------------------------

(defun mis2-ert/mis2-message/setup ()
  "Calls `mis2-ert/setup' for general setup then does setup
specific to this test suite."
  (mis2-ert/setup))


(defun mis2-ert/mis2-message/teardown ()
  "Calls `mis2-ert/teardown' for general setup then does setup
specific to this test suite."
  (mis2-ert/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2/message
;;------------------------------------------------------------------------------
;; (defun mis2/message (&rest args)

;;---
;; Test Case 0
;;---
(ert-deftest spotify-ert/mis2/message/case0 ()
  "Test that `mis2/message' outputs a properly formatted message.
"
  (mis2-ert/mis2-message/setup)

  ;; Setup for a message to test.
  (let ((symbol0 "test")
        (symbol1 '(thing1 thing2))
        (settings (mis2/settings/set nil :echo t :type :default))
        style
        (message "test %S"))

    ;; Put '(:center nil) into mis/settings plist on symbol 'message.
    (mis2/style/update style
                       :center nil)
    (mis2/style/update style
                       :margins '(">>" "<<"))
    (mis2/style/update style
                       :borders '("|" "|"))
    (mis2/style/update style
                       :padding '(?- :empty 3))
    (mis2/style/update style
                       :face :title)

    ;; Output message 0 with settings and style lists.
    (mis2/message :settings settings :style style message symbol0)

    ;; §-TODO-§ [2020-03-12]: verify w/ ert `should', `should-not', etc.

    ;; Output message 1 with settings and style lists.
    (mis2/message :settings settings
                  :style style
                  "test 2: %S" symbol1)

    ;; §-TODO-§ [2020-03-12]: verify w/ ert `should', `should-not', etc.
    )

  (mis2-ert/mis2-message/teardown))


;;---
;; Test Case 1
;;---
(ert-deftest spotify-ert/mis2/message/case1 ()
  "Test that `mis2/message' outputs a properly formatted message.
"
  (mis2-ert/mis2-message/setup)

  ;; Setup for a message to test.

  ;; Html-ish v3?
  (let ((symbol0 "test")
        (symbol1 '(thing1 thing2))
        (settings (mis2/settings :echo t :type :default))
        style
        (message "test %S"))

    ;; Put '(:center nil) into mis/settings plist on symbol 'message.
    (mis2/style/update style
                       :center nil)
    (mis2/style/update style
                       :margins '(">>" "<<"))
    (mis2/style/update style
                       :borders '("|" "|"))
    (mis2/style/update style
                       :padding '(?- :empty 3))
    (mis2/style/update style
                       :face :title)

    ;; Output messages with settings and style lists.
    (mis2/message :settings settings :style style message symbol0)

    ;; §-TODO-§ [2020-03-12]: verify w/ ert `should', `should-not', etc.

    (mis2/message :settings settings
                  :style style
                  "test 2: "
                  ;; Think I need a tag to separate a recursion level from, say,
                  ;; format args. So... `:r'? `:recurse'? `:mis'?
                  :mis '(:style (mis2/style style :face :attention)
                                "%S" symbol1))
    ;; So... I want a non-destructive `mis2/style' for this one.

    ;; §-TODO-§ [2020-03-12]: verify w/ ert `should', `should-not', etc.

    ;; lazy version?
    (mis2/message :settings (mis2/settings :echo t :type :default)
                  :style (mis2/style :face :title)
                  message symbol0)

    ;; §-TODO-§ [2020-03-12]: verify w/ ert `should', `should-not', etc.
    )

  (mis2-ert/mis2-message/teardown))


;;------------------------------------------------------------------------------
;; Local Test Data
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-message-ert)
