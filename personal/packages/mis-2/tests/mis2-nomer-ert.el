;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------misut-------------------------------------
;;--                         Make It So Unit Tests!                           --
;;---------------------------(will center for food)-----------------------------

;; Test Helpers
(require 'mis2-ert-mock-stub)
(require 'mis2-ert-setup)

;; Test Data
;; None right now...

;; Test Requirements
(require 'mis2-themes)
(require 'mis2-settings)
(require 'mis2-message)
(require 'mis2-nomer)


;;---
;; Runner Shortcuts
;;---
;; (ert "mis2-ert/nomer/.*")


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defvar-local mis2-ert/mock/output/to-buffer nil)
(defvar-local mis2-ert/mock/output/to-minibuffer nil)


;;------------------------------------------------------------------------------
;; Setup & Teardown Helpers
;;------------------------------------------------------------------------------

(defun mis2-ert/mis2-nomer/setup ()
  "Calls `mis2-ert/setup' for general setup then does setup/reset
specific to this test suite."
  (mis2-ert/setup/setup)
  (mis2-ert/mis2-nomer/reset))


(defun mis2-ert/mis2-nomer/teardown ()
  "Calls `mis2-ert/teardown' for general setup then does setup
specific to this test suite."
  (mis2-ert/setup/teardown))


(defun mis2-ert/mis2-nomer/reset ()
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
  (setq mis2-ert/mock/output/to-buffer
        (if mis2-ert/mock/output/to-buffer
            (concat mis2-ert/mock/output/to-buffer
                    "\n"
                    mis2-msg)
          mis2-msg)))


(defun mis2-ert/mock/mis2//message/output/to-minibuffer (mis2-msg plist)
  "Save mis2-msg to `mis2-ert/mock/output/to-buffer' instead of normal
functionality."
  (setq mis2-ert/mock/output/to-minibuffer mis2-msg))


;;------------------------------------------------------------------------------
;; Test: mis2//nomer/add
;;------------------------------------------------------------------------------
;; (defun mis2//nomer/add (nomer)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/nomer/add ()
  "Test that `mis2//nomer/parse' pulls inputs out into settings/style/contents
correctly. These tests are for cases:
  - that are valid
  - that are without settings or style
"
  (mis2-ert/mis2-nomer/setup)

  ;; lexically bind settings, style, nomers lists to override any user settings
  (let ((mis2/nomer/settings/user '(:line-width 80))
        (mis2/nomer/style/user    '(:indent 5
                                    :center t
                                    :borders ("||" "||")))
        (mis2/nomer/list nil)
        (misnomer '(:empty :full "Hi." :full :empty)))

    (should (null mis2/nomer/list))

    (mis2/nomer/add misnomer)

    (should-not (null mis2/nomer/list))
    (should (= (length mis2/nomer/list) 1))

    ;; We only have one so we should randomly get it.
    ;; This at least tests that add->random returns nomer correctly.
    (should (seq-set-equal-p (mis2/nomer/random)
                             misnomer)))

  (mis2-ert/mis2-nomer/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//nomer
;;------------------------------------------------------------------------------
;; (defun mis2//nomer (&optional show-buffer)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/nomer ()
  "Test that `mis2//nomer' outputs the nomer correctly.
"
  (mis2-ert/mis2-nomer/setup)

  ;; lexically bind settings, style, nomers lists to override any user settings
  (let ((mis2/nomer/settings/user '(:line-width 80))
        (mis2/nomer/style/user    '(:indent 5
                                    :center t
                                    :borders ("||" "||")))
        (mis2/nomer/list nil)
        (misnomer '(:empty :full "Hi." :full :empty)))

    (should (null mis2/nomer/list))

    (mis2/nomer/add misnomer)

    (should-not (null mis2/nomer/list))
    (should (= (length mis2/nomer/list) 1))
    (should (null mis2-ert/mock/output/to-buffer))

    ;; Call it and check the mock's output.
    (mis2-ert/mock 'mis2//message/output/to-buffer nil
      (mis2-ert/mock 'mis2//message/output/to-minibuffer nil
        (mis2/nomer)))
    (should (string= mis2-ert/mock/output/to-buffer
                     (concat
                      ;; :empty
                      "\n"

                      ;; :full
                      "     ||" ;; indent & border
                      (make-string (- 80 5 2 2) ?-) ;; full fill
                      "||\n" ;; border & newline

                      ;; message line
                      "     ||" ;; indent & border
                      "                                " ;; centering
                      "Hi."
                      "                                    " ;; centering
                      "||\n" ;; border & newline

                      ;; :full
                      "     ||" ;; indent & border
                      (make-string (- 80 5 2 2) ?-) ;; full fill
                      "||\n" ;; border & newline

                      ;; :empty
                      ))))

  (mis2-ert/mis2-nomer/teardown))


;;------------------------------------------------------------------------------
;; Local Test Data
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-message-ert)
