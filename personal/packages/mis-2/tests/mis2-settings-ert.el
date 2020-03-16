;;; mis2-ert-mock-stub.el --- Helpers for mis unit tests. -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2020 Cole Brown

;;; Commentary:

;; Unit tests for mis/settings code using ERT.

;;; Code:


;;------------------------------------misut-------------------------------------
;;--                         Make It So Unit Tests!                           --
;;----------------------------(will center on exit)-----------------------------

;; Test Helpers
(require 'mis2-ert-mock-stub)
(require 'mis2-ert-setup)

;; Test Data
;; None right now...

;; Test Requirements
(require 'mis2-settings)


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Setup & Teardown Helpers
;;------------------------------------------------------------------------------

(defun mis2-ert/mis2-message/setup ()
  "Calls `mis2-ert/setup' for general setup then does setup
specific to this test suite."
  (mis2-ert/setup/setup))


(defun mis2-ert/mis2-message/teardown ()
  "Calls `mis2-ert/teardown' for general setup then does setup
specific to this test suite."
  (mis2-ert/setup/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2/settings/update
;;------------------------------------------------------------------------------
;; (defun mis2/settings/update (plist &rest args)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/settings/update/initially-null ()
  "Test that `mis2/settings/update' updates a mis2 settings plist correctly when
it starts off as nil.
"
  (mis2-ert/mis2-message/setup)

  ;; One setting.
  (let (settings)

    (should (null settings))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :type :default)
                             '(:type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:type :default))))

  ;; Multiple settings in one call.
  (let (settings)

    (should (null settings))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t
                                                   :type :default)
                             '(:echo t :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo t :type :default))))

  ;; Multiple calls.
  (let (settings)

    (should (null settings))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t)
                             '(:echo t)))
    (should-not (null settings))

    (should (seq-set-equal-p settings
                             '(:echo t)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :type :default)
                             '(:echo t :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo t :type :default)))))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/settings/update/initially-set ()
  "Test that `mis2/settings/update' updates a mis2 settings plist correctly when
it starts off as having something.
"
  (mis2-ert/mis2-message/setup)

  ;; One setting.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :type :default)
                             '(:echo-delay 0.3 :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3 :type :default))))

  ;; Multiple settings in one call.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t
                                                   :type :default)
                             '(:echo-delay 0.3 :echo t :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3 :echo t :type :default))))

  ;; Multiple calls.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t)
                             '(:echo-delay 0.3 :echo t)))
    (should-not (null settings))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3 :echo t)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :type :default)
                             '(:echo-delay 0.3 :echo t :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3 :echo t :type :default)))))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/settings/update/initially-set-invalids ()
  "Test that `mis2/settings/update' updates a mis2 settings plist correctly when
it starts off as having something (that is invalid).

We currently ignore anything already in the list, because we are
just 'updating' the list.
"
  (mis2-ert/mis2-message/setup)

  ;; One setting.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :type :default)
                             '(:echo-delay "a string" :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string" :type :default))))

  ;; Multiple settings in one call.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t
                                                   :type :default)
                             '(:echo-delay "a string" :echo t :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string" :echo t :type :default))))

  ;; Multiple calls.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t)
                             '(:echo-delay "a string" :echo t)))
    (should-not (null settings))

    (should (seq-set-equal-p settings
                             '(:echo-delay "a string" :echo t)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :type :default)
                             '(:echo-delay "a string"
                               :echo t
                               :type :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string"
                               :echo t
                               :type :default)))))


;;---
;; Test Case 003
;;---
(ert-deftest mis2-ert/settings/update/invalid-settings ()
  "Test that `mis2/settings/update' updates a mis2 settings plist correctly when
it is given invalid keys, values, or both.
"
  (mis2-ert/mis2-message/setup)

  ;; Setup for a message to test.
  (let (settings)

    ;; §-TODO-§ [2020-03-16]: this here
    ;; (should (null settings))
    ;; (should (seq-set-equal-p (mis2/settings/update settings :echo t :type :default)
    ;;                          '(:echo t :type :default)))
    ;; (should-not (null settings))
    ;; (should (seq-set-equal-p settings
    ;;                          '(:echo t :type :default)))
    ))


;;------------------------------------------------------------------------------
;; Local Test Data
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-message-ert)
