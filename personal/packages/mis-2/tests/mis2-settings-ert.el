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
(require 'mis2-themes)
(require 'mis2-settings)


;;---
;; Runner Shortcuts
;;---
;; (ert "mis2-ert/settings/.*")
;; (ert "mis2-ert/style/.*")
;; (ert "mis2-ert/data/.*")


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Setup & Teardown Helpers
;;------------------------------------------------------------------------------

(defun mis2-ert/mis2-settings/setup ()
  "Calls `mis2-ert/setup' for general setup then does setup
specific to this test suite."
  (mis2-ert/setup/setup))


(defun mis2-ert/mis2-settings/teardown ()
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
  (mis2-ert/mis2-settings/setup)

  ;; One setting.
  (let (settings)

    (should (null settings))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :theme :default)
                             '(:theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:theme :default))))

  ;; Multiple settings in one call.
  (let (settings)

    (should (null settings))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t
                                                   :theme :default)
                             '(:echo t :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo t :theme :default))))

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
                                                   :theme :default)
                             '(:echo t :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo t :theme :default))))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/settings/update/initially-set ()
  "Test that `mis2/settings/update' updates a mis2 settings plist correctly when
it starts off as having something.
"
  (mis2-ert/mis2-settings/setup)

  ;; One setting.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :theme :default)
                             '(:echo-delay 0.3 :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3 :theme :default))))

  ;; Multiple settings in one call.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t
                                                   :theme :default)
                             '(:echo-delay 0.3 :echo t :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3 :echo t :theme :default))))

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
                                                   :theme :default)
                             '(:echo-delay 0.3 :echo t :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3 :echo t :theme :default))))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/settings/update/initially-set-invalids ()
  "Test that `mis2/settings/update' updates a mis2 settings plist correctly when
it starts off as having something (that is invalid).

We currently ignore anything already in the list, because we are
just 'updating' the list.
"
  (mis2-ert/mis2-settings/setup)

  ;; One setting.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :theme :default)
                             '(:echo-delay "a string" :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string" :theme :default))))

  ;; Multiple settings in one call.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t
                                                   :theme :default)
                             '(:echo-delay "a string" :echo t :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string" :echo t :theme :default))))

  ;; Multiple calls.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :echo t)
                             '(:echo-delay "a string" :echo t)))
    (should-not (null settings))

    (should (seq-set-equal-p settings
                             '(:echo-delay "a string" :echo t)))
    (should (seq-set-equal-p (mis2/settings/update settings
                                                   :theme :default)
                             '(:echo-delay "a string"
                                           :echo t
                                           :theme :default)))
    (should-not (null settings))
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string"
                                           :echo t
                                           :theme :default))))
  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 003
;;---
(ert-deftest mis2-ert/settings/update/invalid-settings ()
  "Test that `mis2/settings/update' updates a mis2 settings plist correctly when
it is given invalid keys, values, or both.
"
  (mis2-ert/mis2-settings/setup)

  ;; Invalid key into null list.
  (let (settings)

    (should (null settings))
    (should-error (mis2/settings/update settings
                                        :illegal-key :value-does-not-matter))
    (should (null settings)))

  ;; Invalid value into null list.
  (let (settings)

    (should (null settings))
    (should-error (mis2/settings/update settings
                                        :theme :illegal-value))
    (should (null settings)))


  ;; Invalid key into existing list.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should-error (mis2/settings/update settings
                                        :illegal-key :value-does-not-matter))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3))))

  ;; Invalid value into existing list.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should-error (mis2/settings/update settings
                                        :theme :illegal-value))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3))))

  (mis2-ert/mis2-settings/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2/settings/set
;;------------------------------------------------------------------------------
;; (defun mis2/settings/set (plist &rest args)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/settings/set/initially-null ()
  "Test that `mis2/settings/set' sets a mis2 settings plist correctly when
it starts off as nil.
"
  (mis2-ert/mis2-settings/setup)

  ;; One setting.
  (let (settings)

    (should (null settings))
    (should (seq-set-equal-p (mis2/settings/set settings
                                                :theme :default)
                             '(:theme :default)))
    (should (null settings))) ;; settings should not be changed by set.

  ;; Multiple settings in one call.
  (let (settings)

    (should (null settings))
    (should (seq-set-equal-p (mis2/settings/set settings
                                                :echo t
                                                :theme :default)
                             '(:echo t :theme :default)))
    (should (null settings)))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/settings/set/initially-set ()
  "Test that `mis2/settings/set' sets a mis2 settings plist correctly when
it starts off as having something.
"
  (mis2-ert/mis2-settings/setup)

  ;; One setting.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/set settings
                                                :theme :default)
                             '(:echo-delay 0.3 :theme :default)))
    ;; should still be unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3))))

  ;; Multiple settings in one call.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/set settings
                                                :echo t
                                                :theme :default)
                             '(:echo-delay 0.3 :echo t :theme :default)))
    ;; should still be unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3))))

  ;; Multiple calls.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should (seq-set-equal-p (mis2/settings/set settings
                                                :echo t)
                             '(:echo-delay 0.3 :echo t)))
    ;; should still be unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))

    ;; 2nd call
    (should (seq-set-equal-p (mis2/settings/set settings
                                                :theme :default)
                             '(:echo-delay 0.3 :theme :default)))
    ;; should still be unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3))))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/settings/set/initially-set-invalids ()
  "Test that `mis2/settings/set' sets a mis2 settings plist correctly when
it starts off as having something (that is invalid).

We currently ignore anything already in the list, because we are
just 'updating' the list.
"
  (mis2-ert/mis2-settings/setup)

  ;; One setting.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/set settings
                                                :theme :default)
                             '(:echo-delay "a string" :theme :default)))
    (should-not (null settings))
    ;; unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string"))))

  ;; Multiple settings in one call.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/set settings
                                                :echo t
                                                :theme :default)
                             '(:echo-delay "a string" :echo t :theme :default)))
    (should-not (null settings))
    ;; unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string"))))

  ;; Multiple calls.
  (let ((settings '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/settings/set settings
                                                :echo t)
                             '(:echo-delay "a string" :echo t)))
    (should-not (null settings))

    ;; unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string")))
    (should (seq-set-equal-p (mis2/settings/set settings
                                                :theme :default)
                             '(:echo-delay "a string"
                                           :theme :default)))
    (should-not (null settings))
    ;; unchanged
    (should (seq-set-equal-p settings
                             '(:echo-delay "a string"))))
  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 003
;;---
(ert-deftest mis2-ert/settings/set/invalid-settings ()
  "Test that `mis2/settings/set' sets a mis2 settings plist correctly when
it is given invalid keys, values, or both.
"
  (mis2-ert/mis2-settings/setup)

  ;; Invalid key into null list.
  (let (settings)

    (should (null settings))
    (should-error (mis2/settings/set settings
                                     :illegal-key :value-does-not-matter))
    (should (null settings)))

  ;; Invalid value into null list.
  (let (settings)

    (should (null settings))
    (should-error (mis2/settings/set settings
                                     :theme :illegal-value))
    (should (null settings)))


  ;; Invalid key into existing list.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should-error (mis2/settings/set settings
                                     :illegal-key :value-does-not-matter))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3))))

  ;; Invalid value into existing list.
  (let ((settings '(:echo-delay 0.3)))

    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3)))
    (should-error (mis2/settings/set settings
                                     :theme :illegal-value))
    (should (seq-set-equal-p settings
                             '(:echo-delay 0.3))))

  (mis2-ert/mis2-settings/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//style/check-key
;;------------------------------------------------------------------------------
;; (defun mis2//style/check-key (key)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/style/check-key/basic-validity ()
  "Test that `mis2//style/check-key' validates basic themes correctly
(string, int, etc).
"
  (mis2-ert/mis2-settings/setup)

  (should (eq (mis2//style/check-key :number) #'numberp))
  (should (eq (mis2//style/check-key :integer) #'integerp))
  (should (eq (mis2//style/check-key :float) #'floatp))
  (should (eq (mis2//style/check-key :string) #'stringp))
  (should (eq (mis2//style/check-key :char) #'characterp))
  (should (eq (mis2//style/check-key :list) #'listp))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/style/check-key/null ()
  "Test that `mis2//style/check-key' works ok with nil input.
i.e. it should return :*bad-key-error*.
"
  (mis2-ert/mis2-settings/setup)

  (should (eq (mis2//style/check-key nil) :*bad-key-error*))
  (let ((nil-key nil)
        never-set-key)
    (should (eq (mis2//style/check-key nil-key) :*bad-key-error*))
    (should (eq (mis2//style/check-key never-set-key) :*bad-key-error*)))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/style/check-key/normal-keys ()
  "Test that `mis2//style/check-key' works ok with `mis2/style/keys'
i.e. Check that it can do :const, :alist, :or etc.
"
  (mis2-ert/mis2-settings/setup)

  (should (seq-set-equal-p (mis2//style/check-key :center)
                           '(:const (t nil))))
  (should (seq-set-equal-p (mis2//style/check-key :face)
                           '(:alist ':theme)))
  (should (seq-set-equal-p (mis2//style/check-key :margins)
                           '(:list (:str/nil :str/nil))))
  (should (seq-set-equal-p
           (mis2//style/check-key :padding)
           '(:or ((:list (:str/nil :str/nil))
                  (:list (:char (:const (:empty :fill)) :integer)))))))


;;------------------------------------------------------------------------------
;; Test: mis2//style/check-value
;;------------------------------------------------------------------------------
;; (defun mis2//style/check-value (key value &optional key-info)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/style/check-value/basic-validity ()
  "Test that `mis2//style/check-value' validates basic types correctly
(string, int, etc).
"
  (mis2-ert/mis2-settings/setup)

  ;;---
  ;; number (int, float, whatever)
  ;;---
  (should (= (mis2//style/check-value :number 42)
             42))
  (should (= (mis2//style/check-value :number 42.1)
             42.1))
  (should (eq (mis2//style/check-value :number nil)
              :*bad-value-error*))

  ;;---
  ;; int (no floats allowed)
  ;;---
  (should (= (mis2//style/check-value :integer 42)
             42))
  (should (eq (mis2//style/check-value :integer 42.1)
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :integer nil)
              :*bad-value-error*))

  ;;---
  ;; float (no ints allowed)
  ;;---
  (should (= (mis2//style/check-value :float 42.1)
             42.1))
  (should (eq (mis2//style/check-value :float 42)
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :float nil)
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :float "string-value")
              :*bad-value-error*))

  ;;---
  ;; strings
  ;;---
  (should (string= (mis2//style/check-value :string "string-value")
                   "string-value"))
  (should (eq (mis2//style/check-value :string 1234)
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :string nil)
              :*bad-value-error*))

  ;;---
  ;; chars
  ;;---
  (should (char-equal (mis2//style/check-value :char ?-)
                      ?-))
  (should (= (mis2//style/check-value :char ?-)
             45)) ;; '-' is 45 in the language of integers

  (let ((case-fold-search nil))
    ;; Set emacs to ignore case to make sure we get back the same case
    ;; we sent in.
    (should-not (char-equal (mis2//style/check-value :char ?A)
                            ?a)))
  ;; It's less than max-char, so... ok.
  (should (= (mis2//style/check-value :char 1234)
             1234))
  (should (eq (mis2//style/check-value :char (1+ (max-char)))
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :char nil)
              :*bad-value-error*))

  ;;---
  ;; list
  ;;---
  (should (seq-set-equal-p (mis2//style/check-value :list '(1 2 3))
                           '(1 2 3)))
  (should (seq-set-equal-p (mis2//style/check-value :list '(3 2 1))
                           '(3 2 1)))
  (should (eq (mis2//style/check-value :list :not-a-list)
              :*bad-value-error*))
  ;; Apparently `nil' is a list? Leave for now.
  ;; May fix to return :*bad-value-error* if we need/want it to.
  (should (eq (mis2//style/check-value :list nil)
              nil))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/style/check-value/normal-values ()
  "Test that `mis2//style/check-value' works ok with `mis2/style/keys'
i.e. Check that it can do :center, :face, :padding, etc.
"
  (mis2-ert/mis2-settings/setup)

  ;;---
  ;; center
  ;;---
  (should (eq (mis2//style/check-value :center nil)
              nil))
  (should (eq (mis2//style/check-value :center t)
              t))
  (should (eq (mis2//style/check-value :center :not-valid)
              :*bad-value-error*))

  ;;---
  ;; face
  ;;---
  ;; §-TODO-§ [2020-03-20]: Not quite sure how to do this one yet...
  ;; (should (eq (mis2//style/check-value :face ???)
  ;;             nil))
  ;; (should (eq (mis2//style/check-value :face ???)
  ;;             t))
  ;; (should (eq (mis2//style/check-value :face ???)
  ;;             :*bad-value-error*))

  ;;---
  ;; margins
  ;;---
  (should (seq-set-equal-p (mis2//style/check-value :margins '("hi" "hello"))
                           '("hi" "hello")))
  (should (eq (mis2//style/check-value :margins '("hi" "hello" "jeff"))
              :*bad-value-error*))

  ;;---
  ;; padding (:or in action)
  ;;---
  (should (seq-set-equal-p (mis2//style/check-value :padding '("hi" "hello"))
                           '("hi" "hello")))
  (should (eq (mis2//style/check-value :padding '("hi" "hello" "jeff"))
              :*bad-value-error*))
  (should (seq-set-equal-p (mis2//style/check-value :padding '(?- :empty 3))
                           '(?- :empty 3)))
  (should (eq (mis2//style/check-value :padding '("hi" :empty 3))
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :padding '(?- :not-valid 3))
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :padding '(?- :empty nil))
              :*bad-value-error*))
  (should (eq (mis2//style/check-value :padding nil)
              :*bad-value-error*))

  (mis2-ert/mis2-settings/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2/style/update
;;------------------------------------------------------------------------------
;; (defmacro mis2/style/update (plist &rest args)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/style/update/initially-null ()
  "Test that `mis2/style/update' updates a mis2 style plist correctly when
it starts off as nil.
"
  (mis2-ert/mis2-settings/setup)

  ;; One style.
  (let (style)

    (should (null style))
    (should (seq-set-equal-p (mis2/style/update style
                                                :center t)
                             '(:center t)))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t))))

  ;; Multiple style in one call.
  (let (style)

    (should (null style))
    (should (seq-set-equal-p (mis2/style/update style
                                                :center t
                                                :margins '("<<" ">>"))
                             '(:center t :margins ("<<" ">>"))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t :margins ("<<" ">>")))))

  ;; Multiple calls.
  (let (style)

    (should (null style))
    (should (seq-set-equal-p (mis2/style/update style
                                                :center t)
                             '(:center t)))
    (should-not (null style))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should (seq-set-equal-p (mis2/style/update style
                                                :margins '("<<" ">>"))
                             '(:center t :margins ("<<" ">>"))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t :margins ("<<" ">>")))))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/style/update/initially-set ()
  "Test that `mis2/style/update' updates a mis2 style plist correctly when
it starts off as having something.
"
  (mis2-ert/mis2-settings/setup)

  ;; One style.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should (seq-set-equal-p (mis2/style/update style
                                                :padding '(?- :empty 3))
                             '(:center t :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t :padding (?- :empty 3)))))

  ;; Multiple style in one call.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should (seq-set-equal-p (mis2/style/update style
                                                :margins '("<<" ">>")
                                                :padding '(?- :empty 3))
                             '(:center t
                               :margins ("<<" ">>")
                               :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t
                               :margins ("<<" ">>")
                               :padding (?- :empty 3)))))

  ;; Multiple calls.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should (seq-set-equal-p (mis2/style/update style
                                                :margins '("<<" ">>"))
                             '(:center t :margins ("<<" ">>"))))
    (should-not (null style))

    (should (seq-set-equal-p style
                             '(:center t :margins ("<<" ">>"))))
    (should (seq-set-equal-p (mis2/style/update style
                                                 :padding '(?- :empty 3))
                             '(:center t
                               :margins ("<<" ">>")
                               :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t
                               :margins ("<<" ">>")
                               :padding (?- :empty 3)))))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/style/update/initially-set-invalids ()
  "Test that `mis2/style/update' updates a mis2 style plist correctly when
it starts off as having something (that is invalid).

We currently ignore anything already in the list, because we are
just 'updating' the list.
"
  (mis2-ert/mis2-settings/setup)

  ;; One style.
  (let ((style '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/style/update style
                                                :padding '(?- :empty 3))
                             '(:echo-delay "a string" :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:echo-delay "a string" :padding (?- :empty 3)))))

  ;; Multiple style in one call.
  (let ((style '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/style/update style
                                                :margins '("<<" ">>")
                                                :padding '(?- :empty 3))
                             '(:echo-delay "a string"
                               :margins ("<<" ">>")
                               :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:echo-delay "a string"
                               :margins ("<<" ">>")
                               :padding (?- :empty 3)))))

  ;; Multiple calls.
  (let ((style '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/style/update style
                                                :margins '("<<" ">>"))
                             '(:echo-delay "a string" :margins ("<<" ">>"))))
    (should-not (null style))

    (should (seq-set-equal-p style
                             '(:echo-delay "a string" :margins ("<<" ">>"))))
    (should (seq-set-equal-p (mis2/style/update style
                                                :padding '(?- :empty 3))
                             '(:echo-delay "a string"
                               :margins ("<<" ">>")
                               :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:echo-delay "a string"
                               :margins ("<<" ">>")
                               :padding (?- :empty 3)))))
  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 003
;;---
(ert-deftest mis2-ert/style/update/invalid-style ()
  "Test that `mis2/style/update' updates a mis2 style plist correctly when
it is given invalid keys, values, or both.
"
  (mis2-ert/mis2-settings/setup)

  ;; Invalid key into null list.
  (let (style)

    (should (null style))
    (should-error (mis2/style/update style
                                     :illegal-key :value-does-not-matter))
    (should (null style)))

  ;; Invalid value into null list.
  (let (style)

    (should (null style))
    (should-error (mis2/style/update style
                                     :center :illegal-value))
    (should (null style)))


  ;; Invalid key into existing list.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should-error (mis2/style/update style
                                     :illegal-key :value-does-not-matter))
    (should (seq-set-equal-p style
                             '(:center t))))

  ;; Invalid value into existing list.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should-error (mis2/style/update style
                                     :margins :illegal-value))
    (should (seq-set-equal-p style
                             '(:center t))))

  (mis2-ert/mis2-settings/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2/style/set
;;------------------------------------------------------------------------------
;; (defmacro mis2/style/set (plist &rest args)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/style/set/initially-null ()
  "Test that `mis2/style/set' sets a mis2 style plist correctly when
it starts off as nil.
"
  (mis2-ert/mis2-settings/setup)

  ;; One style.
  (let (style)

    (should (null style))
    (should (seq-set-equal-p (mis2/style/set style
                                             :center t)
                             '(:center t)))
    (should (null style)))

  ;; Multiple styles in one call.
  (let (style)

    (should (null style))
    (should (seq-set-equal-p (mis2/style/set style
                                             :center t
                                             :margins '("<<" ">>"))
                             '(:center t :margins ("<<" ">>"))))
    (should (null style)))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 001
;;---
(ert-deftest mis2-ert/style/set/initially-set ()
  "Test that `mis2/style/set' sets a mis2 style plist correctly when
it starts off as having something.
"
  (mis2-ert/mis2-settings/setup)

  ;; One style.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should (seq-set-equal-p (mis2/style/set style
                                             :padding '(?- :empty 3))
                             '(:center t :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t))))

  ;; Multiple style in one call.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should (seq-set-equal-p (mis2/style/set style
                                             :margins '("<<" ">>")
                                             :padding '(?- :empty 3))
                             '(:center t
                               :margins ("<<" ">>")
                               :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:center t))))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 002
;;---
(ert-deftest mis2-ert/style/set/initially-set-invalids ()
  "Test that `mis2/style/set' sets a mis2 style plist correctly when
it starts off as having something (that is invalid).

We currently ignore anything already in the list, because we are
just 'updating' the list.
"
  (mis2-ert/mis2-settings/setup)

  ;; One style.
  (let ((style '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/style/set style
                                             :padding '(?- :empty 3))
                             '(:echo-delay "a string" :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:echo-delay "a string"))))

  ;; Multiple style in one call.
  (let ((style '(:echo-delay "a string")))

    (should (seq-set-equal-p (mis2/style/set style
                                             :margins '("<<" ">>")
                                             :padding '(?- :empty 3))
                             '(:echo-delay "a string"
                               :margins ("<<" ">>")
                               :padding (?- :empty 3))))
    (should-not (null style))
    (should (seq-set-equal-p style
                             '(:echo-delay "a string"))))

  (mis2-ert/mis2-settings/teardown))


;;---
;; Test Case 003
;;---
(ert-deftest mis2-ert/style/set/invalid-style ()
  "Test that `mis2/style/set' sets a mis2 style plist correctly when
it is given invalid keys, values, or both.
"
  (mis2-ert/mis2-settings/setup)

  ;; Invalid key into null list.
  (let (style)

    (should (null style))
    (should-error (mis2/style/set style
                                  :illegal-key :value-does-not-matter))
    (should (null style)))

  ;; Invalid value into null list.
  (let (style)

    (should (null style))
    (should-error (mis2/style/set style
                                  :center :illegal-value))
    (should (null style)))


  ;; Invalid key into existing list.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should-error (mis2/style/set style
                                  :illegal-key :value-does-not-matter))
    (should (seq-set-equal-p style
                             '(:center t))))

  ;; Invalid value into existing list.
  (let ((style '(:center t)))

    (should (seq-set-equal-p style
                             '(:center t)))
    (should-error (mis2/style/set style
                                  :margins :illegal-value))
    (should (seq-set-equal-p style
                             '(:center t))))

  (mis2-ert/mis2-settings/teardown))


;;------------------------------------------------------------------------------
;; Test: mis2//data/get
;;------------------------------------------------------------------------------
;; (defun mis2//data/get (key plist)

;;---
;; Test Case 000
;;---
(ert-deftest mis2-ert/data/get ()
  "Test that `mis2/style/set' sets a mis2 style plist correctly when
it starts off as nil.
"
  (mis2-ert/mis2-settings/setup)

  ;; Key exists - return value
  (should (eq (mis2//settings/get/from-data
               :echo
               '(:mis2//settings (:echo t :echo-delay 2) :mis2//testing))
              t))
  ;; key DNE - return nil
  (should (eq (mis2//settings/get/from-data
               :interactive
               '(:mis2//settings (:echo t :echo-delay 2) :mis2//testing))
              nil))
  ;; request invalid key - error out
  (should-error (mis2//settings/get/from-data
                 :center ;; not a settings key
                 '(:mis2//settings (:echo t :echo-delay 2) :mis2//testing)))
  ;; no settings in mis2 plist - return nil
  (should (null (mis2//settings/get/from-data
                 :echo
                 '(:mis2//style (:center t) :mis2//testing))))

  ;; some simple tests with in-place mal-formed lists
  (should-error (mis2//settings/get/from-data
                 :echo
                 ;; doesn't pass `mis2//data/plist?' check
                 '(:mis2//style (:center t))))
  (should-error (mis2//settings/get/from-data
                 :echo
                 ;; is a settings plist instead of a mis2 plist
                 '(:echo t :echo-delay 2)))

 (mis2-ert/mis2-settings/teardown))


;;------------------------------------------------------------------------------
;; Local Test Data
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-message-ert)
