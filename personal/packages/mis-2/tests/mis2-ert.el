;;; mis2-test.el --- Test runner for mis2. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for mis2 code using ERT.

;; Do not load/require this file for normal mis2.el usage. Only bother if you
;; want to run the unit tests easily.

;;; Code:

;;------------------------------------misut-------------------------------------
;;--                         Make It So Unit Tests!                           --
;;----------------------------(will center on exit)-----------------------------

;; Not requiring our files as I don't want to push the test dir into the load
;; path. Use `mis2-ert/load' instead.


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defconst mis2-ert/test-selector-regexp "mis2-ert/"
  "String that will select all our unit tests, as we have \"namespaced\" them
all in a 'mis2-ert/' \"namespace\".")


(defconst mis2-ert/test-buffer-name "*mis2 ERT Results*"
  "Name of buffer for ERT test run results to be displayed.")


;;------------------------------------------------------------------------------
;; Test Runners
;;------------------------------------------------------------------------------

(defun mis2-ert/load ()
  "Loads all mis2 unit test files."
  (interactive)

  (message "mis2-ert/load: Loading...")

  ;;---
  ;; Data
  ;;---
  ;; (load-file "data/mis2-ert-data-xyz.el")

  ;;---
  ;; mis2 itself
  ;;---
  (load-file "../mis2-themes.el")
  (load-file "../mis2-settings.el")
  (load-file "../mis2-contents.el")
  (load-file "../mis2-message.el")

  ;;---
  ;; Helpers
  ;;---
  (load-file "helpers/mis2-ert-mock-stub.el")
  ;; (load-file "helpers/mis2-ert-ddd.el")
  ;; ...
  (load-file "helpers/mis2-ert-setup.el") ;; Can depending on all the other helpers.

  ;;---
  ;; Tests (in implementation/simplicity/dependency-pyramid order)
  ;;---
  (load-file "mis2-settings-ert.el")
  (load-file "mis2-contents-ert.el")
  (load-file "mis2-message-ert.el")

  ;;---
  ;; Done
  ;;---
  (message "mis2-ert/load: ...Done."))


(defun mis2-ert/run ()
  "Runs all Mis2.el unit tests."
  (interactive)
  (ert mis2-ert/test-selector-regexp mis2-ert/test-buffer-name))
