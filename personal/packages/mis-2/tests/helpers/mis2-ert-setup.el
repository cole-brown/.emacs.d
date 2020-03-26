;;; mis2-ert-setup.el --- Helpers for mis unit tests. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Cole Brown

;;; Commentary:

;; Unit test helpers for mis code using ERT.

;; Macro for making stub/mocks. Setup functions.

;;; Code:


;;------------------------------------misut-------------------------------------
;;--                    Mocks & Stubs & Helpers, oh my.                       --
;;----------------------------(will center on exit)-----------------------------

(require 'cl) ;; cl-flet*
(require 'mis2-ert-mock-stub)

;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

(defvar mis2-ert/setup/error-out-functions/orig nil
  "Data saved about original function definitons.")


;;------------------------------------------------------------------------------
;; Setup / Reset
;;------------------------------------------------------------------------------

(defun mis2-ert/setup/reset ()
  "Start-of or during test reset.
"
  ;;---
  ;; Reset Mis2.el
  ;;---
  ;; (setq mis2/themes nil)

  ;;---
  ;; Reset Tests
  ;;---

  ;; Our common mocks and stubs.
  (setq mis2-ert/stub/called nil))


(defun mis2-ert/setup/setup ()
  "Start-of-test setup steps.
"
  ;; Reset vars & things.
  (mis2-ert/setup/reset))


(defun mis2-ert/setup/teardown ()
  "End-of-test teardown steps.
"
  ;; Revert fail functions.
  (mis2-ert/teardown/error-out-functions))


;;------------------------------------------------------------------------------
;; Error out of these by default
;;------------------------------------------------------------------------------

(defun mis2-ert/setup/error-out (func)
  "Set up FUNC to throw an error. Uses `fset' to replace the function def with
this error throwing one. Will save the old function definition for reverting to.
"
  (push '(func (symbol-function 'func))
        mis2-ert/setup/error-out-functions/orig)

  (fset func
        (lambda (&rest ignored)
          "Error-Out function."
          (error (concat "(error-out) "
                         (symbol-name func)
                         ": Intentionally dying here - find an upstream "
                         "function to terminate the test at.")))))


(defun mis2-ert/setup/error-out-functions ()
  "Set up functions we don't want to get to via tests to throw errors.
"
  (mis2-ert/setup/error-out 'mis2-oauth2-token)
  (mis2-ert/setup/error-out 'mis2-api-call-async))


;; §-TODO-§ [2020-01-10]: don't think this works...
(defun mis2-ert/teardown/error-out-functions ()
  "Reverts all the functions in `mis2-ert/setup/error-out-functions/orig'.
"
  (dolist (cell mis2-ert/setup/error-out-functions/orig)
    (when cell
      (fset (nth 0 cell) (nth 1 cell)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-ert-setup)
