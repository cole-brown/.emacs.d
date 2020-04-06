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

(defvar mis2-ert/setup/themes/storage nil
  "mis2-ert/mis2-contents' backup and restore helpers store/retrieve a copy of
`mis2/themes' here.")


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
  (mis2-ert/setup/reset)

  ;; get themes to a testing value
  (mis2-ert/setup/themes/test-values))


(defun mis2-ert/setup/teardown ()
  "End-of-test teardown steps.
"
  ;; put back themes
  (mis2-ert/setup/themes/restore)
  ;; Revert fail functions.
  (mis2-ert/teardown/error-out-functions))


;;---
;; Themes
;;---
(defun mis2-ert/setup/themes/test-values (&optional themes)
  "Set `mis2/themes' to THEMES if non-nil, or to default test data if nil.
"
  (mis2-ert/mis2-contents/themes/backup)
  (setq mis2/themes
        (or themes
            '((:default (;; user-friendly stuff
                         :title       font-lock-builtin-face
                         :inattention font-lock-comment-face
                         :highlight   font-lock-constant-face
                         :text        font-lock-warning-face

                         ;; box by piece names
                         :indent      font-lock-doc-face
                         :margins     font-lock-function-name-face
                         :borders     font-lock-comment-delimiter-face
                         :padding     font-lock-string-face))

              (:fancy   (:title       compilation-error
                         :inattention compilation-info
                         :highlight   compilation-line-number
                         :border      compilation-mode-line-exit
                         :padding     compilation-mode-line-fail
                         :text        compilation-warning))))))
;; (mis2-ert/setup/themes/test-values)


(defun mis2-ert/setup/themes/backup ()
  "Reverts `mis2/themes' back to it's backed up value.
"
  (unless mis2-ert/setup/themes/storage
    (setq mis2-ert/setup/themes/storage (copy-sequence mis2/themes))))


(defun mis2-ert/setup/themes/restore ()
  "Reverts `mis2/themes' back to it's backed up value.
"
  (setq mis2/themes (copy-sequence mis2-ert/contents/themes/storage)))


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
  ;; (mis2-ert/setup/error-out 'mis2//whatever)
  )


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
