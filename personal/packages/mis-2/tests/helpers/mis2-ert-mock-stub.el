;;; mis2-ert-mock-stub.el --- Helpers for mis unit tests. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Cole Brown

;;; Commentary:

;; Unit test helpers for mis code using ERT.

;; Macro for making stub/mocks. Setup functions.

;;; Code:


;;------------------------------------misut-------------------------------------
;;--                    Mocks & Stubs & Helpers, oh my.                       --
;;----------------------------(will center on exit)-----------------------------

(require 'cl) ;; cl-flet*


;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

(defvar mis2-ert/stub/called nil
  "List of symbols or nil.")


;;------------------------------------------------------------------------------
;; Mock / Stub Macros
;;------------------------------------------------------------------------------

;; With a bit of help from:
;; https://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
(defmacro mis2-ert/mock (func &optional mock &rest body)
  "Replaces FUNC with MOCK for duration of BODY (`cl-letf' binding).

If MOCK is nil, FUNC will be replaced with a function with symbol name:
  `mis2-ert/mock/FUNC'.

Sets up BODY to not actually call FUNC... Just calls fake
handlers MOCK and then we can inspect status and return what we want.

FUNC should be the function symbol to be replaced (e.g. message).
MOCK should be the function symbol to replace it (e.g. my-message-tester).

Executes BODY forms if successful setting up mock functions.
"
  (declare (indent defun))
  `(let* ((func-sym ,func)
          (mock-sym (or ,mock
                        (intern (concat "mis2-ert/mock/"
                                        (symbol-name func-sym))))))
     (cl-letf (((symbol-function func-sym) mock-sym))
       ,@body)))
;; (defun xx (fmt &rest args) (message "xx: %S" (format fmt args)))
;; (defun yy (fmt &rest args) (message "yy: %S" (format fmt args)))
;; (defun mis2-ert/mock/xx (f &rest a) (message "zz: %S" (format f a)))
;; (macroexpand '(mis2-ert/mock 'xx 'yy (xx "hello?")))
;; (mis2-ert/mock 'xx 'yy (xx "hello?"))
;; (mis2-ert/mock 'xx nil (xx "hello?"))


(defmacro mis2-ert/stub (func &rest body)
  "Replaces FUNC with a stub for duration of BODY (`cl-letf' binding).

The stub will just push the FUNC symbol onto the
`mis2-ert/stub/called' list, so be sure to clear that out as
needed.

Sets up BODY to not actually call FUNC ... Just calls fake
handlers and then we can inspect status and return what we want.

FUNC should be the function symbol to be replaced (e.g. message).

Executes BODY forms if successful setting up mock functions.
"
  (declare (indent defun))
  `(let* ((func-sym ,func))
     ;; Set FUNC to be a lambda that just pushes FUNC symbol to called list.
     (cl-letf (((symbol-function func-sym)
                (lambda (&rest ignored)
                  (push func-sym mis2-ert/stub/called))))
       ,@body)))
;; (defun xx (fmt &rest args) (message "xx: %S" (format fmt args)))
;; (macroexpand '(mis2-ert/stub 'xx (xx "hello?")))
;; mis2-ert/stub/called
;; (mis2-ert/stub 'xx (xx "hello?"))
;; mis2-ert/stub/called


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-ert-mock-stub)
