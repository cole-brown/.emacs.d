;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;---------------------------------mis toolbox----------------------------------
;;--                 Small helper functions for all of mis2.                  --
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; List/Cons Functions
;;------------------------------------------------------------------------------
(defun mis2//length-safe (maybe)
  "Returns `length' of MAYBE if it is a string, list, cons...
otherwise returns 0.
"
  ;; if list and not string, do list/cons version.
  (cond ((mis2//list? maybe)

         ;; If second element is a list, this list is a list.
         ;; Otherwise this list is a cons... hopefully.
         (if (listp (cdr maybe))
             (length maybe)
           ;; cons are 2 long, and we definitely have a cons... right?
           2))

        ;; if string, pass to mis2//string/length-safe.
        ((stringp maybe)
         (mis2//string/length-safe maybe))

        ;; Uh... We have to be safe, so... *shrugs*
        (t
         0)))
;; (mis2//length-safe '(x . y))
;; (mis2//length-safe '(x y))
;; (mis2//length-safe '(x y z))
;; (mis2//length-safe "hello")
;; (mis2//length-safe nil)
;; (mis2//length-safe 'hi)


(defun mis2//length-safe/= (maybe equal-to)
  "Returns if the length of MAYBE is equal to EQUAL-TO. That is,
(= (mis2//length-safe maybe) equal-to)
"
  (= (mis2//length-safe maybe) equal-to))


;;------------------------------------------------------------------------------
;; List Functions
;;------------------------------------------------------------------------------


(defun mis2//list? (element)
  "`listp' is insufficient for checking if an element is a list -
it returns true for strings, which we do not want.

This returns t if element is a list or cons, nil if string or otherwise."
  (eq (type-of element) 'cons))


(defun mis2//list-exists? (element)
  "`listp' is insufficient for checking if an element is a list -
it returns true for strings, which we do not want.

This returns t if element is a /non-nil/ list or cons, nil if
string or otherwise."
  (and element
       (mis2//list? element)))


;;---
;; Easy indexing...
;;---
;; `first', `second', etc requires 'cl.
;; `-first-item', `-second-item', etc requires it be passed in a list.
;; So here we are... It's helper functions all the way down...

(defun mis2//nth (n list)
  "Returns Nth element in LIST if LIST passes predicate `mis2//list-exists?'.

N counts from zero.  If LIST is not that long, nil is returned.
"
  (when (mis2//list-exists? list)
    ;; "cons or list" case - don't use `nth' because it needs a list.
    (cond ((= n 0)
           (car list))
          ((= n 1)
           (if (mis2//list? (cdr list))
               (nth 1 list) ;; is a list - grab just 2nd element
             (cdr list)))   ;; is a cons - grab end-of-list/2nd-element

          ;; default case: just use nth
          (t
           (nth n list)))))
;; (mis2//nth 1 '(1 2 3))
;; (mis2//nth 1 '(1 . 2))


(defun mis2//first (list)
  "Returns first element in LIST if LIST passes predicate `mis2//list-exists?'.
"
  (mis2//nth 0 list))


(defun mis2//second (list)
  "Returns second element in LIST if LIST passes predicate `mis2//list-exists?'.
"
  (mis2//nth 1 list))


(defun mis2//third (list)
  "Returns third element in LIST if LIST passes predicate `mis2//list-exists?'.
"
  (mis2//nth 2 list))


(defun mis2//fourth (list)
  "Returns fourth element in LIST if LIST passes predicate `mis2//list-exists?'.
"
  (mis2//nth 3 list))


(defun mis2//fifth (list)
  "Returns fifth element in LIST if LIST passes predicate `mis2//list-exists?'.
"
  (mis2//nth 4 list))


(defun mis2//sixth (list)
  "Returns sixth element in LIST if LIST passes predicate `mis2//list-exists?'.
"
  (mis2//nth 5 list))


;;------------------------------------------------------------------------------
;; String Functions
;;------------------------------------------------------------------------------
(defun mis2//string/length-safe (str-maybe)
  "Returns `length' of STR-MAYBE if it is a string, otherwise returns 0.
"
  (if (stringp str-maybe)
      (length str-maybe)
    0))

;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-utils)
