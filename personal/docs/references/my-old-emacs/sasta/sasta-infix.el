;; sasta-infix.el -- Convert a list of infix notation to prefix.

;; (c) 2008 Cole Brown
;; See LICENSE or sasta.el for license.

;; blah blah

;; Operator precedence in alist. Equal precedence in same list.
(setq *sasta/operator-precedence* '((lowest  + -)
                                    (highest * /)))

;; Levels of precedence used in operator precedence alist.
;; NOTE: Must be in order, from lowest to highest.
(setq *sasta/precedence-levels* '(lowest highest))

(defun sasta/operators (precedence-level)
  "Get list of operators from precedence alist."
  (cdr (assoc precedence-level *sasta/operator-precedence*)))

;(operators 'lowest)  => (+ -)
;(operators 'mid)     => nil
;(operators 'highest) => (* /)

(defun sasta/min-of-list (list)
  "This min function takes a list as arg and ignores nils."
  (let* ((nilless (remq nil list))
        (len     (length nilless)))
    (cond ((> len 1) (eval (cons 'min nilless)))
          ((equal len 1) (car nilless))
          (t nil))))

; (sasta/min-of-list '(nil nil 8 7)) => 7

(defun sasta/find-first-op-in-precedence (expression op-prec)
  "Find the first occurrence in an expression of one of the operators
in a list of equal precedence ops."
  (min-of-list
   (mapcar (lambda (operator)
             (position operator expression))
           (operators op-prec))))

;(sasta/find-first-op-in-precedence '(2 + 3 / 7 + 2 * 1) 'lowest) => 1

(defun sasta/find-lowest-precedence-operator (expression)
  "Find the index of the lowest precedence operator in the expression."
  (catch 'found-index
    (dolist (op-prec *sasta/precedence-levels*)
      (let ((index (find-first-op-in-precedence expression op-prec)))
        (when index (throw 'found-index index))))))

;(sasta/find-lowest-precedence-operator '(2 * 3 / 7 / 2 - 1)) => 7

(defun sasta/split-expression (expression index)
  "Slices the expression list into pre-element sublist, element, and post-element sublist."
  (if (and (<= 0 index) (<= index (1- (length expression))))
      (values (subseq expression 0 index)
              (elt expression index)
              (subseq expression (1+ index)))
    (values nil nil nil)))

;(sasta/split-expression '(1 * 2 + 3 / 4) 3) => ((1 * 2) + (3 / 4))

(defun sasta/infix->prefix (expression)
  "Transforms an infix notation expression (1 * 2 + 3 / 4) into the proper prefix equivalent (+ (* 1 2) (/ 3 4)).
Operators must be symbols, not strings."
  (let ((index (find-lowest-precedence-operator expression)))
    (if index (multiple-value-bind (before operator after) (split-expression expression index)
                (list operator
                      (infix->prefix before)
                      (infix->prefix after)))
      (car expression))))

;(sasta/infix->prefix '(2 * 3 + 7 / 2 + 1)) => (+ (* 2 3) (+ (/ 7 2) 1))
;(eval (sasta/infix->prefix '(2 * 3 + 7 / 2 + 1))) => 10
;6 + 3.5 + 1 = 10.5 aka 10 in integer math









