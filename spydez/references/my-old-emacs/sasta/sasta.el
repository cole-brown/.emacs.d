;; sasta-infix.el -- Convert a list of infix notation to prefix.

;; (c) 2008 Cole Brown

;; Author: Cole Brown <sasta et sourcepan dawt org>
;; Keywords: ERC, DnD, dice, sasta

;; This work 'as-is' we provide.
;; No warranty, express or implied.
;; We've done our best,
;; to debug and test.
;; Liability for damages denied.
;; 
;; Permission is granted hereby,
;; to copy, share, and modify.
;; Use as is fit,
;; free or for profit.
;; On this notice these rights rely.

;; Commentary: 

;; blah blah blah

(defun sasta/number-list-to-string (list)
  (if (< 1 (length list))
      (concat "(" (mapconcat 'number-to-string list " ") ")")
    (concat (mapconcat 'number-to-string list " "))))

(sasta/number-list-to-string '(1))
(sasta/number-list-to-string '(1 2 3))

(defun sasta/trim-whitespace (str)
  (when (string-match "[ \t]*$" str)
    (replace-match "" nil nil str)))

(sasta/trim-whitespace "mne   ")

;; return: ("3d6" (3 6 1))
(defun sasta/dice-roll (word)
  (if (equal nil (string-match "\\([[:digit:]]*\\)d\\([[:digit:]]+\\)" word))
      `(,word nil)
    (let ((dice-str (match-string 0 word))
          (num-dice (string-to-number (if (string< "" (match-string 1 word))
                                          (match-string 1 word)
                                        "1")))
          (dice-sides (string-to-number (match-string 2 word)))
          (rolled))
      (dotimes (number num-dice rolled)
        (setq rolled (cons (+ (random dice-sides) 1) rolled)))
      `(,dice-str ,rolled))))

(sasta/dice-roll "1d6")
(sasta/dice-roll "d6")
(sasta/dice-roll "10d6")
(sasta/dice-roll "")
(sasta/dice-roll "-")

(defun sasta/sum-roll (roll-alist)
  (let ((roll-sum 0))
    (dolist (element roll-alist roll-sum)
      (setq roll-sum (+ roll-sum 
                        (if (cadr element) ; non-nil value of alist element implies dice roll
                            (eval (cons '+ (cadr element))) ; sum dice roll
                          (string-to-number (car element)))))))) ; else try to convert key to number

(sasta/sum-roll '(("1d6" (3)) ("-" nil) ("d100" (26)) ("+" nil) ("5d3" (3 3 1 3 1)) ("*" nil) ("foo" nil) ("+" nil) ("4" nil)))
(+ 3 0 26 3 3 1 3 1 0 0 0 4)

(defun sasta/dice (words-list)
  (let ((roll-str '("" ""))
        (roll-alist (mapcar 'sasta/dice-roll words-list)))
    (dolist (element roll-alist roll-str)
      (setq roll-str `(,(concat (car roll-str) (car element) " ")
                       ,(concat (cadr roll-str) (if (equal nil (cadr element))
                                                    (car element)
                                                  (sasta/number-list-to-string (cadr element))) " "))))
    ;... now do the stupid infix math.
    (concat (sasta/trim-whitespace (car  roll-str))
            ": "
            (sasta/trim-whitespace (cadr roll-str))
            " = "
            (number-to-string (sasta/sum-roll roll-alist)))))

(sasta/dice '("1d6" "-" "d100" "+" "5d3" "*" "foo" "+" "4")) 
"1d6 - d100 + 5d3 * foo + 4: 4 - 32 + (3 2 2 3 1) * foo + 4 = 51" (+ 4 32 3 2 2 3 1 4)

;;------------------------------------------------------------------------------
;; ERC integration
;;------------------------------------------------------------------------------
(defun erc-cmd-DICE (&rest words)
  (if words
      (erc-send-action (erc-default-target) (concat "rolls " (sasta/dice words)))
    (erc-display-message nil 'notice 'active "usage: /dice 2d6 + 4 + d10")))

(defun erc-cmd-ROLL (&rest words)
  (if words
      (erc-send-action (erc-default-target) (concat "rolls " (sasta/dice words)))
    (erc-display-message nil 'notice 'active "usage: /roll str + 4 + sneak")))
