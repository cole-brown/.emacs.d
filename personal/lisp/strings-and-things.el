;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------(not c-strings...)------------------------------
;;--                          Strings and Things!                            --
;;-----------------------------------------------------------------------------

;; mis.el stole all my things and functions and beat me up and said I look
;; stupid. T_T


(require 'rx)



;; §-TODO-§ [2020-09-15]: Move this to a random.el or whatever.
;; I have other randoms - in .secrets.d maybe.
(defun spydez/random/bool ()
  "Get a random boolean (t nil).
"
  (= (random 2) 0))


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; String Functions
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Case Functions
;;------------------------------------------------------------------------------

(defun spydez/case/upper/char (arg)
  "Uppercasify ARG chars starting from point. Point doesn't move.
"
  (interactive "p")
  (upcase-char arg))


(defun spydez/case/lower/char (arg)
  "Lowercasify ARG chars starting from point. Point doesn't move.
"
  (interactive "p")
  (save-excursion
    (downcase-region (point) (progn (forward-char arg) (point)))))


(defun spydez/case/alternating/region (start end)
  "Use this function to display how very serious and unmockable you find
something.

Will convert the region selected between point and mark (START
and END) into alternating case. Randomly decides between starting
off with first letter as uppercase or lowercase.

aka Alternating Caps
aka Studly Caps
aka 'Mocking SpongeBob'
"
  (interactive "r")

  (if (not (use-region-p))
      (message "No active region to change.")

    ;; Do the conversion.
    (save-excursion
      (let ((to-upper (spydez/random/bool))
            (string (buffer-substring-no-properties start end)))
        (dotimes (i (length string))
          (goto-char (+ start i))
          ;; Case conversion toggle on only visible characters.
          (when (string-match (rx graphic) string i)
            (if to-upper
                (spydez/case/upper/char 1)
              (spydez/case/lower/char 1))

            ;; Toggle case for next letter.
            (setq to-upper (not to-upper))))))))


(defun spydez/case/alternating/word ()
  "Use this function to display how very serious and unmockable you find
something.

Will convert starting at point. Randomly decides between starting
off with first letter as uppercase or lowercase.

aka Alternating Caps
aka Studly Caps
aka 'Mocking SpongeBob'
"
  (interactive)

  ;; Do the conversion.
  (save-excursion
    (let ((pos (point))
          (to-upper (spydez/random/bool)))

      ;; Case conversion toggle on only visible characters.
      (while (string-match (rx graphic) (char-to-string (char-after)))
        ;; Actually convert.
        (if to-upper
            (spydez/case/upper/char 1)
          (spydez/case/lower/char 1))
        ;; Set-up for next char.
        (goto-char pos)
        (setq pos (1+ pos)
              ;; Toggle case for next letter.
              to-upper (not to-upper))))))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'strings-and-things)
