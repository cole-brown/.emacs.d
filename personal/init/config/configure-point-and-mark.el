;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; TODO: anything from here?
;; https://www.masteringemacs.org/article/effective-editing-movement

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces

;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Point-and-mark.html


;;------------------------------------------------------------------------------
;; Point Utils - Checking Out Things About Point
;;------------------------------------------------------------------------------

;; http://ergoemacs.org/emacs/elisp_determine_cursor_inside_string_or_comment.html
(defun spydez/point/inside-string-p ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (let ((result (nth 3 (syntax-ppss))))
    (message "%s" result)
    result))

;; http://ergoemacs.org/emacs/elisp_determine_cursor_inside_string_or_comment.html
(defun spydez/point/inside-comment-p ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (let ((result (nth 4 (syntax-ppss))))
    (message "%s" result)
    result))

;; https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace
(defun spydez/point/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------
;; What is "the 'beginning' of the 'line'" anyways?

;;---
;; Logical Lines
;;---

;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; Trial 1 [2019-01-29]: Use code from link.
;; Trial 2 [2019-05-17]: Do beginning of line first, not second.
;; TRIAL END [2020-02-03]: Trial successful; keep this.
(defun spydez/smarter-move-beginning-of-line (arg)
  "Move point to beginning of line, or indentation.

Move point to the beginning of the line. If point is already
there, move to the first non-whitespace character on this line.
Effectively toggle between the beginning of the line and the
first non-whitespace character.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.
"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  ;; Wasn't liking C-a having unexpected effects. Was originally:
  ;;   1) Go to first non-whitespace.
  ;;   2) Else go to beginning of line.
  ;; I've reversed those so one C-a works how my fingers expect but spamming
  ;; is useful to remind me of the new functionality.
  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    ;; If that did nothing, jump to indentation.
    (when (= orig-point (point))
      (back-to-indentation))))


;; remap C-a to `smarter-move-beginning-of-line'
(bind-key [remap move-beginning-of-line]
          'spydez/smarter-move-beginning-of-line)


;;---
;; Visual Lines
;;---

(defun spydez/smarter-beginning-of-visual-line (arg)
  "Move point to beginning of visual line, or actual line, or indentation.

Move point to the beginning of the (visual) line. If point is
already there, move point to the beginning of the (actual/logical) line.
If point is already there, move to the first non-whitespace
character on this line. Effectively toggle between the beginning
of the visual line, logical line, and the first non-whitespace
character.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.
"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  ;; Move in the line now.
  (let ((orig-point (point)))
    (beginning-of-visual-line 1)
    ;; If that did nothing, jump into `spydez/smarter-move-beginning-of-line'
    ;; for more beginnings.
    (when (= orig-point (point))
      (spydez/smarter-move-beginning-of-line 1))))


(defun spydez/smarter-end-of-visual-line (arg)
  "Move point to end of visual line, or actual line.

Move point to the end of the (visual) line. If point is already
there, move point to the end of the (actual/logical) line.
Effectively toggle between the end of the visual line and
logical line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.
"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  ;; Move in the line now.
  (let ((orig-point (point)))
    (end-of-visual-line 1)
    ;; If that did nothing, jump to end of actual/logical line.
    (when (= orig-point (point))
      (move-end-of-line 1))))


;; remap C-a to `smarter-beginning-of-visual-line' in visual-line-mode-map
(bind-keys :map visual-line-mode-map
           ;; beginning of line
           ([remap beginning-of-visual-line]
            . spydez/smarter-beginning-of-visual-line)
           ([remap move-beginning-of-line]
            . spydez/smarter-beginning-of-visual-line)
           ;; end of line
           ([remap end-of-visual-line]
            . spydez/smarter-end-of-visual-line)
           ([remap move-end-of-line]
            . spydez/smarter-end-of-visual-line))


;;------------------------------------------------------------------------------
;; Pager
;;------------------------------------------------------------------------------
;; Better paging up/down in emacs buffer. E.g. Page dn + Page up = exactly
;; where your point used to be (down to col,row).
;;
;; Orphaned: but I like it, so hopefully the orphanage keeps it around.
;; https://github.com/emacsorphanage/pager
(use-package pager
  ;;---
  :bind ;; global
  ;;---
  (("C-v" . pager-page-down)
   ([next] . pager-page-down)
   ("M-v" . pager-page-up)
   ([prior] . pager-page-up)
   ([M-up] . pager-row-up)
   ([M-kp-8] . pager-row-up)
   ([M-down] . pager-row-down)
   ([M-kp-2] . pager-row-down)))


;;------------------------------------------------------------------------------
;; Multiple Cursors
;;------------------------------------------------------------------------------
;; TODO: test these out?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org1d6dd7c
;; https://github.com/magnars/multiple-cursors.el


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-point-and-mark)
