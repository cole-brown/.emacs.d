;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; TODO: anything from here?
;; https://www.masteringemacs.org/article/effective-editing-movement

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; check to see if they've got the correct pieces

;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Point-and-mark.html

;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------
;; What is "the 'beginning' of the 'line'" anyways?

;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; Trial [2019-01-29]
;; Trial 2 ([2019-05-17]): Do beginning of line first, not second?
;;   This still could be annoying in, for example, macros... We'll see.
(defun spydez/smarter-move-beginning-of-line (arg)
  "Move point to beginning of line.

Move point to the beginning of the line. If point is already
there, move to the first non-whitespace character on this line.
Effectively toggle between the beginning of the line and the
first non-whitespace character.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

;; remap C-a to `smarter-move-beginning-of-line'
(bind-key [remap move-beginning-of-line]
          'spydez/smarter-move-beginning-of-line)

;; Wasn't liking C-a having unexpected effects. Was originally:
;;   1) Go to first non-whitespace.
;;   2) Else go to beginning of line.
;; I've reversed those so one C-a works how my fingers expect but spamming
;; is useful to remind me of the new functionality.


;;------------------------------------------------------------------------------
;; Pager
;;------------------------------------------------------------------------------
;; Better paging up/down in emacs buffer. E.g. Page dn + Page up = exactly
;; where your point used to be (down to col,row).
;;
;; Orphaned: but I like it, so hopefully the orphanage keeps it around.
;; https://github.com/emacsorphanage/pager
(use-package pager
  :bind
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
