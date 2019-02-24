;; -*- emacs-lisp -*-

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
(defun spydez/smarter-move-beginning-of-line (arg)
  ;; TODO: is this really the way for docstrings to be? All wonked over to
  ;; the left instead of neatly indented?
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'spydez/smarter-move-beginning-of-line)

;; TODO: not liking C-a having unexpected effects. May want this undone.
;;   Makes it hard to spam C-a.


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
