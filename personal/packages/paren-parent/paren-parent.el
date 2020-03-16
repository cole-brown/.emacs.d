;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;--------------------------------------(---------------------------------------
;;--                     Parenthesis Parent Highlighting                      --
;;--------------------------------------)---------------------------------------

;; From:
;; https://www.reddit.com/r/emacs/comments/fh1bpg/a_variant_of_elisp_matching_paren_display/
;;   Overlay:    https://gist.github.com/codecoll/335f1c4bf45482bd70832589e96362c6
;;   Minibuffer: https://gist.github.com/codecoll/c861390a518556211388f87914db5023


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

;; overlay for on-screen
(setq paren-parent/show-paren/for-line/overlay/normal
      (make-overlay (point) (point)))
(overlay-put paren-parent/show-paren/for-line/overlay/normal
             'face '(background-color . "cyan"))
;; §-TODO-§ [2020-03-16]: a more Zenburn-y color.
;; hide it
(delete-overlay paren-parent/show-paren/for-line/overlay/normal)


;; Overlay for off-screen.
(setq paren-parent/show-paren/for-line/overlay/out-of-sight
      (make-overlay (point) (point)))
;; Hide it.
(delete-overlay paren-parent/show-paren/for-line/overlay/out-of-sight)


;;------------------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Code
;;------------------------------------------------------------------------------

(defun paren-parent/show-paren/for-line/start ()
  (interactive)
  (add-hook 'post-command-hook
            'paren-parent/show-paren/for-line/setup-overlay
            t t))


(defun paren-parent/show-paren/for-line/stop ()
  (interactive)
  (remove-hook 'post-command-hook
               'paren-parent/show-paren/for-line/setup-overlay
               t))


(defun paren-parent/show-paren/for-line/setup-overlay ()
  (if (and (looking-at ".*))\\s-*$")
           (not (eq (char-before) ?\))))
      (let (out-of-sight)
        (save-excursion
          (ignore-errors
            (end-of-line)
            (backward-sexp)
            (if (< (point) (window-start))
                (progn
                  (beginning-of-line)
                  (skip-syntax-forward " ")
                  (setq out-of-sight (buffer-substring (point)
                                                       (line-end-position)))
                  (let ((max 20))
                    (if (> (length out-of-sight) 20)
                        (setq out-of-sight (concat (substring out-of-sight 0 20)
                                                   "...")))))

              (move-overlay paren-parent/show-paren/for-line/overlay/normal
                            (point)
                            (1+ (point))))))

        (when out-of-sight
          (move-overlay paren-parent/show-paren/for-line/overlay/out-of-sight
                        (1- (line-end-position))
                        (line-end-position))
          (overlay-put
           paren-parent/show-paren/for-line/overlay/out-of-sight
           'after-string
           (propertize (concat " ;; " out-of-sight " ")
                       'face 'font-lock-comment-face)))

        (add-hook 'pre-command-hook
                  'paren-parent/show-paren/for-line/hide-overlay
                  t t))))


(defun paren-parent/show-paren/for-line/hide-overlay ()
  (delete-overlay paren-parent/show-paren/for-line/overlay/normal)
  (delete-overlay paren-parent/show-paren/for-line/overlay/out-of-sight)
  (remove-hook 'pre-command-hook
               'paren-parent/show-paren/for-line/hide-overlay
               t))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-03-16]: Turn into a minor mode?
;; https://nullprogram.com/blog/2013/02/06/

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'paren-parent)
