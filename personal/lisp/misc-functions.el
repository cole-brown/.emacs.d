;; -*- emacs-lisp -*-


;;-----------------------------------elisp--------------------------------------
;;--                       Misc Functions for Elisp.                          --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings?
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; dos2unix Type Functions
;;------------------------------------------------------------------------------

(defun spydez/to-unix-auto ()
  "Change the current buffer's line-ends to Unix, preserving the coding system."
  (interactive)
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (setq coding-str
            (concat (substring coding-str 0 (match-beginning 0)) "-unix"))
      (message "CODING: %s" coding-str)
      (set-buffer-file-coding-system (intern coding-str)) )))

(defun spydez/to-unix-utf8 ()
  "Change the current buffer to UTF-8 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))


;;------------------------------------------------------------------------------
;; Misc Utility Functions
;;------------------------------------------------------------------------------

;; delete? `loop' doesn't exist (now?) :: (spydez/range 0 10)
;; (defun spydez/range (start count &optional step-size)
;;   "`loop' isn't a real thing so this doesn't work right now and there's
;; number-sequence anyways."
;;   (let ((step (if (integerp step-size) step-size 1)))
;;     (loop repeat count for i from start by step collect i)))

;; a `range' type function more like I expect...
(defun spydez/py-range (start &optional end step)
  "Acts like python's `range' function instead of `number-sequence'"
  (unless end
    (setq end start
          start 0))
  (number-sequence start (1- end) step))

;; I think this is better right where it's used as it's just the once.
;; (defun spydez/auto-open-files () 
;;   (if (and window-system (boundp 'spydez/auto-open-list))
;;       (dolist (file spydez/auto-open-list)
;;         (find-file file))))


;;------------------------------------------------------------------------------
;; Whitespaces And Deletion Functions
;;------------------------------------------------------------------------------

;; TODO: these seem... like things emacs already does, I think? Maybe they avoid
;; kill ring or something.

(defun spydez/delete-word (arg)
  "Kill characters forward until encountering the end of a word. With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun spydez/backward-delete-word (arg)
  "Kill characters backward until encountering the beginning of a word. With argument, do this that many times."
  (interactive "p")
  (spydez/delete-word (- arg)))

;; From: http://www.emacswiki.org/cgi-bin/wiki/DeletingWhitespace
(defun spydez/whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
   delete across newlines as well.  The only danger in this is that you
   don't have to actually be at the end of a word to make it work.  It
   skips over to the next whitespace and then whacks it all to the next
   word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: Trim/remove any unused functions?

;; Also these are ~10 years old and back when I knew almost enough about
;; emacs/elisp to set it up correctly. There may be some that don't need
;; to exist (anymore).


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'misc-functions)
