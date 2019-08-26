;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; BUG:
;;   - whitespace-mode's newline marker character deletes a real, accessable
;;     space if line ends in spaces(?), and replaces it with its newline marker
;;     (I think?).
;;   - ws-butler tries to: delete useless whitespace, save file, put you back
;;     where you where (if you were indented in 'useless' whitespace at the
;;     time).
;; Result:
;;   - You lose a space in your indent.

;; Set flag for easier testing:
;; --no-init-file

;; quicker startup for playing with this:
;;   M-x eval-buffer
;;   M-x ws-bug-helper

;;------------------------------------------------------------------------------
;; BUG:
;; Eval this move-to-column, point should be at column 66 after (ready
;; to insert next char in between 'v' and '^'). But with
;; whitespace-mode on, it will be at 65 instead.
;;
;; But first eval these:
;; (setq indent-tabs-mode nil)
;; (whitespace-mode 'toggle)
;;----------------------------------------------------------------v
;; (move-to-column 66 t)
;; (move-to-column 66 t)
;; (move-to-column 66 t)
;;----------------------------------------------------------------^
;; Note: line needs to be clean/clear after for bug to show up (i.e.
;; move-to-column needs to be forced to insert those forced spaces).
;;
;; This is where ws-butler comes in: if on an indented line, thinking about
;; code, hitting the save combo out of pure muscle memory, you will lose a space
;; of your indent when you save as:
;;   0) save requested
;;   1) ws-butler (temporarily) deletes your current indented line's whitespace
;;   2) ws-butler allows save
;;   3) ws-butler restores point to previous spot in line with `move-to-column'
;;   4) whitespace notices new chunk of spaces and deletes a 'useless' one to
;;      make room for its newline marker?
;;------------------------------------------------------------------------------

;; RESULTS of Forced move-to-column Bug:
;; '|' represents point position after move-to-column.
;;----------------------------------------------------------------v
;; Expected behavior (and actual result with whitespace-mode off):
;; (move-to-column 66 t)                                          |
;; Bug behavior with whitespace-mode on:
;; (move-to-column 66 t)                                         |
;;----------------------------------------------------------------^


;;-
;;--
;;---
;; QUESTION:
;; How to fix?
;;---
;;--
;;-

;; helpful helper
(defun ws-bug-helper ()
  (interactive)
  (setq indent-tabs-mode nil)
  (whitespace-mode 'toggle)
  (column-number-mode t)
  (line-number-mode t)
  (goto-char 1038))


;;---
;; info
;;---
;; (emacs-version t) GNU Emacs 26.1 (build 1, x86_64-w64-mingw32) of 2018-05-30
;; whitespace-mode.el version: 13.2.2
;; ws-butler version: 20170111.2334

;;---
;; my condensed use-packages:
;;---
;; (use-package ws-butler
;;   :demand t
;;   :config
;;   (defun move-to-column--info (column &optional force)
;;     (message "move-to-column: %s %s (cur:%s) // wsb-coord:%s, ws-pt:%s (curpt:%s)\nwcp:%s"
;;              column force (current-column)
;;              ws-butler-presave-coord
;;              whitespace-point (point)
;;              (what-cursor-position)))
;;   (advice-add 'move-to-column
;;               :after #'move-to-column--info)
;;
;;   (ws-butler-global-mode 1))
;;
;;
;; (use-package whitespace
;;   :ensure nil
;;   :demand t
;;   (global-whitespace-mode 1))


;;------------------------------------------------------------------------------
;; Review of whitespace.el
;;------------------------------------------------------------------------------
;; ;; only called by `whitespace-turn-on'
;; (defun whitespace-display-char-on ()
;;   "Turn on character display mapping."
;;   (when (and whitespace-display-mappings
;;              (whitespace-style-mark-p))
;;     (let (vecs vec)
;;       ;; Remember whether a buffer has a local display table.
;;       (unless whitespace-display-table-was-local
;;         (setq whitespace-display-table-was-local t)
;;         (unless (or whitespace-mode global-whitespace-mode)
;;           (setq whitespace-display-table
;;                 (copy-sequence buffer-display-table)))
;;         ;; Assure `buffer-display-table' is unique
;;         ;; when two or more windows are visible.
;;         (setq buffer-display-table
;;               (copy-sequence buffer-display-table)))
;;       (unless buffer-display-table
;;         (setq buffer-display-table (make-display-table)))
;;       (dolist (entry whitespace-display-mappings)
;;         ;; check if it is to display this mark
;;         (when (memq (car entry) whitespace-style)
;;           ;; Get a displayable mapping.
;;           (setq vecs (cddr entry))
;;           (while (and vecs
;;                       (not (whitespace-display-vector-p (car vecs))))
;;             (setq vecs (cdr vecs)))
;;           ;; Display a valid mapping.
;;           (when vecs
;;             (setq vec (copy-sequence (car vecs)))
;;             ;;;; at this point, `vec' is one vec with a char or 2 chars for
;;             ;;;; whatever is being replaced
;;             ;; NEWLINE char
;;             (when (and (eq (cadr entry) ?\n) ;;;; looking at a newline replacement and...
;;                        (memq 'newline whitespace-active-style)) ;;;; want to replace it?
;;               ;; Only insert face bits on NEWLINE char mapping to avoid
;;               ;; obstruction of other faces like TABs and (HARD) SPACEs
;;               ;; faces, font-lock faces, etc.
;;               ;;;; ...do this twice... (only newlines still) because vec for newline is 2 chars
;;               (dotimes (i (length vec))
;;                 (or (eq (aref vec i) ?\n) ;;;; don't do a thing if looking at \n char in `vec'
;;                     ;;;; if we are looking at the non-`\n` char, set it to a new... "glyph-code".
;;                     (aset vec i
;;                           ;;;; turns the normal '$' char into a glyph code of '$' in `whitespace-newline face.
;;                           (make-glyph-code (aref vec i)
;;                                            whitespace-newline)))))
;;             ;; Display mapping
;;             ;;;; now `vec' is [glyph ?\n] for newline
;;             ;;;; this sets index ?\n (which is int 10) to the [glyph ?\n]
;;             ;;;; so... this does nothing to actually replace shit, and the bug
;;             ;;;; is in emacs elsewhere?
;;             ;;;;   buffer-display-table application to the buffer code somewhere
;;             (aset buffer-display-table (cadr entry) vec)))))))
