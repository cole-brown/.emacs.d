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
