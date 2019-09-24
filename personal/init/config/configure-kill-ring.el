;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Delete selected text on insert instead of just bumping it over.
;; aka: turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode 1)

;; taken from http://emacs-bootstrap.com/
;;
;;   "When an Emacs kill command puts text in the clipboard, the existing
;; clipboard contents are normally lost. Optionally, you can change
;; save-interprogram-paste-before-kill to t. Then Emacs will first save the
;; clipboard to its kill ring, preventing you from losing the old clipboard
;; data—at the risk of high memory consumption if that data turns out to be
;; large."
;;   - https://www.gnu.org/software/emacs/manual/html_node/emacs/Clipboard.html
;; Trial [2019-02-26]
(setq save-interprogram-paste-before-kill t)


;;------------------------------------------------------------------------------
;; Mouse Settings
;;------------------------------------------------------------------------------

;; Mouse button functionality:
;;   Mouse 1: click: Move point to mouse click location.
;;   Mouse 1: drag:  Select region.
;;   Mouse 2: click: Yank from kill ring.
;;   Mouse 3: click: Change/select region based on point and click location.

;; taken from http://emacs-bootstrap.com/
;;
;;   "If you change the variable mouse-yank-at-point to a non-nil value, mouse-2
;; does not move point; it inserts the text at point, regardless of where you
;; clicked or even which of the frame's windows you clicked on. This variable
;; affects both mouse-yank-primary and mouse-yank-at-click."
;;   - https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Commands.html
;; Trial [2019-02-26]
;;   -- I don't use mouse 2, 3 much but it looks potentially useful.
(setq mouse-yank-at-point t)


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; Some keybinds related to killing, yanking, kill-ring, undo, etc...
(bind-keys*
  ;; Switch C-z to undo, instead of hide emacs window.
  ([(control z)] . undo) ;; unused keybind - I just don't like C-z hiding emacs window

  ;; I don't like C-backspace dumping to the kill ring. I kill line or something instead.
  ([(control backspace)] . spydez/backward-delete-word)

  ;; Move backward-kill-word to C-w. Kill-region to C-x C-k...
  ;; from my old emacs config, muscle memory and ultimately:
  ;;   http://steve.yegge.googlepages.com/effective-emacs
  ("C-w" . backward-kill-word)
  ("C-x C-k" . kill-region)
  ("C-c C-k" . kill-region) ;; Turns out I do use C-c version. Probably more than C-x...
  )

;;------------------------------------------------------------------------------
;; Aliases
;;------------------------------------------------------------------------------

;; from: http://steve.yegge.googlepages.com/effective-emacs
(defalias 'qrr 'query-replace-regexp)
;; Don't really use this... I tend to go for just M-% (query-replace).
;; TODO: Start using, or comment out?


;;------------------------------------------------------------------------------
;; Disabled: Undo-Tree
;;------------------------------------------------------------------------------
;; A visual tree for the Kill/Yank Ring (aka Undo/Redo)

;; This lets you use C-x u (undo-tree-visualize) to visually walk through the
;; changes you've made, undo back to a certain point (or redo), and go down
;; different branches. (C-n/p/f/b mostly?)
(use-package undo-tree
  :disabled
  ;; OK, so undo-tree eats my "C-n/C-p/space/etc as undo/redo manipulation".
  ;; It turns C-_ into basically-just-stupid-undo (it stops being able to undo an undo)?
  ;; Which is not cool.
  ;; Disabling this atrocity for now.
  ;; TODO: Fix this if/when I want to try out undo-tree again. It is annoying and bad functionality.

  :delight undo-tree-mode " ↺" ;; fancy UTF-8
  ;; :delight undo-tree-mode ;; just squelched entirely

  ;;---
  :config
  ;;---
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))
;; Trial: [2019-01-17]
;;   - Not sure if I'll use this... I'm fine with the kill ring functionality usually.
;;   - [2019-01-28] Disabled for massive violations of expected undo functionality.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-kill-ring)
