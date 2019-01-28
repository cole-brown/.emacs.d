;; -*- emacs-lisp -*-

;;------------------------------------------------------------------------------
;; Undo-Tree
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

  :diminish undo-tree-mode " ↺" ;; fancy UTF-8
  ;:diminish undo-tree-mode ;; just squelched entirely

  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))
;; Trial: [2019-01-17]
;;   - Not sure if I'll use this... I'm fine with the kill ring functionality usually.
;;   - [2019-01-28] Disabled for massive violations of expected undo functionality.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-kill-ring)
