;; -*- emacs-lisp -*-

;;------------------------------------------------------------------------------
;; Undo-Tree
;;------------------------------------------------------------------------------
;; A visual tree for the Kill/Yank Ring (aka Undo/Redo)

;; This lets you use C-x u (undo-tree-visualize) to visually walk through the
;; changes you've made, undo back to a certain point (or redo), and go down
;; different branches. (C-n/p/f/b mostly?)
(use-package undo-tree
  :diminish undo-tree-mode "↺" ;; fancy UTF-8
  ;:diminish undo-tree-mode ;; just squelched entirely
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))
;; [2019-01-17] Trial package...
;; Not sure if I'll use this... I'm fine with the kill ring functionality usually.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-kill-ring)
