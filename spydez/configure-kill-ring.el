;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; Some keybinds related to killing, yanking, kill-ring, undo, etc...
(bind-keys*
  ;; Switch C-z to undo, instead of hide emacs window.
  ([(control z)] . undo) ;; unused keybind - I just don't like C-z hiding emacs window

  ;; I don't like C-backspace dumping to the kill ring. I kill line or something instead.
  ([(control backspace)] 'spydez/backward-delete-word)

  ;; Move backward-kill-word to C-w. Kill-region to C-x C-k...
  ;; from my old emacs config, muscle memory and ultimately:
  ;;   http://steve.yegge.googlepages.com/effective-emacs
  ("C-w" . backward-kill-word)
  ("C-x C-k" . kill-region)
  ;; (global-set-key "C-c C-k" 'kill-region) ;; don't really use the C-c varient.
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

  :diminish undo-tree-mode " ↺" ;; fancy UTF-8
  ;; :diminish undo-tree-mode ;; just squelched entirely

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
