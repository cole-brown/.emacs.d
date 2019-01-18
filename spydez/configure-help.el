;; -*- emacs-lisp -*-

;; (Help) Not just anybody.

;;------------------------------------------------------------------------------
;; Guide-Key
;;------------------------------------------------------------------------------

;; TODO: Doesn't work til turned off/back on. Need debugged?
;; It's hard to remember keyboard shortcuts. The guide-key package pops up help after a short delay. 
;;
;; Currently only set for:
;;   1) Rectangles    "C-x r"
;;   2) Window frames "C-x 4"
;;   3) Mode specific "C-c"
(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))  ; Enable guide-key-mode
;; Trial: [2019-01-17]

;; (Help) You know I need someone.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-help)
