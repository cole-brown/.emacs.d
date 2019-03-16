;; -*- emacs-lisp -*-


;; a local var we can refer to while not being complete yet...
(let ((spydez/bootstrap/expected-complete 'specific))

  ;;------------------------------------------------------------------------------
  ;; General Settings
  ;;------------------------------------------------------------------------------
  ;; Vars to override, other general stuff to provide so bootstrap can finish
  ;; successfully for this computer.



  ;;------------------------------------------------------------------------------
  ;; System Setup
  ;;------------------------------------------------------------------------------

  ;;---
  ;; Directories
  ;;---

  ;; TODO: get rid of this step if it's just empty for all computers?


  ;;------------------------------------------------------------------------------
  ;; TODOs
  ;;------------------------------------------------------------------------------


  ;;------------------------------------------------------------------------------
  ;; The End.
  ;;------------------------------------------------------------------------------
  (setq spydez/bootstrap/complete spydez/bootstrap/expected-complete)

  ) ;; (let ...)
(provide 'bootstrap-this-early)
