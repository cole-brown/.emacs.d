;; -*- emacs-lisp -*-

;; init-debug.el - some debug-related stuff for init code

;; benchmark-init from https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; git repo: https://github.com/dholm/benchmark-init-el
;; disable when not debugging?
(use-package benchmark-init
;  :disabled (not (spydez/debugging-p))
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'init-debug)
