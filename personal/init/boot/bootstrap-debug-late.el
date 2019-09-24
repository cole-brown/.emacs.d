;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; benchmark-init from https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; git repo: https://github.com/dholm/benchmark-init-el
;; disable when not debugging?

;; benchmark-init records time taken in load and require, so it should notice
;; ours as well as other packages.
(use-package benchmark-init
  ;; only use when "debugging" init
  :when (spydez/debugging-p)

  ;; Think I either have to have the hook in ":config", or have ":demand t" with
  ;; :hook" - otherwise it gets turned into an autoload which would probably not
  ;; ever load...
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-debug-late)
