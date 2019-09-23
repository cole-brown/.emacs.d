;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; The Final Chance for Others to Do a Thing
;;------------------------------------------------------------------------------

;; Run it!
(run-hooks 'spydez/hook/finalize/run-the-final-hooks)


;;------------------------------------------------------------------------------
;; Debugging: clean up and finalize
;;------------------------------------------------------------------------------

;; show benchmark if debugging
;; https://github.com/dholm/benchmark-init-el
(when (spydez/debugging-p)
  (benchmark-init/show-durations-tree))
;;(benchmark-init/show-durations-tabulated))

;; This really should be the end for properness.
;; Prints timer message to minibuf & *Messages* buffer.
;; https://github.com/zzamboni/dot-emacs/blob/master/init.org
(defun spydez/startup-hook ()
  (message "Emacs ready in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time
        (time-subtract after-init-time before-init-time)))
    gcs-done))
(add-hook 'emacs-startup-hook 'spydez/startup-hook)


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zzz-finalize)
