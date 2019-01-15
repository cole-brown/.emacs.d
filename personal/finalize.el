;; -*- emacs-lisp -*-

;; finalize.el - non-computer-specific emacs init - the final bits



;; This really should be the end for properness. Prints timer message to minibuf & *Messages* buffer.
;; https://github.com/zzamboni/dot-emacs/blob/master/init.org
(defun spydez/startup-hook ()
  (message "Emacs ready in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time
        (time-subtract after-init-time before-init-time)))
    gcs-done))
(add-hook 'emacs-startup-hook 'spydez/startup-hook)
