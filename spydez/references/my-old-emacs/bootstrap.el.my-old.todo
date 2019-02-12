;; these vars must be defined:
;;    kooru/emacs-personal

;; bootstrap.el, modified from:
;; http://www.io.com/~jimm/emacs_tips.html#my-dot-emacs

(defun bootstrap-file (domain machine file-name)
  (concat kooru/emacs-personal domain "/" machine "/" file-name))

(defun load-init-if-exists (domain machine file)
  (let ((f (bootstrap-file domain machine (concat file ".el"))))
    (if (file-exists-p f)
        (load-file f))))

(defun bootstrap-init (domain machine)
  (load-init-if-exists domain machine "before")
  (load-file (concat kooru/emacs-personal "emacs.el"))
  (load-init-if-exists domain machine "after")
  (setq bookmark-default-file
	(bootstrap-file domain machine "emacs.bmk")))
