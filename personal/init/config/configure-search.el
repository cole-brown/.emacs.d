;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Blank
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

;;   "Native emacs grep functions (like M-x rgrep) as well as projectile-grep
;; and helm-projectile-grep all ignore directories specified by the variable
;; grep-find-ignored-directories. Let's add some to the defaults."
;;   - http://nhoffman.github.io/.emacs.d/#org9119f86
;; Why would he not want to grep source directory?..
;; (when (boundp 'grep-find-ignored-directories)
;;   (add-to-list 'grep-find-ignored-directories ".eggs")
;;   (add-to-list 'grep-find-ignored-directories "src"))

;;   "Make a function to ignore contents of a project's virtualenv, and advise
;; functions using grep to apply it before execution."
;;   - http://nhoffman.github.io/.emacs.d/#org9119f86
;; I'm not using python or virtual env so avoiding for now...
;; (defun grep-ignore-venv-current-project (&rest args)
;;   (interactive)
;;   (let ((venv (find-venv-current-project)))
;;     (if venv
;;         (progn
;;           (setq venv (file-name-nondirectory
;;                       (replace-regexp-in-string "/$" "" venv)))
;;           (message "adding '%s' to grep-find-ignored-directories" venv)
;;           (add-to-list 'grep-find-ignored-directories venv))
;;       (message "no virtualenv at this location")
;;       )))
;;
;; (advice-add 'rgrep :before #'grep-ignore-venv-current-project)
;; (advice-add 'projectile-grep :before #'grep-ignore-venv-current-project)
;; (advice-add 'helm-projectile-grep :before #'grep-ignore-venv-current-project)



;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: try out ag / silver searcher
;;   - if no, ack?

;; TODO: get grep working?
;;   M-x rgrep
;;   projectile-grep
;;   helm-projectile-grep
;;   ...


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-search)

