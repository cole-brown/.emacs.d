;; -*- emacs-lisp -*-


;; NOTE: shell and eshell are different things... apparently.


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
;; (defun spydez/shell ()
;;   (interactive)
;;   (message "Undefined for OS: %s" system-type))


;;------------------------------------------------------------------------------
;; Git Bash for Windows?
;;------------------------------------------------------------------------------
;; (when (spydez/tools/os-and-tool-p 'windows-nt "bash")
;;   (defun spydez/windows/shell ()
;;     (interactive)
;;     (let* ((explicit-shell-file-name (executable-find "bash")))
;;       (shell)))
;;   (defalias 'spydez/shell 'spydez/windows/shell)
;;   )

;; TODO: move to configure OS?
;; Set shell command to use git bash.
(when (spydez/tools/os-and-tool-p 'windows-nt "bash")
  (let ((bash-path (executable-find "bash")))
    (when bash-path
      (setq explicit-shell-file-name bash-path)
      (setq shell-file-name "bash")
      (setq explicit-bash.exe-args '("--login" "-i"))
      (setenv "SHELL" shell-file-name)
      ;; todo strip ctrl m?
      ;; todo fix error and MSYS line cruft
      ;;   - https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
      )))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-shell)
