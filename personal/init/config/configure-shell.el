;; -*- emacs-lisp -*-


;; NOTE: shell and eshell are different things... apparently.


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
;; (defun spydez/shell ()
;;   (interactive)
;;   (message "Undefined for OS: %s" system-type))

;; TODO: This seems useful maybe
;; (use-package exec-path-from-shell
;;   :config
;;   ;; Add GOPATH to shell
;;   (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-copy-env "GOPATH")
;;     (exec-path-from-shell-copy-env "PYTHONPATH")
;;     (exec-path-from-shell-initialize)))


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

;; TODO: figure out how to integrate this into use-tool
;; Set shell command to use git bash.
(when (use-tool-os-and-tool-p 'windows-nt "bash")
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
