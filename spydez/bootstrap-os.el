;; -*- emacs-lisp -*-


;;--------------------------------So what OS?-----------------------------------
;;--                                Plan 9?                                   --
;;---------------------------------Mmh, yes.------------------------------------
;;                                     .
;;                                     .
;;                                     .
;;--------------------------Wait, no. A boring one.-----------------------------



;;------------------------------------------------------------------------------
;; General Settings?
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Setup Tool Exec-Path and ENV PATH
;;------------------------------------------------------------------------------

(defun spydez/tools/os-and-tool-p (sys-type ext-tool)
  "t if on system-type and tool exists on system-type, else nil"
  (if (and
       ;; on certain OS...
       (eq system-type sys-type)
       ;; ...and looking for an expected external tool
       (and (boundp 'spydez/tools/external)
            (executable-find ext-tool)))
      t
    nil))
;; e.g. (when (spydez/tools/os-and-tool-p 'windows-nt "bash") (spydez/debug/message nil "hello there"))


;;---------
;; Windows
;;---------
(when (eq system-type 'windows-nt)

  ;; set up paths so external tools can be found by emacs, magit, eshell, etc.
  (if (boundp 'spydez/tools/external)
      (dolist (tool-pair spydez/tools/external)
        (let* ((tool-name (car tool-pair))
               (tool-path (cdr tool-pair))
               (exec-path-known-p (if (executable-find tool-name) t nil))
               (env-path (getenv "PATH"))
               (env-path-known-p (if (string-match-p (regexp-quote tool-path) env-path) t nil)))
          ;; (spydez/debug/message nil "win tool: %s known? %s %s" tool-name exec-path-known-p env-path-known-p)

          ;;---
          ;; exec-path
          ;;---
          ;; add to exec-path unless it's already a known executable
          (unless exec-path-known-p
            (add-to-list 'exec-path tool-path t)) ;; append to end
          
          ;;---
          ;; PATH
          ;;---
          ;; TODO: Should there be some other step for adding a many tools dir? Like Cygwin or Msys's bin dir?
          ;; I think may I rely on env-path-known-p and simplicity.

          ;; Add each external tool to env PATH?
          ;; NOTE: This has a few holes. There are 'C:\foo\bar' and 'C;/baz/qux'.
          ;;   But I'll try the stupid approach first.

          ;; skip if known, or if empty/nil string for path
          (unless (or env-path-known-p
                      (or (string= tool-path "")
                          (string= tool-path nil)))
            ;; this one we didn't skip, append to end of PATH var, which is just
            ;; a string (paths separated by OS-appropriate separator)
            (setenv "PATH" (concat env-path
                                   ";"
                                   tool-path
                                   ))
            ;; (spydez/debug/message nil "now tool %s in path?: %s %s"
            ;;          tool-name
            ;;          (string-match-p (regexp-quote tool-path) (getenv "PATH"))
            ;;          tool-path)
            )))
    (spydez/warning/message nil :error "spydez/tools/external not defined. You are on windows and need it, probably."))


  ;;---
  ;; Cygwin? (TODO: I would rather Msys probably..?)
  ;;---
  ;; (setenv "CYGWIN" "nodosfilewarning")
  ;; (setq shell-file-name "C:/emacs/libexec/emacs/24.4/i686-pc-mingw32/cmdproxy.exe")
  ;; (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
  ;; (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)


  ;;---
  ;; GnuTLS on Windows?
  ;;---
  ;; http://pages.sachachua.com/.emacs.d/Sacha.html#org7149e50
  ;; http://xn--9dbdkw.se/diary/how_to_enable_GnuTLS_for_Emacs_24_on_Windows/index.en.html has lots of tips.
  ;; (setq gnutls-trustfiles '("c:/sacha/cacert.pem.txt"))

  ;;---
  ;; TRAMP and SSH
  ;;---
  ;; TODO: get this working if I need TRAMP?
  ;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgd9057e7
  
  ) ;; (when (eq system-type 'windows-nt) ...)


;;---------
;; Linux?
;;---------


;;---------
;; OS X?
;;---------


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-os)
