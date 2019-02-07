;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; General
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
;; e.g. (when (spydez/tools/os-and-tool-p 'windows-nt "bash") (message "hello there"))


;;------------------------------------------------------------------------------
;; Windows?
;;------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)

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
  )


;;------------------------------------------------------------------------------
;; Linux?
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; OS X?
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-os)
