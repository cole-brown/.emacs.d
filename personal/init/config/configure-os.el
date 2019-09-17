;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General
;;------------------------------------------------------------------------------

;; os-and-tool-p exists in bootstrap-os.el now
;; e.g. (when (spydez/tools/os-and-tool-p 'windows-nt "bash") (spydez/debug/message-if nil "hello there"))


;;------------------------------------------------------------------------------
;; Windows?
;;------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)

  ;;---
  ;; Font
  ;;---
  ;; Default font (Courier New) has height issues between regular/bold and
  ;; italic, which makes things shift around by a pixel annoyingly. Trying
  ;; Consolas instead.
  ;; Note: "Courier New" height was 98
  (set-face-attribute 'default nil
                      :family "Consolas" :height 100)

  ;;---
  ;; PATH
  ;;---
  ;; Do I need to add anything specifically for windows here?
  ;; (when (eq system-type 'windows-nt)
  ;;   (setenv "PATH" (concat "\"c:/program files/postgresql/9.3/bin;\"" (getenv "PATH"))))

  ;; Add each external tool to env PATH?
  ;; And add msys or cygwin too?
  ;; ;; Add gnuwin utils to env PATH as well?
  ;; ;; This is so diff and find can work.
  ;; (setenv "PATH" (concat "C:/util/gnuwin/bin;"
  ;;                        "C:/msys/1.0/bin;"
  ;;                        (getenv "PATH")))



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
