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
;; Setup Tools
;;------------------------------------------------------------------------------

;;---
;; Use-Tool
;;---
(when (eq system-type 'windows-nt)
  (require 'use-tool)

  ;; source: Git for Windows
  (progn
    (use-tool-source git-for-windows)

    ;; tools:
    (use-tool git)
    (use-tool gpg)
    (use-tool bash)
    (use-tool diff))

  ;; source & tool: hunspell (for Windows)
  (progn
    (use-tool-source hunspell-for-windows)
    (use-tool hunspell))
  )

;; NOTE: BE MINIMAL AND LAZY
;; NOTE: GET ANYTHING WORKING


;;---------
;; Windows
;;---------

;; (when (eq system-type 'windows-nt)
;;   ;;---
;;   ;; Cygwin? (TODO: I would rather Msys probably..?)
;;   ;;---
;;   ;; (setenv "CYGWIN" "nodosfilewarning")
;;   ;; (setq shell-file-name "C:/emacs/libexec/emacs/24.4/i686-pc-mingw32/cmdproxy.exe")
;;   ;; (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;;   ;; (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
;; 
;; 
;;   ;;---
;;   ;; GnuTLS on Windows?
;;   ;;---
;;   ;; http://pages.sachachua.com/.emacs.d/Sacha.html#org7149e50
;;   ;; http://xn--9dbdkw.se/diary/how_to_enable_GnuTLS_for_Emacs_24_on_Windows/index.en.html has lots of tips.
;;   ;; (setq gnutls-trustfiles '("c:/sacha/cacert.pem.txt"))
;; 
;;   ;;---
;;   ;; TRAMP and SSH
;;   ;;---
;;   ;; TODO: get this working if I need TRAMP?
;;   ;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgd9057e7
;;   
;;   ) ;; (when (eq system-type 'windows-nt) ...)


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
