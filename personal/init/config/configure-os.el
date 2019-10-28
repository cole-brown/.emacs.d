;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General
;;------------------------------------------------------------------------------

;; os-and-tool-p exists in bootstrap-os.el now
;; e.g. (when (spydez/tools/os-and-tool-p 'windows-nt "bash") (mis/debug/when nil "hello there"))


;;------------------------------------------------------------------------------
;; Windows?
;;------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)

  ;;---
  ;; Font: NOTES
  ;;---

  ;; This is just... IDK, what the frame has available maybe?
  ;; (fontset-list)
  ;;
  ;; This is all fonts installed as far as emacs knows?
  ;; (font-family-list)
  ;;
  ;; This is for setting up a fontset so that fallbacks automatically happen?
  ;; Maybe? https://emacs.stackexchange.com/a/47052
  ;; variable: face-font-family-alternatives

  ;;---
  ;; Font
  ;;---

  ;; Old Consolas-only way:
  ;; ;; Default font (Courier New) has height issues between regular/bold and
  ;; ;; italic, which makes things shift around by a pixel annoyingly. Trying
  ;; ;; Consolas instead.
  ;; ;; Note: "Courier New" height was 98
  ;; (set-face-attribute 'default nil
  ;;                     :family "Consolas" :height 100)

  ;; Hand-Made Fallbacks
  (cond
   ;; Microsoft's Cascadia Code font:
   ;;   https://github.com/microsoft/cascadia-code
   ;;
   ;; Well... Bold cascadia just results in blurred together chunks of word on
   ;; my 1080p monitor... This may be a "go big, no bold, or go 4k" font.
   ;; ((member "Cascadia Code" (font-family-list))
   ;;  (set-face-attribute 'default nil
   ;;                      :family "Cascadia Code" :height 90)
   ;;  (mis/debug/when nil "Using Cascadia Code font."))

   ((member "Consolas" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Consolas" :height 100)
    (mis/debug/when nil "Using Consolas font."))

    (t
     (mis/debug/when nil
                     "No desired font found. Using whetever Emacs feels like.")))


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
