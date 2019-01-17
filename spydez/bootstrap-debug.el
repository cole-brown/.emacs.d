;; -*- emacs-lisp -*-

;; debug.el - some debug-related stuff for init code

;; Show *Messages* buffer.
;;   https://stackoverflow.com/questions/40075912/want-to-view-emacs-messages-buffer-while-emacs-loads

(defconst spydez/init-debug t) ;; nil)

;; How about putting this at the beginning of your init.el?
;; (view-echo-area-messages)

;; Or if you instead want the messages buffer to take up the whole screen,
(when spydez/init-debug
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (switch-to-buffer (current-buffer)))
)

;; However, putting messages in your init-file is a crude workaround. You probably
;; actually want to launch Emacs with the --debug-init option:
;;
;; $ emacs --debug-init

;; This will halt on the error and present you with a backtrace.
;;
;; Or alternatively, you could just M-x toggle-debug-on-error and reload your 
;; init-file (M-x load-file RET ~/.emacs.d/init.el RET).

;; benchmark-init from https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; git repo: https://github.com/dholm/benchmark-init-el
;; disable when not debugging
;(use-package benchmark-init
;  :disabled (not spydez/init-debug)
;  :config
;  ;; To disable collection of benchmark data after init is done.
;  (add-hook 'after-init-hook 'benchmark-init/deactivate))
