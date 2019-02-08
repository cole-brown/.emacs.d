;; -*- emacs-lisp -*-

;; Show *Messages* buffer.
;;   https://stackoverflow.com/questions/40075912/want-to-view-emacs-messages-buffer-while-emacs-loads


(defconst spydez/init-debug t) ;; nil)
(defun spydez/debugging-p ()
  (bound-and-true-p spydez/init-debug))

;; How about putting this at the beginning of your init.el?
;; (view-echo-area-messages)

;; Or if you instead want the messages buffer to take up the whole screen,
(when (spydez/debugging-p)
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
;;
;; Or these:
;; Enable debugging
;; (setq debug-on-error t)
;; (setq debug-on-signal t)


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'bootstrap-debug)
