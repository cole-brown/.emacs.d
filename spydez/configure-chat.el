;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgcef4da0
;; (erc :server "irc.freenode.net" :port 6667 :nick "sachac")


;;------------------------------------------------------------------------------
;; ERC for IRC
;;------------------------------------------------------------------------------

;; TODO: set up erc??

;; http://pages.sachachua.com/.emacs.d/Sacha.html#org2f38edf
;; IRC is a great way to hang out with other Emacs geeks.
;; Trial [2019-02-06]
;; (use-package erc
;;   :config
;;   (setq erc-hide-list '("PART" "QUIT" "JOIN"))
;;   (setq erc-autojoin-channels-alist '(("freenode.net"
;;                                        "#org-mode"
;;                                        "#emacs"
;;                                        "#emacs-beginners"
;;                                        "#emacs-ops"))
;;         erc-server "irc.freenode.net"
;;         erc-nick "sachac") ;; move nick out?
;;   (defun erc-cmd-OPME ()
;;     "Request chanserv to op me."
;;     (erc-message "PRIVMSG"
;;                  (format "chanserv op %s %s"
;;                          (erc-default-target)
;;                          (erc-current-nick)) nil))
;; 
;;   (defun erc-cmd-DEOPME ()
;;     "Deop myself from current channel."
;;     (erc-cmd-DEOP (format "%s" (erc-current-nick))))
;;   (defun erc-cmd-BAN (nick)
;;     (let* ((chan (erc-default-target))
;;            (who (erc-get-server-user nick))
;;            (host (erc-server-user-host who))
;;            (user (erc-server-user-login who)))
;;       (erc-server-send (format "MODE %s +b *!%s@%s" chan user host))))
;; 
;;   (defun erc-cmd-KICKBAN (nick &rest reason)
;;     (setq reason (mapconcat #'identity reason " "))
;;     (and (string= reason "")
;;          (setq reason nil))
;;     (erc-cmd-BAN nick)
;;     (erc-server-send (format "KICK %s %s %s"
;;                              (erc-default-target)
;;                              nick
;;                              (or reason
;;                                  "Kicked (kickban)"))))
;;   )


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: turn this into yasnippet?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-chat)
