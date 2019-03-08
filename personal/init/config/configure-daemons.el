;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; Emacs Server
;;------------------------------------------------------------------------------
;; Make sure Emacs Server is running so emacs will collect itself into one
;; instance and drag-drop, send from Visual Studio, etc will work as expected.

;; (Not using emacs as daemon right now... https://www.emacswiki.org/emacs/EmacsAsDaemon )
;; Just start server for emacs when needed by emacs.

(require 'server)

;; This `server-running-p' doesn't work.
;;   - http://emacshorrors.com/posts/determining-if-the-server-is-started-or-the-wonders-of-server-running-p.html
;;     -> http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00723.html
;; (unless (server-running-p) (server-start))

(defun server-started-p ()
    "Return non-nil if this Emacs has a server started."
    (and (boundp server-process) server-process))

(unless (server-started-p)
  (when (spydez/debugging-p)
    (spydez/debug/message nil "Starting server."))
  (server-start))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-daemons)
