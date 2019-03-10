;; -*- emacs-lisp -*-


;;-----------------------------------emacs--------------------------------------
;;--                  for things that change emacs itself                     --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
(require 'delight nil 'noerror) ;; ask nicely for delight...
(when (featurep 'delight) ;; and shorten "Emacs-Lisp" mode text a bit if we can:
  (delight 'emacs-lisp-mode "Elisp" :major))

;;------------------------------------------------------------------------------
;; General Keybinds?
;;------------------------------------------------------------------------------
;; TODO: a custom, more useful layout for the function keys? I use F3 and
;; F4 currently for doing macros, but none of the rest. 
;; e.g.:
;; (global-set-key (kbd "<f1>") 'kmacro-start-macro)
;; (global-set-key (kbd "<f2>") 'kmacro-end-macro)
;; (global-set-key (kbd "<f3>") 'kmacro-end-and-call-macro)
;;                                         ; F4 is free
;;                                         ; Keep F5 free per mode
;;                                         ; Keep F6 free per mode
;;                                         ; Keep F7 free per mode
;;                                         ; F8 is free
;;                                         ; F9 is free
;;                                         ; F10 is free
;; (global-set-key (kbd "<f11>") 'gcr/insert-timestamp)
;; (global-set-key (kbd "<f12>") 'gcr/comment-or-uncomment)
;; from: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html#sec-10-4
;;
;; But use bind-keys:
;;
;; (bind-keys*
;;   ([(control backspace)] . spydez/backward-delete-word)
;;   ("C-w" . backward-kill-word)
;;   ("C-c C-k" . kill-region)
;;   )


;;------------------------------------------------------------------------------
;; Scrolling
;;------------------------------------------------------------------------------

;; TODO: this might belong in configure-text or something.

;; Sets mouse scroll to one line regardless.
;; Default is 5 lines if normal, 1 line if S-scroll, and 1 page if C-scroll.
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Disables mouse scroll acceleration, basically?
;; (setq mouse-wheel-progressive-speed nil)

;; Don't really want any of these changed right now...
;; (setq mouse-wheel-follow-mouse t) is for which frame scrolls: focused or hovered
;; scroll-step: how much to scroll by to try to get point back in frame
;; scroll-conservatively: ...same thing? but tries to do 'just enough'

;; TODO: experiment with this one, might be good at 2-3 lines...
;; scroll-margin: how many lines of buffer to leave


;;------------------------------------------------------------------------------
;; Title
;;------------------------------------------------------------------------------

;; Found out about frame-title-format in this init.org:
;;   https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html#sec-10-2-2
;; The examples here are from:
;;   http://emacs-fu.blogspot.com/2011/01/setting-frame-title.html
;; All the %-constructs:
;;   https://www.emacswiki.org/emacs/FrameTitle

;;(setq frame-title-format "Hello there. %b %s")

;; ;; emacs: ~/.emacs.d/init.el
;; (setq frame-title-format
;;   '("" invocation-name ": "(:eval (if (buffer-file-name)
;;                 (abbreviate-file-name (buffer-file-name))
;;                 "%b"))))

;; ;; emacs-Cole's-PC: ~/.emacs.d/init.el [-]
;; ;; %@: - means default/nothing
;; ;;     @ means that `default-directory' is on remote system
;; ;; %*: - means default/nothing (writeable with no changes)
;; ;;     % means read-only buffer
;; ;;     * means modified buffer
;; (setq frame-title-format
;;   '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
;;                 (abbreviate-file-name (buffer-file-name))
;;                   "%b")) " [%*]"))

;; Annnd... think this one is good for now.
(setq frame-title-format
    ;; %*: buffer status (read/write/modified)
  '(" [%*] "
    ;; invocation-name: emacs
    ;; :eval keyword: re-eval sexpr every time when updating title
    ;; %@: user-emacs-directory status (local/remote)
    invocation-name "%@" (:eval (system-name)) ": "
    ;; (abbreviated) file name, else buffer name
    (:eval (if (buffer-file-name)
            (abbreviate-file-name (buffer-file-name))
            "%b"))
    ;; %X: human-readable buffer size
    ;;  B: "human-readable" means just "k, M, etc" so add the B for Byte.
    " %IB"
    ))

;; Consider: updating it to use (multiple-frames ...) if I start using more
;; than one frame normally.


;;------------------------------------------------------------------------------
;; Garbage Collection
;;------------------------------------------------------------------------------
;; Give the minibuffer more gc room:
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defconst spydez/gc-cons-threshold/minibuffer (* 100 1000 1000)
  "Give minibuffer 100MB limit temporarily.")

;;---
;; Hooks
;;---
(defun spydez/hook/gc-minibuffer/enter ()
  (setq gc-cons-threshold spydez/gc-cons-threshold/minibuffer))

(defun spydez/hook/gc-minibuffer/exit ()
  (setq gc-cons-threshold spydez/gc-cons-threshold/normal))

;; TODO: profile this - does it add/save time, does it feel better/laggier?
(add-hook 'minibuffer-setup-hook #'spydez/hook/gc-minibuffer/enter)
(add-hook 'minibuffer-exit-hook #'spydez/hook/gc-minibuffer/exit)


;;------------------------------------------------------------------------------
;; Performance: (Very) Long Lines
;;------------------------------------------------------------------------------

;; See in spydez/dir/docs/notes
;;   - performance.long-lines.org
;;   - performance.long-lines.example.log

;; The issue: Something is causing terrible slowness, laggy responsiveness in
;; files with long lines (like the example).

;;---
;; Failures
;;---
;; `So Long' package failed to be useful.
;; Disabling as many minor modes as possibel also useless.


;;---
;; Solution: Too Long Lines Mode
;;---
;; Not on MELPA, but is on GitHub.
;; https://github.com/rakete/too-long-lines-mode

;; "To use this mode just require this file, configure too-long-lines-threshold
;; and too-long-lines-show-number-of-characters to your pleasing and call
;; too-long-lines-mode to enable the mode globally."
(use-package too-long-lines-mode
  ;; have it specify that it's in the manual package archive dir
  :load-path spydez/dir/emacs/manual-packages
  ;; May want a "version" on the file?
  ;; For now... I downloaded that on [2019-02-25 Mon].

  :demand t

  ;; TODO: do I really want to delight this? Maybe a nice noticable icon
  ;; if I get the all-the-icons package... It's "tll" by default.
  :delight

  :config
  (too-long-lines-mode t))

;; Ok. This /does/ help with the example file. Excellent responsiveness.


;;---
;; Misc
;;---
;; Minorly helpful? But didn't profile or anything so could be entirely wrong.

;; bi-directional display not needed for my english-only stuff...
(setq bidi-display-reordering nil)


;;------------------------------------------------------------------------------
;; Performance: (Very) Large Files
;;------------------------------------------------------------------------------
;;   "Enter vlf, vlf is short for "View Large Files" and is a very nice way to
;; handle viewing extremely large files in Emacs, not just log files. I've used
;; it successfully for reading log files over 10 gigabytes. I'll leave it to you
;; to read the page about the features it provides, but suffice it to say that
;; it breaks up large files into manageable chunks, and then provides tools to
;; operate on either a small chunk, or across all the chunks of a very large
;; file."
;;   - https://writequit.org/articles/working-with-logs-in-emacs.html

;; As logs are my only very large files that I want to be looking at with emacs,
;; I'll set this up there:

;;---
;; Logs
;;---
;; See `configure-logs.el'
;;
;; There were enough goodies in that link
;;   https://writequit.org/articles/working-with-logs-in-emacs.html
;; that I decided it deserved its own config file.


;;------------------------------------------------------------------------------
;; Network
;;------------------------------------------------------------------------------

;; Have TRAMP use SCP... if I get around to setting up TRAMP.
;; TODO: get around to setting up TRAMP?

;; tramp-default-method...
;; (setq tramp-default-user "root")
;; Uh... this looks wrong? Like it won't set tramp-default-method...
;; (condition-case nil
;;     (require 'tramp)
;;   (setq tramp-default-method "scp")
;;   (error (message "** could not load tramp")))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; Also could make buffer read-only?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-emacs)
