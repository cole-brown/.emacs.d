;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; 'Themed' Warnings/Messages for More Help in Fixing Things 
;;------------------------------------------------------------------------------

(require 'warnings)
;; `:warning' should pop up the *Warning* buffer
(setq warning-minimum-level :warning)
;; `:debug' should be logged, but not pop up the *Warning* buffer
(setq warning-minimum-log-level :debug)

;; shouldn't really need adjusting much. maybe to :debug
(setq spydez/warning/current-level ':warning)

;; First type in list: always 'spydez for my stuff
;;
;; Second type in list: current part of init sequence.
;;   - post (power-on-self-test)
;;   - early (early-init)
;;   - interstitial-prose (aka don't have code here plz part of init)
;;   - bootstrap (first bit of init)
;;   - config (setup packages, tools, keybinds, etc)
;;   - finalize (checks and cleanup)
;;   - running (totally done with init)
;;
;; Third and greater type: whatever sub-types you want.
(setq spydez/warning/current-type '(spydez early))

;; TODO: Move that thing about "failure is not an option in init (for my code)" to here?
;; NOTE: Keep the lwarn at the beginning of this file roughly like this.
(defun spydez/warning/message (type level message &rest args)
  "type: list with spydez first or nil e.g. nil will become '(spydez bootstrap)
level: level for lwarn e.g. nil will become :warning
https://www.gnu.org/software/emacs/manual/html_node/elisp/Warning-Basics.html#Warning-Basics"
  (let* ((type (or type spydez/warning/current-type))
         (level (or level spydez/warning/current-level))
         (injected-message (format "  %s:  %s" type message)))
    (apply 'lwarn type level injected-message args)
    ;; basically becomes e.g.:
    ;; (lwarn '(spydez bootstrap) :warning "  %s:  Update 'Master List' for this system (%s) here." '(spydez bootstrap) spydez/setup/system/hash)
    ))
;; (spydez/warning/message nil nil "My spydez/warning/message test: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;; 'Themed' Debugss/Messages for More Help in Fixing Things.
;;------------------------------------------------------------------------------

;; See bootstrap-debug-early.el for some init debug options, settings, and flags.

;; Can also debug into warnings buffer:
;; (spydez/warning/message nil :debug "Couldn't do a thing.")
;; (spydez/warning/message (append spydez/warning/current-type (list 'info)) :debug "Couldn't do a thing.")

(defconst spydez/init-debug t) ;; nil)
(defun spydez/debugging-p ()
  (bound-and-true-p spydez/init-debug))

;; Third and greater type: whatever sub-types you want.
(defconst spydez/debug/current-type '(spydez debug general))
(defconst spydez/info/current-type '(spydez info))

;; TODO: all these types... I'm not using them much.
;; Should I make them take a symbol or list, then append that to their (current) defaults?

;; TODO: currently have spydez/{warning,debug,info}/message[-{if,always}].
;; Is that correct, or should I have e.g.
;;   - spydez/message[-{if,always}]/{warning,debug,info}
;;   - spydez/message/{warning,debug,info}[-{if,always}]

;; obeys global enable/disable flag
(defun spydez/debug/message-if (type message &rest args)
  ;; Figured out a lisp thing.
  ;; Thanks: https://stackoverflow.com/a/26707692
  (when (spydez/debugging-p) (apply #'spydez/debug/message-always type message args)))
;;(spydez/debug/message-if nil "My spydez/debug/message test: %s %s" '(testing list) 'test-symbol)

(defun spydez/debug/message-always (type message &rest args)
  (let* ((type (or type spydez/debug/current-type))
        (injected-message (format "  %s:  %s" type message)))
    (apply 'message injected-message args)))
;;(spydez/debug/message-always nil "My spydez/debug/message test: %s %s" '(testing list) 'test-symbol)

(defun spydez/info/message-if (type message &rest args)
  (let* ((type (or type spydez/info/current-type)))
    (when (spydez/debugging-p) (apply #'spydez/debug/message-always type message args))))
;;(spydez/info/message-if nil "My spydez/info/message-if test: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: also log all into my own buffer (regardless of level, debugging-p?)


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-debug)
