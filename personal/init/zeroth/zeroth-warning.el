;; -*- emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; 'Themed' Warnings/Messages for More Help in Fixing Things 
;;------------------------------------------------------------------------------

;;---
;; Warnings
;;---
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


;;---
;; Debugs
;;---

;; TODO: move this somewhere? debug-early or something?..
;; TODO: global enable/disable flag
;; TODO: lwarn w/ ":debug"?
(defun spydez/debug/message (type message &rest args)
  (let* ((type (or type '(spydez debug general)))
        (injected-message (format "  %s:  %s" type message)))
    (apply 'message injected-message args)))
;;(spydez/debug/message nil "My spydez/debug/message test: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-warning)
