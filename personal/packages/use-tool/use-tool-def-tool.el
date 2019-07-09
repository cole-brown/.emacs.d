;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Functions and vars for defining tools and tool sources.
;; Actual defining should probably go in another file.

(require 'cl-lib)

;;------------------------------------------------------------------------------
;; Debug
;;------------------------------------------------------------------------------
(setq use-tool-debug-enabled nil)
(defun use-tool-debug-message (message &rest args)
  "a debug helper"
  (unless (or (null use-tool-debug-enabled)
              (memq :disabled args))
    (let* ((type '(use-tool debug))
           (level :debug)
           (injected-message (format "  %s:  %s" type message)))
      (apply 'lwarn type level injected-message args)
      ;; basically becomes e.g.:
      ;; (lwarn '(use-tool debug) :debug "  %s:  Debug message (args %s) here." '(use-tool debug) args)
      )))
;; (use-tool-debug-message "use-tool-debug message test: %s %s" '(testing list) 'test-symbol)


;;------------------------------------------------------------------------------
;; Tools
;;------------------------------------------------------------------------------

;;---
;; Tool struct
;;---
(cl-defstruct (use-tool-struct-tool (:constructor use-tool-struct-tool--create)
                                    (:copier nil))
  name doc exec-name
  versions used-by
  ;; not sure. may go away but I think we need more stuff of nebuluous nature?
  vars
  ;; think these are for after create, when it's actually found
  path version source)


;; simple list of tools defined for use-tool?
(defvar use-tool-defined-tools '()
  "docstring.")

(defun use-tool-def-tool (tool)
  (use-tool-debug-message "%s" tool :disabled)

  ;; make tool? let caller make?
  ;; verify?

  ;; put into tool list
  (add-to-list 'use-tool-defined-tools tool))


;;---
;; Source struct
;;---
(cl-defstruct (use-tool-struct-source (:constructor use-tool-struct-source--create)
                                      (:copier nil))
  name doc
  versions get-version
  tools systems
  paths)

;; simple list of tool sources registered for use-tool?
(defvar use-tool-defined-sources '()
  "docstring.")

(defun use-tool-def-source (source)
  (use-tool-debug-message "%s" source :disabled)

  ;; make source? let caller make?
  ;; verify?

  ;; put into tool list
  (add-to-list 'use-tool-defined-sources source)
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool-def-tool)
