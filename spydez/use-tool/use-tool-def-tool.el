;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Functions and vars for defining tools and tool sources.
;; Actual defining should probably go in another file.

(require 'cl-lib)

;;------------------------------------------------------------------------------
;; Tools
;;------------------------------------------------------------------------------

;;---
;; Tool struct
;;---
(cl-defstruct (tool (:constructor tool--create)
                    (:copier nil))
  name doc
  versions used-by
  ;; not sure. may go away but I think we need more stuff of nebuluous nature?
  vars
  ;; think these are for after create, when it's actually found
  path version system)


;; simple list of tools defined for use-tool?
(defvar use-tool-defined-tools '()
  "docstring.")

(defun def-tool (tool)
  (message "%s" tool)

  ;; make tool? let caller make?

  ;; verify?

  ;; put into tool list
  (add-to-list 'use-tool-defined-tools tool))


;;---
;; Source struct
;;---
(cl-defstruct (tool-source (:constructor tool-source--create)
                           (:copier nil))
  name doc
  versions get-version
  tools systems
  paths)

;; simple list of tool sources registered for use-tool?
(defvar use-tool-defined-sources '()
  "docstring.")

(defun def-tool-source (tool-source)
  (message "%s" tool-source)

  ;; make tool-source? let caller make?

  ;; verify?

  ;; put into tool list
  (add-to-list 'use-tool-defined-sources tool-source)
  )


;;   (let ((version (magit-git-version)))
;;     (when (and version
;;                (version< version magit--minimal-git)
;;                (not (equal (getenv "TRAVIS") "true")))
;;       (display-warning 'magit (format "\
;; Magit requires Git >= %s, you are using %s.

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool-def-tool)
