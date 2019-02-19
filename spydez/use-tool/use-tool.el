;;; use-tool.el --- For git, gpg, etc.  -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Intended to be like use-package but for tools - a way to say, e.g.
;; I have "Git for Windows" and I know it has git and gpg. Figure out where they
;; are and how to hook them up to magit, EPA, etc.

;; python virtualenv got anything interesting I should know about?

;; GNU Emacs FAQ for Windows
;;   https://www.gnu.org/software/emacs/manual/html_mono/efaq-w32.html


;;------------------------------------------------------------------------------
;; Idea 4 - more than one file
;;------------------------------------------------------------------------------
(require 'cl-lib)
(require 'dash)
(require 'use-tool-def-tools)
(require 'use-tool-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: grep: TODO:use-tool-done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup tools for Exec-Path and ENV PATH
;; NOTE: BE MINIMAL AND LAZY
;; NOTE: GET ANYTHING WORKING
;; TODO: do like this:
;; (use-tool-source git-for-windows :disabled) ; or
;; (use-tool-source git-for-windows)
;; ...
;; then
;; (use-tool git)
;; (use-tool gpg)
;; ...
;; then... done? or `final go' function?

;;---
;; Use Tool
;;---

;; simple list of tools registered by use-tool?
(defvar use-tool-selected-tools nil
  "Tools in use or to-be found.")

;; I had 'emacs-environment in here as an element, but it's a constant thing
;; whereas these can change.
(defvar use-tool-selected-sources nil
  "Start off with just emacs itself (and the environment it lives in/knows about).")

;; https://stackoverflow.com/questions/26102889/how-do-i-make-named-arguments-in-elisp
;; Though packages that use it like, for all their options might be a better look.

;; (setq lista '('afirst))
;; (setq listb '('bfirst))
;; (defmacro test (name &rest args)
;;   (add-to-list 'listb name)
;;   (message "test: %s %s lista: %s listb: %s" name args lista listb)
;;   )
;; (test foo "bar")
;; (add-to-list 'lista "hi")
;; (add-to-list 'lista 'symbol)

(defmacro use-tool-source (name &rest args)
  "Sparse now... just enable/put into selected sources for tools to check?"
  ;; TODO: if not registered in use-tool-defined-tool-sources, quit and complain
  (unless (or (memq :disabled args)
              (not (memq name use-tool-defined-sources))) ;; TODO: complain if not member
    (message "uts: %s %s" name args)
    ;; todo: if don't know :name, complain and done.

    ;; get working minimally... I think just putting in list is good...

    (add-to-list 'use-tool-selected-sources name)
    (message "use-tool-source `%s' added: %s" name use-tool-selected-sources)

    ;; return what? add-to-list result?
    ))

;; (use-tool-source git-for-windows :disabled)
;; (use-tool-source git-for-windows)

(defmacro use-tool (name &rest args)
  "Sparse now... just enable/put into selected sources for tools to check?"
  ;; TODO: if not registered in use-tool-defined-tools, quit and complain
  (unless (or (memq :disabled args)
              (not (memq name use-tool-defined-tools))) ;; TODO: complain if not member
    (message "uts: %s %s" name args)
    ;; todo: if don't know :name, complain and done.

    ;; TODO: get working minimally...
    ;; if known by emacs, we should check versions some day
    ;; but today just ok out?

    ;; TODOTODOTODO: HERE! check if known,
    ;; TODOTODOTODO: (dolist use-tool-defined-tools ...) if not
    ;; if unknown, look through selected sources
    ;; first one found, use that.
    ;; add to path/exec path (someday check for ensure all vs exec vs env path keywords)

    ;; if config to do, check again
    ;; if known now, do config
    ;;   i.e. get gpg working or whatever

    ;; return known/unknown?

    (add-to-list 'use-tool-selected-tools name)
    (message "use-tool-source `%s' added: %s" name use-tool-selected-tools)
    ))

;; (use-tool git :disabled)
;; (use-tool git)


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------
(defun use-tool-os-and-tool-p (sys-type tool-name)
  "t if on system-type and tool exists on system-type, else nil"
  (if (and
       ;; on certain OS...
       (eq system-type sys-type)
       ;; ...and looking for an expected external tool
       (executable-find tool-name))
      t
    nil))
;; e.g. (when (use-tool-os-and-tool-p 'windows-nt "bash") (message "hello there"))

;; (use-tool-defined-p tool-name)
;; (use-tool-source-defined-p source-name)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool)
