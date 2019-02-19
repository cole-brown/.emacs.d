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

;;---
;; Use Tool
;;---

;; simple list of tools registered by use-tool?
(defvar use-tool-selected-tools '()
  "Tools in use or to-be found.")

(defvar use-tool-selected-sources '(emacs-environment) ;; should emacs be a 'emacs or a :emacs?
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

;; setup tools for Exec-Path and ENV PATH
;; NOTE: BE MINIMAL AND LAZY
;; NOTE: GET ANYTHING WORKING
(defmacro use-tool-source (name &rest args)
  "todo: docstring"
  (unless (memq :disabled args)
    (message "uts: %s %s" name args)
    ;; if don't know :name, complain and done.

    ;; TODO: get working minimally

    (add-to-list 'use-tool-selected-sources name)
    (message "use-tool-source `%s' added: %s" name use-tool-selected-sources)
    ))

(use-tool-source git-for-windows :disabled)
(use-tool-source git-for-windows)

(defun use-tool-source (source)
  (plist-get args keyword) default)

  (dolist 
  ;; if :disabled keyword, done.
  :disabled

  ;; if :preface, do that thing now
  
  ;; if :init, do that thing now
  ;; TODO: lots of stuff?..

  (add-to-list 'use-tool-available-sources source)
  (message "use-tool-source `%s' added: %s" source use-tool-available-sources)
  ;; if :config, do that thing now
  )


;; ;;(listp use-tool-selected-tools)
;; ;;(not (null use-tool-selected-tools))
;; ;; (add-to-list 'use-tool-selected-tools 'gpg)
;; ;; (use-tool 'gpg)
;; ;; (use-tool 'git)
;; ;; (memq 'ngit use-tool-selected-tools)
;; (defun use-tool (tool) ;; or tool-name or something if not a full tool struct
;;   ;; if :disabled keyword, done.
;;   :disabled
;; 
;;   ;; if :preface, do that thing now
;; 
;;   ;; if don't know :source, complain and done.
;;   :source
;; 
;;   (if (memq tool use-tool-selected-tools)
;;       (message "use-tool: `%s' already registered: %s" tool use-tool-selected-tools)
;;     
;;     ;; if don't know :package, complain, tell to do manually in :config.
;; 
;;     ;; if :init, do that thing now
;; 
;;     ;; ensure in both or one path?
;;     ;; :ensure-path
;;     ;; :ensure-env-path
;;     ;; :ensure-exec-path
;; 
;;     ;; TODO: lots of stuff?..
;; 
;;     (add-to-list 'use-tool-selected-tools tool)
;;     (message "use-tool `%s' added: %s" tool use-tool-selected-tools))
;;   ;; if :config, do that thing now
;;   )

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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool)
