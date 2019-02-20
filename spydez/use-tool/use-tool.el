;;; use-tool.el --- For git, gpg, etc.  -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Intended to be like use-package but for tools - a way to say, e.g.
;; "I have "Git for Windows" and I know it has git and gpg. Figure out where they
;; are and how to hook them up to magit, EPA, etc."

;; python virtualenv got anything interesting I should know about?
;; or this? https://github.com/purcell/exec-path-from-shell

;; GNU Emacs FAQ for Windows
;;   https://www.gnu.org/software/emacs/manual/html_mono/efaq-w32.html

(require 'use-tool-def-tool)
(require 'use-tool-definitions)
(require 'use-tool-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: grep: TODO:use-tool-done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------------
;; Use-Tool macros
;;------------------------------------------------------------------------------

;; https://stackoverflow.com/questions/26102889/how-do-i-make-named-arguments-in-elisp
;; Though packages that use it like, for all their options might be a better look.

(defmacro use-tool-source (name &rest args)
  "Sparse now... just enable/put into selected sources for tools to check?"
  (use-tool-debug-message "use-tool-source: %s %s" name args)
  (unless (memq :disabled args)
    (let* ((source (use-tool-find-source name)))
      (if (null source) ;; TODO: complain if not defined?
          ;; failure case - return nil
          (progn
            (use-tool-debug-message "use-tool-source `%s' not found. source: %s" name source)
            ;; return nil
            nil)
        
        ;; success - have source
        (use-tool-debug-message "use-tool-source ok: %s %s" name args)

        ;; get working minimally... I think just putting in list is good...
        (use-tool-enable-source name)
        ;; return enable's return val? I wanted to but I get:
        ;;   use-tool-source: Symbol’s function definition is void: <name>
        ;; and I don't know enough elisp to fix that right now. So:
        'use-tool-selected-sources
        ))))
;; (use-tool-source git-for-windows :disabled)
;; (use-tool-source git-for-windows)


(defmacro use-tool (name &rest args)
  "Sparse now... just enable/put into selected sources for tools to check?"
  (use-tool-debug-message "use-tool: %s %s" name args)
  (unless (memq :disabled args)
    ;; TODO: need to allow something through for "no source, maybe no tool, but emacs knows about it"?
    (let* ((source (use-tool-find-source-by-tool name))
           (tool (use-tool-find-tool name))
           (tool-str (use-tool-struct-tool-exec-name tool)))
      (if (or (null source) ;; TODO: complain if not defined?
              (null tool)  ;; TODO: complain if not defined
              (not (memq (use-tool-struct-source-name source) use-tool-selected-sources)) ;; todo: complain not registered
              (memq name use-tool-selected-tools)) ;; TODO: complain or not on second use-tool for this?
          ;; failure case - return nil
          (progn
            ;; TODO: better debugging. Complain about which failed how?
            (use-tool-debug-message "use-tool `%s' not found. use-source?: %s source: %s tool: %s"
                                    name
                                    (memq (use-tool-struct-source-name source) use-tool-selected-sources)
                                    source tool)
            ;; return nil
            nil)
        
        ;; success - have source, tool 
        (use-tool-debug-message "use-tool `%s' ok. source: %s tool: %s"
                                name
                                (use-tool-struct-source-name source)
                                (use-tool-struct-tool-name tool))

        (if (executable-find tool-str)
            ;; if known by emacs, we should check versions some day
            ;; but today just ok out?
            (progn
              (use-tool-debug-message "use-tool `%s' ok. pre-existing: %s" name use-tool-default-source)
              ;; TODO: set tool runtime stats like source?
              ;; (use-tool-tool-runtime-setup ...)
              ;; (use-tool-source-runtime-setup ...)?
              'use-tool-default-source)

          ;; else, we do things ourself
          (use-tool-enable-tool source tool)
          
          ;; ok, emacs should know about it now, if we've done correctly.
          
          ;; TODOTODOTODO: if config to do, check again
          ;; if known now, do config
          ;;   i.e. get gpg working or whatever

          (add-to-list 'use-tool-selected-tools name)
          (use-tool-debug-message "use-tool `%s' added: %s" name use-tool-selected-tools)
          ;; return tool or source or something non-nil
          `(quote ,(use-tool-struct-source-name source))
          )))))
;; (use-tool git :disabled)
;; (use-tool git)
;; (use-tool diff)
;; (use-tool bash)


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------
(defun use-tool-os-and-tool-p (sys-type tool-name)
  "t if on system-type and tool exists on system-type, else nil"
  ;; todo: can probably just return (and ...) sexpr, but I'm not lispy enough to know yet?
  (if (and
       ;; on certain OS...
       (eq system-type sys-type)

       ;; TODO: find tool for it if known about by use-tool?

       ;; ...and emacs knows about this expected external tool
       (executable-find tool-name))
      t
    nil))
;; e.g. (when (use-tool-os-and-tool-p 'windows-nt "bash") (message "hello there"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool)
