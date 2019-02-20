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

;; TODO: foo-name vs foo-symbol vs foo when foo expected to be symbol
;; in func args, but it names a thing...?

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

(defconst use-tool-default-source 'emacs-environment
  "Your particular setup knew about the tool all on its own.")

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
  (unless (memq :disabled args)
    (let* ((source (use-tool-find-source name)))
      (if (null source) ;; TODO: complain if not defined
          ;; todo: if don't know name, complain and done.
          
          ;; failure case - return nil
          (progn
            ;; TODO: better debugging maybe. Complain about which failed how?
            (message "use-tool-source `%s' not found. source: %s" name source)
            ;; return nil
            nil)
        
        ;; success - have source
        ;; TODO: if not registered in use-tool-defined-tool-sources, quit and complain
        (message "use-tool-source ok: %s %s" name args)

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
  (unless (memq :disabled args)
    ;; TODO: need to allow something through for "no source, maybe no tool, but emacs knows about it"?
    ;; TODO: if don't know name, complain and done? Think this is already covered
    (let* ((source (use-tool-find-source-by-tool name))
           (tool (use-tool-find-tool name))
           (tool-str (use-tool-struct-tool-name tool)))
      (if (or (null source) ;; TODO: complain if not defined?
              (null tool)  ;; TODO: complain if not defined
              (memq name use-tool-selected-tools)) ;; TODO: complain or not on second use-tool for this?
          ;; failure case - return nil
          (progn
            ;; TODO: better debugging maybe. Complain about which failed how?
            (message "use-tool `%s' not found. source: %s tool: %s" name source tool)
            ;; return nil
            nil)
        
        ;; success - have source, tool 
        (message "use-tool `%s' ok. source: %s tool: %s"
                 name
                 (use-tool-struct-source-name source)
                 (use-tool-struct-tool-name tool))

        ;; TODO: get working minimally...
        (if (executable-find tool-str)
            ;; if known by emacs, we should check versions some day
            ;; but today just ok out?
            (progn
              (message "use-tool `%s' ok. pre-existing: %s" name use-tool-default-source)
              ;; TODO: set tool runtime stats like source?
              ;; (use-tool-tool-runtime-setup ...)
              ;; (use-tool-source-runtime-setup ...)?
              use-tool-default-source)

          ;; else, we do things ourself
          (use-tool-enable-tool source tool)
          
          ;;---
          ;; exec-path
          ;;---
          ;; add to exec-path unless it's already a known executable
;;           (unless (executable-find tool-name)
;;             (add-to-list 'exec-path tool-path t)) ;; append to end
          
          ;;---
          ;; PATH
          ;;---
          ;; TODO: Should there be some other step for adding a many tools dir? Like Cygwin or Msys's bin dir?
          ;; I think may I rely on env-path-known-p and simplicity.

          ;; Add each external tool to env PATH?
          ;; NOTE: This has a few holes. There are 'C:\foo\bar' and 'C;/baz/qux'.
          ;;   But I'll try the stupid approach first.

          ;; skip if known, or if empty/nil string for path
;;           (unless (or env-path-known-p
;;                       (or (string= tool-path "")
;;                           (string= tool-path nil)))
;;             ;; this one we didn't skip, append to end of PATH var, which is just
;;             ;; a string (paths separated by OS-appropriate separator)
;;             (setenv "PATH" (concat env-path
;;                                    ";"
;;                                    tool-path
;;                                    ))
            ;; (spydez/debug/message nil "now tool %s in path?: %s %s"
            ;;          tool-name
            ;;          (string-match-p (regexp-quote tool-path) (getenv "PATH"))
            ;;          tool-path)
          
          
          ;; TODOTODOTODO: HERE! check if known,
          ;; add to path/exec path (someday check for ensure all vs exec vs env path keywords)
          ;; (exec-path-known-p (if (executable-find tool-name) t nil))
          ;; (env-path (getenv "PATH"))
          ;; (env-path-known-p (if (string-match-p (regexp-quote tool-path) env-path) t nil)))

          ;; if config to do, check again
          ;; if known now, do config
          ;;   i.e. get gpg working or whatever

          ;; (add-to-list 'use-tool-selected-tools name)
          (message "use-tool-source `%s' added: %s" name use-tool-selected-tools)
          ;; return tool or source or something non-nil
          (use-tool-struct-source-name source)
          )))))
;; (use-tool git :disabled)
;; (use-tool git)
;; (executable-find "git")
;; (executable-find 'git)
        

(defun use-tool-find-source-by-tool (tool-name)
  "Returns first source in list use-tool-defined-sources that has tool as registered."
  ;; need sanity? (symbolp tool-name)
  (let ((found-source nil))
    (dolist (source use-tool-defined-sources found-source)
      (message "%s %s %s"
               tool-name
               (use-tool-struct-source-name source)
               (use-tool-struct-source-tools source))
      (when (and (not found-source)
                 (memq tool-name (use-tool-struct-source-tools source)))
        (setq found-source source)))))
;;(use-tool-find-source-by-tool 'git)

(defun use-tool-find-source (source-name)
  "Returns first source in list use-tool-defined-sources that is eq source-name."
  ;; need sanity? (symbolp source-name)
  (let ((found-source nil))
    (dolist (source use-tool-defined-sources found-source)
      (message "%s %s" source-name (use-tool-struct-source-name source))
      (when (and (not found-source)
                 (eq source-name (use-tool-struct-source-name source)))
        (setq found-source source)))))
;;(use-tool-find-source 'git-for-windows)

(defun use-tool-find-tool (tool-name)
  ;; need sanity? (symbolp tool-name)
  (let ((found-tool nil))
    (dolist (tool use-tool-defined-tools found-tool)
      (when (and (not found-tool)
                 (eq tool-name (use-tool-struct-tool-name tool)))
        (setq found-tool tool)))))
;;(use-tool-find-tool 'git)

;; TODO: pass this source instead of source-name?
(defun use-tool-enable-source (source-name)
  (message "use-tool-enable-source `%s' added: %s" source-name use-tool-selected-sources)
  ;; Don't want to do much, really. Wait for a tool to need the source.

  ;; todo: verify (system-name) in source's :systems list?

  ;; want to return something... the updated list is ok for now probably
  (add-to-list 'use-tool-selected-sources source-name))
;; (use-tool-enable-source 'git-for-windows)

(defun use-tool-enable-tool (source tool)
  (let ((tool-name (use-tool-struct-tool-name tool))
        (source-name (use-tool-struct-source-name source))
        (paths (use-tool-struct-source-paths source)))

    ;; for each path list in in paths list of lists
    ;;   check system
    ;;   if ok, construct paths
    ;;     0th elem: system type
    ;;     1th elem: root path
    ;;     2+ elems: relative paths to tack onto root
    (dolist (sys-paths paths)
      (let ((path-sys-type (car sys-paths))
            (rooted-paths (cdr sys-paths)))
        (when (eq system-type path-sys-type)
          (message "use-tool-enable-tool: %s::%s: %s" source-name tool-name rooted-paths)
          ;; TODO:TODO:TODO : hihi here - make with the multiple paths plz
          )))))
(use-tool-enable-tool (use-tool-find-source 'git-for-windows)
                      (use-tool-find-tool 'git))

    ;;---
    ;; exec-path
    ;;---
    ;; add to exec-path unless it's already a known executable
    (unless (executable-find tool-name)
      (add-to-list 'exec-path tool-path t)) ;; append to end
    
    ;;---
    ;; PATH
    ;;---
    ;; TODO: Should there be some other step for adding a many tools dir? Like Cygwin or Msys's bin dir?
    ;; I think may I rely on env-path-known-p and simplicity.

    ;; Add each external tool to env PATH?
    ;; NOTE: This has a few holes. There are 'C:\foo\bar' and 'C;/baz/qux'.
    ;;   But I'll try the stupid approach first.

    ;; skip if known, or if empty/nil string for path
    (unless (or env-path-known-p
                (or (string= tool-path "")
                    (string= tool-path nil)))
      ;; this one we didn't skip, append to end of PATH var, which is just
      ;; a string (paths separated by OS-appropriate separator)
      (setenv "PATH" (concat env-path
                             ";"
                             tool-path
                             ))
      ;; (spydez/debug/message nil "now tool %s in path?: %s %s"
      ;;          tool-name
      ;;          (string-match-p (regexp-quote tool-path) (getenv "PATH"))
      ;;          tool-path)

      ;; TODO: that per-tool extra config here???

      (message "use-tool-enable-tool `%s' from `%s' added: %s"
               tool-name source-name use-tool-selected-tools)
      (add-to-list 'use-tool-selected-sources source-name)
      )


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
