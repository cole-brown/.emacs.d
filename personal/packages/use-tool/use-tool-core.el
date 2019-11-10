;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'use-tool-def-tool)

;;------------------------------------------------------------------------------
;; Vars
;;------------------------------------------------------------------------------

;; simple list of tools registered by use-tool?
(defvar use-tool-selected-tools nil
  "Tools in use or to-be found.")

;; I had 'emacs-environment in here as an element, but it's a constant thing
;; whereas these can change.
(defvar use-tool-selected-sources nil
  "Start off with just emacs itself (and the environment it lives in/knows about).")

(defconst use-tool-default-source 'emacs-environment
  "Your particular setup knew about the tool all on its own.")


;;------------------------------------------------------------------------------
;; Helpers - Find
;;------------------------------------------------------------------------------
(defun use-tool-find-source-by-tool (tool-name)
  "Returns first source in list use-tool-defined-sources that has tool as registered."
  ;; need sanity? (symbolp tool-name)
  (let ((found-source nil))
    (dolist (source use-tool-defined-sources found-source)
      (use-tool-debug-message "%s %s %s"
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
      (use-tool-debug-message "%s %s" source-name (use-tool-struct-source-name source))
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


;;------------------------------------------------------------------------------
;; Helpers - Enable, Add
;;------------------------------------------------------------------------------
;; TODO: pass this source instead of source-name?
(defun use-tool-enable-source (source-name)
  (use-tool-debug-message "use-tool-enable-source `%s' added: %s" source-name use-tool-selected-sources)
  ;; Don't want to do much, really. Wait for a tool to need the source.

  ;; todo: verify (system-name) in source's :systems list?

  ;; want to return something... the updated list is ok for now probably
  (add-to-list 'use-tool-selected-sources source-name))
;; (use-tool-enable-source 'git-for-windows)

(defun use-tool-enable-tool (source tool)
  (let ((tool-name (use-tool-struct-tool-name tool))
        (source-name (use-tool-struct-source-name source))
        (paths (use-tool-struct-source-paths source))
        (exec-name (use-tool-struct-tool-exec-name tool)))

    ;; for each path list in paths list of lists
    ;;   check system type
    ;;   if ok, construct paths
    ;;     0th elem: system type
    ;;     1th elem: root path
    ;;     2+ elems: relative paths to tack onto root
    (dolist (sys-paths paths)
      (let ((path-sys-type (nth 0 sys-paths))
            (path-base (nth 1 sys-paths))
            (paths-rel (nthcdr 2 sys-paths)))
        (when (eq system-type path-sys-type)
          (use-tool-debug-message "use-tool-enable-tool: %s::%s: %s %s %s"
                                  source-name tool-name path-sys-type path-base paths-rel)
          (use-tool-add-paths exec-name path-base paths-rel)
          )))))
;; (use-tool-enable-tool (use-tool-find-source 'git-for-windows)
;;                       (use-tool-find-tool 'git))

;; TODO: add ability to have keywords like :ensure or something
;; to force add path?
;; TODO: keyword for prepend vs append? :ensure forces prepend?
(defun use-tool-add-paths (exec-name base relatives)
  ;; This is kind of half-braindead, in that it adds things to paths until it
  ;; knows about the exec-name. Think maybe go full braindead (add all paths) or
  ;; smarter (find the one that work, just add that).
  (dolist (rel relatives)
    (let ((env-paths (getenv "PATH"))
          (cur-path (concat (file-name-as-directory base) (file-name-as-directory rel))))
      ;; now we can build each path from base and a rel
      (use-tool-debug-message "utap: %s: %s %s %s %s" cur-path base rel exec-name (executable-find exec-name))

      ;;---
      ;; exec-path
      ;;---
      ;; [2019-11-09]: Windows 10 has bash, but it doesn't like windows paths. I
      ;; need to decide what to do about this... Mean time, I want to be able to
      ;; use ripgrep.
      ;; So... prepend to exec-path?
      (add-to-list 'exec-path cur-path)
      ;; ;; add to exec-path unless it's already a known executable
      ;; (unless (executable-find exec-name)
      ;;   (add-to-list 'exec-path cur-path t)) ;; append to end for now

      ;;---
      ;; PATH
      ;;---
      ;; NOTE: This has a few holes. There are both 'C:\path\to' and 'C:/path/to', for example.
      ;;   But I'll try the stupid approach first.
      ;; skip if known, or if empty/nil string for path
      (unless (or (string-match-p (regexp-quote cur-path) env-paths) ;; bad way to check if known
                  (or (string= cur-path "") ;; also sanity check empty/nil cur-path
                      (string= cur-path nil)))
        ;; this one we didn't skip, append to end of PATH var, which is just
        ;; a string (paths separated by OS-appropriate separator)
        ;; so... todo: find os-appropriate way to concat to env path?
        ;; TODO before that: complain if we're in not-windows so that we can fix
        ;; it where we can test it
        (setenv "PATH" (concat env-paths
                               ";"
                               cur-path
                               ))
        ;; (use-tool-debug-message nil "now tool %s in path?: %s %s"
        ;;          tool-name
        ;;          (string-match-p (regexp-quote cur-path) (getenv "PATH"))
        ;;          cur-path)
        ))))

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool-core)
