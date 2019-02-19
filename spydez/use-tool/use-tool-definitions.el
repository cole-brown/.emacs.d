;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Actual defining of tools and sources.

(require 'use-tool-def-tool)

;; cl-defstruct accessors:
;; (setq tool-example ...)
;; (tool-name tool-example)
;; (setf (tool-name tool-example) 'new-name)

;; Should think about serialize someday? maybe this? probably should google for
;; that specifically instead of just saving a page I happened across...
;; https://www.tutorialspoint.com/lisp/lisp_structures.htm
;; ?? (write hhhh)
;; ?? (terpri)
;; (read

 ;; TODO: figure out how to check git and other exe versions

;;------------------------------------------------------------------------------
;; Tools
;;------------------------------------------------------------------------------

(def-tool (tool--create :name 'gpg
  :doc
  "GnuPG used by e.g. EasyPG in emacs."
  
  :versions
  '("gpg (GnuPG) 2.2.11-unknown")
  
  :used-by
  '('epa-file)

  ;; Hey, we can leave things nil.
  ;; :vars
  ;; nil
  ;; 
  ;; :path
  ;; nil
  ;; 
  ;; :version
  ;; "z.3"
  ;; 
  ;; :system
  ;; 'windows-nt
  ))

(def-tool (tool--create :name 'git
  :doc
  "git. For Version Control."
  
  :versions
  '("git version 2.20.1.windows.1")
  
  :used-by
  '('magit)
  ))

(def-tool (tool--create :name 'diff
  :doc
  "For external diff."
  
  :versions
  '("diff (GNU diffutils) 3.6")
  
  :used-by
  '('magit)
  ))

(def-tool (tool--create :name 'bash
  :doc
  "Bourne Again Shell."
  
  :versions
  '("GNU bash, version 4.4.23(1)-release (x86_64-pc-msys)")
  
  :used-by
  '('shell)
  ))

;;------------------------------------------------------------------------------
;; Tool Sources
;;------------------------------------------------------------------------------

;; This one source - Git for Windows - is all I currently need.
(def-tool-source (tool-source--create :name 'git-for-windows
  :doc
  "Git for Windows is its own little MinGW eco-system."
  
  :versions
  '("git version 2.20.1.windows.1")
  
  ;; :get-version
  ;; (...)
  
  :tools
  '('git 'gpg 'diff 'bash)
  
  :systems
  '('windows-nt)
  
  :paths
   '('('windows-nt "C:/Program Files/Git" "mingw64/bin" "usr/bin"))
   ))

;; (message "%s" use-tool-defined-tool-sources)
;; (message "%s" use-tool-defined-tools)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool-definitions)
