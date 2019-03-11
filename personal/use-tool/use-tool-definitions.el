;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Actual defining of tools and sources.

(require 'use-tool-def-tool)

;; Should think about serialize someday? maybe this? probably should google for
;; that specifically instead of just saving a page I happened across...
;; https://www.tutorialspoint.com/lisp/lisp_structures.htm

;; TODO: figure out how to check git and other exe versions

;;------------------------------------------------------------------------------
;; Tools
;;------------------------------------------------------------------------------

;; TODO: pdf-tools? https://github.com/politza/pdf-tools
;;   https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el

;; TODO: also try to use use-tool for vsvars?
;; ;; Visual Studio: vsvars: for setting up env for vs compile
;; (use-tool-def-tool (use-tool-struct-tool--create :name 'vsvars
;;   :doc
;;   "vsvars for setting up environment for Visual Studio compile"
;;   :exec-name
;;   "vsvars32"
;;
;;   :versions
;;   '("")
;;
;;   :used-by ;;todo
;;   '(my-build-vc-proj-func-hopefully)
;;
;;   ;; Hey, we can leave things nil.
;;   ;; :vars
;;   ;; nil
;;   ;;
;;   ;; :path
;;   ;; nil
;;   ;;
;;   ;; :version
;;   ;; "z.3"
;;   ;;
;;   ;; :source
;;   ;; 'git-for-windows
;;   ))


;; TODO TODAY TODO HERE
;; Spell check: hunspell
;;(use-tool-def-tool (use-tool-struct-tool--create :name 'hunspell
;;  :doc
;;  "Spell checker program. Also needs a dictionary... somewhere..."
;;  :exec-name
;;  "hunspell"
;;
;;  :versions
;;  '("") ;; todo
;;
;;  :used-by
;;  '(ispell flyspell)
;;
;;  ;; Hey, we can leave things nil.
;;  ;; :vars
;;  ;; nil
;;  ;;
;;  ;; :path
;;  ;; nil
;;  ;;
;;  ;; :version
;;  ;; "z.3"
;;  ;;
;;  ;; :source
;;  ;; 'hunspell-for-windows
;;  ))

;; todo: mark as a git-for-windows tool?
(use-tool-def-tool (use-tool-struct-tool--create :name 'gpg
  :doc
  "GnuPG used by e.g. EasyPG in emacs."
  :exec-name
  "gpg"
  
  :versions
  '("gpg (GnuPG) 2.2.11-unknown")
  
  :used-by
  '(epa-file)

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
  ;; :source
  ;; 'git-for-windows
  ))

;; todo: mark as a git-for-windows tool?
(use-tool-def-tool (use-tool-struct-tool--create :name 'git
  :doc
  "git. For Version Control."
  :exec-name
  "git"
  
  :versions
  '("git version 2.20.1.windows.1")
  
  :used-by
  '(magit)
  ))

;; todo: mark as a git-for-windows tool?
(use-tool-def-tool (use-tool-struct-tool--create :name 'diff
  :doc
  "For external diff."
  :exec-name
  "diff"
  
  :versions
  '("diff (GNU diffutils) 3.6")
  
  :used-by
  '(magit)
  ))

;; todo: mark as a git-for-windows tool?
(use-tool-def-tool (use-tool-struct-tool--create :name 'bash
  :doc
  "Bourne Again Shell."
  :exec-name
  "bash"
  
  :versions
  '("GNU bash, version 4.4.23(1)-release (x86_64-pc-msys)")
  
  :used-by
  '(shell)
  ))


;;------------------------------------------------------------------------------
;; Tool Sources
;;------------------------------------------------------------------------------

;; TODO: VS2010 source

;; TODO TODAY TODO HERE
;; hunspell tool is from hunspell - yeah.
;;(use-tool-def-source (use-tool-struct-source--create :name 'hunspell-for-windows
;;  :doc
;;  "Hunspell spell checker for Windows."
;;
;;  :versions
;;  '("") ;; todo
;;
;;  ;; :get-version
;;  ;; (...)
;;
;;  :tools
;;  '(hunspell)
;;
;;  ;; TODO: a lil' section for, like, config code of tools...
;;
;;  :systems
;;  '(windows-nt)
;;
;;  :paths ;; todo
;;   '((windows-nt "C:/Program Files/Git" "mingw64/bin" "usr/bin"))
;;   ))

;; This one source - Git for Windows - is actually MinGW with a lot of
;; unix/gnu programs git needs.
(use-tool-def-source (use-tool-struct-source--create :name 'git-for-windows
  :doc
  "Git for Windows is its own little MinGW eco-system."
  
  :versions
  '("git version 2.20.1.windows.1")
  
  ;; :get-version
  ;; (...)
  
  :tools
  '(git gpg diff bash)

  ;; TODO: a lil' section for, like, config code of GPG, bash, etc to live for git-for-windows or whatever the source is...
  ;; belongs in the source a wee bit more than in the tool, I think?
  ;; but also the tool needs to know what source it's in. Right now the first one registers with it and then
  ;; the next think emacs just knew. Use exec path or something?
  
  :systems
  '(windows-nt)
  
  :paths
   '((windows-nt "C:/Program Files/Git" "mingw64/bin" "usr/bin"))
   ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'use-tool-definitions)
