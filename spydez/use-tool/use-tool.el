;;; use-tool.el --- For git, gpg, etc.  -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Intended to be like use-package but for tools - a way to say, e.g.
;; I have "Git for Windows" and I know it has git and gpg. Figure out where they
;; are and how to hook them up to magit, EPA, etc.

;; python virtualenv got anything interesting I should know about?

;; GNU Emacs FAQ for Windows
;;   https://www.gnu.org/software/emacs/manual/html_mono/efaq-w32.html

;;------------------------------------------------------------------------------
;; Idea 2 - maybe figure out one way to kinda-object it and start there...
;;------------------------------------------------------------------------------

(require 'cl)


;;---
;; Tool
;;---

(cl-defstruct (tool (:constructor def-tool)
                    (:copier nil))
  name doc
  versions used-by
  vars
  path version system)

(setq hhhh (def-tool :name 'gpg
  :doc
  "GnuPG used by e.g. EasyPG in emacs."
  :versions
  '("x.y" "z.2")
  :used-by
  '('epa)
  :vars
  nil
  :path
  nil
  :version
  "z.3"
  :system
  'windows-nt))

;;---
;; Source
;;---
(cl-defstruct (tool-source (:constructor def-tool-source)
                           (:copier nil))
  name doc
  versions get-version
  tools systems
  paths)

(setq ffff (def-tool-source :name 'git-for-windows
  :doc
  "Git for Windows is its own little MinGW eco-system."
  
  :versions
  '("20190202.1535" "18000101.0700") ;; but I want git (for windows) version
  :get-version
  (magit-version) ;; but I want git (for windows) version
  
  :tools
  '('git 'gpg 'diff 'bash)
  :systems
  '('windows-nt)
  :paths
   '('('windows-nt "C:/Program Files/Git" "mingw64/bin" "usr/bin")
     '('windows-nt "C:/Program Files (x86)/Git" "mingw32/bin" "usr/bin"))
   ))


;;---
;; Use Tool
;;---

;; simple list of tools registered by use-tool?
(defvar use-tool-selected-tools '()
  "Tools in use or to-be found.")

(defvar use-tool-available-sources '('emacs-environment) ;; should emacs be a 'emacs or a :emacs?
  "Start off with just emacs itself (and the environment it lives in/knows about).")

(defun use-tool-source (source) ;; or source-name or something if not a full tool-source struct
  ;; if don't know :name, complain and done.

  ;; if :disabled keyword, done.
  :disabled

  ;; if :preface, do that thing now
  
  ;; if :init, do that thing now
  ;; TODO: lots of stuff?..

  (add-to-list 'use-tool-available-sources source)
  (message "use-tool-source `%s' added: %s" source use-tool-available-sources)
  ;; if :config, do that thing now
  )

;;(listp use-tool-selected-tools)
;;(not (null use-tool-selected-tools))
;; (add-to-list 'use-tool-selected-tools 'gpg)
;; (use-tool 'gpg)
;; (use-tool 'git)
;; (memq 'ngit use-tool-selected-tools)
(defun use-tool (tool) ;; or tool-name or something if not a full tool struct
  ;; if :disabled keyword, done.
  :disabled

  ;; if :preface, do that thing now

  ;; if don't know :source, complain and done.
  :source

  (if (memq tool use-tool-selected-tools)
      (message "use-tool: `%s' already registered: %s" tool use-tool-selected-tools)
    
    ;; if don't know :package, complain, tell to do manually in :config.

    ;; if :init, do that thing now

    ;; ensure in both or one path?
    ;; :ensure-path
    ;; :ensure-env-path
    ;; :ensure-exec-path

    ;; TODO: lots of stuff?..

    (add-to-list 'use-tool-selected-tools tool)
    (message "use-tool `%s' added: %s" tool use-tool-selected-tools))
  ;; if :config, do that thing now
  )



;;------------------------------------------------------------------------------
;; Idea 1 - maybe figure out what I want?
;;------------------------------------------------------------------------------

;; (use-tool gpg
;;           :disabled
;;           
;;           ;; :requires ???
;; 
;;           ;; :provider git-for-windows?
;;           ;; :source? git-for-windows?
;;           ;;  or do we just keep looking til found?
;; 
;;           ;; :provider-path ? or is this part of provider?
;;           
;;           ;; :package epa?
;;           ;;  what needs config'd
;; 
;;           ;; ensure in both or one path?
;;           ;; :ensure-path
;;           ;; :ensure-env-path
;;           ;; :ensure-exec-path
;; 
;;           ;; :preface ;; before everything
;;           ;; :init ;; before main thing?
;;           ;; :config ;; after main thing?
;;           ;; (setq stuff "hi")
;;           )
;; 
;; ;; TOOD: eieio this or that lisp post or try to be base-lispy with it?
;; ;; that post: https://nullprogram.com/blog/2018/02/14/
;; 
;; (def-tool gpg
;;   :versions ("hi" "hi2")
;; 
;;   :compatible ('epa "version")
;;   ;; or
;;   :provides ('epa "version")
;; 
;;   ;; from user... anything?
;;   :vars (???)
;; 
;;   ;; things found after init, or what?
;;   :path "foo"
;;   :version "bar"
;;   :system 'windows-nt ;; will win + cygwin cause some to be win and some to be nix?
;; 
;;   )
;; 
;; (def-tool-group git-for-windows
;;   ;; gonna need a way to know what version these versions is for
;;   :versions ("version string" "other version" "third")
;;   :tools ('git 'gpg 'bash 'diff))
;;   :systems ('windows-nt)
;;   :paths ;; possible paths, usual suspects, w/e
;;    ('windows-nt "C:/Program Files/Git" "mingw64/bin" "usr/bin")
;; 
;;   ;; Also need a version or check to help verify user has thing they says
;;   
;;   ;; Also need ability for different versions to provide different tools and exist in different places?
;;   )
;; ;; So... git-for-windows is a thing that needs to find git... somehow... even
;; ;; though it's the thing that's supposed to set up/know that. Then ask
;; ;; git which git it is. Then look in itself in some gnarly table...
;; ;; Then it can set itself up. Then we can know stuff about diff, epa, etc when
;; ;; told they're from git-for-windows.



;;------------------------------------------------------------------------------
;; Idea 0 - be like use-package?
;;------------------------------------------------------------------------------

;; (defgroup use-tool nil
;;   "A use-tool declaration for simplifying your `.emacs'."
;;   :group 'startup)
;; 
;; (defconst use-tool-version "0.0"
;;   "This version of use-tool.")
;; 
;; (defcustom use-tool-keywords
;;   '(:disabled
;;     :requires
;;     ;; probably a lot more...
;;     ;;   operating system? (e.g. windows)
;;     ;;   platform, package, or something? (e.g. git for windows or cygwin or mingw)
;;     )
;;   "Valid keywords with a great docstr."
;;   :type '(repeat symbol)
;;   :group 'use-tool)
