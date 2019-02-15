;; -*- emacs-lisp -*-


;; This file can be used to override variables using `setq' forms.


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
;; Vars to override, other general stuff to provide so bootstrap can finish
;; successfully for this computer.


;;------------------------------------------------------------------------------
;; External Tools
;;------------------------------------------------------------------------------
;; Do I want to do the "find git" on windows like this or like in the Windows
;; PATH env var?

;; Associative List of tool symbol to... tool path. Empty path means just check
;; if external tool is known by emacs' current environment.
(defconst spydez/tools/external
  '(
    ;; configure-shell wants bash (git bash (same path as diff))
    ;; here and in Windows PATH atm... (debugging eshell)
    ("bash" . "C:/Users/cole/AppData/Local/GitHub/PortableGit_69bd5e6f85e4842f07db71c9618a621154c52254/usr/bin")

    ;; magit wants git and diff
    ;; TODO: find 2.19+ git that Git Bash & Git GUI are using - on 2.14 right now
    ("git" . "") ; in windows system env var PATH right now
    ("diff" . "C:/Users/cole/AppData/Local/GitHub/PortableGit_69bd5e6f85e4842f07db71c9618a621154c52254/usr/bin")
    )
  "An alist for tool name -> exec path. These will be front-to-back appended to list, so if e.g. there's several git binaries and only one will work, put git in front of this alist."
  ;; If I need more than a pair or triple tuple:
  ;;   Options for Structured Data in Emacs Lisp: https://nullprogram.com/blog/2018/02/14/
  )


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: allow disabling of packages by just setting a var here.
;;   "An example use is to disable loading of selected packages.
;;     Format to be used: (setq disable-pkg-PKGNAME t)"
;;   Note: all packages CANNOT be disabled in this manner.
;;   Note 2: No idea how that pkg disabling is done.
;; from:
;;   https://github.com/kaushalmodi/.emacs.d/blob/master/personal/setup-var-overrides-EXAMPLE.el


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-this-late)
