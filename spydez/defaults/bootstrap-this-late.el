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
    ;; Defining these in a first-to-last way as I am (currently) appending
    ;; to the end of exec-path and env PATH.

    ;; magit
    ("git" . "C:/Program Files/Git/mingw64/bin")
    ;; Um... or one of these??
    ;;   C:/Program Files/Git/cmd
    ;;   C:/Program Files/Git/mingw64/bin
    ;;   C:/Program Files/Git/usr/bin
    ;;   C:/Users/cole/AppData/Local/GitHubDesktop/app-1.6.1/resources/app/git/mingw64/bin
    ;;   C:/Users/cole/AppData/Local/GitHubDesktop/app-1.6.1/resources/app/git/cmd

    ;; EPA
    ("gpg" . "C:/Program Files/Git/usr/bin")

    ;; shell
    ("bash" . "C:/Program Files/Git/usr/bin")

    ;; magit for external diff
    ("diff" . "C:/Program Files/Git/usr/bin")
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
;;   from:
;;     https://github.com/kaushalmodi/.emacs.d/blob/master/personal/setup-var-overrides-EXAMPLE.el


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'bootstrap-this-late)
