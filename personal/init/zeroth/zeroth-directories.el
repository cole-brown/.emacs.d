;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------


(defun spydez/path/to-file (parent &rest path)
  "Given a base dir, and a &rest of e.g. ('path/to' 'dir' 'with-file' 'file.txt),
will return full path in platform-agnostic manner. Does not 'fix' any `path'
components; they are expected to be valid."
  ;; fully qualify base as start of return value
  (let ((out-path (expand-file-name "" parent)))
    (dolist (component path out-path)
      ;; For each component of path supplied, concat it to the result.
      ;; `concat' is correct; see manual entry "Directory Names":
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html#Directory-Names
      (setq out-path (concat (file-name-as-directory out-path) ;; assume we had a dir all along?
                             component)) ;; and add the next component on
      )))
;; (spydez/path/to-file spydez/dir/home "personal" "something.exe" "zort.txt")


(defun spydez/path/to-dir (parent &rest path)
  "Given a base dir, and a &rest of e.g. ('path/to' 'dir' 'with-file' 'file.txt),
will return full path in platform-agnostic manner. Does not 'fix' any `path'
components; they are expected to be valid."
  ;; fully qualify base as start of return value
  (file-name-as-directory (apply #'spydez/path/to-file parent path)))
;; (spydez/path/to-dir spydez/dir/home "personal" "something" "zort")


(defun spydez/path/to-relative (&optional path root)
  "Given a possibly absolute PATH, try to trim out ROOT. If both
nil, returns file name."
  (interactive)
  (let ((path (or path (buffer-file-name)))
        (root (or root "")))
    (s-replace (file-name-as-directory  ;; make sure to have an ending slash
                (expand-file-name root)) ;; and expand it out fully
               "" ;; replace with nothing
               (expand-file-name path)))) ;; make sure we're all expanded here too.
;; (spydez/path/to-relative spydez/dir/dev/system-this spydez/dir/emacs/personal)
;; (spydez/path/to-relative)

;; There are some existing packages for dealing with windows->unix or unix->windows paths...
;;   Windows emacs, Cygwin paths: https://www.emacswiki.org/emacs/cygwin-mount.el
;;   Cygwin/WSL emacs, win paths: https://github.com/victorhge/windows-path
;; but..: aren't on melpa, haven't been updated in years, etc.
(defun spydez/path/windows-to-mingw (dir)
  "Bad hack. Absolute minimum to change from `c:/path/to/dir' to `/c/path/to/dir'"
  (concat "/" (replace-regexp-in-string ":" "" dir) "/"))


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;; TODO: spydez/dir/setup-blah, spydez/dir/setup/blah, spydez/setup/dir/blah, spydez/dir/blah....?

;; Directory names:
;;   In general:
;;     spydez/dir/...
;;     - In general, only have 1-2 dirs in name to keep length down.
;;   Examples:
;;     spydez/dir/path/...
;;     spydez/dir/path/to/...
;;     spydez/dir/to/folder/...

;;---
;; Base Dirs
;;---

(defconst spydez/dir/home (spydez/path/to-dir "~")
  "User's $HOME directory. In native format (unix vs windows paths).")

(defconst spydez/dir/emacs (spydez/path/to-dir user-emacs-directory)
  ;; user-init-file and user-emacs-directory can be helpful here
  "This should be a platform-agnostic way to find .emacs.d. Especially when I
can't decided on where, exactly, $HOME is for bash/emacs/etc on Windows.")


;;---
;; Personal Dirs
;;---

(defconst spydez/dir/emacs/personal
  (spydez/path/to-dir spydez/dir/emacs "personal")
  "All of my own personal/custom setup code/vars/definitions...")


;;---
;; Device/System Dirs
;;---

(defconst spydez/dir/personal/dev
  (spydez/path/to-dir spydez/dir/emacs/personal "dev")
  "Device/system-specific init/config files and settings
(including their defaults) should reside under here.")

(defconst spydez/dir/dev/defaults
  (spydez/path/to-dir spydez/dir/personal/dev "defaults")
  "All of my optional/default setup elisp files...")

(defconst spydez/dir/dev/domain-all
  (spydez/path/to-dir spydez/dir/personal/dev "domains")
  "Domains folder. For subdirs of work, home, etc.")
(defconst spydez/dir/dev/domain-this
  (spydez/path/to-dir spydez/dir/dev/domain-all spydez/setup/domain/name)
  "Anything that has to be domain specific. Tab widths or whatnot.")

(defconst spydez/dir/dev/system-all
  (spydez/path/to-dir spydez/dir/personal/dev "computers")
  "Computers folder. For subdirs of different computers.")
(defconst spydez/dir/dev/system-this
  (spydez/path/to-dir spydez/dir/dev/system-all spydez/setup/system/hash)
  "Anything that has to be computer specific. Overriding tab widths or whatnot.")


;;---
;; Init Dirs
;;---

(defconst spydez/dir/personal/init
  (spydez/path/to-dir spydez/dir/emacs/personal "init")
  "Base dir for my personal init files.")
(defconst spydez/dir/init/zeroth
  (spydez/path/to-dir spydez/dir/personal/init "zeroth")
  "Dir for my personal init files related to early-init, basic consts/funcs.")
(defconst spydez/dir/init/boot
  (spydez/path/to-dir spydez/dir/personal/init "boot")
  "Dir for my personal init files related to early-init, bootstrapping.")
(defconst spydez/dir/init/config
  (spydez/path/to-dir spydez/dir/personal/init "config")
  "Dir for my personal init files related to normal init,
  configuration, use-package, etc.")
(defconst spydez/dir/init/finalize
  (spydez/path/to-dir spydez/dir/personal/init "finalize")
  "Dir for my personal init files related to sanity, double
  checking, final steps.")


;;---
;; Other Dirs
;;---

(defconst spydez/dir/personal/lisp
  (spydez/path/to-dir spydez/dir/emacs/personal "lisp")
  "Extra, non-init files for lisp code I've made or scrounged...")

;; not used in code (right now? [2019-02-25]) but trying out using
;; a const in org notes...
(defconst spydez/dir/personal/docs
  (spydez/path/to-dir spydez/dir/emacs/personal "docs")
  "Here there be +monsters+ documentation. Or documents... or a few doctors...")
(defconst spydez/dir/docs/notes
  (spydez/path/to-dir spydez/dir/personal/docs "notes")
  "Here there be +monsters+ documentation.")
(defconst spydez/dir/docs/issues
  (spydez/path/to-dir spydez/dir/personal/docs "issues")
  "Here there be +monsters+ documentation about
  errors/bugs/issues/etc I've had.")


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-directories)
