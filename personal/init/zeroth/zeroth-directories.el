;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Path Functions
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
;; (spydez/path/to-file "~" "personal" "something.exe" "zort.txt")


(defun spydez/path/to-dir (parent &rest path)
  "Given a base dir, and a &rest of e.g. ('path/to' 'dir' 'with-file' 'file.txt),
will return full path in platform-agnostic manner. Does not 'fix' any `path'
components; they are expected to be valid."
  ;; fully qualify base as start of return value
  (file-name-as-directory (apply #'spydez/path/to-file parent path)))
;; (spydez/path/to-dir "~" "personal" "something" "zort")


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
;; (spydez/path/to-relative "/path/to/a/file/location.txt" "/path/to/a/")
;; (spydez/path/to-relative)


;; There are some existing packages for dealing with windows->unix or unix->windows paths...
;;   Windows emacs, Cygwin paths: https://www.emacswiki.org/emacs/cygwin-mount.el
;;   Cygwin/WSL emacs, win paths: https://github.com/victorhge/windows-path
;; but..: aren't on melpa, haven't been updated in years, etc.
(defun spydez/path/windows-to-mingw (dir)
  "Bad hack. Absolute minimum to change from `c:/path/to/dir' to `/c/path/to/dir'"
  (replace-regexp-in-string "//" "/"
                            (concat "/" (replace-regexp-in-string ":" "" dir) "/")))


;;------------------------------------------------------------------------------
;; Dirky: Dynamic Directories by Key(word)
;;------------------------------------------------------------------------------

(defun spydez/dirky/add (keybase dirkey value
                         &optional comment allow-keybase-creation)
  "Add another entry to `spydez/dirky/keys'.

KEYBASE should be a first-level keyword (e.g. `:default').

DIRKEY should be a second-level keyword (e.g. `:emacs.d').

PATH should be one of:
  - A fully-expanded directory path in OS-native format (e.g. c:/dir).
  - A list of dir names and dirkeys (e.g. '(:emacs.d \"personal\")).

COMMENT is an optional docstr.

If ALLOW-KEYBASE-CREATION is non-nil, KEYBASE will be inserted
into `spydez/dirky/keys' if it is not present. Otherwise an error
is raised.
"
  ;; Get the keybase list that we'll be inserting into.
  (let ((keybase (or keybase spydez/dirky/domain))
        (base (assoc :init spydez/dirky/keys))
        (element (list dirkey value comment)))
    ;; Add keybase to dirky if needed/allowed.
    (if (not base)
        (if allow-keybase-creation
            ;; Push new keybase list w/ its one new element. Then we're done.
            (push (list keybase element) spydez/dirky/keys)
          (error (concat "Cannot create new keybase '%s'. Enable creation via "
                         "ALLOW-KEYBASE-CREATION arg if desired, create an "
                         "empty entry in `spydez/dirky/keys', or figure out "
                         "what is wrong...")
                 keybase))

      ;; Have base; check if we already have dirkey too.
      (if (alist-get dirkey (cdr base))
          (error (concat "Cannot add '%s' to `spydez/dirky/keys' under '%s' "
                         "- it already exists: %s")
                 dirkey keybase (alist-get dirkey base))

        ;; Ok; just need to add element to it in place...
        (push element (alist-get keybase spydez/dirky/keys))))))
;; (defvar spydez/dirky/keys/test '())
;; (spydez/dirky/add :test :jeff "jeffe" "I am a comment.")
;; (spydez/dirky/add :test :jeff "jeffe" "I am a comment." t)
;; (spydez/dirky/add :test :jeff "jeffe")
;; (spydez/dirky/add :test :jill "jill")
;; spydez/dirky/keys/test


(defun spydez/dirky/get (keybase dirkey &optional fallback)
  "Get a path value from `spydez/dirky/keys' and return it.

KEYBASE should be a first-level keyword (e.g. `:default').

DIRKEY should be a second-level keyword (e.g. `:emacs.d').

Return FALLBACK (default nil) if nothing found.
"
  (let ((keybase (or keybase spydez/dirky/domain)))
    ;; Ultimately, return a value or fallback?
    (or
     ;; Penultimately, get just the dir-related element.
     (nth 0
          ;; Second, get dirkey from keybase entry.
          (alist-get dirkey
                     ;; First, get keybase entry.
                     (alist-get keybase
                                spydez/dirky/keys)))
     (cond
      ;; If `t' is the fallback, then fallback to the... default.
      ((or (eq fallback t)
           (eq fallback :default))
       (spydez/dirky/get :default dirkey))

      ;; If the fallback is a keyword, then assume it's a keybase.
      ((keywordp fallback)
       (spydez/dirky/get fallback dirkey))

      ;; If provided fallback function, call it with our args.
      ((functionp fallback)
       (funcall fallback keybase dirkey))

      ;; If provided a value, return it.
      ((not (null fallback))
       fallback)

      ;; default: No fallback; raise error.
      (t
       (error (concat "No value found for '%s' under '%s' and no "
                      "fallback value to use.")
              dirkey keybase))))))
;; (spydez/dirky/get :default :emacs.d)
;; (spydez/dirky/get :home :emacs.d)
;; (spydez/dirky/get :home :emacs.d t)
;; (spydez/dirky/get :home :emacs.d :default)
;; (spydez/dirky/get :home :emacs.d :jeff)
;; (spydez/dirky/get :work :logbook)
;; (spydez/dirky/get :work :roam)
;; (spydez/dirky/get :work :roam t)
;; (spydez/dirky/get nil :roam)
;; (spydez/dirky/get :load-path :domain)


(defun spydez/dirky/resolve (keybase &rest elements)
  "Resolves all ELEMENTS - strings and dirky keywords in
`spydez/dirky/keys' expected.

KEYBASE is used for all elements that are not strings (uses
`spydez/dirky/get' to try to resolve those).
"
  (let ((keybase (or keybase spydez/dirky/domain)))
    (when (and keybase elements)
      (let ((builder '()))
        ;; Build our fragments, sans nils.
        (dolist (element elements builder)
          (cond
           ((stringp element)
            ;; Just push it if it's a str.
            (push element builder))

           ((and (not (null element))
                 (symbolp element)
                 (stringp (symbol-value element)))
            ;; Push its value if it's a symbol holding a str.
            (push (symbol-value element) builder))

           ((not (null element))
            ;; Recurse with the element if it's not a str.
            (let ((sub-elements (spydez/dirky/get keybase element t)))
              ;; Could get back a list, could get back just a thing...
              ;; Turn it into a list if it's not.
              (when (stringp sub-elements)
                  (setq sub-elements (list sub-elements)))
              (dolist (sub-elt (apply #'spydez/dirky/resolve
                                      keybase
                                      sub-elements))
              (when sub-elt
                (push sub-elt builder)))))

           (t
            ;; Do nothing with remainders (only nils probably).
            nil)))

        ;; Make our return list values the right way round.
        (nreverse builder)))))
;; (spydez/dirky/resolve :default "/hi")
;; (spydez/dirky/resolve :default :emacs.d)
;; (spydez/dirky/resolve :home :emacs.d)
;; (spydez/dirky/resolve :work :logbook)
;; (spydez/dirky/resolve nil :logbook)
;; (spydez/dirky/resolve :load-path :domain)


(defun spydez/dirky/path (keybase &rest elements)
  "Build a path based on ELEMENTS, which should either be paths, path fragments,
dir names, or keywords in `spydez/dirky/keys'.

KEYBASE is used for all elements that are not strings (uses
`spydez/dirky/get' to try to resolve those).
"
  (if-let* ((keybase (or keybase spydez/dirky/domain))
            (elements elements)
            (resolved (apply #'spydez/dirky/resolve
                             keybase elements)))
    ;; Resolve elements and turn into a path.
    (apply #'spydez/path/to-file
           resolved)))
;; (spydez/dirky/path :default "/hi")
;; (spydez/dirky/path :default :emacs.d)
;; (spydez/dirky/path :home :emacs.d)
;; (spydez/dirky/path :work :logbook)
;; (spydez/dirky/path nil :logbook)


(defun spydez/dirky/domain/key-to-str (keybase)
  "Returns domain string paired with domain key in `spydez/dev/domains', or nil
if none found.
"
  (nth 0 (alist-get keybase spydez/dev/domains)))
;; (spydez/dirky/domain/key-to-str :home)
;; (spydez/dirky/domain/key-to-str :jeff)


;;------------------------------------------------------------------------------
;; Constants / Vars
;;------------------------------------------------------------------------------

(defvar spydez/dev/domains
  '((:home "home")
    (:work "work"))
  "All known domains in the known universe.
")


(defvar spydez/dirky/domain :default
  "A 'default' domain to use if the KEYBASE arg is nil.
You probably want: `:home' or `:work'
You'll probably set it in 'master-list.el'.
")
;; Any keyword is safe for local-variable-ing this... Though ':work or :home'
;; would be the (current) safest check.
(put 'spydez/dirky/domain 'safe-local-variable #'keywordp)


(defvar spydez/dirky/keys
  (list
   ;;------------------------------
   ;; If no domain-specific override, place in default.
   ;;------------------------------
   (list :default

         ;;---
         ;; Base Directories
         ;;---
         (list :home
               (spydez/path/to-dir "~")
               (concat "User's $HOME directory. In native format "
                       "(unix vs windows paths)."))

         (list :emacs.d
               (spydez/path/to-dir user-emacs-directory)
               ;; user-init-file and user-emacs-directory can be helpful here
               (concat "This should be a platform-agnostic way to find "
                       ".emacs.d. Especially when I can't decided on where, "
                       "exactly, $HOME is for bash/emacs/etc on Windows."))

         (list :personal
               '(:emacs.d "personal")
               (concat "Device/system-specific init/config files and settings "
                       "(including their defaults) should reside under here."))

         ;;---
         ;; Not base, but... They are most often here so default.
         ;;---
         (list :roam
               '(:home ".lily.d")
               "Org-Roam's base directory."))


   ;;------------------------------
   ;; Stuff for Initialization
   ;;------------------------------
   (list :init
         )

   ;;------------------------------
   ;; Stuff for Initialization Too:
   ;;   - These will all go into the load path.
   ;;   - A few more will too...
   ;;------------------------------
   (list :load-path

         (list :zeroth
               '(:personal "init" "zeroth")
               (concat "Dir for my personal init files related to early-init, "
                       "basic consts/funcs."))

         (list :boot
               '(:personal "init" "boot")
               (concat "Dir for my personal init files related to "
                       "early-init, bootstrapping."))

         (list :config
               '(:personal "init" "config")
               (concat "Dir for my personal init files related to normal init, "
                       "configuration, use-package, etc."))

         (list :finalize
               '(:personal "init" "finalize")
               (concat "Dir for my personal init files related to sanity, "
                       "double checking, final steps."))

         (list :lisp
               '(:personal "lisp")
               "Extra, non-init files for lisp code I've made or scrounged..."
               )

         (list :devices
               '(:personal "dev")
               (concat "Device/system-specific init/config files and settings "
                       "(including their defaults) should reside under here."))

         (list :dev/defaults
               '(:devices "defaults")
               "All of my optional/default setup elisp files...")

         (list :dev/domains
               '(:devices "domains")
               "Anything for multiple domains.")

         (list :dev/domain
               '(:dev/domains spydez/dev/domain/name)
               (concat "Anything that has to be domain specific. "
                       "Tab widths or whatnot."))

         (list :dev/system
               '(:devices "computers" spydez/dev/system/hash)
               (concat "Anything that has to be computer specific. "
                       "Overriding tab widths or whatnot.")))



   ;;------------------------------
   ;; Stuff for Wherever 'Here' is.
   ;; Probably in .emacs.d...
   ;;------------------------------
   (list :emacs
         ;;---
         ;; Inside .emacs.d
         ;;---
         (list :docs
               '(:emacs.d "personal" "docs")
               (concat "Here there be +monsters+ documentation. "
                       "Or documents... or a few doctors..."))

         (list :docs/notes
               '(:emacs.d/docs "notes")
               "Here there be +monsters+ documentation.")

         (list :docs/issues
               '(:emacs.d/docs "issues")
               (concat "Here there be +monsters+ documentation about "
                       "errors/bugs/issues/etc I've had.")))


   ;;------------------------------
   ;; 'Home' Stuff.
   ;;------------------------------
   (list :home

         (list :logbook
               '(:roam "logbook" "home")
               "Org-Journal folder for home domain.")

         ;; todo: taskspace
         )


   ;;------------------------------
   ;; 'Work' Stuff.
   ;;------------------------------
   (list :work

         (list :logbook
               '(:roam "logbook" "work")
               "Org-Journal folder for work domain.")

         ;; todo: taskspace
         )


   ;;------------------------------
   ;; ...(No Comment.)
   ;;------------------------------
   (list :secrets
         (list :secrets.d
               '(:home ".secrets.d")
               "Location of secrets dir on this computer."))

   ;;------------------------------
   ;; Other Stuff?
   ;;------------------------------
   )

  "'Base' directories or path fragments that we can riff off of
for making most of the paths in the .emacs.d init code.

Layout:
  1) defaults or domain specific breakout
     2) info for finding a directory for a keyword

The list overall is an alist of alists.

Each alist entry is a list (not a cons). There are three cells like so:
'(:keyword str-or-list docstr)

`str-or-list' can be a function that resolves to a string when this gets
defined on start-up, but aren't (currently) expected to be resolved at
run-time. Directories don't move that often?

If it is a list, the list will be used to possibly recurse. If there's a
keyword in it that keyword is expected to be in here.
")


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-directories)
