;; -*- lexical-binding: t -*-


;;----------------------------------------------------------------------------;;
;;                                 Bootstrap.                                 ;;
;;---To pull oneself up by the bootstraps, one must first find one's boots.---;;
;;                           ...Oh, here they are.


;; After agonizing about this for all of [2019-02-13], and various points
;; throughout from the start, I think the new early-init.el is the best place to
;; put enough of the bootstrap to find the local settings here.
;;
;; Also, it appears that some of the window settings improve startup by
;; significant fractions of a second. Other settings too.
;;
;; However, early-init is an Emacs 27 thing. So for Emacs 26.1 til then, we'll
;; just have to fudge things. For Emacs 27 and onward, the message above will
;; start being spit out, and eventually, decades from now, I'll notice and fix
;; things to how they should be.
;;
;; See: https://github.com/search?q=early-init.el&type=Code
(when (boundp 'early-init-file)
  (let ((warn-type '(spydez post))) ;; as in power-on-self-test, not the HTTP thing.
    (lwarn warn-type :warning "  %s:  Update early-init.el for actual active duty! Emacs %s uses early-init proper." warn-type emacs-version)))


;;-----------------------------------zeroth-----------------------------------;;
;;                              0th Order Stuff                               ;;
;;----------------------------------------------------------------------------;;
;;              For when the first thing is too soon of a thing.

;; TODO-after-reorg-done: put some of this stuff into the personal/init/zeroth
;; folder and load/call it here.

;;---
;; Load Path
;;---

;; Save off orginal. We're going to have a reduced set for bootstrapping, then reset and add the full monty.
(defconst spydez/dir/load-path/orig load-path)


;;------------------------------------------------------------------------------
;; ...But first...
;;------------------------------------------------------------------------------
;; Adjust garbage collection thresholds.

;; Testing startup aka (emacs-init-time)
;;   with defaults:            6.5 sec (~50 GCs)
;;   with 0.5 and 100 MB:   -> 5.8 sec (4 GCs)
;;   with 0.5 and 1000 MB:  -> 5.8 sec (4 GCs)
;;   with 0.75 and 1000 MB: -> 5.7 sec (4 GCs)
;;   with biggest thresh:   -> 5.8 sec (4 GCs)
;; (setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-percentage 0.75)
;; (setq gc-cons-threshold (* 1000 1000 1000))
;; (setq gc-cons-percentage 0.5)
;; (setq gc-cons-threshold (* 100 1000 1000)) ;; up to 100 MB from default (800k)

(defconst spydez/gc-cons-threshold/normal (* 20 1000 1000)
  "Upped to 20MB for GC threshold.")

;; In init, have big gc so it doesn't get hit much.
;; In normal running, have smaller gc so you don't get affected by lag too much
;; when it does go off on you.
;; Could consider messing with gc-cons-percentage as well.
;; (default thresh is 800k, percent is 0.1)
;; https://github.com/purcell/emacs.d/commit/6d9439b74153f91f614bba48b734a43e8cacf57e

(let ((spydez/gc-cons-threshold/init (* 256 1000 1000))
      ;; TODO: mess with gc-cons-percentage?
      )
  (setq gc-cons-threshold spydez/gc-cons-threshold/init)
  (add-hook 'after-init-hook
            (lambda ()  (setq gc-cons-threshold spydez/gc-cons-threshold/normal))))

;; Note that here:
;;   https://github.com/syl20bnr/spacemacs/issues/3011#issuecomment-140536210
;; an argument against messing with this is presented, but saving most of a
;; second and dozens of gc cycles is worthwile.

;; See also configure-emacs.el.


;;------------------------------------------------------------------------------
;; Necessary stuff.
;;------------------------------------------------------------------------------

;; Need to know something about my boots so I can pull the right straps.

;;---
;; Warnings
;;---
(require 'warnings)
;; `:warning' should pop up the *Warning* buffer
(setq warning-minimum-level :warning)
;; `:debug' should be logged, but not pop up the *Warning* buffer
(setq warning-minimum-log-level :debug)

;; shouldn't really need adjusting much. maybe to :debug
(setq spydez/warning/current-level ':warning)

;; First type in list: always 'spydez for my stuff
;;
;; Second type in list: current part of init sequence.
;;   - post (power-on-self-test)
;;   - early (early-init)
;;   - interstitial-prose (aka don't have code here plz part of init)
;;   - bootstrap (first bit of init)
;;   - config (setup packages, tools, keybinds, etc)
;;   - finalize (checks and cleanup)
;;   - running (totally done with init)
;;
;; Third and greater type: whatever sub-types you want.
(setq spydez/warning/current-type '(spydez early))

;; TODO: Move that thing about "failure is not an option in init (for my code)" to here?
;; NOTE: Keep the lwarn at the beginning of this file roughly like this.
(defun spydez/warning/message (type level message &rest args)
  "type: list with spydez first or nil e.g. nil will become '(spydez bootstrap)
level: level for lwarn e.g. nil will become :warning
https://www.gnu.org/software/emacs/manual/html_node/elisp/Warning-Basics.html#Warning-Basics"
  (let* ((type (or type spydez/warning/current-type))
         (level (or level spydez/warning/current-level))
         (injected-message (format "  %s:  %s" type message)))
    (apply 'lwarn type level injected-message args)
    ;; basically becomes e.g.:
    ;; (lwarn '(spydez bootstrap) :warning "  %s:  Update 'Master List' for this system (%s) here." '(spydez bootstrap) spydez/setup/system/hash)
    ))
;; (spydez/warning/message nil nil "Update foo %s %s" 'bar 'baz)

;; TODO: move this somewhere? debug-early or something?..
;; TODO: global enable/disable flag
;; TODO: lwarn w/ ":debug"?
(defun spydez/debug/message (type message &rest args)
  (let* ((type (or type '(spydez debug general)))
        (injected-message (format "  %s:  %s" type message)))
    (apply 'message injected-message args)))
;;(spydez/debug/message nil "hi %s %s" 'bar 'foo)


;;---
;; Strings
;;---
(defun spydez/list-join (join prefix &rest args)
  (let ((full-list (cons prefix args)))
    (mapconcat (function (lambda (x) (format "%s" x)))
               full-list
               join)))


;;---
;; Hashing
;;---
(defconst spydez/hash/default 'sha512
  "Default hashing function to use for spydez/hash-input.")
(defconst spydez/hash/slice 4
  "Default hashing slice size to use for spydez/hash-and-reduce.")
(defconst spydez/hash/join "-"
  "Default hashing slice size to use for spydez/hash-and-reduce.")
(defconst spydez/hash/prefix "hash"
  "Default hashing slice size to use for spydez/hash-and-reduce.")

;; Wanted to use dash for this but its way before packages have been bootstrapped.
;; Well, even more now that it's in early-init.el...
(defun spydez/hash-input (input &optional hash)
  "Returns a hash string of input. Hard-coded to sha512 atm."
  (let ((hash (or hash spydez/hash/default))) ;; set hash to default if unspecified
    (if (member hash (secure-hash-algorithms)) ;; make sure it exists as a supported algorithm
        (secure-hash hash input)
      (error "Unknown hash: %s" hash))
    ))

(defun spydez/hash-and-reduce (input &optional prefix hash slice join)
  (let* ((hash-full (spydez/hash-input input hash))
         ;; (hash-len (length hash-full)) ;; negative numbers work in substring, yay
         (slice (or slice spydez/hash/slice))
         (join (or join spydez/hash/join))
         (prefix (or prefix spydez/hash/prefix)))
    ;; (spydez/debug/message "hashed: %s %s %s %s" input prefix hash slice join)
    (spydez/list-join join
                      prefix
                      (substring hash-full 0 slice)
                      (substring hash-full (- slice) nil))
    ))


;;---
;; Domain & System Setup
;;---
(defconst spydez/file/bootstrap/local "bootstrap-this-early.el"
  "Definitions for how this computer is different from others, for getting bootstrap started.")
(defconst spydez/setup/domain/name "work"
  "A domain/folder for setups similar to this. E.g. work vs personal.")
(defconst spydez/setup/domain/subname "computer"
  "A sub-domain/folder for setups similar to this. E.g. work laptop vs work PC.")
(defconst spydez/setup/system/name (system-name)
  "(Plain String) Intended for this specific computer's setup folder.")
(defconst spydez/setup/system/hash (spydez/hash-and-reduce spydez/setup/system/name spydez/setup/domain/subname)
  "(Hashed) Intended for this specific computer's setup folder.")


;;---
;; Master List & Reason I'm doing system/hash.
;;---
;; C-u C-x C-e with point on end of next line to find out your hash:
;;   spydez/setup/system/hash
;;   e.g.: "computer-898a-27ab"
;; This is so bootstrap-this-early.el can live in its proper home, and be found with
;; an easy (load ...) or (require ...)

;; Find this system, setup domain names, reform reduced hash string.
(defvar spydez/bootstrap/system/known-p t)
(cond ;; switch case on system name hashes

  ;; next system here:
  ;; ((equal spydez/setup/system/hash "<something>")
  ;;  (setq spydez/setup/domain/name "home"
  ;;        spydez/setup/domain/subname "comp"
  ;;        spydez/setup/system/hash (spydez/hash-and-reduce spydez/setup/system/name spydez/setup/domain/subname)))

  ;; Home Desktop 2017-
 ((equal spydez/setup/system/hash "computer-2418-d188")
  (setq spydez/setup/domain/name "home"
        ;; I never know what to call my home stuff...
        ;;   - Name it after less popular OSes? Like Plan 9 from Bell Labs?
        ;;   - Name it an androgynous name? Like Sasha from this list?
        ;;     https://en.wikipedia.org/wiki/Unisex_name#English
        ;;   - Name it an element, like back when I was doing that? But I got
        ;;     stuck on Boron for a phone and then there were 3 borons and it
        ;;     got confusing...
        ;; Also, this is just the prefix for a hash (right now), so...
        spydez/setup/domain/subname "plan9"
        spydez/setup/system/hash (spydez/hash-and-reduce spydez/setup/system/name spydez/setup/domain/subname)))
 
  ;; PFO Desktop 2013-
  ((equal spydez/setup/system/hash "computer-898a-27ab")
   (setq spydez/setup/domain/name "work"
         spydez/setup/domain/subname "pfo"
         spydez/setup/system/hash (spydez/hash-and-reduce spydez/setup/system/name spydez/setup/domain/subname)))
 
 ;; fallthrough case - nothing specified so defaults will be used
 (t (spydez/warning/message nil nil "Update 'Master List' for this system (%s) here." spydez/setup/system/hash)
    (setq spydez/bootstrap/system/exists-p nil)))


;;---
;; Directories
;;---
(defun spydez/dir-name (name parent)
  "Expand name as child dir of parent in platform-agnostic manner."
  (file-name-as-directory (expand-file-name name parent)))

;; There are some existing packages for dealing with windows->unix or unix->windows paths...
;;   Windows emacs, Cygwin paths: https://www.emacswiki.org/emacs/cygwin-mount.el
;;   Cygwin/WSL emacs, win paths: https://github.com/victorhge/windows-path
;; but..: aren't on melpa, haven't been updated in years, etc.
(defun spydez/dir/windows-to-mingw (dir)
  "Bad hack. Absolute minimum to change from `c:/path/to/dir' to `/c/path/to/dir'"
  (concat "/" (replace-regexp-in-string ":" "" dir) "/"))

;; TODO: spydez/dir/setup-blah, spydez/dir/setup/blah, spydez/setup/dir/blah, spydez/dir/blah....?

(defconst spydez/dir/home (expand-file-name "~")
  "User's $HOME directory. In native format (unix vs windows paths).")

(defconst spydez/dir/emacs (expand-file-name user-emacs-directory)
  ;; user-init-file and user-emacs-directory can be helpful here
  "This should be a platform-agnostic way to find .emacs.d. Especially when I
can't decided on where, exactly, $HOME is for bash/emacs/etc on Windows.")

(defconst spydez/dir/emacs/personal (spydez/dir-name "personal" spydez/dir/emacs)
  "All of my own personal/custom setup code/vars/definitions...")

(defconst spydez/dir/personal/dev (spydez/dir-name "dev" spydez/dir/emacs/personal)
  "Device/system-specific init/config files and settings
(including their defaults) should reside under here.")

;; TODO-reorg-done
;; TODO: personal to "personal", or maybe a list of guesses at where the "defaults" would be...
;; Then the min necessary for loading/getting file from defaults that contains these
;; consts and funcs necessary for 1st step of bootstrap?
;; derived-TODO: move personal to "personal"? OR guess "personal", then "spydez"... etc?
;;   TODO-TOO: name the looking for stuff to start bootstrap: "finding shoe", cuz you need a shoe
;;     to pull yourself up by your bootstraps, right? RIGHT?!


(defconst spydez/dir/dev/defaults (spydez/dir-name "defaults" spydez/dir/personal/dev)
  "All of my optional/default setup elisp files...")

(defconst spydez/dir/dev/domain-all (spydez/dir-name "domains" spydez/dir/personal/dev)
  "Domains folder. For subdirs of work, home, etc.")
(defconst spydez/dir/dev/domain-this (spydez/dir-name spydez/setup/domain/name spydez/dir/dev/domain-all)
  "Anything that has to be domain specific. Tab widths or whatnot.")

(defconst spydez/dir/dev/system-all (spydez/dir-name "computers" spydez/dir/personal/dev)
  "Computers folder. For subdirs of different computers.")
(defconst spydez/dir/dev/system-this (spydez/dir-name spydez/setup/system/hash spydez/dir/dev/system-all)
  "Anything that has to be computer specific. Overriding tab widths or whatnot.")

(defconst spydez/dir/personal/lisp (spydez/dir-name "lisp" spydez/dir/emacs/personal)
  "Extra, non-init files for lisp code I've made or scrounged...")

(defconst spydez/dir/personal/init (spydez/dir-name "init" spydez/dir/emacs/personal)
  "Base dir for my personal init files.")
(defconst spydez/dir/init/boot (spydez/dir-name "boot" spydez/dir/personal/init)
  "Dir for my personal init files related to early-init, bootstrapping.")
(defconst spydez/dir/init/config (spydez/dir-name "config" spydez/dir/personal/init)
  "Dir for my personal init files related to normal init, configuration, use-package, etc.")
(defconst spydez/dir/init/finalize (spydez/dir-name "finalize" spydez/dir/personal/init)
  "Dir for my personal init files related to sanity, double checking, final steps.")


;;---
;; Load Path
;;---

;; Bare minimum in the Load-Path for finding bootstrapping files:
(add-to-list 'load-path spydez/dir/dev/defaults) ;; defaults first so everything else overrides.
;; Could add a domain level if needed?
(add-to-list 'load-path spydez/dir/dev/system-this) ;; most specific to this computer last


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(defconst spydez/bootstrap/complete 'early
  "values: nil, 'early, 'default, 'specific
compare: (eq spydez/bootstrap/complete 'early)")

;; (provide 'early-init)
;; early-init ends here
