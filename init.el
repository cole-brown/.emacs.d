;;; init.el --- init the emacs -*- mode: emacs-lisp; lexical-binding: t -*-
;;; Commentary:

;; TODO: A better commentary.

;;; Code:

(setq spydez/warning/current-type '(spydez interstitial-prose))
;;(spydez/info/message-if spydez/warning/current-type "init.el... Intermission.")

;; TODO: a pretty centered header or something here for to be pretty

;; TODO: use-tool package, like use-package, but for external tools like git, gpg
;; say what versions you want, what os you expect, have ways for packages
;; to hook in so like gpg can be connected to EPA even if half windows, half MinGW environment.

;;------------------------------------------------------------------------------
;; Notes, TODOs, Links
;;------------------------------------------------------------------------------
;; TODO: rename all these "TODOs" in my section headers so I can search for
;; 'TODO' without dozens of false alarms...

;;---
;; Search...                                             (knights who say...)
;;---
;; find . -path "./elpa" -prune -o -iname "*.el" -print0 | xargs -0 grep -ni "provide"
;; find . -path "./elpa" -prune -o -iname "*.el" -print0 | xargs -0 grep -ni "

;; (w/ no-littering):
;; just org:
;; find . -path "./elpa" -prune -o -path "./etc" -prune -o -path "./var" -prune -o -iname "*.el" -print0 | xargs -0 grep -ni ""
;;
;; org and el:
;; find . -path "./elpa" -prune -o -path "./etc" -prune -o -path "./var" -prune -o -iname "*.el" -o -iname "*.org" -print0 | xargs -0 grep -ni ""

;; Those are getting complicted and not working right...
;; find . -iname "*.el" -o -iname "*.org" | grep -v "/var/" | grep -v "/pre-no-littering/" | grep -v "/manual-package-archive/" | grep -v "/elpa/" | xargs grep "yegge"

;; TODO: a search in my .emacs.d project that is, like... emacs.
;; Seems silly to be upgrading emacs so much and still use bash for that...


;;---
;; Misc
;;---

;; TODO: can 'literate programming' do multiple files?
;; Think nhoffman has a good setup and some helpful commands for it.
;;   http://nhoffman.github.io/.emacs.d/#org858a11b
;; http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html
;; https://en.wikipedia.org/wiki/Literate_programming
;; Can also check out this for general layout:
;;   https://www.emacswiki.org/emacs/DotEmacsStructuring


;;---
;; Help I Often Forget.
;;---
;; It doesn't help when you just totally space on something though.

;; which key is what function
;; command -> keybind: C-h w <cmd>
;; keybind -> command: C-h k <keys>
;; C-h f <cmd> -> help for function name
;; C-h a -> helm-apropos: Helm-driven function search, kinda


;;---
;; References.
;;---

;; Special extra useful links:
;;   1) Sacha Chua's init.org: http://pages.sachachua.com/.emacs.d/Sacha.html
;;      - Firmly in number 1. Followed her init pretty exclusively until done
;;        with it, then branched off to other's stuff (like my own old .emacs).
;;   2) Noah Hoffman's init.org: https://github.com/nhoffman/.emacs.d/blob/master/init.org
;;      - Well set up init.org. Second in line (well, third after my old .emacs).
;;   3) Grant Rettke's (C3F) init.org: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
;;      - Interesting layout for literate programming. Sections come up in
;;        whatever order and then when tangled, get put into a specific order.

;; Todo: Do we want ./spydez/references/ ignored or saved in git?
;;   - Don't really want to save if public in repo...
;;
;; TODO: Start: zzamboni init.el: https://github.com/zzamboni/dot-emacs/blob/master/init.org
;; Many neat things?: https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
;;   todo: see if I want more of them.
;;   todo: is this the same as C3F below? Update all old links to this if so... Also save a new one to references.
;; TODO: https://www.gitignore.io/api/emacs
;; And of course my old setup: https://github.com/spydez/emacs
;; TODO: check this? https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;;    from: https://www.reddit.com/r/emacs/comments/2wzhxh/how_to_organize_initel/covmnl5
;; general layout ideas:
;;   "How to organize init.el?" https://www.reddit.com/r/emacs/comments/2wzhxh/how_to_organize_initel/
;;
;; Left off at "Projectile", but have to use Google Cache:
;; http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
;; https://webcache.googleusercontent.com/search?q=cache:pccrs3LhmCoJ:https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html+&cd=1&hl=en&ct=clnk&gl=us&client=firefox-b-1
;;
;; http://ergoemacs.org

;; Maybe look at all the settings in here:
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el


;;---
;; TODOs and Misc.
;;---

;; TODO: icons maybe? Do I need icons?
;;   https://github.com/domtronn/all-the-icons.el
;; TODO: other stuff from here?: https://huytd.github.io/emacs/emacs-from-scratch.html

;; TODO: change sanity checks and others over from (and (boundp var) var) to (bound-and-true-p var)

;; TODO: go through saved-off files of old.emacs in spydez/references/my-old-emacs

;; TODO: see if any settings from custom.el.my-old.todo file should go in this new one.
;;       org-mode, transient-mark-mode(?), paren-* settings, paren-face-* settings (match, mismatch), ...

;; TODO: read this maybe https://sanctum.geek.nz/arabesque/series/unix-as-ide/

;; TODO: change functions I've acquired (e.g. "xah-") over to "spydez/" prefix so there's one search for them.
;;   Note the old name in comment or docstring.

;; TODO: resume (CV, not un-pause) in emacs/org-mode/latex?
;; https://www.reddit.com/r/emacs/comments/6pp9z3/noob_using_emacs_to_write_a_resume/
;; https://github.com/xiaohanyu/resume
;; https://www.google.com/search?hl=en&output=search&sclient=psy-ab&q=org%20mode%20resume&=&=&oq=&gs_l=&pbx=1

;; TODO: bind-key vs global-set-key vs define-key vs the 50 other ways... which one should we be using where?
;; https://www.google.com/search?hl=en&output=search&sclient=psy-ab&q=bind-key%20vs%20global-set-key&=&=&oq=&gs_l=&pbx=1
;;  - https://www.reddit.com/r/emacs/comments/6hbb22/when_do_you_use_definekey_vs_globalsetkey/

;; TODO: my current configure-<functionality> versus what most others seem to do: configure-<package>

;; TODO: defer load absolutely nothing? Seems to make for just random pauses for a while when you do something
;; after loading emacs. An extra second or two at start might be less annoying?
;; A global setting would be cool... (if/when (spydez/defer-load-p) ...)

;; TODO: check if missed any of old .emacs files

;; TODO: reduce this down to as few lines as possible? 
;;  e.g. http://ergoemacs.org/emacs/organize_your_dot_emacs.html
;;  but I'd still need all the stuff setting up the paths, probably?

;; TODO: For when compiling... https://github.com/jwiegley/use-package#byte-compiling-your-emacs
;;
;; Another feature of use-package is that it always loads every file that it can
;; when .emacs is being byte-compiled. This helps to silence spurious warnings
;; about unknown variables and functions.
;;
;; However, there are times when this is just not enough. For those times, use
;; the :defines and :functions keywords to introduce dummy variable and function
;; declarations solely for the sake of the byte-compiler.
;;
;; So... TODO: figure out recompiling everything and define some
;; dummy vars/funcs as needed (if they don't exist).

;; TODO: tags files? http://pages.sachachua.com/.emacs.d/Sacha.html#orgfb77d93

;; TODO: (require 'nothing) or something for useful comments not attached to init code or even emacs?
;;  like this for speeding up builds on windows?
;;    - https://github.com/Microsoft/WSL/issues/1932#issuecomment-294362848
;;      - https://support.microsoft.com/en-us/help/4028485/windows-10-add-an-exclusion-to-windows-security

;; TODO: does a gen'd http://emacs-bootstrap.com/ have anything neat in it?
;;   downloaded and unzipped to C:\home\cole\zzz_new-emacs\emacs.d

;; TODO: any way to have emacs run a command (async) to get output from a remote server?
;; Right now: emacs and ssh don't talk to each other well.


;;---
;; Trials and Try-outs.
;;---
;; Packages and experiments should be marked with something like... 
;; Trial: [yyyy-mm-dd]
;;
;; TODO: change to some sort of elisp documentation or annotation?
;; TODO: maybe with some help text about how to use the feature on trial?
;; TODO: Then we can, somewhere in finalize, choose one of those helps to add into scratch buffer message?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgfb9b12e
;; Add advice to funcs? https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html

;; Packages to consider trying out:
;;   - Treemacs: tree layout file explorer
;;     https://github.com/Alexander-Miller/treemacs

;;---
;; License, Licensing, and Limericks
;;---

;; MIT Licensed.
;; See LICENSE file in top level of this git repo for details.

;; Note: explains MIT vs BSD vs Apache 2.0: https://snyk.io/blog/mit-apache-bsd-fairest-of-them-all/


;;------------------------------------------------------------------------------
;; About Layout.
;;------------------------------------------------------------------------------

;; early-init.el:
;;   TODO: update this now that I'm using it...
;;   Not used right now.
;;   Can do some even earlier stuff with early-init.el if needed...
;;     https://www.reddit.com/r/emacs/comments/7yns85/emacs_adds_support_for_a_second_read_earlier_init/
;;     https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b


;; init.el:
;;   First, emacs loads init.el.
;;   Our init.el will then load our files roughly in this order:
;;     1) bootstrap-*.el
;;     2) init-*.el
;;     3) configure-*.el
;; TODO: I moved from setup-*.el to configure-*.el because it seemed more apropros.
;;   However now my ordering is not also alphabetical...
;; TODO: bootstrap-*, configure-*, finalize-* maybe?


;; I don't want a megalithic emacs init file. So how best to break it up?
;;
;; Trying require/provide now.
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Named-Features.html
;;   https://emacs.stackexchange.com/questions/3310/difference-between-load-file-and-load
;;
;; Note: 'require' works off of file name (just like load) if not already
;; loaded. 'provide' can provide anything.  But if we want 'require' to find the
;; right thing, we'll have to have require/provide/filename on the same page.
;; Alternative is, like, (require 'spydez/bootstrap/debug/early "bootstrap-debug-early")?
;; Don't think that looks great.

;; TODO: if we try to get this optimized, the endgame is (probably?):
;;  1) barest basic setup
;;     - make sure to call something for overrides
;;  2) (bootstrap ...) or something
;;     1) overrides here?
;;     2) all bootstrapping here
;;  3) (configure ...) or soemthing
;;     1) calls out to various configures for packages and settings here
;;  4) (finalize ...) or something
;;     1) calls out to finalize steps here (sanity checks, etc)
;;  So this file becomes very small (code-wise) with some decent text filler explaining things.
;;    - e.g.: old .emacs's .emacs file (aka <computer-name>.emacs)
;;            spydez/references/bootstrap.el
;;    - e.g.: https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;;            (kinda - bit of a hybrid (steps 2-4 in init.el instead of sub-files))
;;  Alternatively, megalithic:
;;    - e.g.: Sacha.org's file


;;------------------------------------------------------------------------------
;; Concerning Consts, Vars, and Funcs.
;;------------------------------------------------------------------------------
;; TODO: figure out naming scheme and put info here maybe
;; TODO: do I want to follow:
;;   - Emacs/Elisp conventions (e.g. spydez--)?
;;   - common lisp (*var-name*, +const-name+)?
;;     - https://en.wikipedia.org/wiki/Naming_convention_(programming)#Lisp
;;
;; Currently kinda:
;; prefix/namespace: spydez/
;; domain/concern/concept separator: /
;;   e.g.: spydez/dir/, spydez/hash/, spydez/setup/
;; word joiner: -
;;   e.g.: spydez/hash-and-reduce, spydez/backup-files
;; TODO: Do I want anything for func vs var in name?

;; Evolving vars seem to be emerging into these groups:
;;   - early-init: needed by all, used to figure out my system and specifics
;;   - bootstrap-this-early:
;;     - needed early (before now, after early-init) to bootstrap this system
;;     - or only applicable/used by this system (e.g. "work.org" isn't a file at home)
;;   - this section:
;;     - defaults - can be overridden by bootstrap-this-late at the end off bootstrapping

;; defconst vs defvar vs setq
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html
;; Looks like I can use defconst for desired functionality/hinting, while also being
;; allowed to ignore the const-ness and override via a setq in a later init sub-file.


;;---
;; Cross Platform
;;---

;; I don't have any more than just some Windowses (7, 10) right now so a robust
;; cross-platform init with proper places for overriding things will probably
;; have to wait until I encounter it.
;; I think this is it (probably this is overkill), but I can't properly test.

;; cross-platform dir and file names:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html#Directory-Names


;;----------------------------------------------------------------------------;;
;;                                 Bootstrap.                                 ;;
;;---To pull oneself up by the bootstraps, one must first find one's boots.---;;
;;                           ...Where are my boots?                           ;;

;;---
;; We're faking early-init's earlyness, so can't do these here right now.
;; See below for place for now.
;; (setq spydez/warning/current-type '(spydez bootstrap))
;; (spydez/info/message-if spydez/warning/current-type "init.el... Bootstrapping.")
;;---

;;---
;; I have only tried this init on:
;;   (emacs-version)
;;   "GNU Emacs 26.1 (build 1, x86_64-w64-mingw32)\n of 2018-05-30"
;; So say we're restricted to emacs 26+?
(unless (>= emacs-major-version 26)
  (error "Emacs version 26 or higher is required"))
;;---


;;---
;; We need to setup some vars, load paths, etc before getting going. But those
;; might should be different for different domains (work vs home), subdomains
;; (company a vs consulting), systems (old pc vs new, pc vs tablet/laptop), etc.
;;
;; So we do part of that in early-init, and another part in bootstrap-this-early.
;; After which we can finish bootstrapping and get ready for the main emacs init.
;;---


;;-----
;; Bootloader loading...
;;---
;; I'm from the Future.
;; Compatibility with versions 26 and below.
(unless (boundp 'early-init-file)
  (load (expand-file-name "early-init" user-emacs-directory)))
;; Remove the load when early-init is real.
;; Also move this bootstrapping info message up when early-init is real.
(setq spydez/warning/current-type '(spydez bootstrap))
(spydez/info/message-if spydez/warning/current-type "init.el... Bootstrapping.")
;;-----


;;---
;; Little bit of Sanity...
;;---
;; Not too much.
(unless (and
         (boundp 'spydez/bootstrap/complete)
         (eq spydez/bootstrap/complete 'early))
  (error "Bootstrap: Early Bootstrap sanity check failed."))
;; TODO: copy important errors into scratch buffer or something?
;; Probably or something.
;; And then make sure it gets focus in final finalize.


;;---
;; Bootloader loading...
;;---
(require 'bootstrap-this-early)
;; The default bootstrap-this-early provider will check...
;; if system/known-p and no this-comp dir:
;;   - make this-comp dir
;;   - copy a default bootstrap-this-early there


;;---
;; Little bit of Sanity...
;;---
;; Not too much.
;; TODO: also have bootstrap-this-early set it.
(cond ((eq spydez/bootstrap/complete 'specific) t) ;; good to go
      ;; using default - should probably warn
      ((eq spydez/bootstrap/complete 'default)
       (spydez/warning/message nil nil "Specific bootstrap does not exist for this computer: %s %s"
                               spydez/bootstrap/complete spydez/setup/system/hash))
      ;; fallthrough cases - nothing used
      (t (error (spydez/warning/message nil nil "Bootstrap: No bootstrap for this computer?: %s %s"
                                        spydez/bootstrap/complete spydez/setup/system/hash))))

;;---
;; Little bit of Sanity...
;;---
;; TODO: move other early sanity checks into here?
;;   Then move this up earlier and move those checks into functions?
;;   Then do the sanity check functions at the proper points?
;;   Then maybe a new file name, maybe not.
;; TODO: sanity check: make sure every var in a list is boundp so my vars/consts can't just wander off on me.
;; TODO: sanity check: make sure every func in a list is fboundp so my funcs can't just wander off on me.
(require 'bootstrap-sanity-early)


;;------------------------------------------------------------------------------
;; Bootstrap Consts, Vars, and Funcs.
;;------------------------------------------------------------------------------

;; TODO: shift even more out into bootstrap-* files/steps?

;;---
;; Setup some very basics, so we can get moving...
;;---
;; Here, early-init, and bootstrap-this-early are the defaults.
;; To adjust for this system:
;;   Define differently in early-init;
;;   Define differently in bootstrap-this-early;
;;   Or override with setq later in bootstrap-this-late.

;; TODO: setq vs customize-set-variable (...vs being in the custom file?)


;;---
;; Directories for Emacs or Packages
;;---

(defun spydez/dir/self-policing-p ()
  "Keep .emacs.d tidy (ish)!
Trying to let no-littering take care of most/all this.
For the transition, maybe a func for checking..."
  (null (featurep 'no-littering)) ;; self-police if we don't have no-littering.
  ;; TODO: also t if in spydez/dir/personal?
  )

;; folders for auto-save files and backup-files (#*# and *~)
;; TODO: remove if I like no-littering?
(defconst spydez/dir/backup-files
  (spydez/dir-name "backups" spydez/dir/emacs)
  "Full path before no-littering package.")

(defconst spydez/dir/auto-save-files
  (spydez/dir-name "auto-save-list" spydez/dir/emacs))

(defconst spydez/file/save-history
  (expand-file-name "savehist" spydez/dir/emacs)
  "History of commands, etc.")

(defconst spydez/dir/yasnippets
  (expand-file-name "snippets" spydez/dir/emacs/personal)
  "My Yasnippets directory.")
;; Could add an override of my own snippets if needed.

(defconst spydez/dir/personal/use-tool
  (expand-file-name "use-tool" spydez/dir/emacs/personal)
  "use-tool directory.")


;;---
;; Identity / Personal Information
;;---
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/User-Identification.html
;; TODO: change these in a projectile hook or something for repo-dependant git identity?
;; TODO: but only change after secrets are loaded so as not to leak irrelevant work email addrs
(setq user-full-name "Cole Brown"
      user-mail-address "git@spydez.com")
;; user-login-name exists if needed

;; TODO: some consts for significant files or folders to jump to for e.g. opening a work project file.


;;---
;; Misc
;;---
(defconst spydez/undo-limit 160000
  "Upscale the default soft undo-limit. 80kB isn't a lot, so double it?")
(defconst spydez/undo-strong-limit (* 2 spydez/undo-limit)
  "Upscale default hard undo-limit as well. 120kB -> double soft limit.")


;;---
;; Load Path
;;---

;; Reset to orginal first. We had some subset in for bootstrapping. Now we're ready for the full set.
;; TODO: sanity check? boundp and set to anything...
(setq load-path spydez/dir/load-path/orig)

;; TODO-reorg-done: updated this comment?
;; Load-Path dirs, and places for overrides to exist (in ascending order):
;;   ./personal/dev/defaults/
;;   ./personal/
;;   ./personal/dev/domains/[work, home, whatever]
;;   ./personal/dev/computers/[pfo-dead-beef, home-1234-abcd, whatever]
;; (Assuming default dir names for personal, etc.)

;; This is setting priorities for overrides towards the front/head/car of
;; load-path (add-to-list does this for us).
;;
;; Don't use .emacs.d. 
;; https://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path
;; (add-to-list 'load-path spydez/dir/emacs)
(add-to-list 'load-path spydez/dir/personal/use-tool)
(add-to-list 'load-path spydez/dir/personal/lisp) ;; non-init; don't care about and should be overridable.
(add-to-list 'load-path spydez/dir/dev/defaults) ;; defaults first so everything else overrides.
(add-to-list 'load-path spydez/dir/emacs/personal)
(add-to-list 'load-path spydez/dir/personal/init)
(add-to-list 'load-path spydez/dir/init/boot)
(add-to-list 'load-path spydez/dir/init/config)
(add-to-list 'load-path spydez/dir/init/finalize)
(add-to-list 'load-path spydez/dir/dev/domain-all)
(add-to-list 'load-path spydez/dir/dev/system-all)
(add-to-list 'load-path spydez/dir/dev/domain-this)
(add-to-list 'load-path spydez/dir/dev/system-this) ;; most specific to this computer last
;; TODO-reorg-done: more in the load path? new dirs (dev, init...)?


;;---
;; Debug
;;---
;; All the way down here because I want my load paths, but we could put at the top if needed with a little adjustment.
(require 'bootstrap-debug-early)


;;---
;; Custom file
;;---
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

;; no-littering note:
;; They have a suggestion for storing in a no-littering dir, but I want to keep
;; custom.el notably under my control so it goes into my personal always.
;;
;; Some packages want to write to our custom file, so set that up first.
;; An unadorned filename (just "custom.el") wasn't getting picked up as the custom file, so expanded:
(setq custom-file (expand-file-name "custom.el" spydez/dir/emacs/personal))
;; May need a better setter if custom-file needs adjusted per computer...
;;
;; Possibly move custom-file setting up, and loading down below loading of bootstrap-this-late overrides.
(load custom-file t)


;;---
;; Misc Stuff
;;---
;; TODO: move this to configure-crypt? And/or rejigger configure-crypt so some can happen soon enough for this to be happy actually loading a file.
;; Load sensitive information from outside of .emacs.d
(if (bound-and-true-p spydez/dir/common-doc-save)
    (when (not (load (expand-file-name ".emacs.secrets" spydez/dir/common-doc-save) 'noerror))
      (spydez/warning/message nil :debug "No secrets to load."))
  (spydez/warning/message nil nil "No secrets loaded. Do not know where to look. '%s' undefined." 'spydez/dir/common-doc-save))


;;---
;; Dev Env vars
;;---
;; TODO: put in bootstrap-consts? bootstrap-this-late? bootstrap-data?

;; Tab widths. I like a smaller one for verbose/highly indented code (web dev mainly).
;; Normally use a larger one for non-web programming.
(defconst spydez/dev-env/tab/min 2
  "Small tab width for more compact but readable code.")
(defconst spydez/dev-env/tab/normal 4
  "Normal tab width for more usual use cases for code languages like C++, C#...")
(defconst spydez/dev-env/fill-column/normal 80
  "Normal tab width for more usual use cases for code languages like C++, C#...")
(defconst spydez/dev-env/fill-column/long 100 ;; 120
  ;; 120 would be nice, but 2 equal panes on 1080p monitor is more like 100 (~109 actual, 100 to be safer)
  "Normal tab width for more usual use cases for code languages like C++, C#...")

;; TODOS:
;;  bootstrap-this-early, bootstrap-this-late?
;;  copy external tools to bootstrap-this-late for this pc
;;    - make default/empty here
;;    - do tool exec/env step after bootstrap (or at end of bootstrap?)

;;---
;; External Tools
;;---
;; Do I want to do the "find git" on windows like this or like in the Windows PATH env var?
;; Associative List of tool symbol to... tool path.
(defconst spydez/tools/external
  '(
    ;; configure-shell wants bash (git bash (same path as diff))
    ;; here and in Windows PATH atm... (debugging eshell)
    ("bash" . "")

    ;; Emacs EPA (EasyPG Assistant) should have latest GPG version
    ;; TODO: add in a check for version numbers when checking for tools?
    ("gpg" . "")

    ;; magit wants git and diff
    ("git" . "") ; in windows system env var PATH  right now
    ("diff" . "")
    )
  "An alist for tool name -> exec path. These will be front-to-back appended to list, so if e.g. there's several git binaries and only one will work, put git in front of this alist."
  ;; If I need more than a pair or triple tuple:
  ;;   Options for Structured Data in Emacs Lisp: https://nullprogram.com/blog/2018/02/14/
  )


;;---
;; Try-Load overrides (from bootstrap-this-late.el)?
;;---
;;(when (require bootstrap-this-late nil 'noerror)
;;  (spydez/warning/message nil nil "Empty bootstrap-this-late."))
;; I'm fine if this system has no late step.
(require 'bootstrap-this-late nil 'noerror)


;;------------------------------------------------------------------------------
;; Final Bootstrap.
;;------------------------------------------------------------------------------
;; Actual bootstrapping, or finalizing of bootstrap, depending on how you look
;; at it. The above was, essentially, the min needed to get ready for
;; use-package which is the min needed for bootstrap-debug-late which is possibly useful
;; for debugging as I go through the rest of this init.el rewrite.

;; Bootstrap of OS and External Tools.
(require 'bootstrap-os)

;; Keep near (require 'bootstrap-package):
;;---
;; Not sure how true this is, but...
;; source: https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; (package-initialize) ; Do NOT delete this comment
;;   In emacs 25+, the `package-initialize' call is auto-added to the top of
;; init.el unless the user already has a commented or uncommented
;; `(package-initialize)' line present in their init.el.
;;   I call this function in bootstrap-packages.el and so am keeping the
;; commented out version here so that package.el does not add it again.
;; TODO: emacs 27: Figure out how true this is.
(when (boundp 'early-init-file)
  (spydez/warning/message nil nil "TODO: figure out how early-init affects call to package-initialize"))

;; Init use-package so we can use use-package for the rest of the packages we use.
(require 'bootstrap-package)

;; ASAP after use-package is available (debug prints, init load timings)
(require 'bootstrap-debug-late)

;; todo: mess with garbage collection at all?
;; todo: up this a waybunch? May be a more annoying hit to gc huge chunks infrequently instead of tiny chunks frequently.
;gc-cons-threshold
;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; or hooks... http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;; Packages used by other packages.
(use-package bind-key)
(use-package diminish)
(use-package delight)
;; diminish vs delight... Having no sound knowledge on this subject, no real
;; interest in it, and after weakly trying to get Projectile to use delight but
;; failing and having to resort to asking Projectile nicely... I am fully
;; qualified to s-
;; I dunno... Delight maybe?

;; library used for some list functions (by me and by some packages)
;; making it explicit now that I use it too
;; https://github.com/magnars/dash.el
(use-package dash)

;; Setup backups, autosaves, and history.
(require 'bootstrap-backups)

;; keyboard stuff
(require 'bootstrap-keyboard)

;; todo:
;; conditional use-package stuff? 
;; https://jwiegley.github.io/use-package/keywords/

(require 'bootstrap-final)


;;------------------------------------------------------------------------------
;; Configuration.
;;------------------------------------------------------------------------------
(setq spydez/warning/current-type '(spydez config))
(spydez/info/message-if spydez/warning/current-type "init.el... Configuration.")
;; Loading and init are done - now do any more required setup.

;; Interactive funcs I don't use in init but may want sometimes interactively,
;; as they are interactive functions.
;; Actually I do use one or two in init:
;;   - set keybinds to some
;;   - use e.g. range for e.g. tabstops
(require 'misc-functions)

;; Stuff that affects emacs itself, like garbage collection.
;; Anything earlier than this might be in early-init.
(require 'configure-emacs)

;; TODO: A keymap prefix of my own? Or a hydra...
;;   M-s is used by: https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;;     - search: custom-keys-mode-prefix-map
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;;   C-c letter: reserved for user (only letters, not numbers or control characters or punctuation)
;;   F5 thru F9: reserved for user
;; https://stackoverflow.com/questions/1144424/are-there-any-emacs-key-combinations-reserved-for-custom-commands
;;   M-x describe-unbound-keys supplied by `unbound' package.

;; OS: May need to add a bootstrap-os if need anything earlier than this...
;; Any windows vs Linux vs etc stuff.
;; Also a decent place for XEmacs vs Emacs if we need any of that.
;; Mainly just windows stuff...
(require 'configure-os)

;; Make sure emacs server daemon is running.
(require 'configure-daemons)

;; log or large file stuff
(require 'configure-logs)

;; Helm/ido/etc
(require 'configure-completion)

;; Minibuffer and mode line tweaks
(require 'configure-minibuffer)

;; Window setup (menu bar, color theme, etc)
(require 'configure-window)

;; Kill/Yank Ring (aka Undo/Redo)
(require 'configure-kill-ring)

;; Help?
;; I need somebody...
(require 'configure-help)

;; VC: git, magit, svn, etc.
(require 'configure-version-control)
;; todo: finish this.
;; TODO: svn-of-some-sort
;; TODO-maybe-as-well: multiple git users, upload to github repo
;;   good instructions so it's easy to setup in order to download .emacs.d from github repo next time.

;; TODO: Reorder? hydra might depend on later stuff maybe?
;;   - hydra might should go higher...
;; key-chords, hydra, some helper functions
(require 'configure-hydra)


;; TODO: move this to proper place
;;------------------------------------------------------------------------------
;; Tab Settings
;;------------------------------------------------------------------------------
;; Winging this in here for now - belongs in text or dev-env or something.
;; https://www.emacswiki.org/emacs/NoTabs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Just-Spaces.html
;; https://www.emacswiki.org/emacs/TabsAreEvil
;; https://www.emacswiki.org/emacs/SmartTabs
(setq-default indent-tabs-mode nil)   ; always replace tabs with spaces
(setq-default tab-width spydez/dev-env/tab/normal) ; set default tab width for all buffers
;; https://www.emacswiki.org/emacs/TabStopList
;; (setq tab-stop-list (spydez/range 0 120 spydez/tab-width)) ; TODO: range function is in lisp/misc-functions... load them earlier?

;; NOTE: M-x tabify and M-x untabify exist and work on regions.


;; For moving around in and messing with text via or at point and/or mark.
;; Hi Mark.
;; Obviously this and configure-text and configure-dev-env
(require 'configure-point-and-mark)

;; Dired, recentf, other file or folder operations
(require 'configure-files-and-folders)

;; Text: fill-column, UTF-8, etc.
(require 'configure-text)

;; Org-Mode: Final Boss of Emacs Major Modes
(require 'configure-org-mode)

;; Dired Mode - seems IDE adjacent so it may go into configure-dev-env. Putting it there for now.
;;   - It could go into its own configure-dired though
;; Auto-Complete - hippie and dabbrev
;; Parenthesis Matching/Delimiters
;; Basically stuff that is development/programmer in nature but global or for multiple modes?
(require 'configure-dev-env)

;; DevOps - ain't got much right now
(require 'configure-dev-ops)

;; projectile, helm-projectile
(require 'configure-project)

;; TODO: Tags and tag files?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgfb77d93
;; They belong in dev-env or ide...
;; But they may not be used if company or cedet or something is used.
;; Gnu Global for tags? https://stackoverflow.com/questions/1598351/emacs-etags-and-using-emacs-as-an-ide

;; todo: configure IDE?
;;
;; Setup Visual Studio to auto check/notice changed files? (or does it do that by default?)
;;
;; https://www.emacswiki.org/emacs/MSVisualStudio#toc5
;; Notably from there:
;;   - "Sending current file to Emacs from Visual Studio"
;;     - "with a macro..."
;;
;; For tool: Visual Studio -> "Tools" -> "External Tools" -> "Add"
;; For kbd shortcut: Visual Studio -> "Tools" -> "Options" -> Environment -> Keyboard -> Search: "Tools.ExternalComm" -> ExternalCommandN where N is number of your added external tool. Set to e.g. C-M-. (Ctrl + Alt + .)
;; In visual Studio, Configure external tools. Add a tool with the following configuration:
;;   Name: Emacs (for example)
;;   Location: c:\path-to\emacsclientw.exe
;;   Arguments: -n +$(CurLine):$(CurCol) $(ItemFileName)$(ItemFileExt)
;;   *note there is a variable button that will help you with the shortcuts like $(CurLine)
;;   WorkingDirectory: $(ItemDirectory)
;;
;; Sample for VS2010
;;   Name: Emacs
;;   Location: c:\path-to\emacsclientw.exe
;;   Arguments: -n +$(CurLine):$(CurCol) $(ItemPath)
;;   *note there is a variable button that will help you with the shortcuts like $(CurLine)
;;   WorkingDirectory: $(ItemDirectory)

;; TODO: shortcut for building Pathfinder_Everything in Emacs with Emacs output buffer?

;; TODO: company-mode? http://company-mode.github.io/
;;   How's this compete/interact with yasnippets, dabbrev, hippie, etc..?

;; todo: configure shell?
;; will need an 'if windows'?
;; ...and maybe an 'if git bash exists'
;; https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
;; (setq explicit-shell-file-name "C:/git-for-windows/bin/bash.exe")
;; (setq explicit-bash.exe-args '("--login" "-i"))
(require 'configure-shell)

;; todo: configure parenthesis

;; todo: give rainbow-mode a try. What I do in old .emacs?

;; todo: yasnippet?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
;; TODO: you were here... where did you go?
;; TODO: take snippets out of M-/ completion maybe?
;;   Getting annoying when writing elisp and I don't really use them right now...
(require 'configure-templates)

;; Programming Modes
(require 'configure-csharp)
;; todo: configure code modes
;;  - C
;;  - C++
;;  - go?
(require 'configure-python)
(require 'configure-elisp)

;; todo: htmlize? I don't think I ever really used it...
;; converts buffers to html
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
;; (require 'htmlize)

;; TODO-maybe?: web-mode for django work?
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#org0acdde9
;;   - http://web-mode.org/
;; TODO-maybe?: Javascript config?: http://pages.sachachua.com/.emacs.d/Sacha.html#org457f5a6
;;   - js2-mode, coffee-mode, jasminejs-mode
;; TODO-maybe?: HTML config?
;; Skewer-mode lets you send HTML/CSS/JS fragments to Chrome...
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#org915393b
;; Tern (for js): http://pages.sachachua.com/.emacs.d/Sacha.html#org17343fb

;; todo: configure keyboard?
;; Y'know...
;;   - dvorak vs qwerty.
;;   - Any binds that make sense for dvorak but not qwerty.
;;   - vice versa
;;   - maybe mapping memory muscle ones to something weird if not dvorak?
;; Caveat: use-package and bind make keys a bit more spread out.
;;   My "C-x C-m" instead of "M-x" for instance, is in helm's use-package.
;;   So maybe this needs to be in bootstrap or something so I can set up a predicate to check later.
;;   spydez/keyboard/dvorak-p or whatever. (spydez/keyboard/dvorak 'dvorak-thing 'else-slash-qwerty-thing).
;; TODO: See what this is on about; seems maybe too vimmy:
;;   http://ergoemacs.org/emacs/emacs_keybinding_redesign_2.html
;;   http://ergoemacs.org/misc/ergoemacs_vi_mode.html
;; TODO: does this apply to key-chord and/or hydra packages?

;; TODO: Autorevert. Tweak ARev minor mode? Disable it?

(require 'configure-crypt)

;; Do search late as it probably just relies on/sets up other things.
(require 'configure-search)

;; chat, social stuff
(require 'configure-chat)

(require 'configure-fulfillment)

;; org-d20, dice roller?, other sources of random
(require 'configure-dice)
;; TODO: anything useful in my old emacs/ERC dice roller (sasta.el)?
;;   - may have to go back to find author I derived from?
;;     - (did I?)
;;       - limerick license:
;;         https://writing.stackexchange.com/questions/6428/converting-the-mit-license-into-a-limerick
;;         https://www.google.com/search?client=firefox-b-1-d&q=This+work+%27as-is%27+we+provide.No+warranty%2C+express+or+implied.We%27ve+done+our+best%2Cto+debug+and+test.Liability+for+damages+denied.Permission+is+granted+hereby%2Cto+copy%2C+share%2C+and+modify.Use+as+is+fit%2Cfree+or+for+profit.On+this+notice+these+rights+rely.

;; money, accounting, or other such stuff?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgc91b6e1
;; TODO: https://github.com/ledger/ledger-mode


;;---
;; Misc Config
;;---

;; autocomplete?
;; Can you thrive and profit without auto-completion? Surely. The feature is
;; kind of a comfort blanket for most of us; you will never fail to bild a
;; system without it (unless you are using Java, then you need IntelliJ). Still
;; it is quite nice to have popup documentation.
;; (require 'fuzzy)
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-auto-start nil)
;; (ac-set-trigger-key "TAB")
;; (eval-after-load "delight"
;;   '(progn
;;      (eval-after-load "auto-complete"
;;        '(delight 'auto-complete-mode "↝"))))
;; TODO: see what I was using first. Some of this is dead links.
;; https://webcache.googleusercontent.com/search?q=cache:nvHM1b9JhGcJ:https://github.com/suzp1984/auto-complete+&cd=1&hl=en&ct=clnk&gl=us
;; https://webcache.googleusercontent.com/search?q=cache:pccrs3LhmCoJ:https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html+&cd=1&hl=en&ct=clnk&gl=us&client=firefox-b-1
;; TODO: was using fuzzy-match.el ver 1.04 (2008)


;; Templates/snippets
;; (require 'yasnippet)
;; (yas-load-directory (concat (cask-elpa-dir)
;;                             "/yasnippet-20140306.5/snippets"))
;; (eval-after-load "delight"
;;   '(progn
;;      (eval-after-load "yasnippet"
;;        '(delight 'yas-minor-mode "✂"))))
;; (yas-global-mode 1)

;; Config TRAMP for getting at server text files?
;; I don't need it day-to-day, but it'd be nice to already have if I do need it.

;; TODO: doom modeline?
;; https://github.com/hlissner/emacs-doom-themes

;; TODO: row, column in modeline?
;;   - right now it has "%/Bot/Top L<line num>" and don't want that?

;; TODO: ... I forget what.

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(setq spydez/warning/current-type '(spydez finalize))
(spydez/info/message-if spydez/warning/current-type "init.el... Finalizing...")

;; todo: initial-buffer-choice vs spydez/auto-open-list???

;; todo: Maybe start a spydez/debugging file?..
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/message.html
;(spydez/debug/message nil "%s" use-package-always-ensure)

; todo: require sanity
;   - sanity ido-mode off?
;   - sanity other things? emacs version complainer? platform complainer?
(require 'finalize-sanity)

;; TODO: define shortcuts to frequently used files?
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#org9750649

;; TODO: split frame, open these in other-window?
;; TODO: move to a finalize probably
(defun spydez/auto-open-files ()
  (if (and window-system (boundp 'spydez/auto-open-list))
      (dolist (file spydez/auto-open-list)
        (find-file file))))
(add-hook 'emacs-startup-hook 'spydez/auto-open-files)

;; TODO: move to a finalize probably?
;; Have a shell open and ready.
;; (when window-system (shell))
;; TODO: set a default dir for shell to open to?
;; TODO: why does (shell) here cause a vertical split? I want it in background.

;; TODO: finalize help - maybe some how-to in a buffer?
;; for zzz-finalize to choose as shown.
;; for my custom stuff or new stuff e.g. magit, hydra, my shortcuts...
;; Choose a random one maybe. Have various things push their
;; help info for this into a list during use-package init or config...

(require 'zzz-finalize)
(setq spydez/warning/current-type '(spydez running))
(spydez/info/message-if spydez/warning/current-type "init.el...Ok. 3 2 1, let's go.")
;; fin

;; TODO: check out old cole-PC.emacs and bootstrap.el.
;; old setup: https://github.com/spydez/emacs
;; See how much this init.el can be reduced to minimum?
