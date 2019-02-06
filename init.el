;; -*- emacs-lisp -*-
;; TODO: those code headers I see in emacs packages?

;; TODO: add license? MIT probably?
;; https://snyk.io/blog/mit-apache-bsd-fairest-of-them-all/
;; TODO: Short copyright in header pointing to LICENSE.txt or something.


;;------------------------------------------------------------------------------
;; Notes, TODOs, Links
;;------------------------------------------------------------------------------


;; search...
;; find . -path "./elpa" -prune -o -iname "*.el" -print0 | xargs -0 grep "provide"


;; TODO: can 'literate programming' do multiple files?
;; http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html
;; https://en.wikipedia.org/wiki/Literate_programming


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
;; Todo: Do we want ./spydez/references/ ignored or saved in git?
;; TODO: Finish: Sacha init.el: http://pages.sachachua.com/.emacs.d/Sacha.html
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

;; TODO: read this maybe https://sanctum.geek.nz/arabesque/series/unix-as-ide/

;; TODO: Ctrl-del shouldn't push to kill ring

;; TODO: change functions I've acquired (e.g. "xah-") over to "spydez/" prefix so there's one search for them.
;;   Note the old name in comment or docstring.

;; TODO: resume in emaics/org-mode/latex?
;; https://www.reddit.com/r/emacs/comments/6pp9z3/noob_using_emacs_to_write_a_resume/
;; https://github.com/xiaohanyu/resume
;; https://www.google.com/search?hl=en&output=search&sclient=psy-ab&q=org%20mode%20resume&=&=&oq=&gs_l=&pbx=1

;; TODO: bind-key vs global-set-key vs define-key vs the 50 other ways... which one should we be using where?
;; https://www.google.com/search?hl=en&output=search&sclient=psy-ab&q=bind-key%20vs%20global-set-key&=&=&oq=&gs_l=&pbx=1
;;  - https://www.reddit.com/r/emacs/comments/6hbb22/when_do_you_use_definekey_vs_globalsetkey/

;; TODO: my current configure-<functionality> versus what most others seem to do: configure-<package>

;; TODO: overwrite selection with paste

;; TODO: defer load absolutely nothing? Seems to make for just random pauses for a while when you do something
;; after loading emacs. An extra second or two at start might be less annoying?
;; A global setting would be cool... (if/when (spydez/defer-load-p) ...)

;; TODO: try a let or something for vars that should be different during my init
;; e.g. gc threshold.
;; like...
;; (let ((gc-cons-threshold most-positive-fixnum))
;;  # existing init code
;;  )
;; But a nicer way to do that? (let ((vars...)) (load "actual-init")) maybe?

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


;;---
;; Trials and Try-outs.
;;---
;; Packages and experiments should be marked with something like... 
;; Trial: [yyyy-mm-dd]
;;
;; TODO: change to some sort of elisp documentation or annotation?
;; TODO: maybe with some help text about how to use the feature on trial?
;; TODO: Then we can, somewhere in finalize, choose one of those helps to add into scratch buffer message?


;;------------------------------------------------------------------------------
;; Layout.
;;------------------------------------------------------------------------------

;; early-init.el:
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
;; Alternative is, like, (require 'spydez/bootstrap/debug "bootstrap-debug")?
;; Don't think that looks great.


;;------------------------------------------------------------------------------
;; Initial vars bootstrap.
;;------------------------------------------------------------------------------
;; We probably need to setup some vars, load paths, etc in our init.el before getting going.

;; I don't have any more than just some Windowses (7, 10) right now so a robust
;; cross-platform init with proper places for overriding things will probably
;; have to wait until I encounter it.


;;---
;; Setup some very basics, so we can get moving...
;;---
;; These are the defaults. Override with setq later if needed.

;; TODO: setq vs customize-set-variable (...vs being in the custom file?)

;; defconst vs defvar vs setq
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html
;; Looks like I can use defconst for desired functionality/hinting, while also being
;; allowed to ignore the const-ness and override via a setq in a later init sub-file.

;; cross-platform dir and file names:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html#Directory-Names

(defun spydez/dir-name (name parent)
  "Expand name as child dir of parent in platform-agnostic manner."
    (file-name-as-directory (expand-file-name name parent)))

(defconst spydez/name/setup-domain "work"
  "A domain/folder for setups similar to this. E.g. work vs personal.")
(defconst spydez/name/setup-comp (system-name)
  "Intended for this specific computer's setup folder.")
(defconst spydez/dir/setup-emacs (expand-file-name user-emacs-directory)
  ;; user-init-file and user-emacs-directory can be helpful here
  "This should be a platform-agnostic way to find .emacs.d.")

;; Path dirs, and places for overrides to exist.
;;   ./spydez/defaults/
;;   ./spydez/
;;   ./spydez/work/
;;   ./spydez/work/WORK-PC-NAME/
(defconst spydez/dir/setup-personal (spydez/dir-name "spydez" spydez/dir/setup-emacs)
  "All of my own personal/custom setup code/vars/definitions...")
(defconst spydez/dir/setup-defaults (spydez/dir-name "defaults" spydez/dir/setup-personal)
  "All of my optional/default setup elisp files...") ; TODO: rename to "overrides" or something?
(defconst spydez/dir/setup-domain-specific (spydez/dir-name spydez/name/setup-domain spydez/dir/setup-personal)
  "Anything that has to be domain specific. Tab widths or whatnot.")
(defconst spydez/dir/setup-comp-specific (spydez/dir-name spydez/name/setup-comp spydez/dir/setup-domain-specific)
  "Anything that has to be computer specific. Overriding tab widths or whatnot.")

;; todo: updated to c:/home/<user>/documents
(defconst spydez/dir/common-doc-save "c:/home/documents"
  "Place for auto-open files or secrets or something to be.")

;; todo: move to an end script or something? Or leave up here so it can be overridden...
;; auto-open this file list at end of emacs init/setup
(defvar spydez/auto-open-list
  ;; don't do: '(
  ;; do this: (list
  ;; Need a thing that will eval the list items to strings either now or in auto-open-files.
  ;; So we choose now.
  (list
    ;; top level work org-mode file for notes & such
    (expand-file-name "work.org" spydez/dir/common-doc-save)

    ;; Working on .emacs.d a lot right now so add these in.
    (expand-file-name "init.el" spydez/dir/setup-emacs)
    (expand-file-name "configure-completion.el" spydez/dir/setup-personal)
    ))

;; folders for auto-save files and backup-files (#*# and *~)
(defconst spydez/dir/backup-files
  (spydez/dir-name "backups" spydez/dir/setup-emacs))

(defconst spydez/dir/auto-save-files
  (spydez/dir-name "auto-save-list" spydez/dir/setup-emacs))

(defconst spydez/file/save-history
  (expand-file-name "savehist" spydez/dir/setup-emacs)
  "History of commands, etc.")

(defconst spydez/dir/yasnippets
  (expand-file-name "snippets" spydez/dir/setup-personal)
  "Yasnippets directory.")
;; Could add an override of my own snippets if needed.


;;---
;; Identity / Personal Information
;;---
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/User-Identification.html
(setq user-full-name "Cole Brown"
      user-mail-address "git@spydez.com")
;; user-login-name exists if needed

;; TODO: some consts for significant files or folders to jump to for e.g. opening a work project file.


;;---
;; Add stuff to our load path.
;;---
;; Setting overrides towards front of load-path (add-to-list does this for us).
;;
;; Don't use .emacs.d. 
;; https://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path
;; (add-to-list 'load-path spydez/dir/setup-emacs)
(add-to-list 'load-path spydez/dir/setup-defaults) ;; defaults first so everything else overrides.
(add-to-list 'load-path spydez/dir/setup-personal)
(add-to-list 'load-path spydez/dir/setup-domain-specific)
(add-to-list 'load-path spydez/dir/setup-comp-specific) ;; most specific to this computer last


;;---
;; Debug
;;---
;; All the way down here because I want my load paths, but we could put at the top if needed with a little adjustment.
(require 'bootstrap-debug)


;;---
;; Custom file
;;---
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

;; Some packages want to write to our custom file, so set that up first.
;; An unadorned filename (just "custom.el") wasn't getting picked up as the custom file, so for now:
(setq custom-file (expand-file-name "custom.el" spydez/dir/setup-personal))
;; May need a better setter if custom-file needs adjusted per computer...
;; todo: Helper func to look for file to define place or maybe try provide/require?
;; Possibly move custom-file setting up, and loading down below loading of bootstrap-vars overrides.
(load custom-file t)


;;---
;; Misc Stuff
;;---
;; Load sensitive information from outside of .emacs.d
(when
    (not
     (load (expand-file-name ".emacs.secrets" spydez/dir/common-doc-save) 'noerror))
  (message "No secrets to load."))


;;---
;; Dev Env vars
;;---
;; TODO: put in bootstrap-consts? bootstrap-vars? bootstrap-data?

;; Tab widths. I like a smaller one for verbose/highly indented code (web dev mainly).
;; Normally use a larger one for non-web programming.
(defconst spydez/dev-env/tab/min 2
  "Small tab width for more compact but readable code.")
(defconst spydez/dev-env/tab/normal 4
  "Normal tab width for more usual use cases for code languages like C++, C#...")

;;---
;; External Tools
;;---
;; Do I want to do the "find git" on windows like this or like in the Windows PATH env var?
;; Associative List of tool symbol to... tool path.
(defconst spydez/tools/external
  '(;; magit wants git and diff
    ("git" . "") ; in windows system env var PATH  right now
    ("diff" . "C:/Users/cole/AppData/Local/GitHub/PortableGit_69bd5e6f85e4842f07db71c9618a621154c52254/usr/bin")
    )
  "An alist for tool name -> exec path. These will be front-to-back appended to list, so if e.g. there's several git binaries and only one will work, put git in front of this alist.")
;; set up PATHs so external tools can be found.
(when (boundp 'spydez/tools/external)
  (dolist (tool spydez/tools/external)
    (unless (executable-find (car tool))
      (add-to-list 'exec-path (cdr tool) t)) ;; append to end
    ))
;; find in alist: (when (assoc 'diff spydez/tools/external) (message "truedat"))
;; todo: move this to bootstrap-externals


;;---
;; Try-Load overrides (from bootstrap-vars.el)?
;;---
;(when (require bootstrap-vars nil 'noerror)
;  (message "Empty bootstrap-vars."))
(require 'bootstrap-vars nil 'noerror)
;; todo: rename to override-bootstrap-vars?
;; override-vars? bootstrap-override?
;; bootstrap-refine? bootstrap-post? post-bootstrap?
;; hm...
;;   second take: I like:
;;     1) bootstrap-overrides maybe.
;;     2) bootstrap-local maybe
;;     3) bootstrap-preload-local-overrides... no


;;------------------------------------------------------------------------------
;; Bootstrap.
;;------------------------------------------------------------------------------
;; Actual bootstrapping, or finalizing of bootstrap, depending on how you look
;; at it. The above was, essentially, the min needed to get ready for
;; use-package which is the min needed for init-debug which is possibly useful
;; for debugging as I go through the rest of this init.el rewrite.

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

;; Init use-package so we can use use-package for the rest of the packages we use.
(require 'bootstrap-package)

;; ASAP after use-package is available (debug prints, init load timings)
(require 'init-debug) ;; TODO: rename to... configure-debug?

;; todo: mess with garbage collection at all?
;; todo: up this a waybunch? May be a more annoying hit to gc huge chunks infrequently instead of tiny chunks frequently.
;gc-cons-threshold
;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; or hooks... http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;; Packages used by other packages.
(use-package diminish)
;; TODO: diminish vs delight? Do I care enough to try delight?

;; Setup backups, autosaves, and history.
(require 'bootstrap-backups)

;; keyboard stuff
(require 'bootstrap-keyboard)

;; todo:
;; conditional use-package stuff? 
;; https://jwiegley.github.io/use-package/keywords/


;;------------------------------------------------------------------------------
;; Configuration.
;;------------------------------------------------------------------------------
;; Loading and init are done - now do any more required setup.

;; Make sure emacs server daemon is running.
(require 'configure-daemons)

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
;; (setq tab-stop-list (spydez/range 0 120 spydez/tab-width)) ; TODO: pull range function over from old

;; NOTE: M-x tabify and M-x untabify exist and work on regions.


;; For moving around in and messing with text via or at point and/or mark.
;; Hi Mark.
;; Obviously this and configure-text and configure-dev-env
(require 'configure-point-and-mark)

;; Dired, recentf, other file or folder operations
(require 'configure-files-and-folders)

;; Text: fill-column, UTF-8, etc.
(require 'configure-text)

;; Any windows vs Linux vs etc stuff.
;; Also a decent place for XEmacs vs Emacs if we need any of that.
;; Mainly just windows stuff...
(require 'configure-os)

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

;; todo: configure parenthesis

;; todo: give rainbow-mode a try. What I do in old .emacs?

;; todo: yasnippet?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
;; TODO: you were here... where did you go?
(require 'configure-templates)

;; Programming Modes
(require 'configure-csharp)
;; todo: configure code modes
;;  - C
;;  - C++
;;  - go?
(require 'configure-python)
(require 'configure-elisp)

;; todo: htmlize? I forget what it's for...
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


(require 'configure-fulfillment)

;; org-d20, dice roller?, other sources of random
(require 'configure-dice)

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
;; (eval-after-load "diminish"
;;   '(progn
;;      (eval-after-load "auto-complete"
;;        '(diminish 'auto-complete-mode "↝"))))
;; TODO: see what I was using first. Some of this is dead links.
;; https://webcache.googleusercontent.com/search?q=cache:nvHM1b9JhGcJ:https://github.com/suzp1984/auto-complete+&cd=1&hl=en&ct=clnk&gl=us
;; https://webcache.googleusercontent.com/search?q=cache:pccrs3LhmCoJ:https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html+&cd=1&hl=en&ct=clnk&gl=us&client=firefox-b-1


;; Templates/snippets
;; (require 'yasnippet)
;; (yas-load-directory (concat (cask-elpa-dir)
;;                             "/yasnippet-20140306.5/snippets"))
;; (eval-after-load "diminish"
;;   '(progn
;;      (eval-after-load "yasnippet"
;;        '(diminish 'yas-minor-mode "✂"))))
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

;; todo: initial-buffer-choice vs spydez/auto-open-list???

;; todo: Maybe start a spydez/debugging file?..
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/message.html
;(message "%s" use-package-always-ensure)

; todo: require sanity
;   - sanity ido-mode off?
;   - sanity other things? emacs version complainer? platform complainer?
(require 'finalize-sanity)

;; TODO: define shortcuts to frequently used files?
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#org9750649

;; TODO: move to a finalize probably
(defun spydez/auto-open-files ()
  (if (and window-system (boundp 'spydez/auto-open-list))
      (dolist (file spydez/auto-open-list)
        (find-file file))))
(add-hook 'emacs-startup-hook 'spydez/auto-open-files)

;; TODO: finalize help - maybe some how-to in a buffer?
;; for zzz-finalize to choose as shown.
;; for my custom stuff or new stuff e.g. magit, hydra, my shortcuts...
;; Choose a random one maybe. Have various things push their
;; help info for this into a list during use-package init or config...

(require 'zzz-finalize)
;; fin

;; TODO: check out old cole-PC.emacs and bootstrap.el.
;; old setup: https://github.com/spydez/emacs
;; See how much this init.el can be reduced to minimum?
