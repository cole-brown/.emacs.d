;;; init.el --- init the emacs -*- mode: emacs-lisp; lexical-binding: t -*-
;;; Commentary:

;; Initialize the emacs.

;; 1) use-package is quite central.

;; 2) zenburn is The One True Theme.
;;    - Can switch to others (without issues I think?), but tweaks have been
;;      made. They should all be fenced in by '(with-feature zenburn-theme ...)'
;;      sexpr blocks.

;; 3) Some of my own 'packages' in personal/packages or personal/lisp.
;;    - ala `taskspace' or `with'

;; 4) Excellent replacement/override and extensibility.
;;    - Per-file/feature replacement by just having the source
;;      (provide 'zort) be in the (add-to-list 'load-path ...)
;;      later in the sequence.
;;    - Per-file/feature piggybacking by just having the extra source be named
;;      according to `spydez/require/piggyback-format' and be in the
;;      load-path. E.g. (spydez/require 'zort) will require `zort' feature
;;      and require (noerror) `zort-secret'.
;;    - Can do more surgical overriding or futzing in `finalize-domains' or some
;;      similar thing.
;;    - All of this can be done on a per device, per domain, or global basis
;;      based on load-path so that e.g. computers tagged 'work' domain setup
;;      dirs one way and your 'home' computer sets up another.

;;; Code:



;;------                                                                ------;;
;;----                                                                    ----;;
;;----------------------------------init.el-----------------------------------;;
;;--                              Hello there.                              --;;
;;----------------------------------------------------------------------------;;
;;----                                                                    ----;;
;;------              (...this is maybe a bit complicated.)             ------;;


;;(spydez/init/step/set-completed '(intermission none))
;;(spydez/message/init "init.el... Intermission.")
;; TODO: emacs 27: Turn back on.

;; TODO: use-tool package, like use-package, but for external tools like git,
;; gpg say what versions you want, what os you expect, have ways for packages to
;; hook in so like gpg can be connected to EPA even if half windows, half MinGW
;; environment.
;; This is started, but really sucks right now.


;;------------------------------------------------------------------------------
;; Notes, TODOs, Links
;;------------------------------------------------------------------------------

;;---
;; Current Shortcomings:
;;---
;; - Bit over-engineered, probably. My last .emacs was not intuitive to
;;   tweak/fix when getting set up on a new device, so I probably went
;;   overboard with warnings/messages and overridability this time around...
;;   - Tiny bit.


;;---
;; The Many Faces of TODO
;;---
;; TODO-EASY: I'm not doing it today for obvious reasons, but I bet future
;;   me can knock it out of the park EZ.
;; TODO-TODAY: You were here yesterday(/in the faint and distant past) and
;;   thought you'd be back tomorrow.
;; TODO-SECRETS: Something emacs dir vs secrets dir related.
;; TODO-DOC: Make some documentation about this something here.
;; TODO-HERE: Was doing clean up or something and left off there.
;; TODO: Generic someday TODO


;;---
;; Search...
;;---

;;-
;; `ripgrep/deadgrep': It's a lot better than `find'. A lot a lot.
;;-
;; <F5> search term <RET>

;;-
;; Projectile:
;;-
;; find/replace with: Projectile's `C-c p r'

;;-
;; `find':                                            (knights who say...: '  ')
;;-
;; find . -path "./elpa" -prune -o -iname "*.el" -print0 | xargs -0 grep -ni "zort"

;; (w/ no-littering):
;; just org:
;; find . -path "./elpa" -prune -o -path "./etc" -prune -o -path "./var" -prune -o -iname "*.el" -print0 | xargs -0 grep -ni "zort"
;;
;; org and el:
;; find . -path "./elpa" -prune -o -path "./etc" -prune -o -path "./var" -prune -o -iname "*.el" -o -iname "*.org" -print0 | xargs -0 grep -ni "zort"

;; Those are getting complicted and not working right...
;; find . -iname "*.el" -o -iname "*.org" | grep -v "/var/" | grep -v "/pre-no-littering/" | grep -v "/manual-package-archive/" | grep -v "/elpa/" | xargs grep -i "zort"


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

;; Which Key is What Function:
;; command -> keybind:     C-h f <cmd>   :: describe-function (mentions keybind)
;; command -> keybind:     C-h w <cmd>   :: helm-descbind (nice fuzzy search)
;; keybind -> command:     C-h k <keys>  :: describe-key
;; help for function name: C-h f <cmd>   :: describe-function
;; function help search:   C-h a <fuzzy> :: helm-apropos

;; Flatten a list for some function that takes &rest instead:
;;   You're looking for `apply'. And you (apply #'func ...).
;;   ...Don't forget to quote that function symbol.
;;   https://emacs.stackexchange.com/questions/17985/turn-a-list-into-a-set-of-arguments-for-a-function

;;---
;; References.
;;---

;; TODO: gather up urls, for domains/pages that show up often enough,
;;   save off into secrets.d/references or something?
;;   Cuz I want 'em around next time. Which will be 10-16 years from now
;;   if history repeats itself.

;; TODO-SECRETS:
;;   Take out load-path and participation in init/config/etc?
;;   Hand off to any post-init secrets-init once stuff is done?

;; Special extra useful links:
;;   1) Sacha Chua's init.org:
;;      http://pages.sachachua.com/.emacs.d/Sacha.html
;;      - Was the first... Followed her init pretty exclusively until done
;;        with it, then branched off to other's stuff (like my own old .emacs).
;;   2) Noah Hoffman's init.org:
;;      https://github.com/nhoffman/.emacs.d/blob/master/init.org
;;      - Well set up init.org.
;;      - If you want not-GPL, beware the GPL waaaaay down at the end.
;;   3) Grant Rettke's (C3F) init.org:
;;      https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
;;      - Interesting layout for literate programming. Sections come up in
;;        whatever order and then when tangled, get put into a specific order.
;;   4) zzamboni's init.org:
;;      https://github.com/zzamboni/dot-emacs/blob/master/init.org
;;      - Well explained. Useful org-mode stuff.

;; Todo: Do we want ./spydez/references/ ignored or saved in git?
;;   - Don't really want to save if public in repo...
;;   - Cannot save some of the references I want publically in my repo as it is
;;     GPL'd. Minorly annoying wrench in the works.
;;
;; And of course my old setup: https://github.com/spydez/emacs
;; TODO: check this? https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;;    from: https://www.reddit.com/r/emacs/comments/2wzhxh/how_to_organize_initel/covmnl5
;; general layout ideas:
;;   "How to organize init.el?" https://www.reddit.com/r/emacs/comments/2wzhxh/how_to_organize_initel/
;;
;; http://ergoemacs.org

;; Maybe look at all the settings in here:
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el

;; TODO: Turn `use-package' up to 11?
;;   https://github.com/a13/emacs.d

;; TODO: Try `rx' for something regex related. Comes with emacs.


;;---
;; Links
;;---
;; Nice browsable unicode table:
;;   https://unicode-table.com/en/blocks/miscellaneous-symbols/

;;---
;; TODOs and Misc.
;;---

;; TODO: icons maybe? Do I need icons?
;;   https://github.com/domtronn/all-the-icons.el
;; TODO: other stuff from here?: https://huytd.github.io/emacs/emacs-from-scratch.html

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

;; Consider: my current configure-<functionality> versus what most others seem to do: configure-<package>
;;   - Sticking with this for now...

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
;; I think just put them in the right/related file, or in a file in docs/...

;; TODO: any way to have emacs run a command (async) to get output from a remote server?
;; Right now: emacs and ssh don't talk to each other well.

;; TODO: focus.el? https://www.reddit.com/r/emacs/comments/b1vrar/lsp_support_for_focusel_using_lspmode/

;; TODO-HERE: todo/comment cleanup
;; TODO: SSH/Shell better?

;; TODO: prettify my JSON(ish) logs?
;;   Does a mode exist already? Do I want the whole buffer pretty or one line
;;   expanded to pretty at a time?

;; TODO: desktop-save-mode but not really? Something that saves sessions, but
;;   doesn't restore automatically. Can manually restore (possibly choosing the
;;   buffers wanted out of the saved session)?


;;---
;; Trials and Try-outs.
;;---
;; Packages and experiments should be marked with something like...
;; Trial: [yyyy-mm-dd]
;;
;; TODO: change to some sort of elisp documentation or annotation?
;; TODO: maybe with some help text about how to use the feature on trial?
;; TODO: Then we can, somewhere in finalize, choose one of those helps to add
;;       into scratch buffer message?
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#orgfb9b12e
;;   Add advice to funcs? https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html

;; Packages to consider trying out:
;;   -

;; Use `bind-key' and `use-package' as much as possible for configuration?
;;   See for an example: https://github.com/a13/emacs.d

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
;;   emacs 26:  Fakes loading it early by loading it ASAP when init.el is called.
;;   emacs 27+: Loads during early-init step.
;;
;; Early, pre-init, setup includes mostly getting prepared for init by setting
;; bare minimum load paths and spydez/* functions for loading the environment
;; with domain- and device-specific overrides once init.el gets going.
;;
;; Can do some even earlier stuff with early-init.el if needed...
;;   https://www.reddit.com/r/emacs/comments/7yns85/emacs_adds_support_for_a_second_read_earlier_init/
;;   https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b

;; init.el:
;;   First, emacs loads init.el.
;;   Our init.el will then load our files roughly in this order:
;;     0) zeroth-*.el
;;     1) bootstrap-*.el
;;     2) configure-*.el
;;     3) finalize-*.el

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
;;  1) (zeroth ...) barest basic setup
;;     - make sure to call something for overrides
;;  2) (bootstrap ...) or something
;;     1) overrides here?
;;     2) all bootstrapping here
;;  3) (configure ...) or soemthing
;;     1) calls out to various configures for packages and settings here
;;  4) (finalize ...) or something
;;     1) calls out to finalize steps here (sanity checks, etc)
;;  So this file becomes very small (code-wise) with some decent text filler
;;  explaining things.
;;    - e.g.: old .emacs's .emacs file (aka <computer-name>.emacs)
;;            spydez/references/bootstrap.el
;;    - e.g.: https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;;            (or a bit of a hybrid (steps 2-4 in init.el instead of sub-files))
;;  Alternatively, megalithic:
;;    - e.g.: Sacha.org's file


;;------------------------------------------------------------------------------
;; Concerning Consts, Vars, and Funcs.
;;------------------------------------------------------------------------------

;;---
;; Defining Consts & Variables:
;;---
;;    `setq' vs `defconst' vs `defcustom' vs `customize-set-variable' vs
;;    use-package ":custom"...
;; - For packages, try to use ":custom".
;; - For consts, `defconst' is good for hinting that it's constant while also
;;   allowing a middle-finger via `setq' at any time.
;; - For sorta consts that are more configuration of my things, use
;;   `defcustom' with ":group spydez/group" and ":type <type>".
;;   - See for types:
;;     - Simple: https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Types.html#Simple-Types
;;     - Not:    https://www.gnu.org/software/emacs/manual/html_node/elisp/Composite-Types.html#Composite-Types
;; - This leaves `defvar', `setq', `customize-set-variable', et al. for
;;   special cases.


;;---
;; Naming Schemes
;;---
;; Emacs/Elisp convention seems to be "prefix--" or "prefix-", with "descriptive-function-name".
;; However, this gives me names that can't be grokked into sub-components at-a-glance.
;;
;; I like:
;;   Prefix: spydez/
;;   Infix:  -
;;   Grouping Separator: /
;;
;; So, e.g. `spydez/datetime/format/org-inactive-derivative'.
;; At a glance:
;;   1. This is mine (spydez/)
;;   2. This is datetime-related.
;;   3. This is a format.
;;   4. "org-inactive-derivative" is hopefully info enough when looking at
;;      datetime formats to possibly use.
;;
;; The downsides so fare are:
;;   - Not conforming to Emacs convention.
;;   - Getting a bit verbose at times.


;;---
;; Cross Platform
;;---
;; I don't have any more than just some Windowses (7, 10) right now so a robust
;; cross-platform init with proper places for overriding things will probably
;; have to wait until I encounter it.
;;
;; I think this is it (probably this is overkill), but I can't properly test.
;; Well, this is definitely overengineered overkill.

;; Cross-platform dir and file names:
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html#Directory-Names
;; Helpers in "spydez/path/*", and dir names are "spydez/dir/*".


;;----------------------------------------------------------------------------;;
;;                                 Bootstrap.                                 ;;
;;---To pull oneself up by the bootstraps, one must first find one's boots.---;;
;;                           ...Where are my boots?                           ;;
;;----------------------------------------------------------------------------;;

;;---
;; We're faking early-init's earlyness, so can't do these here right now.
;; See below for place for now.
;; (spydez/init/step/set-completed '(intermission interstitial-prose))
;; (spydez/message/init "init.el... Bootstrapping.")
;;---

;;---
;; I have only tried this init on:
;;   (emacs-version)
;;   "GNU Emacs 26.1 (build 1, x86_64-w64-mingw32)\n of 2018-05-30"
;; So say we're restricted to emacs 26+?
(unless (>= emacs-major-version 26)
  (error "Emacs version 26 or higher is required"))
;; TODO-EASY: Move to early-init file? Or have in both.
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
(spydez/init/step/set-completed 'bootstrap 'none)
(spydez/message/init "init.el... Bootstrapping.")
;;-----

;; Nothing really before system bootstrap, right now.
(spydez/init/step/set-completed 'bootstrap 'early)

;;---
;; System Setup...
;;---
;; w/ sides of sanity and also sanity
(spydez/require 'bootstrap-system)
;; `spydez/init/step/completed' should either be:
;;   '(bootstrap . '(system default))
;;   '(bootstrap . '(system specific))


;;---
;; Little Bit of Sanity...
;;---
;; Had this here:
;;   (spydez/require 'bootstrap-sanity-early)
;; But there was never anything in it. So it's gone. But if sanity is required,
;; here is a good place for it.


;;------------------------------------------------------------------------------
;; Bootstrap Consts, Vars, and Funcs.
;;------------------------------------------------------------------------------

;; TODO: shift even more out into bootstrap-* files/steps?


;;---
;; Setup some very basics, so we can get moving...
;;---
;; Here, we still have a very bare `load-path' defined by early-init. It's soon
;; to be corrected to actual load path for rest of init, but if something is
;; needed before then:
;;   Add to early-init.
;;   Add to bootstrap-system.
;;   Wedge it in wherever it makes sense.
;;   Fourth thing.

;; TODO: move these to a file?
;;   - but they're before the path so must limit to places zeroth knows.

;;---
;; Directories for Emacs or Packages
;;---
;; This sets up the final load-path.
(spydez/require 'bootstrap-directories)


;;---
;; Debug
;;---
;; All the way down here because I want my load paths, but we could put at the
;; top if needed with a little adjustment.
(spydez/require 'bootstrap-debug-early)


;;---
;; Misc Stuff
;;---
(spydez/require 'bootstrap-consts)

;; TODO: just delete? configure-crypt does not care about this anymore, I don't think...
;; TODO: move this to configure-crypt? And/or rejigger configure-crypt so some can happen soon enough for this to be happy actually loading a file.
;; Load sensitive information from outside of .emacs.d
;;(if (bound-and-true-p spydez/dir/doc-save-common)
;;    (when (not (load (spydez/path/to-file spydez/dir/doc-save-common ".emacs.secrets") 'noerror))
;;      (spydez/message/warning nil :debug "No secrets to load."))
;;  (spydez/message/warning nil nil "No secrets loaded. Do not know where to look. '%s' undefined." 'spydez/dir/doc-save-common))


;;---
;; Try-Load overrides (from bootstrap-this-late.el)?
;;---
;;(when (spydez/require bootstrap-this-late nil 'noerror)
;;  (spydez/message/warning nil nil "Empty bootstrap-this-late."))
;; I'm fine if this system has no late step.
(spydez/require 'bootstrap-this-late nil 'noerror)
;; I have a default, but it's a big commented out no-op right now.

;;---
;; Final Chance to Affect Bootstrap?
;;---
(spydez/require 'dev-directories nil 'noerror)
(spydez/init/step/set-completed 'bootstrap '(system finalized))


;;------------------------------------------------------------------------------
;; Final Bootstrap.
;;------------------------------------------------------------------------------
;; Actual bootstrapping, or finalizing of bootstrap, depending on how you look
;; at it. The above was, essentially, the min needed to get ready for
;; use-package which is the min needed for bootstrap-debug-late which is
;; possibly useful for debugging as I go through the rest of this init.el
;; rewrite.

;; Bootstrap of OS and External Tools.
(spydez/require 'bootstrap-os)

;; Keep near (spydez/require 'bootstrap-package):
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
  (spydez/message/warning nil nil
      "TODO: figure out how early-init affects call to package-initialize"))

;; Init use-package so we can use that for the rest of the packages we use.
;;   - no-littering required here
;;   - custom file: location set & deliberately not loaded here
(spydez/require 'bootstrap-package)
(spydez/init/step/set-completed 'bootstrap 'package)

;; ASAP after use-package is available (debug prints, init load timings)
(spydez/require 'bootstrap-debug-late)

;; Packages used by other packages.
(spydez/require 'bootstrap-libraries)

;; Setup backups, autosaves, and history.
(spydez/require 'bootstrap-backups)

;; keyboard stuff
(spydez/require 'bootstrap-keyboard)

;; TODO:
;; conditional use-package stuff?
;; https://jwiegley.github.io/use-package/keywords/

(spydez/require 'bootstrap-final)
(spydez/init/step/set-completed 'bootstrap 'complete)


;;------------------------------------------------------------------------------
;; Configuration.
;;------------------------------------------------------------------------------
(spydez/init/step/set-completed 'config 'none)
(spydez/message/init "init.el... Configuration.")
;; Loading and init are done - now do any more required setup.

;; If needed, could make a "configure-packages" here for disabling/enabling
;; packages per device/domain?..
;; But for now, it's in configure-emacs-secret.el

(spydez/init/step/set-completed 'config 'early)

;; Interactive funcs I don't use in init but may want sometimes interactively,
;; as they are interactive functions.
;; Actually I do use one or two in init:
;;   - set keybinds to some
;;   - use e.g. range for e.g. tabstops
;;
;; Also, now, useful non-interactive functions used in init.
(spydez/require 'misc-functions)
(spydez/require 'date-and-time) ;; Need datetime formats from here...
(spydez/require 'buffer-functions)
(spydez/init/step/set-completed 'config 'libraries)

;; Stuff that affects emacs itself, like garbage collection.
;; Anything earlier than this might be in early-init.
(spydez/require 'configure-emacs)

;; TODO: A keymap prefix of my own? Or a hydra...
;;   M-s is used by: https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;;     - search: custom-keys-mode-prefix-map
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;;   C-c letter: reserved for user
;;               (only letters; not numbers, control characters or punctuation)
;;   F5 thru F9: reserved for user
;; https://stackoverflow.com/questions/1144424/are-there-any-emacs-key-combinations-reserved-for-custom-commands
;;   M-x describe-unbound-keys supplied by `unbound' package.

;; OS: May need to add a bootstrap-os if need anything earlier than this...
;; Any windows vs Linux vs etc stuff.
;; Also a decent place for XEmacs vs Emacs if we need any of that.
;; Mainly just windows stuff...
(spydez/require 'configure-os)

;; Make sure emacs server daemon is running.
(spydez/require 'configure-daemons)

;; log or large file stuff
(spydez/require 'configure-logs)

(spydez/init/step/set-completed 'config 'backend)

;; inserting, searching for my own 'signatures' in notes, code.
(spydez/require 'configure-signatures)

;; key-chords, hydra, some helper functions
;;   - probably high enough in the order now...
(spydez/require 'configure-hydra)

;; Helm/ido/etc
(spydez/require 'configure-completion)

;; Minibuffer and mode line tweaks
(spydez/require 'configure-minibuffer)
(spydez/require 'configure-modeline)

;; Window setup (menu bar, color theme, etc)
(spydez/require 'configure-window)

;; Kill/Yank Ring (aka Undo/Redo)
(spydez/require 'configure-kill-ring)

;; Help?
;; I need somebody...
(spydez/require 'configure-help)

;; VC: git, magit, svn, etc.
(spydez/require 'configure-version-control)
;; todo: finish this.
;; TODO: svn-of-some-sort
;; TODO-maybe-as-well: multiple git users (work, personal) for magit/github
;;   good instructions so it's easy to setup in order to download .emacs.d from
;;   github repo next time.

;; For moving around in and messing with text via or at point and/or mark.
;; Hi Mark.
;; Obviously this and configure-text and configure-dev-env
(spydez/require 'configure-point-and-mark)

;; Dired, recentf, other file or folder operations
(spydez/require 'configure-files-and-folders)

;; Text: fill-column, UTF-8, etc.
(spydez/require 'configure-text)

;; Org-Mode: Final Boss of Emacs Major Modes
(spydez/require 'configure-org-mode)

;; Dired Mode - seems IDE adjacent so it may go into configure-dev-env.
;;   Putting it there for now.
;;   - It could go into its own configure-dired though
;; Auto-Complete - hippie and dabbrev
;; Parenthesis Matching/Delimiters
;; Basically stuff that is development/programmer in nature but global or for
;; multiple modes?
(spydez/require 'configure-dev-env)

;; DevOps - ain't got much right now
(spydez/require 'configure-dev-ops)

;; projectile, helm-projectile
(spydez/require 'configure-project)

;; TODO: Tags and tag files?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgfb77d93
;; They belong in dev-env or ide...
;; But they may not be used if company or cedet or something is used.
;; Gnu Global for tags? https://stackoverflow.com/questions/1598351/emacs-etags-and-using-emacs-as-an-ide

;; Language Server Protocol
;; Language-agnostic protocol for on-the-fly syntax, auto-complete, go-to
;; definitions, etc.
(spydez/require 'configure-lsp)

;; TODO: configure IDE?
;;------------------------------------------------------------------------------
;; IDE Settings
;;------------------------------------------------------------------------------
;;
;; Setup Visual Studio to auto check/notice changed files? (or does it do that
;; by default?)
;;
;; https://www.emacswiki.org/emacs/MSVisualStudio#toc5
;; Notably from there:
;;   - "Sending current file to Emacs from Visual Studio"
;;     - "with a macro..."
;;
;; For tool: Visual Studio -> "Tools" -> "External Tools" -> "Add"
;; For kbd shortcut: Visual Studio -> "Tools" -> "Options" -> Environment ->
;;   Keyboard -> Search: "Tools.ExternalComm" -> ExternalCommandN
;;   where N is number of your added external tool.
;;   - Set to e.g. C-M-. (Ctrl + Alt + .)

;; In visual Studio, Configure external tools. Add a tool with the following
;; configuration:
;;   Name: Emacs (for example)
;;   Location: c:\path-to\emacsclientw.exe
;;   Arguments: -n +$(CurLine):$(CurCol) $(ItemFileName)$(ItemFileExt)
;;     - Note there is a variable button that will help you with the
;;       shortcuts like $(CurLine).
;;   WorkingDirectory: $(ItemDirectory)
;;
;; Sample for VS2010
;;   Name: Emacs
;;   Location: c:\path-to\emacsclientw.exe
;;   Arguments: -n +$(CurLine):$(CurCol) $(ItemPath)
;;   WorkingDirectory: $(ItemDirectory)

;; TODO: shortcut for building a project in Emacs with Emacs output
;; buffer?
;;------
;; END IDE Settings
;;------

;; TODO: company-mode? http://company-mode.github.io/
;;   How's this compete/interact with yasnippets, dabbrev, hippie, etc..?

;; TODO: configure shell?
;; will need an 'if windows'?
;; ...and maybe an 'if git bash exists'
;; https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
;; (setq explicit-shell-file-name "C:/git-for-windows/bin/bash.exe")
;; (setq explicit-bash.exe-args '("--login" "-i"))
(spydez/require 'configure-shell)

;; TODO: configure parenthesis

;; TODO: yasnippet?
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
;; TODO: you were here... where did you go?
;; TODO: take snippets out of M-/ completion maybe?
;;   Getting annoying when writing elisp and I don't really use them right now...
(spydez/require 'configure-templates)

;; What to do with all that whitespace?
(spydez/require 'configure-whitespace)

;; Programming Modes
(spydez/require 'configure-prog-mode) ;; The generic stuff
(spydez/require 'configure-csharp)
;; TODO: configure code modes
;;  - C
;;  - C++
;;  - go?
(spydez/require 'configure-python)
(spydez/require 'configure-elisp)

;; web related things (restclient)
(spydez/require 'configure-web)

;; TODO: htmlize? I don't think I ever really used it...
;; converts buffers to html
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
;; (spydez/require 'htmlize)

;; TODO-maybe?: web-mode for django work?
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#org0acdde9
;;   - http://web-mode.org/
;; TODO-maybe?: Javascript config?: http://pages.sachachua.com/.emacs.d/Sacha.html#org457f5a6
;;   - js2-mode, coffee-mode, jasminejs-mode
;; TODO-maybe?: HTML config?
;; Skewer-mode lets you send HTML/CSS/JS fragments to Chrome...
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#org915393b
;; Tern (for js): http://pages.sachachua.com/.emacs.d/Sacha.html#org17343fb

;; TODO: configure keyboard?
;; Y'know...
;;   - dvorak vs qwerty.
;;   - Any binds that make sense for dvorak but not qwerty.
;;   - vice versa
;;   - maybe mapping memory muscle ones to something weird if not dvorak?
;; Caveat: use-package and bind make keys a bit more spread out.
;;   My "C-x C-m" instead of "M-x" for instance, is in helm's use-package.
;;   So maybe this needs to be in bootstrap or something so I can set up a
;;   predicate to check later.
;;     spydez/keyboard/dvorak-p or whatever.
;;       (spydez/keyboard/dvorak 'dvorak-thing 'else-slash-qwerty-thing).
;; TODO: See what this is on about; seems maybe too vimmy:
;;   http://ergoemacs.org/emacs/emacs_keybinding_redesign_2.html
;;   http://ergoemacs.org/misc/ergoemacs_vi_mode.html
;; TODO: does this apply to key-chord and/or hydra packages?

(spydez/require 'configure-crypt)

;; Do search late as it probably just relies on/sets up other things.
(spydez/require 'configure-search)

;; chat, social stuff
(spydez/require 'configure-chat)

(spydez/require 'configure-distractions)
(spydez/require 'configure-fulfillment)

;; org-d20, dice roller?, other sources of random
(spydez/require 'configure-dice)
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
;; (spydez/require 'fuzzy)
;; (spydez/require 'auto-complete)
;; (spydez/require 'auto-complete-config)
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
;; (spydez/require 'yasnippet)
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

(spydez/require 'taskspace)

(spydez/init/step/set-completed 'config 'libraries)


(spydez/init/step/set-completed 'config 'complete)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(spydez/init/step/set-completed 'finalize 'none)
(spydez/message/init "init.el... Finalizing...")

(spydez/init/step/set-completed 'finalize 'early)

;; TODO: initial-buffer-choice vs spydez/auto-open-list???

;; TODO: Maybe start a spydez/debugging file?..
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/message.html
;(spydez/debug/message nil "%s" use-package-always-ensure)

;; TODO: require sanity
;;   - sanity ido-mode off?
;;   - sanity other things? emacs version complainer? platform complainer?
(spydez/require 'finalize-sanity)
(spydez/init/step/set-completed 'finalize 'sanity)

(spydez/require 'finalize-boot-and-config)

;; TODO: define shortcuts to frequently used files?
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#org9750649
;; todo: rename something better for its function here instead of what
;; it happens to reside right now. `finalize-user-startup' or something
;; TODO: leaving off noerror until home domain works as desired there
(spydez/require 'finalize-domain) ;; nil 'noerror)
(spydez/init/step/set-completed 'finalize 'system)

(spydez/require 'finalize-keybinds)

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

(spydez/require 'zzz-finalize)
(spydez/init/step/set-completed 'finalize 'complete)
(spydez/message/init "init.el...Ok. 3 2 1, let's go.")

(spydez/init/step/set-completed 'running 'none)
;; fin

;; §-TODO-§ [2019-10-07]: proper ending. Maybe a nice poem or one art please.
