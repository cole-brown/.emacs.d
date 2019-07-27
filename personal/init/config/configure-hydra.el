;; -*- emacs-lisp -*-


;; Dvorak layout, so these are probably optimized for that...


;;------------------------------------------------------------------------------
;; General keyboard settings
;;------------------------------------------------------------------------------
;; See bootstrap-keyboard for some earlier setup.

;; We could put some general keybinding in here if we want a place for it?


;;------------------------------------------------------------------------------
;; key-chord
;;------------------------------------------------------------------------------
;; Emacs doesn't have enough weird keyboarding - let's add more.

;; Can't use this as key-chord only accepts printable ASCII characters
;; for its chording.
;; (defconst spydez/key-chord/prefix ?\C--
;;   "'C--' (aka 'Ctrl+hyphen' is our common key-chord character.")

;; Hyphen would be awesome except I like using it for little headers and
;; separators and stuff in the code... And holding it down to generated
;; those just goes into my "--" hydra instead. Or typing function names
;; in lisp will catch some "-<letter>" hydra. Or in scripts or notes or...
(defconst spydez/key-chord/prefix ?-
  "'-' is our common key-chord character.")

;; Function for not forgetting what my main thingy is for key-chords.
;; Also for redefining it (again) if it is too annoying.
(defun spydez/key-chord/chord (key)
  "Returns string for a key-chord starting with our default key-chord character."
  ;; Error Checking:
  ;; Must be char or string.
  (if (not (char-or-string-p key))
      (error "Key-chord key supplied must be char or string: %s" key))
  ;; Must be 1 char string if string.
  (if (and (stringp key)
           (/= 1 (length key)))
      (error "Key-chord key string supplied must have one element: %s" key))

  (let* ((key-char (if (stringp key) (string-to-char key) key))
         (chord (string spydez/key-chord/prefix key-char)))
    chord))

(defun spydez/key-chord/define-global (key function-symbol)
  "Helper for using key-chord-define-global with spydez/key-chord/prefix."
  ;; TODO: Error or warn if overwriting an existing chord.
  (key-chord-define-global (spydez/key-chord/chord key) function-symbol))

;; TODO: Move to lisp/misc or somewhere better suited.
;; From https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun spydez/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Setup key chords
;; TODO: prune the list to actually used stuff
;; Trial: [2019-01-28]
;; Trial-Paused: [2019-07-18]
;;   - Cannot figure out a useful, non-obtrusive, near-nil false positive way
;;     getting useful, usable key-chords. >.<
;; TODO: Fucking key-chords god dammit 3.0... >.<
;;   - "-t" this time. And this 'alt-tab' bug I have to investigate...
;;     - Maybe just have "--" or something be the __ONLY__ key-chord,
;;       and then that can have entries into hydras and stuff?
;;     - Also elisp function names are killing me.
;;   - Need that dupe checker. I have two '-j' chords now.
;; TODO: or maybe:
;;   - don't key-chord if a previous non-chord key has been
;;     pressed in key-chord-____-delay seconds?
;;   - Better maybe: Count chain of input events. Event streak only incremented
;;     if each happens within key-chord-one-key-delay of previous? Ignore
;;     while chain count > N. Set chain count to 0 when streak broken.
;;     Ignore a key-chord if count > N.
;; TODO-FIRST: are there more key-chord vars to help out other than one-key-delay?
;; (use-package key-chord
;;   :init
;;   (progn
;;     ;; (fset 'key-chord-define 'spydez/key-chord-define)
;;
;;     ;; TODO: [2019-01-28] 0.16 one-key-delay feels off/fast/aweful fast. But leaving as when I
;;     ;; do manage it, it feels like a separate action from normal typing.
;;     ;; TODO: [2019-03-08] Still not using this enough and >50% of the time
;;     ;;   I'm not doing the chord - just inserting letters into buffers...
;;     (setq key-chord-one-key-delay 0.16)
;;
;;     (key-chord-mode 1)
;;
;;     ;; https://www.reddit.com/r/emacs/comments/22hzx7/what_are_your_keychord_abbreviations/
;;     ;; https://github.com/Russell91/emacs/blob/master/key-chord.el has these and maybe others:
;;     ;;    (spydez/key-chord/define c++-mode-map ";;"  "\C-e;")
;;     ;;    (spydez/key-chord/define c++-mode-map "{}"  "{\n\n}\C-p\t")
;;
;;     ;; k can be bound too
;;     ;; TRIAL [2019-01-28]: test all these cuz I'm not sure
;;     (spydez/key-chord/define-global "m" 'spydez/hydra/common-stuff/body)
;;
;;     (spydez/key-chord/define-global "u" 'undo)
;;     (spydez/key-chord/define-global "k" 'kill-whole-line)
;;     (spydez/key-chord/define-global "j" 'spydez/switch-to-previous-buffer)
;;     (spydez/key-chord/define-global "y" 'spydez/hydra/window-movement/body)
;;     (spydez/key-chord/define-global "w" 'switch-window)
;;     (spydez/key-chord/define-global "j" 'spydez/hydra/join-lines/body)
;;     (spydez/key-chord/define-global "f" 'helm-find-files)
;;     (spydez/key-chord/define-global "h" 'spydez/hydra/misc/body)
;;     (spydez/key-chord/define-global "x" 'er/expand-region)
;;
;;     ;; TODO: Hold down spacebar to space something out 10 or 20 spaces or whatever...
;;     ;;   and this gets called a lot. I don't think I like space-space as a chord?..
;;     ;;   M-/ might be enough for expanding. It was before this update/upgrade.
;;     ;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
;;     ;; Trial: [2019-01-28]; Disabled: [2019-02-06]
;;     ;;(spydez/key-chord/define-global "  " 'spydez/insert-space-or-expand)
;;
;;     ;; Not using avy right now. Similar to ace-jump or easymotion.
;;     ;; https://github.com/abo-abo/avy
;;     ;; (spydez/key-chord/define-global "jl" 'avy-goto-line)
;;     ;; (spydez/key-chord/define-global "jj" 'avy-goto-word-1)
;;
;;     ;; Not using god-mode, but could consider. https://github.com/chrisdone/god-mode
;;     ;; (spydez/key-chord/define-global "vv" 'god-mode-all)
;;   ))


;;------------------------------------------------------------------------------
;; Hydra Hydra Hydra
;;------------------------------------------------------------------------------
;; For when you need even more emacs keyboard shenanigans.

;; TODO: A hydra for my hydras? I.e. a `hydra-launcher'?
;; I think yes. Sounds good. Maybe hydras and common random shit.
;;   mentioned:  http://nhoffman.github.io/.emacs.d/#org9119f86
;;   actual launcher hydra: http://nhoffman.github.io/.emacs.d/#org09d7a13

;; RTFM 0: Hubris: This was going to be a helper macro that included the
;;   keybind/mode/etc for the hydra and then bound it... But defhydra already
;;   does that with its `body' arg. Could still do this for bind-key's
;;   describe-personal-keybindings, but it is less important now. I'm keeping
;;   this because I'm thinking of using bind-key's full potential instead of
;;   hydra's call to define-key with a map.
;;
;; RTFM 8: After, like, 8th R'ing of TFM... Actually not Hubris. Bad
;;   expectations and unclear docs for the expectations. The `body' arg just
;;   dumps all the hydra heads into the map/prefix you provide. Which is not
;;   what I want - I want them as their own things. I want to summon a hydra and
;;   deal with its heads, not deal with the heads of a hydra that - oh yeah, you
;;   summoned it by cutting off one of its heads and also the hydra
;;   is the castle.
;;
;; (defconst spydez/hydra/hydra-list '())
;; (defmacro spydez/hydra/defhydra (name keybind &optional docstring &rest body)
;;   (declare (indent defun) (doc-string 3))
;;   ;; TODO: call defhydra w/ name, rest
;;   (defhydra name docstring body)
;;
;;   (message "%s %s %s %s" name keybind docstring body)
;;
;;   ;; TODO: call bind-key w/ keybind if not nil
;;   )
;; (spydez/hydra/defhydra test-hydra "C--"
;;   "testing?"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))


;; Trial: [2019-01-28]
(use-package hydra
  :config

  (defhydra spydez/hydra/window-movement ()
    "Window Movement"
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("y" other-window "other")
    ("h" switch-window "switch-window")
    ("f" find-file "file")
    ("F" find-file-other-window "other file")
    ("v" (progn (split-window-right) (windmove-right)))
    ("o" delete-other-windows :color blue)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("d" delete-window "delete")
    ("D" ace-delete-window "ace delete")
    ("i" ace-maximize-window "maximize")
    ("b" helm-buffers-list)
    ("q" nil))

  (defhydra spydez/hydra/join-lines ()
    "Join Lines"
    ("<up>" join-line)
    ("<down>" (join-line 1))
    ("t" join-line)
    ("n" (join-line 1)))

  ;; There was a hydra for Quantified Awesome/Org Clock In that looked neat maybe.
  ;; http://pages.sachachua.com/.emacs.d/Sacha.html#key-chord
  ;; Can add it back in if I start clocking stuff.

  ;; Could use this if I identify some org things I do a lot...
  ;; (defhydra spydez/hydra/org (:color blue)
  ;;   "Convenient Org stuff."
  ;;   ("p" spydez/org-show-active-projects "Active projects")
  ;;   ("a" (org-agenda nil "a") "Agenda"))

  ;; (defhydra spydez/hydra/misc ()
  ;;   "Misc Unrelated Things"
  ;;   ("k" kill-sexp "Kill Sexp")
  ;;   ("b" helm-buffers-list :color blue "Buffers")
  ;;   ("f" find-file :color blue "Find File")
  ;;   ;; ("." repeat "Repeat Last Cmd") ;; doesn't work like that, lol
  ;;   ;; ("m" imenu :color blue)
  ;;   ;; org-mode stuff ;; todo: do I want? these?
  ;;   ("l" org-insert-last-stored-link "Org Insert Link")
  ;;   ("c" (call-interactively 'org-capture) "Org-Capture Interactive" :color blue)
  ;;   ("t" (org-capture nil "T") "Org-Capture task")
  ;;   ;; TODO: leave in or try to nuke from emacs entirely?
  ;;   ;; TODO: if leave in, I need the "reset scale back to default" equivaluent of these two.
  ;;   ("+" text-scale-increase "Text++")
  ;;   ("-" text-scale-decrease "Text--")
  ;;   ;; Not sure if I want any of these...
  ;;   ;; ("q" quantified-track :color blue)
  ;;   ;; ("h" spydez/org-jump :color blue)
  ;;   ;; ("x" spydez/org-finish-previous-task-and-clock-in-new-one "Finish and clock in" :color blue)
  ;;   ;; ("i" spydez/org-quick-clock-in-task "Clock in" :color blue)
  ;;   ;; ("o" spydez/org-off-my-computer :color blue)
  ;;   ;; ("a" spydez/org-check-agenda :color blue)
  ;;   ;; ("r" spydez/describe-random-interactive-function)
  ;;   ;; ("L" spydez/org-insert-link)
  ;;   )

  ;; TODO: define shortcut into this hydra?
  (defhydra spydez/hydra/engine-mode (:color blue)
    "Engine Mode"
    ("g" engine/search-google "google")
    ("e" engine/search-emacswiki "emacswiki")
    ("w" engine/search-wikipedia "wikipedia")

    ("#" engine/search-csharp "c#")
    ("s" engine/search-stack-overflow "stack overflow")
    ("h" engine/search-github "github")

    ("m" engine/search-mail "mail")
    )

  ;; What hydra body color? red, blue, teal, those other colors?
  ;;   https://github.com/abo-abo/hydra/wiki/Hydra-Colors
  ;;   I think blue or teal. Blue if I want this less mean/sticky.
  (defhydra spydez/hydra/main (:color teal)
    "Hydras"
    ("t" spydez/hydra/transpose/body "Transposes")
    ("e" spydez/hydra/engine-mode/body "Search Engines")
    ("c" spydez/hydra/common-stuff/body "Common Stuff")
    ("w" spydez/hydra/window-movement/body "Window Movement")
    ("j" spydez/hydra/join-lines/body "Join Lines")
    ;; ("o" spydez/hydra/org/body "Org Mode")
    ;; ("m" spydez/hydra/misc/body "Misc")
    )

  ;; "C--" nukes one of the `negative-argument' keybinds but I've never used
  ;; that before... But that is acting fucky, so...... "C-c -" maybe? Oh...
  ;; Maybe my color is wrong. It's trying to stay in this hydra instead of
  ;; recursing.
  (bind-key "C--" 'spydez/hydra/main/body)
  )


;;------------------------------------------------------------------------------
;; Search
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org12309ed
;; https://github.com/hrs/engine-mode
;; Trial [2019-02-06]
(use-package engine-mode
  :config
  ;; NOTE: keep synced with spydez/hydra/engine-mode?
  ;; NOTE: Escape any % needed with another %. E.g. here: "c%%23" -> url: "c%23" -> search term: "c#"
  ;; NOTE: ones without keybind/hydra can be got at via engine/search-<engine>
  (progn
    ;; general
    (defengine google "http://google.com/search?ie=utf-8&oe=utf-8&q=%s" :keybinding "g")
    (defengine emacswiki "http://google.com/search?q=site:emacswiki.org+%s" :keybinding "e")
    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "w"
      :docstring "Searchin' the wikis.")

    ;; code
    (defengine csharp "https://google.com/search?q=c%%23+%s" :keybinding "#")
    ;; TODO: python, c++, c?, django?
    (defengine stack-overflow "https://stackoverflow.com/search?q=%s")
    (defengine github "https://github.com/search?ref=simplesearch&q=%s")

    ;; gmail - which google user will this use?
    (defengine mail "https://mail.google.com/mail/u/0/#search/%s" :keybinding "m")

    ;; google w/ hardcoded "site:example.comp"
    ;; (defengine emacswiki "http://google.com/search?q=site:example.com+%s" :keybinding "e")

    ;; more here: https://github.com/hrs/engine-mode#engine-examples
    (defengine amazon "http://www.amazon.com/s/ref=nb_sb_noss?field-keywords=%s")
    (defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d")
    (defengine google-images
      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
    (defengine google-maps
      "http://maps.google.com/maps?q=%s"
      :docstring "Mappin' it up.")
    (defengine project-gutenberg
      "http://www.gutenberg.org/ebooks/search/?query=%s")
    (defengine rfcs
      "http://pretty-rfc.herokuapp.com/search?q=%s")
    (defengine twitter
      "https://twitter.com/search?q=%s")
    (defengine wiktionary
      "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")
    (defengine wolfram-alpha
      "http://www.wolframalpha.com/input/?i=%s")
    (defengine youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s")

    (bind-key* "C-c /" 'spydez/hydra/engine-mode/body)
    (engine-mode)))


;;------------------------------------------------------------------------------
;; Additional Hydra Stuff
;;------------------------------------------------------------------------------
;; funcs for hydras and such

;; I don't use org agenda any right now.
;; (defun spydez/org-check-agenda ()
;;   "Peek at agenda."
;;   (interactive)
;;   (cond
;;    ((derived-mode-p 'org-agenda-mode)
;;     (if (window-parent) (delete-window) (bury-buffer)))
;;    ((get-buffer "*Org Agenda*")
;;     (switch-to-buffer-other-window "*Org Agenda*"))
;;    (t (org-agenda nil "a"))))

;; Not sure what this does exactly. Think I know. It should maybe be in
;; configure-org-mode.el.
;; (defun spydez/org-insert-link ()
;;   (interactive)
;;   (when (org-in-regexp org-bracket-link-regexp 1)
;;     (goto-char (match-end 0))
;;     (insert "\n"))
;;   (call-interactively 'org-insert-link))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------
;; https://www.reddit.com/r/emacs/comments/am0kbn/new_package_majormodehydra_inspired_by_spacemacs/
;; TODO: major-mode-hydra?
;; TODO: pretty-hydra?

;; TODO: a hydra for opening files in various spots? like
;; open-file hydra shortcut:
;;   h $HOME (home env for linuxy things on windows)
;;   u C/D:/Users/<me>/
;;   d <dropbox location>
;;   t or w <temp/workspace>
;;   etc..?


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-hydra)
