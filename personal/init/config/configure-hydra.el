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

;; [2019-03-16 Sat] Commenting this func out to see if it makes key-chords
;; more intuitive for me (will now accept transpose of defined
;; (e.g. chord "eu" == "ue")
;;   - NOTE: also see `fset' call below in key-chord
;;
;; ;; Why is this needed over normal key-chord-define?
;; ;; From http://pages.sachachua.com/.emacs.d/Sacha.html#key-chord
;; ;; Is it just the "MODIFICATION" from the docstring?
;; ;;   - yes.
;; ;; TODO: delete this? Comment out? Use normal define and see if keychord is
;; ;;   any more finger-intuitive...
;; ;;   Honestly maybe it's my chiclet keyboard w/ key cover... I should get
;; ;;   a mechanical keyboard for work already...
;; (defun spydez/key-chord-define (keymap keys command)
;;   "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
;; \nKEYS can be a string or a vector of two elements. Currently only elements
;; that corresponds to ascii codes in the range 32 to 126 can be used.
;; \nCOMMAND can be an interactive function, a string, or nil.
;; If COMMAND is nil, the key-chord is removed.
;; 
;; MODIFICATION: Do not define the transposed key chord.
;; "
;;   (if (/= 2 (length keys))
;;       (error "Key-chord keys must have two elements"))
;;   ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
;;   (let ((key1 (logand 255 (aref keys 0)))
;;         (key2 (logand 255 (aref keys 1))))
;;     (define-key keymap (vector 'key-chord key1 key2) command)))
;; (fset 'key-chord-define 'spydez/key-chord-define)
;; ;; TODO: why is this fset here and in use-package key-chord :init?

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
  (key-chord-define-global (spydez/key-chord/chord key) function-symbol))

;; From https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun spydez/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Setup key chords
;; TODO: prune the list to actually used stuff
;; Trial: [2019-01-28]
(use-package key-chord
  :init
  (progn
    ;; (fset 'key-chord-define 'spydez/key-chord-define)
    
    ;; TODO: [2019-01-28] 0.16 one-key-delay feels off/fast/aweful fast. But leaving as when I
    ;; do manage it, it feels like a separate action from normal typing.
    ;; TODO: [2019-03-08] Still not using this enough and >50% of the time
    ;;   I'm not doing the chord - just inserting letters into buffers...
    (setq key-chord-one-key-delay 0.16)

    (key-chord-mode 1)

    ;; https://www.reddit.com/r/emacs/comments/22hzx7/what_are_your_keychord_abbreviations/
    ;; https://github.com/Russell91/emacs/blob/master/key-chord.el has these and maybe others:
    ;;    (key-chord-define c++-mode-map ";;"  "\C-e;")
    ;;    (key-chord-define c++-mode-map "{}"  "{\n\n}\C-p\t")

    ;; k can be bound too
    ;; TRIAL [2019-01-28]: test all these cuz I'm not sure
    (spydez/key-chord/define-global "m" 'spydez/hydra/common-stuff/body)

    (spydez/key-chord/define-global "u" 'undo)
    (spydez/key-chord/define-global "k" 'kill-whole-line)
    (spydez/key-chord/define-global "j" 'spydez/switch-to-previous-buffer)
    (spydez/key-chord/define-global "y" 'spydez/hydra/window-movement/body)
    (spydez/key-chord/define-global "w" 'switch-window)
    (spydez/key-chord/define-global "j" 'spydez/hydra/join-lines/body)
    (spydez/key-chord/define-global "f" 'helm-find-files)
    (spydez/key-chord/define-global "h" 'spydez/hydra/key-chord-commands/body)
    (spydez/key-chord/define-global "x" 'er/expand-region)

    ;; TODO: Hold down spacebar to space something out 10 or 20 spaces or whatever...
    ;;   and this gets called a lot. I don't think I like space-space as a chord?..
    ;;   M-/ might be enough for expanding. It was before this update/upgrade.
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
    ;; Trial: [2019-01-28]; Disabled: [2019-02-06]
    ;;(key-chord-define-global "  "     'spydez/insert-space-or-expand)

    ;; Not using avy right now. Similar to ace-jump or easymotion.
    ;; https://github.com/abo-abo/avy
    ;; (key-chord-define-global "jl"     'avy-goto-line)
    ;; (key-chord-define-global "jj"     'avy-goto-word-1)

    ;; Not using god-mode, but could consider. https://github.com/chrisdone/god-mode
    ;; (key-chord-define-global "vv"     'god-mode-all)
  ))


;;------------------------------------------------------------------------------
;; Hydra Hydra Hydra
;;------------------------------------------------------------------------------
;; For when you need even more emacs keyboard shenanigans.

;; TODO: A hydra for my hydras? I.e. a `hydra-launcher'?
;; I think yes. Sounds good. Maybe hydras and common random shit.
;;   mentioned:  http://nhoffman.github.io/.emacs.d/#org9119f86
;;   actual launcher hydra: http://nhoffman.github.io/.emacs.d/#org09d7a13

;; Trial: [2019-01-28]
(use-package hydra
  :config

  (defhydra spydez/hydra/window-movement ()
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

  (defhydra spydez/hydra/key-chord-commands ()
    "Main"
    ("k" kill-sexp)
    ("b" helm-buffers-list :color blue)
    ("f" find-file :color blue)
    ("." repeat)
    ("C-t" transpose-chars)
    ("m" imenu :color blue)
    ;; org-mode stuff ;; todo: do I want? these?
    ("l" org-insert-last-stored-link)
    ("c" (call-interactively 'org-capture) "capture" :color blue)
    ("t" (org-capture nil "T") "Capture task")
    ;; TODO: leave in or try to nuke from emacs entirely?
    ;; TODO: if leave in, I need the "reset scale back to default" equivaluent of these two.
    ("+" text-scale-increase)
    ("-" text-scale-decrease)
    ;; Not sure if I want any of these... 
    ;; ("q" quantified-track :color blue)
    ;; ("h" spydez/org-jump :color blue)
    ;; ("x" spydez/org-finish-previous-task-and-clock-in-new-one "Finish and clock in" :color blue)
    ;; ("i" spydez/org-quick-clock-in-task "Clock in" :color blue)
    ;; ("o" spydez/org-off-my-computer :color blue)
    ("w" spydez/hydra/engine-mode/body "web" :exit t)
    ;; ("a" spydez/org-check-agenda :color blue)
    ;; ("r" spydez/describe-random-interactive-function)
    ;; ("L" spydez/org-insert-link)
    )

  ;; TODO: define shortcut into this hydra?
  (defhydra spydez/hydra/engine-mode (:color blue)
    "Engine mode"
    ("g" engine/search-google "google")
    ("e" engine/search-emacswiki "emacswiki")
    ("w" engine/search-wikipedia "wikipedia")

    ("#" engine/search-csharp "c#")
    ("s" engine/search-stack-overflow "stack overflow")
    ("h" engine/search-github "github")

    ("m" engine/search-mail "mail")
  ))

;; TODO: Consider C-t for a hydra entry point?
;; "Hmm, good point about C-t being more useful as a Hydra than as
;; transpose-char. It turns out I actually do use C-t a fair bit, but I can
;; always add it back as an option."
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#key-chord
;; (bind-key "C-t" 'my/key-chord-commands/body)


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

;; Not sure what this does exactly.
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
