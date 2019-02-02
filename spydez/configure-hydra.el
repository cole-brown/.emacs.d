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

;; Why is this needed over normal key-chord-define?
;; From http://pages.sachachua.com/.emacs.d/Sacha.html#key-chord
;; Is it just the "MODIFICATION" from the docstring?
(defun spydez/key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

MODIFICATION: Do not define the transposed key chord.
"
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))
(fset 'key-chord-define 'spydez/key-chord-define)
;; TODO: why is this fset here and in use-package key-chord :init?

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
    (fset 'key-chord-define 'spydez/key-chord-define)
    ;; TODO: [2019-01-28] 0.16 one-key-delay feels off/fast/aweful fast. But leaving as when I
    ;; do manage it, it feels like a separate action from normal typing.
    (setq key-chord-one-key-delay 0.16)
    
    (key-chord-mode 1)
    
    ;; k can be bound too
    ;; TODO: test all these cuz I'm not sure
    (key-chord-define-global "uu"     'undo)
    (key-chord-define-global "kk"     'kill-whole-line)
    (key-chord-define-global "jj"     'spydez/switch-to-previous-buffer)
    (key-chord-define-global "yy"     'spydez/window-movement/body)
    (key-chord-define-global "jw"     'switch-window)
    (key-chord-define-global "j."     'join-lines/body)
    (key-chord-define-global "FF"     'find-file) ;; TODO: helm or ido find instead?
    (key-chord-define-global "hh"     'spydez/key-chord-commands/body)
    (key-chord-define-global "xx"     'er/expand-region)

    ;; TODO: Hold down spacebar to space something out 10 or 20 spaces or whatever...
    ;;   and this gets called. I don't think I like space-space as a chord?..
    ;;   M-/ might be enough for expanding. It was before this update/upgrade.
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
    (key-chord-define-global "  "     'spydez/insert-space-or-expand)

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

;; Trial: [2019-01-28]
(use-package hydra
  :config

  ;; "yy" key chord
  (defhydra spydez/window-movement ()
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

  ;; "j." key chord
  (defhydra join-lines ()
    ("<up>" join-line)
    ("<down>" (join-line 1))
    ("t" join-line)
    ("n" (join-line 1)))

  ;; There was a hydra for Quantified Awesome/Org Clock In that looked neat maybe.
  ;; http://pages.sachachua.com/.emacs.d/Sacha.html#key-chord
  ;; Can add it back in if I start clocking stuff.

  ;; Could use this if I identify some org things I do a lot...
  ;; (defhydra spydez/org (:color blue)
  ;;   "Convenient Org stuff."
  ;;   ("p" spydez/org-show-active-projects "Active projects")
  ;;   ("a" (org-agenda nil "a") "Agenda"))

  ;; "hh" key chord
  (defhydra spydez/key-chord-commands ()
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
    ;; ("w" spydez/engine-mode-hydra/body "web" :exit t)
    ;; ("a" spydez/org-check-agenda :color blue)
    ;; ("r" spydez/describe-random-interactive-function)
    ;; ("L" spydez/org-insert-link)
    )

  ;; TODO: define shortcut into this hydra?
  (defhydra spydez/engine-mode-hydra (:color blue)
    "Engine mode"
    ("b" engine/search-my-blog "blog") ;; probably no
    ("f" engine/search-my-photos "flickr") ;; probably no
    ("m" engine/search-mail "mail") ;; probably no
    ("g" engine/search-google "google") ;; maybe?
    ("e" engine/search-emacswiki "emacswiki")) ;; maybe?
  )

;; TODO: Consider C-t for a hydra entry point?
;; "Hmm, good point about C-t being more useful as a Hydra than as
;; transpose-char. It turns out I actually do use C-t a fair bit, but I can
;; always add it back as an option."
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html#key-chord
;; (bind-key "C-t" 'my/key-chord-commands/body)


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


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-hydra)
