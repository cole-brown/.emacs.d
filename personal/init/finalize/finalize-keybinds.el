;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------
(require 'with)

;;------------------------------------------------------------------------------
;; All of the transposes!
;;------------------------------------------------------------------------------

;; If I still need better/less common bigrams/chords...
;; https://english.stackexchange.com/a/110579
;; https://www.reddit.com/r/emacs/comments/22hzx7/what_are_your_keychord_abbreviations/

;; https://github.com/abo-abo/hydra/wiki/Emacs#Transpose
;; Hydra for all the transposes because all I ever remember is letter and word.
(with-all-features '(key-chord hydra)
  (defhydra spydez/hydra/transpose (:color red)
    "Transpose"
    ("c" transpose-chars "characters")
    ("w" transpose-words "words")
    ("l" transpose-lines "lines")
    ("s" transpose-sentences "sentences")
    ("p" transpose-paragraphs "paragraphs")
    ("x" transpose-sexps "sexprs")
    ("o" org-transpose-words "Org mode words")
    ("e" org-transpose-elements "Org mode elements")
    ("t" org-table-transpose-table-at-point "Org mode table")
    ("q" nil "cancel" :color blue))
  ;; Not sure about key chord... "cg" maybe better?
  ;; Going with 'tp' transpose and also 'p' is in the common-stuff hydra
  (spydez/key-chord/define-global "t" 'spydez/hydra/transpose/body))
;;  ;; else
;;  (mis/warning nil nil
;;                          "Key-Chord or Hydra package not present. Cannot make spydez/hydra/transpose."))


;;------------------------------------------------------------------------------
;; Common Actions
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-01]: nicer hydra help w/ less effort?
;;   https://oremacs.com/2015/07/20/hydra-columns/
;; Namely, there seems to be no good way of defining a format string var in a
;; non-global way in the defhydras?.. -_- And every time a hydra is change a lot
;; of tweaking is done on the docstring for the comments and such.

;;---
;; Hydra for some random collection of things I happen to do a lot...
;;---
(with-feature 'hydra
  ;; Our "Sigs, Org-Mode, and Misc Stuff We Do Often" hydra.
  (defhydra spydez/hydra/grab-bag (:color blue ;; default exit heads
                                   :idle 0.75  ;; no help for x seconds
                                   :hint none) ;; no hint - just docstr
    ;; [2019-10-28]: So I have a Heisenbug right now... Calling this hydra the
    ;; 1st (or 1st three?!) times after startup (maybe only if called w/o doing
    ;; (much) else?) results in:
    ;;   "Error running timer: (wrong-type-argument stringp nil)"
    ;;
    ;; But if I set `debug-on-error' it never happens. And I'm not sure what
    ;; possible string is nil. Or why stringp dies on nil in whatever case it is
    ;; dying on.
    ;; Yay...
    "
^Signatures^                           | ^Org-Mode^                      | ^Sig/TODO Search^         | ^Align^
^----------^---------------------------+-^--------^----------------------+-^---------------^---------+-^-----^-----------------------
_t c_: ?t c?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^| _h n_: (home) New Journal Entry | _s m_: ?s m?^^^^^^^^^^^^^ | _; a_: Align Region Before...
_t t_: ?t t?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^| _h v_: (home) Visit Journal     | _s s_: Search...          | _; o_: Align Region After....
_t g_: ?t g?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^| _w n_: (work) New Journal Entry | ^   ^                     | _; ;_: Align Regexp...
_t h_: ?t h?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^| _w v_: (work) Visit Journal     | ^   ^                     | _; q_: Complex Align Regexp...
_n n_: ?n n?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^| ^   ^                           | ^   ^                     | _a_:   Align Region
_n s_: ?n s?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^| _p h_: Pomodoro History         | ^   ^                     | _'_:   Align Current
_n m_: ?n m?^^^^^^^^^^^^^^^^^^^^^^^^^^^^^| _p s_: Pomodoro Start           |
_e w_: ?e w?
_e c_: ?e c?
_e p_: ?e p?
"
    ;; "%-26" for right-padded string, which should as long as these,
    ;; which should be longest signatures... Update as needed.
    ;;   (length (spydez/signature/todo/comment t))
    ;;   (length spydez/signature/name-post)
    ;;
    ;; Also include signature type in "%-5s":
    ;;   - "sig:  "  -> signature
    ;;   - "mark: " -> one-char signature
    ;;   - "todo: " -> TODO signature
    ;;   - "comm: " -> TODO comment signature
    ;;
    ;; If comments are not relevant, they will be marked as "N/A" and do nothing
    ;; when invoked.


    ;;-------------------------------------------------------------------------
    ;; Signatures
    ;;-------------------------------------------------------------------------

    ;;---
    ;; TODO Signatures
    ;;---

    ;; Timestamped
    ("t c" (mis/comment/unless (insert (spydez/signature/todo/comment t)))
     (apply #'format "%-5s %-26s"
                     (if (mis/comment/ignore)
                         '("" "  (N/A for Major Mode)")
                       `("todo:"
                         ,(spydez/signature/todo/comment t)))))
    ("t t" (insert (spydez/signature/todo/timestamp t))
     (format "%-5s %-26s" "todo:" (spydez/signature/todo/timestamp t)))

    ;; Bare
    ("t g" (mis/comment/unless (insert (spydez/signature/todo/comment nil)))
     (apply #'format "%-5s %-26s"
                     (if (mis/comment/ignore)
                         '("" "  (N/A for Major Mode)")
                       `("todo:"
                         ,(spydez/signature/todo/comment nil)))))
    ("t h" (insert (spydez/signature/todo/timestamp nil))
     (format "%-5s %-26s" "todo:" (spydez/signature/todo/timestamp nil)))

    ;;---
    ;; Normal Signatures
    ;;---
    ("n n" (spydez/signature/insert spydez/signature/short-pre)
     (format "%-5s %-26s" "note:" spydez/signature/short-pre))
    ("n s" (spydez/signature/insert spydez/signature/name-post)
     (format "%-5s %-26s" "note:" spydez/signature/name-post))

    ;; just a mark char
    ("n m" (spydez/signature/insert spydez/signature/char)
     (format "%-5s %-26s" "mark:" spydez/signature/char))


    ;;---
    ;; Email Addresses
    ;;---
    ;; Could truncate w/ "..." if not enough room to work with.
    ("e c" (insert (spydez/signature/email/get :code))
     (format "%-8s %-23s" "email c:" (spydez/signature/email/get :code)))
    ("e w" (insert (spydez/signature/email/get :code))
     (format "%-8s %-23s" "email w:" (spydez/signature/email/get :work)))
    ("e p" (insert (spydez/signature/email/get :code))
     (format "%-8s %-23s" "email p:" (spydez/signature/email/get :personal)))


    ;;-----------------------------------------------------------------------
    ;; Org-Mode
    ;;-----------------------------------------------------------------------

    ;;---
    ;; Org-Journal
    ;;---
    ("h n" (let ((org-journal-dir (spydez/dirky/path :home :logbook))
                 (org-journal-file-format
                  (spydez/jerky/get 'custom 'org-journal 'filename :home)))
             (org-journal-new-entry nil)))
    ("h v" (let ((org-journal-dir (spydez/dirky/path :home :logbook))
                 (org-journal-file-format
                  (spydez/jerky/get 'custom 'org-journal 'filename :home)))
             (funcall org-journal-find-file
              (org-journal-get-entry-path))))
    ("w n" (let ((org-journal-dir (spydez/dirky/path :work :logbook))
                 (org-journal-file-format
                  (spydez/jerky/get 'custom 'org-journal 'filename :work)))
             (org-journal-new-entry nil)))
    ("w v" (let ((org-journal-dir (spydez/dirky/path :work :logbook))
                 (org-journal-file-format
                  (spydez/jerky/get 'custom 'org-journal 'filename :work)))
             (funcall org-journal-find-file
              (org-journal-get-entry-path))))

    ;;-----------------------------------------------------------------------
    ;; Redtick Pomodoro Timer
    ;;-----------------------------------------------------------------------

    ("p h" (find-file redtick-history-file))
    ("p s" (funcall #'redtick))

    ;;-------------------------------------------------------------------------
    ;; Search
    ;;-------------------------------------------------------------------------

    ("s m" (spydez/signature/search spydez/signature/char)
     (format "Search for Mark: %s" spydez/signature/char))
    ("s s" (call-interactively #'spydez/signature/search)
     "Search for Signature...")

    ;;-------------------------------------------------------------------------
    ;; Alignment
    ;;-------------------------------------------------------------------------
    ("; a" (call-interactively #'spydez/align-before))
    ("; o" (call-interactively #'spydez/align-after))
    ("; ;" (call-interactively #'align-regexp))
    ("; q" (lambda () (interactive)
             (setq current-prefix-arg '(4))
             (call-interactively #'align-regexp)))
    ("a" (call-interactively #'align))
    ("'" (call-interactively #'align-current)))

  ;; global keybind (can override minor mode binds)
  (bind-key* "C-," #'spydez/hydra/grab-bag/body))

;; (with-function 'spydez/hydra/grab-bag/body
;;   ;; global keybind (can override minor mode binds)
;;   (bind-key* "C-," spydez/hydra/grab-bag/body))


;;---
;; Dynamic Hydra that Doesn't Work.
;;---
;; I want this one to be a way to visit and open common files...
;; Haven't wrapped my head around dynamic hydras yet though.
(with-all-features '(key-chord hydra)
  (defhydra spydez/hydra/common-stuff (:color blue)
    "Common Actions/Files"

    ;; this already has its own hydra
    ("w" spydez/hydra/engine-mode/body "web search" :exit t)

    ;; (if (bound-and-true-p spydez/file/auto-open-list)
    ;;     (let (;;(file-list spydez/file/auto-open-list)
    ;;           (i 1))
    ;;       (dolist (file spydez/file/auto-open-list)
    ;;         ;; hydra head is 1 through 0, so iter 1 through 10
    ;;         (when (and (<= i 10)
    ;;                    (not (null file)))
    ;;           ;; and then mod 10 to string for the head keybind
    ;;           ((number-to-string (% i 10))
    ;;            ;; our specific file in a lambda...
    ;;            (lambda () (find-file file))
    ;;            ;; and a helpful docstring
    ;;            (format "open %s" (file-name-nondirectory file))
    ;;            )))))
    )

  ;; "eu" is super convenient on Dvorak, but it's a bad combo for English words...
  ;; ".p" might work better... (one row up)
  ;; "-p"? Key chords that aren't English or Codelish are hard..ep.
  ;; double dash for the common hydra?
  (spydez/key-chord/define-global "-" 'spydez/hydra/common-stuff/body))
;; [2019-09-20]Switched from "if featurep"s to with, so lost this fail warning.
;; (mis/warning nil nil
;;                         "Key-Chord or Hydra package not present. Cannot make spydez/hydra/common-stuff."))


;; TODO: make `with' better?
;; (<with-macro> <args>
;;  (:error <body>)
;;  <standard body>)


;; TODO: make a hydra here for... files and stuff with 'eu' keychord.
;;    TODO: add a 'reload-init' type func in it if I'm in the right project or something?


;; TODO: need macros and stuff for the open-files bit, I think?
;; And this looks like a close-enough-to-start:
;;   https://www.reddit.com/r/emacs/comments/3ba645/does_anybody_have_any_real_cool_hydras_to_share/cskfthg
;; soo.... what do I want? In the middle fo a defhydra, I want to provide two
;; lists,
;;   (spydez/py-range 0 10)
;; and
;;   'auto-open-files
;; and have it generate those entries.
;;
;; Alternatively we can do the hint define thing to have it just put them
;; in every time the hydra is called?


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'finalize-keybinds)
