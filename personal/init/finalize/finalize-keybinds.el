;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; All of the transposes!
;;------------------------------------------------------------------------------

;; TODO: better/less common bigrams... "script" is triggering my "tp" transpose hydra
;; https://english.stackexchange.com/a/110579
;; https://www.reddit.com/r/emacs/comments/22hzx7/what_are_your_keychord_abbreviations/

;; https://github.com/abo-abo/hydra/wiki/Emacs#Transpose
(if (and (featurep 'key-chord) ;; TODO: with-features
         (featurep 'hydra))
    ;; then
    (progn
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
      (key-chord-define-global "tp" 'spydez/hydra/transpose/body))
  ;; else
  (spydez/warning/message nil nil
                          "Key-Chord or Hydra package not present. Cannot make spydez/hydra/transpose."))


;;------------------------------------------------------------------------------
;; Common Actions
;;------------------------------------------------------------------------------

(if (and (featurep 'key-chord)
         (featurep 'hydra))
    (progn
      (defhydra spydez/hydra/common-stuff (:color blue)
        "Common Actions/Files"

        ("w" spydez/engine-mode-hydra/body "web search" :exit t)

;;         (if (bound-and-true-p spydez/file/auto-open-list)
;;             (let (;;(file-list spydez/file/auto-open-list)
;;                   (i 1))
;;               (dolist (file spydez/file/auto-open-list)
;;                 ;; hydra head is 1 through 0, so iter 1 through 10
;;                 (when (and (<= i 10)
;;                            (not (null file)))
;;                   ;; and then mod 10 to string for the head keybind
;;                   ((number-to-string (% i 10))
;;                    ;; our specific file in a lambda...
;;                    (lambda () (find-file file))
;;                    ;; and a helpful docstring
;;                    (format "open %s" (file-name-nondirectory file))
;;                    )))))
        )
      
      ;; "eu" is super convenient on Dvorak, but it's a bad combo for English words...
      ;; ".p" might work better... (one row up)
      (key-chord-define-global ".p" 'spydez/hydra/common-stuff/body)
      )
  (spydez/warning/message nil nil
                          "Key-Chord or Hydra package not present. Cannot make spydez/hydra/common-stuff."))
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
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'finalize-keybinds)
