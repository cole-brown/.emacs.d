;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


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
  (spydez/warning/message nil nil "Key-Chord or Hydra package not present. Cannot make spydez/common-stuff."))
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
