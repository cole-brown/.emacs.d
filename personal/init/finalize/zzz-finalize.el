;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;------------------------------------------------------------------------------
;; The Final Chance for Others to Do a Thing
;;------------------------------------------------------------------------------

;; Run it!
(run-hooks 'spydez/hook-runner/finalize/final-finalities)


;;------------------------------------------------------------------------------
;; Final Buffer Things?
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-11-05]: split window, switch one to Messages?

;;------------------------------------------------------------------------------
;; Debugging: clean up and finalize
;;------------------------------------------------------------------------------

;; show benchmark if debugging
;; https://github.com/dholm/benchmark-init-el
(when (spydez/debugging-p)
  (benchmark-init/show-durations-tree))
;;(benchmark-init/show-durations-tabulated))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zzz-finalize)
