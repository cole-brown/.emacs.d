;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------d20---------------------------------------
;;--     Here there be dice, dungeons, dragons, paths, and stars to find.     --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Org-Mode: org-d20
;;------------------------------------------------------------------------------

;; https://www.reddit.com/r/emacs/comments/aly9jh/minor_mode_for_dungeon_masters/
;; https://github.com/spwhitton/org-d20
(use-package org-d20)

;; Usage:
;;   - https://github.com/spwhitton/org-d20#usage
;;
;; It's useful to activate the mode automatically when you open the Org-mode
;; file in which you are keeping your campaign notes. You can also specify the
;; names of your party members, and their initiative modifiers, if you want to
;; use org-d20's combat tracker.
;;
;; You can either end the Org-file with a footer like this:
;;
;;  # Local Variables:
;;  # mode: org
;;  # mode: org-d20
;;  # org-d20-party: (("Zahrat" . 2) ("Ennon" . 4) ("Artemis" . 5))
;;  # End: ;; NOTE: stop this line at 'End:' to have emacs notice it.
;;
;; or start it with a first line like this:
;;
;; # -*- mode: org; mode: org-d20; org-d20-party: (("Zahrat" . 0) ("Anca" . 1)) -*-
;;
;; Then close and reopen the file. Emacs will probably ask you to confirm that
;; the value for org-d20-party that you've supplied is safe.
;;
;; The following bindings should then be available:
;;
;;     C-c , i -- start a combat, or advance the turn/round counter for an existing combat, depending on point
;;     C-c , a -- add new monsters to an existing combat
;;     C-c , d -- apply damage to the monster at point
;;     C-c , r -- prompt for dice expression and evaluate it
;;     <f10> -- evaluate the dice expression (e.g. 4d10) at point
;;     <f11> -- roll the last dice expression again
;;     <f12> -- roll d20, with advantage and disadvantage shown
;;     S-<f12> -- roll percentile dice


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: find or make a lisp function dice roller? like for dice ('xdy') and consts.
;; org-d20's <f10> does it for human-formatted (e.g. 4d10+11) dice expressions
;; so probably it's hiding a function that'll do it for lisp formatted.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-dice)
