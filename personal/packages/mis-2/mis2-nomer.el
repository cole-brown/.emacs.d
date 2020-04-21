;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------When You Can Do Nothing...---------------------------
;;--                            What Can You Do?                              --
;;------------------------------------------------------------------------------
;;                 Make koans while the sun shines, I guess?
;;                                 ----------
;;  Except these aren't really koans, mostly. Just quotes or stupid stuff...
;;                          So "koan" is a misnomer.
;;
;;                                    ...
;;
;;       ...therefore "nomer"! mis/nomer? Get it? Because this is mis?
;;------------------------------(just humor me)---------------------------------

(require 'dash)
(require 's)

(require 'mis2-utils)
(require 'mis2-settings)
;; (require 'mis2-contents)
(require 'mis2-message)


;;-----------------------------There is no spoon.-------------------------------
;;--                             Nomer Settings                               --
;;------------------------------------------------------------------------------

(defcustom mis2/nomer/settings/user
  '(:theme :nomer
    :line-width 90)
  "Variable for user to put their default/most used settings into
for mis/nomers.
"
  :group 'mis2)


(defcustom mis2/nomer/style/user
  '(:indent 5
    :center t
    :borders ("||" "||"))
  "Variable for user to put their default/most used stylings into
for mis/nomers.
"
  :group 'mis2)


;;--------------
;; Consts & Vars
;;--------------

(defvar mis2/nomer/list nil
  "Nomers loaded up and ready to go.")


;;---------------------No, really. Go get your own spoon.-----------------------
;;--                            Nomer Functions                               --
;;------------------------------------------------------------------------------

(defun mis2/nomer/output (lines)
  "Prints a pretty little message to the *Messages* buffer.
"
  (mis2/block :settings mis2/nomer/settings/user
              :style    mis2/nomer/style/user
              lines))
;; (mis2/nomer/output '(:empty :full "Hi." :full :empty))


(defun mis2/nomer/add (nomer)
  "Pushes NOMER into `mis2/nomer/list'.
"
  (push nomer mis2/nomer/list))


(defun mis2/nomer/random ()
  "Pulls a random nomer from `mis2/nomer/list'.
"
  (nth (random (length mis2/nomer/list))
       mis2/nomer/list))


(defun mis2/nomer (&optional show-buffer)
  "Prints a random nomer. Pops to buffer if SHOW-BUFFER is non-nil.
"
  (interactive "p")
  (when (mis2//list-exists? mis2/nomer/list)
    (mis2/nomer/output (mis2/nomer/random)))

  (when show-buffer
    (pop-to-buffer "*Messages*" nil t)
    (spydez/point/to-end "*Messages*")))
;; (mis2/nomer)


;;----------------------------There is only spork.------------------------------
;;--                            Nomer Collection                              --
;;------------------------------------------------------------------------------

;; (mis2/nomer/message '(:line-empty :line-full "Hi." :line-full :line-empty))


;;---
;; My Own Things.
;;---

;; A Bad Koan.
;; Not really a koan, probably...
(mis2/nomer/add
 '(:empty
   :full
   (:padding (?- :fill 7) "I was juggling and trying to come to a good stopping point,")
   (:padding (?- :fill 17) "But I had 5 balls and only two hands...")
   :full
   (:padding (?- :fill 9) "So I came to a bad stopping point,")
   (:padding (?- :fill 7) "And now I'm chasing balls around instead.")
   (:padding (?- :fill 13) "- Programmer's Guide to the Void")
   ;; There was no time scheduled to learn juggling...
   :full
   :empty))


;; Uh...
;; True Story. :|
;; Except it was svn...
(mis2/nomer/add
 '(:empty
   :full
   "git commit -am \"fixed bugs caused by bug fixes\""
   "- Programmer's Guide to the Void"
   :full
   :empty))


;; Uh...
;; Also a True Story. :|
;; Every programming test I've taken, for instance...
(mis2/nomer/add
 '(:empty
   :full
   "A task is an hour,"
   "Only if little goes wrong..."
   "For the first two hours."
   "- Programmer's Guide to the Void"
   :full
   :empty))


;; Just find your towel.
(mis2/nomer/add
 '(:empty
   :full
   (:padding (?- :fill 28) "Mh... Panic?")
   (:padding (?- :fill 13) "- Programmer's Guide to the Void")
   :full
   :empty))


;; Such language...
(mis2/nomer/add
 '(:empty
   :full
   "You can't say 'fuck' in the source code..."
   :full
   :empty))


;;---
;; Non-me Things.
;;---

;; General Kenobi!
(mis2/nomer/add
 '(:empty
   :full
   "Hello there."
   :full
   :empty))


;; §-TODO-§ [2019-10-15]: get more from here? http://www.catb.org/~esr/writings/unix-nomers/
;; http://www.catb.org/~esr/writings/unix-nomers/
(mis2/nomer/add
 '(:empty
   :full
   (:padding (?- :fill 13) "“Even the hacker who works alone")
   (:padding (?- :fill  9) "collaborates with others,")
   (:padding (?- :fill  7) "and must constantly communicate clearly to them,")
   (:padding (?- :fill  9) "lest his work become confused and lost,”")
   (:padding (?- :fill 13) "said the Master.")
   :full
   "“Of what others do you speak?” the Prodigy demanded."
   :full
   (:padding (?- :fill 13) "Master Foo said: “All your future selves.”")
   :full
   :empty))


;; (theme song)
(mis2/nomer/add
 '(:empty
   :full
   "Standby for Reincarnation..."
   "  - Futurama"
   :full
   :empty))


;; (theme song)
(mis2/nomer/add
 '(:empty
   :full
   "“When You Do Things Right,"
   "People Won’t Be Sure You’ve Done Anything at All”"
   "  - Futurama"
   :full
   :empty))


;; Spybreak! - Short One
(mis2/nomer/add
 '(:empty
   :full
   (:padding (?- :fill 13) "“Do not try and bend the spoon,")
   (:padding (?- :fill  9) "that’s impossible.")
   (:padding (?- :fill  7) "Instead, only try to realize the truth...")
   :full
   "There is no spoon."
   :full
   (:padding (?- :fill  7) "Then you’ll see that it is not the spoon that bends,")
   (:padding (?- :fill  9) "it is only yourself.”")
   (:padding (?- :fill 13) "    - Spoon Boy to Neo")
   :full
   :empty))


;; The Programmer's Curse
(mis2/nomer/add
 '(:empty
   :full
   (:padding (?- :fill 13) "~♫~99 Little Bugs In The Code!~♫~")
   (:padding (?- :fill 15) "~♫~99 ♪~Little~♪ Bugs!~♫~")
   (:padding (?- :fill  7) "~♫~Take One Down and Patch It Around...~♫~")
   (:padding (?- :fill 13) "~♫~127 Little Bugs in the Code.~♫~")
   :full
   (:padding (?- :fill 28) "...")
   (:padding (?- :fill 3) "(programmer sobs in binary...)")
   :full
   :empty))


;; Do Not Tap Glass.
;; Do Not Handle Fork-Bomb.
;; Do Not Startle Fork-Bomb.
;; No Flash Photography, Please.
(mis2/nomer/add
 '(:empty
   :full
   ":(){ :|:& };:"
   :full
   "(kaboom)"
   :empty))


;;---
;; Just for good measure, "Don't Panic."
;;---

;; Just find your towel.
(mis2/nomer/add
 '(:empty
   :full
   (:padding (?- :fill 28) "DON'T PANIC")
   (:padding (?- :fill 13) "- Hitchhiker's Guide to the Galaxy")
   :full
   :empty))

(mis2/nomer/add
 '(:empty
   :full
   (:padding (?- :fill 28) "DON'T PANIC")
   :full
   :empty))


;; (mis2/nomer)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-nomer)
