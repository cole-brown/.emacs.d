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
    :center t)
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

;; (defun mis2/nomer/process-line (line)
;;   "Returns a propertized line of a nomer.

;; Will use `mis2/nomer/line-width' if non-nil, else `fill-column'."
;;   (let ((faces-text (nth 1 (assoc '(mis2 nomer text)
;;                                   mis2/type->faces)))
;;         (faces-other (nth 1 (assoc '(mis2 nomer presence)
;;                                    mis2/type->faces)))
;;         (fill-column (or mis2/nomer/line-width
;;                          fill-column))
;;         ;; §-TODO-§ [2019-11-14]: Make this more dynamic/settable.
;;         ;; Maybe a plist of mis2 settings or something... IDK.
;;         (mis2/parts/symbols-alist
;;          ;; redefining to get our margins in here...
;;          '((:newline      nil "\n")
;;            (:line-empty   nil "\n")
;;            (:string-empty nil "")
;;            (:line-full    mis2/center/parts
;;                           "" nil mis2/center/char/padding ("     " "     "))
;;            (:string-full  mis2/center/parts
;;                           "" nil mis2/center/char/padding ("     " "     ")))))

;;     (cond
;;      ;; 0) specials
;;      ((and (symbolp line)
;;            (alist-get line mis2/parts/symbols-alist))
;;       (mis2/parts/build line faces-other))

;;      ;; 1) general
;;      ;; a) Just a string
;;      ((stringp line)
;;       (mis2/center
;;        (mis2/center/parts
;;         line
;;         nil nil
;;         (mis2/center/margins 5)
;;         nil nil)
;;        faces-text))

;;      ;; b) ("string" padding-width-int)
;;      ((and (listp line)
;;            (= (length line) 2)
;;            (stringp (nth 0 line))
;;            (numberp (nth 1 line)))
;;       ;; center, with parts built from string and padding-width
;;       (mis2/center
;;        (mis2/center/parts
;;         (nth 0 line)
;;         nil nil
;;         (mis2/center/margins 5)
;;         nil
;;         (mis2/center/paddings (nth 1 line)))
;;        faces-text))

;;      ;; 3) errors
;;      (t
;;       (error "unknown line type: %s" line)
;;       nil))))
;; ;; (mis2/nomer/process-line :line-empty)
;; ;; (mis2/nomer/process-line :line-full)
;; ;; (mis2/nomer/process-line "Hi.")
;; ;; (mis2/nomer/process-line '("Hi." 5))
;; ;; (mis2/nomer)


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


;; ;; Uh...
;; ;; True Story. :|
;; ;; Except it was svn...
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    "git commit -am \"fixed bugs caused by bug fixes\""
;;    "- Programmer's Guide to the Void"
;;    :line-full
;;    :line-empty))


;; ;; Uh...
;; ;; Also a True Story. :|
;; ;; Every programming test I've taken, for instance...
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    "A task is an hour,"
;;    "Only if little goes wrong..."
;;    "For the first two hours."
;;    "- Programmer's Guide to the Void"
;;    :line-full
;;    :line-empty))


;; ;; Just find your towel.
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    ("Mh... Panic?" 28)
;;    ("- Programmer's Guide to the Void" 13)
;;    :line-full
;;    :line-empty))


;; ;; Such language...
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    "You can't say 'fuck' in the source code..."
;;    :line-full
;;    :line-empty))


;; ;;---
;; ;; Non-me Things.
;; ;;---

;; ;; General Kenobi!
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    "Hello there."
;;    :line-full
;;    :line-empty))


;; ;; §-TODO-§ [2019-10-15]: get more from here? http://www.catb.org/~esr/writings/unix-nomers/
;; ;; http://www.catb.org/~esr/writings/unix-nomers/
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    ("“Even the hacker who works alone" 13)
;;    ("collaborates with others," 9)
;;    ("and must constantly communicate clearly to them," 7)
;;    ("lest his work become confused and lost,”" 9)
;;    ("said the Master." 13)
;;    :line-full

;;    "“Of what others do you speak?” the Prodigy demanded."
;;    :line-full
;;    ("Master Foo said: “All your future selves.”" 13)
;;    :line-full
;;    :line-empty))


;; ;; (theme song)
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    "Standby for Reincarnation..."
;;    "  - Futurama"
;;    :line-full
;;    :line-empty))


;; ;; (theme song)
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    "“When You Do Things Right,"
;;    "People Won’t Be Sure You’ve Done Anything at All”"
;;    "  - Futurama"
;;    :line-full
;;    :line-empty))


;; ;; Spybreak! - Short One
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    ("“Do not try and bend the spoon," 13)
;;    ("that’s impossible." 9)
;;    ("Instead, only try to realize the truth..." 7)
;;    :line-full
;;    "There is no spoon."
;;    :line-full
;;    ("Then you’ll see that it is not the spoon that bends," 7)
;;    ("it is only yourself.”" 9)
;;    ("    - Spoon Boy to Neo" 13)
;;    :line-full
;;    :line-empty))


;; ;; The Programmer's Curse
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    ("~♫~99 Little Bugs In The Code!~♫~" 13)
;;    ("~♫~99 ♪~Little~♪ Bugs!~♫~" 15)
;;    ("~♫~Take One Down and Patch It Around...~♫~" 7)
;;    ("~♫~127 Little Bugs in the Code.~♫~" 13)
;;    :line-full
;;    ("..." 28)
;;    ("(programmer sobs in binary...)" 3)
;;    :line-full
;;    :line-empty))


;; ;; Do Not Tap Glass.
;; ;; Do Not Handle Fork-Bomb.
;; ;; Do Not Startle Fork-Bomb.
;; ;; No Flash Photography, Please.
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    ":(){ :|:& };:"
;;    :line-full
;;    "(kaboom)"
;;    :line-empty))


;; ;;---
;; ;; Just for good measure, "Don't Panic."
;; ;;---

;; ;; Just find your towel.
;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    ("DON'T PANIC" 28)
;;    ("- Hitchhiker's Guide to the Galaxy" 13)
;;    :line-full
;;    :line-empty))

;; (mis2/nomer/add
;;  '(:line-empty
;;    :line-full
;;    ("DON'T PANIC" 28)
;;    :line-full
;;    :line-empty))


;; ;; (mis2/nomer)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-nomer)
