;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------When You Can Do Nothing...---------------------------
;;--                            What Can You Do?                              --
;;------------------------------------------------------------------------------
;; Make koans while the sun shines, I guess?


(require 'cl-lib)

(require 'mis2-parts)
(require 'mis2-center)
(require 'mis2-message)

;;-----------------------------There is no spoon.-------------------------------
;;--                              Koan Settings                               --
;;------------------------------------------------------------------------------

(defconst mis2/koan/line-width 90
  "Line width for formatting koans.")


(defvar mis2/koan/list nil
  "Koans loaded up and ready to go.")


;;---------------------No, really. Go get your own spoon.-----------------------
;;--                             Koan Functions                               --
;;------------------------------------------------------------------------------

(defun mis2/koan/process-line (line)
  "Returns a propertized line of a koan.

Will use `mis2/koan/line-width' if non-nil, else `fill-column'."
  (let ((faces-text (nth 1 (assoc '(mis2 koan text)
                                  mis2/type->faces)))
        (faces-other (nth 1 (assoc '(mis2 koan presence)
                                   mis2/type->faces)))
        (fill-column (or mis2/koan/line-width
                         fill-column))
        ;; §-TODO-§ [2019-11-14]: Make this more dynamic/settable.
        ;; Maybe a plist of mis2 settings or something... IDK.
        (mis2/parts/symbols-alist
         ;; redefining to get our margins in here...
         '((:newline      nil "\n")
           (:line-empty   nil "\n")
           (:string-empty nil "")
           (:line-full    mis2/center/parts
                          "" nil mis2/center/char/padding ("     " "     "))
           (:string-full  mis2/center/parts
                          "" nil mis2/center/char/padding ("     " "     ")))))

    (cond
     ;; 0) specials
     ((and (symbolp line)
           (alist-get line mis2/parts/symbols-alist))
      (mis2/parts/build line faces-other))

     ;; 1) general
     ;; a) Just a string
     ((stringp line)
      (mis2/center
       (mis2/center/parts
        line
        nil nil
        (mis2/center/margins 5)
        nil nil)
       faces-text))

     ;; b) ("string" padding-width-int)
     ((and (listp line)
           (= (length line) 2)
           (stringp (nth 0 line))
           (numberp (nth 1 line)))
      ;; center, with parts built from string and padding-width
      (mis2/center
       (mis2/center/parts
        (nth 0 line)
        nil nil
        (mis2/center/margins 5)
        nil
        (mis2/center/paddings (nth 1 line)))
       faces-text))

     ;; 3) errors
     (t
      (error "unknown line type: %s" line)
      nil))))
;; (mis2/koan/process-line :line-empty)
;; (mis2/koan/process-line :line-full)
;; (mis2/koan/process-line "Hi.")
;; (mis2/koan/process-line '("Hi." 5))
;; (mis2/koan)



(defun mis2/koan/message (lines)
  "Prints a pretty little message to the *Messages* buffer."
  (dolist (line lines)
    (mis2/message/preserve-properties nil (mis2/koan/process-line line))))
;; (mis2/koan/message '(:line-empty :line-full "Hi." :line-full :line-empty))


(defun mis2/koan/add (koan)
  "Pushes KOAN into `mis2/koan/list'."
  (push koan mis2/koan/list))


(defun mis2/koan (&optional show-buffer)
  "Prints a random koan, or the one indicated by WHICH if non-nil."
  (interactive "p")
  (when (and mis2/koan/list
             (listp mis2/koan/list))
    (mis2/koan/message (nth (random (length mis2/koan/list))
                           mis2/koan/list))
    (when show-buffer
      (pop-to-buffer "*Messages*" nil t)
      (spydez/point/to-end "*Messages*"))))
;; (mis2/koan)


;;----------------------------There is only spork.------------------------------
;;--                             Koan Collection                              --
;;------------------------------------------------------------------------------

;; (mis2/koan/message '(:line-empty :line-full "Hi." :line-full :line-empty))


;;---
;; My Own Things.
;;---

;; A Bad Koan.
;; Not really a koan, probably...
(mis2/koan/add
 '(:line-empty
   :line-full
   ("I was juggling and trying to come to a good stopping point," 7)
   ("But I had 5 balls and only two hands..." 17)
   :line-full
   ("So I came to a bad stopping point," 9)
   ("And now I'm chasing balls around instead." 7)
   ("- Programmer's Guide to the Void" 13)
   ;; There was no time scheduled to learn juggling...
   :line-full
   :line-empty))


;; Uh...
;; True Story. :|
;; Except it was svn...
(mis2/koan/add
 '(:line-empty
   :line-full
   "git commit -am \"fixed bugs caused by bug fixes\""
   "- Programmer's Guide to the Void"
   :line-full
   :line-empty))


;; Uh...
;; Also a True Story. :|
;; Every programming test I've taken, for instance...
(mis2/koan/add
 '(:line-empty
   :line-full
   "A task is an hour,"
   "Only if little goes wrong..."
   "For the first two hours."
   "- Programmer's Guide to the Void"
   :line-full
   :line-empty))


;; Just find your towel.
(mis2/koan/add
 '(:line-empty
   :line-full
   ("Mh... Panic?" 28)
   ("- Programmer's Guide to the Void" 13)
   :line-full
   :line-empty))


;;---
;; Non-me Things.
;;---

;; General Kenobi!
(mis2/koan/add
 '(:line-empty
   :line-full
   "Hello there."
   :line-full
   :line-empty))


;; Such language...
(mis2/koan/add
 '(:line-empty
   :line-full
   "You can't say 'fuck' in the source code..."
   :line-full
   :line-empty))


;; §-TODO-§ [2019-10-15]: get more from here? http://www.catb.org/~esr/writings/unix-koans/
;; http://www.catb.org/~esr/writings/unix-koans/
(mis2/koan/add
 '(:line-empty
   :line-full
   ("“Even the hacker who works alone" 13)
   ("collaborates with others," 9)
   ("and must constantly communicate clearly to them," 7)
   ("lest his work become confused and lost,”" 9)
   ("said the Master." 13)
   :line-full

   "“Of what others do you speak?” the Prodigy demanded."
   :line-full
   ("Master Foo said: “All your future selves.”" 13)
   :line-full
   :line-empty))


;; (theme song)
(mis2/koan/add
 '(:line-empty
   :line-full
   "Standby for Reincarnation..."
   "  - Futurama"
   :line-full
   :line-empty))


;; (theme song)
(mis2/koan/add
 '(:line-empty
   :line-full
   "“When You Do Things Right,"
   "People Won’t Be Sure You’ve Done Anything at All”"
   "  - Futurama"
   :line-full
   :line-empty))


;; Spybreak! - Short One
(mis2/koan/add
 '(:line-empty
   :line-full
   ("“Do not try and bend the spoon," 13)
   ("that’s impossible." 9)
   ("Instead, only try to realize the truth..." 7)
   :line-full
   "There is no spoon."
   :line-full
   ("Then you’ll see that it is not the spoon that bends," 7)
   ("it is only yourself.”" 9)
   ("    - Spoon Boy to Neo" 13)
   :line-full
   :line-empty))


;; The Programmer's Curse
(mis2/koan/add
 '(:line-empty
   :line-full
   ("~♫~99 Little Bugs In The Code!~♫~" 13)
   ("~♫~99 ♪~Little~♪ Bugs!~♫~" 15)
   ("~♫~Take One Down and Patch It Around...~♫~" 7)
   ("~♫~127 Little Bugs in the Code.~♫~" 13)
   :line-full
   ("..." 28)
   ("(programmer sobs in binary...)" 3)
   :line-full
   :line-empty))


;; Do Not Tap Glass.
;; Do Not Handle Fork-Bomb.
;; Do Not Startle Fork-Bomb.
;; No Flash Photography, Please.
(mis2/koan/add
 '(:line-empty
   :line-full
   ":(){ :|:& };:"
   :line-full
   "(kaboom)"
   :line-empty))


;;---
;; Just for good measure, "Don't Panic."
;;---

;; Just find your towel.
(mis2/koan/add
 '(:line-empty
   :line-full
   ("DON'T PANIC" 28)
   ("- Hitchhiker's Guide to the Galaxy" 13)
   :line-full
   :line-empty))

(mis2/koan/add
 '(:line-empty
   :line-full
   ("DON'T PANIC" 28)
   :line-full
   :line-empty))


;;---
;; Shuffle The Deck
;;---

;; Finally, shuffle the list?
;; (mis2/shuffle 'mis2/koan/list)
;; I randomize the get... why did I think I need this!? -_-

;; (mis2/koan)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-21]: Refactor?
;; - Move zeroith-debug.el string/message functions into here.
;; - Break here up into:
;;   - strings?  (strings-and-things.el?)
;;   - messages? (messages-and-mis2c.el?)
;;   - koans?    (koans-and-spoons.el?)
;; - `require' what's needed (or all 3?) in zeroth-debug.el
;;
;; So we have (in here right now) roughly:
;; - string/parts
;; - type->faces
;; - string/center
;; - propertize
;; - koan


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-koan)
