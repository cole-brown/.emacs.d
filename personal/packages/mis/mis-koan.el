;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------When You Can Do Nothing...---------------------------
;;--                            What Can You Do?                              --
;;------------------------------------------------------------------------------
;; Make koans while the sun shines, I guess?


(require 'cl-lib)

(require 'mis-parts)
(require 'mis-center)
(require 'mis-message)

;;-----------------------------There is no spoon.-------------------------------
;;--                              Koan Settings                               --
;;------------------------------------------------------------------------------

(defconst mis/koan/line-width 90
  "Line width for formatting koans.")


(defvar mis/koan/list nil
  "Koans loaded up and ready to go.")


;;---------------------No, really. Go get your own spoon.-----------------------
;;--                             Koan Functions                               --
;;------------------------------------------------------------------------------

(defun mis/koan/process-line (line)
  "Returns a propertized line of a koan.

Will use `mis/koan/line-width' if non-nil, else `fill-column'."
  (let ((faces-text (nth 1 (assoc '(mis koan text)
                                  mis/type->faces)))
        (faces-other (nth 1 (assoc '(mis koan presence)
                                   mis/type->faces)))
        (fill-column (or mis/koan/line-width
                         fill-column))
        ;; §-TODO-§ [2019-11-14]: Make this more dynamic/settable.
        ;; Maybe a plist of mis settings or something... IDK.
        (mis/parts/symbols-alist
         ;; redefining to get our margins in here...
         '((newline      nil "\n")
           (line-empty   nil "\n")
           (string-empty nil "")
           (line-full   mis/center/parts
                        "" nil mis/center/char/padding ("     " "     "))
           (string-full mis/center/parts
                        "" nil mis/center/char/padding ("     " "     ")))))

    (cond
     ;; 0) specials
     ((and (symbolp line)
           (alist-get line mis/parts/symbols-alist))
      (mis/parts/build line faces-other))

     ;; 1) general
     ;; a) Just a string
     ((stringp line)
      (mis/center
       (mis/center/parts
        line
        nil nil
        (mis/center/margins 5)
        nil nil)
       faces-text))

     ;; b) ("string" padding-width-int)
     ((and (listp line)
           (= (length line) 2)
           (stringp (nth 0 line))
           (numberp (nth 1 line)))
      ;; center, with parts built from string and padding-width
      (mis/center
       (mis/center/parts
        (nth 0 line)
        nil nil
        (mis/center/margins 5)
        nil
        (mis/center/paddings (nth 1 line)))
       faces-text))

     ;; 3) errors
     (t
      (error "unknown line type: %s" line)
      nil))))
;; (mis/koan/process-line 'line-empty)
;; (mis/koan/process-line 'line-full)
;; (mis/koan/process-line "Hi.")
;; (mis/koan/process-line '("Hi." 5))
;; (mis/koan)



(defun mis/koan/message (lines)
  "Prints a pretty little message to the *Messages* buffer."
  (dolist (line lines)
    (mis/message/preserve-properties nil (mis/koan/process-line line))))
;; (mis/koan/message '(line-empty line-full "Hi." line-full line-empty))


(defun mis/koan/add (koan)
  "Pushes KOAN into `mis/koan/list'."
  (push koan mis/koan/list))


(defun mis/koan (&optional show-buffer)
  "Prints a random koan, or the one indicated by WHICH if non-nil."
  (interactive "p")
  (when (and mis/koan/list
             (listp mis/koan/list))
    (mis/koan/message (nth (random (length mis/koan/list))
                           mis/koan/list))
    (when show-buffer
      (pop-to-buffer "*Messages*" nil t)
      (spydez/point/to-end "*Messages*"))))
;; (mis/koan)


;;----------------------------There is only spork.------------------------------
;;--                             Koan Collection                              --
;;------------------------------------------------------------------------------

;; (mis/koan/message '(line-empty line-full "Hi." line-full line-empty))


;; General Kenobi!
(mis/koan/add
  '(line-empty
    line-full
    "Hello there."
    line-full
    line-empty))


;; Such language...
(mis/koan/add
  '(line-empty
    line-full
    "You can't say 'fuck' in the source code..."
    line-full
    line-empty))


;; §-TODO-§ [2019-10-15]: get more from here? http://www.catb.org/~esr/writings/unix-koans/
;; http://www.catb.org/~esr/writings/unix-koans/
(mis/koan/add
 '(line-empty
   line-full
                        ("“Even the hacker who works alone" 13)
                           ("collaborates with others," 9)
                ("and must constantly communicate clearly to them," 7)
                    ("lest his work become confused and lost,”" 9)
                                ("said the Master." 13)
   line-full

               "“Of what others do you speak?” the Prodigy demanded."
   line-full
                   ("Master Foo said: “All your future selves.”" 13)
   line-full
   line-empty))


;; (theme song)
(mis/koan/add
 '(line-empty
   line-full
   "Standby for Reincarnation..."
   "  - Futurama"
   line-full
   line-empty))


;; (theme song)
(mis/koan/add
 '(line-empty
   line-full
                          "“When You Do Things Right,"
              "People Won’t Be Sure You’ve Done Anything at All”"
                                 "  - Futurama"
   line-full
   line-empty))


;; Spybreak! - Short One
(mis/koan/add
  '(line-empty
    line-full
                        ("“Do not try and bend the spoon," 13)
                               ("that’s impossible." 9)
                   ("Instead, only try to realize the truth..." 7)
    line-full
                                "There is no spoon."
    line-full
              ("Then you’ll see that it is not the spoon that bends," 7)
                             ("it is only yourself.”" 9)
                               ("    - Spoon Boy to Neo" 13)
    line-full
    line-empty))


;; The Programmer's Curse
(mis/koan/add
  '(line-empty
    line-full
                      ("~♫~99 Little Bugs In The Code!~♫~" 13)
                          ("~♫~99 ♪~Little~♪ Bugs!~♫~" 15)
                  ("~♫~Take One Down and Patch It Around...~♫~" 7)
                      ("~♫~127 Little Bugs in the Code.~♫~" 13)
    line-full
                                     ("..." 28)
                        ("(programmer sobs in binary...)" 3)
    line-full
    line-empty))


;; A Bad Koan.
;; Not really a koan, probably...
(mis/koan/add
  '(line-empty
    line-full
         ("I was juggling and trying to come to a good stopping point," 7)
                    ("But I had 5 balls and only two hands..." 17)
    line-full
                      ("So I came to a bad stopping point," 9)
                  ("And now I'm chasing balls around instead." 7)
    line-full
    line-empty))


;; Uh...
;; True story. :|
;; Except it was svn...
(mis/koan/add
  '(line-empty
    line-full
    "git commit -am \"fixed bugs caused by bug fixes\""
    line-full
    line-empty))


;; Do Not Tap Glass.
;; Do Not Handle Fork-Bomb.
;; Do Not Startle Fork-Bomb.
;; No Flash Photography, Please.
(mis/koan/add
 (mis/koan/message
  '(line-empty
    line-full
    ":(){ :|:& };:"
    line-full
    "(kaboom)"
    line-empty))


;;---
;; Just for good measure, "Don't Panic."
;;---

;; Just find your towel.
(mis/koan/add
 '(line-empty
   line-full
   ("DON'T PANIC" 28)
   ("- Hitchhiker's Guide to the Galaxy" 13)
   line-full
   line-empty))

(mis/koan/add
 '(line-empty
   line-full
   ("DON'T PANIC" 28)
   line-full
   line-empty))


;;---
;; Shuffle The Deck
;;---

;; Finally, shuffle the list?
;; (mis/shuffle 'mis/koan/list)
;; I randomize the get... why did I think I need this!? -_-

;; (mis/koan)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-21]: Refactor?
;; - Move zeroith-debug.el string/message functions into here.
;; - Break here up into:
;;   - strings?  (strings-and-things.el?)
;;   - messages? (messages-and-misc.el?)
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
(provide 'mis-koan)
