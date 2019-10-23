;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------When You Can Do Nothing...---------------------------
;;--                            What Can You Do?                              --
;;------------------------------------------------------------------------------
;; Make koans while the sun shines, I guess?



;;-----------------------------There is no spoon.-------------------------------
;;--                              Koan Settings                               --
;;------------------------------------------------------------------------------

(defvar mis/koan/list nil
  "Koans loaded up and ready to go.")


;;---------------------No, really. Go get your own spoon.-----------------------
;;--                             Koan Functions                               --
;;------------------------------------------------------------------------------

(defun mis/koan/process-line (line)
  "Returns a propertized line of a koan."
  (let ((faces-text (nth 1 (assoc '(mis koan text)
                                  mis/type->faces)))
        (faces-other (nth 1 (assoc '(mis koan presence)
                                   mis/type->faces))))

    (cond
     ;; 0) specials
     ((and (symbolp line)
           (alist-get line mis/parts/symbols-alist))
      (mis/parts/build line faces-other))

     ;; 1) general
     ;; a) Just a string
     ((stringp line)
      (mis/center line faces-text))

     ;; b) ("string" border-width-int)
     ((and (listp line)
           (= (length line) 2)
           (stringp (nth 0 line))
           (numberp (nth 1 line)))
      ;; center, with parts built from string and border-width
      (mis/center
       (mis/center/parts
        (nth 0 line)
        nil nil nil
        (mis/center/borders (nth 1 line)))
       faces-text))

     ;; 3) errors
     (t
      (error "unknown line type: %s" line)
      nil))))
;; (mis/koan/process-line 'line-empty)
;; (mis/koan/process-line 'line-full)
;; (mis/koan/process-line "Hi.")
;; (mis/koan)


(defun mis/koan/message (lines)
  "Prints a pretty little message to the *Messages* buffer."
  (dolist (line lines)
    (mis/message/preserve-properties (mis/koan/process-line line))))
;; (mis/koan/message '(line-empty line-full "Hi." line-full line-empty))


(defun mis/koan/add (koan)
  "Pushes KOAN into `mis/koan/list'."
  (push koan mis/koan/list))


(defun mis/koan ()
  "Prints a random koan, or the one indicated by WHICH if non-nil."
  (when (and mis/koan/list
             (listp mis/koan/list))
    (mis/koan/message (nth (random (length mis/koan/list))
                              mis/koan/list))))



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
