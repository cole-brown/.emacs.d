;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;┌┬┬───────────────────────────"One art please!"────────────────────────────┬┬┐
;;├┼┤                             -Dr Zoidberg                               ├┼┤
;;└┴┴────────────────────────────────────────────────────────────────────────┴┴┘


;;------------------------------------------------------------------------------
;; Stuff Emacs Already Has
;;------------------------------------------------------------------------------

;; http://ergoemacs.org/emacs/emacs_ascii_diagram.html
;;
;; picture-mode:
;; Lets you draw ASCII diagrams.
;;  - More info: 'M-x picture-mode', 'M-x describe-mode' OR 'C-h m'
;;
;; artist-mode:
;; Lets you dra ASCII pictures with mouse.
;;   Left Mouse:   Draw
;;   Right Mouse:  Erase
;;   Middle Mouse: Cycle rectangle, ellipse, other draw tools.


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Manual Unicode Box Drawing Hydra
;;------------------------------------------------------------------------------

(defhydra spydez/hydra/box-drawing (:color pink  ;; default exit heads
                                    ;;:idle 0.75 ;; no help for x seconds
                                    :hint none)  ;; no hint - just docstr)
  "
Draw box characters. Left-hand \"Grid\" layout according to Dvorak keyborad.
_'_: ?'?  _,_: ?,?  _._: ?.?
_a_: ?a?  _o_: ?o?  _e_: ?e?
_;_: ?;?  _q_: ?q?  _j_: ?j?

_p_: ?p?  _u_: ?u?  _ESC_: quit
"
  ("'" (funcall #'insert "┌") "┌")
  ("," (funcall #'insert "┬") "┬")
  ("." (funcall #'insert "┐") "┐")
  ("a" (funcall #'insert "├") "├")
  ("o" (funcall #'insert "┼") "┼")
  ("e" (funcall #'insert "┤") "┤")
  (";" (funcall #'insert "└") "└")
  ("q" (funcall #'insert "┴") "┴")
  ("j" (funcall #'insert "┘") "┘")
  ("p" (funcall #'insert "─") "─")
  ("u" (funcall #'insert "│") "│")
  ("ESC" nil :color blue))
;; §-TODO-§ [2019-10-21]: instead of just insert, try to insert or overwrite,
;; and then move in the proper direction? Or does pink hydra make moving around
;; acceptable? Still could do overwrite instead of insert when applicable.


(defun spydez/art/draw-box ()
  "Don't think I want to have `spydez/hydra/box-drawing' bound
anywhere yet, but... This may let me find it easier."
  (interactive)
  (call-interactively #'spydez/hydra/box-drawing/body))
;; ┌────┐
;; ├────┤
;; │ hi │
;; └────┘


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; Nice package for converting to unicode if used picture-mode:
;;   https://elpa.gnu.org/packages/ascii-art-to-unicode.html

;; Webpage for making ASCII art.
;; http://asciiflow.com/

;; Small lib for boxing/unboxing strings (single and multiline) with unicode box
;; drawing characters. Elisp; no UI.
;; https://www.emacswiki.org/emacs/UnicodeEnbox

;; Library for making unicode font blocks work better. Or, rather, for making
;; emacs better at figuring out what font to use for what unicode character.
;; https://www.emacswiki.org/emacs/UnicodeFonts


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-art)
