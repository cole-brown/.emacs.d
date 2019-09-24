;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Minibuffer
;;------------------------------------------------------------------------------

;;---
;; Properties
;;---

;; Added property to prevent point from entering prompt/read-only
;; text in minibuffer.
(setq minibuffer-prompt-properties '(read-only t
                                     cursor-intangible t
                                     face minibuffer-prompt))


;;---
;; Minibuffer editting
;;---

;; from Sacha: http://pages.sachachua.com/.emacs.d/Sacha.html#org3a92988
;;
;; Sometimes you want to be able to do fancy things with the text that you're
;; entering into the minibuffer. Sometimes you just want to be able to read it,
;; especially when it comes to lots of text. This binds C-M-e in a minibuffer)
;; so that you can edit the contents of the minibuffer before submitting it.
;; Trial: [2019-01-17]
(use-package miniedit
  :commands minibuffer-edit

  :init
  (miniedit-install))


;; Alternative:
;;  minibuffer often displays so much information, even temporarily, that 
;;  it is nice to give it some room to breath.
;; (setq resize-mini-windows t)
;; (setq max-mini-window-height 0.33)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-Edit.html
;; TODO: try this resize out


;;---
;; Misc
;;---
;; Don't want to bother typing out a whole 'yes'...
(fset 'yes-or-no-p 'y-or-n-p)


;;------------------------------------------------------------------------------
;; Mode Line
;;------------------------------------------------------------------------------
;; Moved to a configure-modeline.el.


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: some of these are modeline? Do we have enough modeline stuff for a
;; configure-modeline? some here, some all-the-icons?
;; Some in configure-window, apparently.... -_-


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-minibuffer)
