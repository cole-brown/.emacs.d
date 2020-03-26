;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------Do we have a song?-------------------------------
;;--                             We have Themes.                              --
;;----------------------------Why not a Theme Song?-----------------------------



;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

;; M-x list-faces-display
(defcustom mis2/themes
  '(;;---
    ;; General, Maybe Useful Faces.
    ;;---
    (:default (;; Text Things
              :title       font-lock-preprocessor-face ;; brightish blue
              :highlight   font-lock-keyword-face      ;; gold/bold
              :highlight2  font-lock-constant-face     ;; brightish green
              :text        font-lock-builtin-face      ;; white/bold
              :inattention font-lock-string-face       ;; darkish red
              :attention   font-lock-preprocessor-face ;; brightish blue
              :attention2  font-lock-constant-face     ;; brightish green

              ;; Non-texty Things:
              :border      font-lock-comment-delimiter-face
              :padding     font-lock-comment-face
              ))

    ;; §-TODO-§ [2020-03-25]: Remove custom stuff from in here.
    ;;---
    ;; My Custom Stuff
    ;;---
    ;; §-TODO-§ [2019-11-15]: change do (:spydez :homeward)?
    (:homeward (:border      font-lock-comment-delimiter-face
                :padding     font-lock-comment-face
                :text        font-lock-builtin-face
                :highlight   font-lock-keyword-face
                :highlight2  font-lock-constant-face
                :title       font-lock-preprocessor-face
                :inattention font-lock-string-face))

    ;;---
    ;; Koans and Misnomers
    ;;---
    ;; text lines
    (:mis/nomer/text (;; :indent nil
                      :border  font-lock-comment-delimiter-face
                      :padding   font-lock-comment-face
                      :text     font-lock-keyword-face))
    ;; non-text lines
    (:mis/nomer/presence (;; :indent nil
                          :border   font-lock-comment-delimiter-face
                          :padding  font-lock-comment-face
                          :text     font-lock-comment-face)))
  "alist of plists. Each entry should be a list (not cons) of:
  (:theme-keyword faces-list)

Each faces-list should be a plist of:
  (:face-keyword defined-face-symbol)

See 'M-x list-faces-display' for all defined faces."
  :group 'mis2
  :type '(alist :key-type list :value-type list))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-themes)
