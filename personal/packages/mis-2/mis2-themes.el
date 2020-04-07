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
    (:default (;;---
               ;; Text Things
               ;;---
               :title       font-lock-keyword-face       ;; gold/bold
               :highlight   font-lock-variable-name-face ;; orangeish/peachish
               :highlight2  font-lock-warning-face       ;; darker orangish/brownish
               :highlight3  font-lock-warning-face       ;; darker orangeish/peachish
               :text        font-lock-builtin-face       ;; white/bold
               :text-pop    font-lock-string-face        ;; darkish pinkish
               :inattention font-lock-doc-face           ;; almost darkish green (close to comment/delimiter)
               :attention   font-lock-preprocessor-face  ;; brightish blue
               :attention2  font-lock-constant-face      ;; brightish green

               ;; remaining:
               ;; font-lock-function-name-face ;; light blue/teal
               ;; Others that are too close to ones I use for me to tell the difference...

               ;;---
               ;; Non-texty Things:
               ;;---
               ;; Box by piece names:
               :indent      font-lock-comment-delimiter-face ;; darker green
               :margins     font-lock-comment-delimiter-face ;; darker green
               :borders     font-lock-comment-delimiter-face ;; darker green
               :padding     font-lock-comment-face           ;; darkish green
               ))

    ;; §-TODO-§ [2020-03-25]: Remove custom stuff from in here; add when
    ;; configuring package.
    ;;---
    ;; My Custom Stuff
    ;;---
    (:homeward (:borders     font-lock-comment-delimiter-face
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
                      :borders font-lock-comment-delimiter-face
                      :padding   font-lock-comment-face
                      :text     font-lock-keyword-face))
    ;; non-text lines
    (:mis/nomer/presence (;; :indent nil
                          :borders  font-lock-comment-delimiter-face
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
;; Helper Functions
;;------------------------------------------------------------------------------


(defun mis2//themes/get/theme (plist)
  "Get the theme from the mis2 PLIST. theme is a keyword symbol alist
key for the `mis2/themes' alist.

If cannot find a theme, returns default theme: `:default'.
"
  ;; Get style, then get theme from style.
  (or (plist-get (plist-get plist :mis2//style) :theme)
      ;; Fallback to default if none found.
      :default))
;; (mis2//themes/get/theme '(:mis2//style (:theme 'jeff)))


(defun mis2//themes/get/faces-all (theme)
  "Get specific THEME faces from `mis2/themes' data. THEME is a
keyword symbol alist key for the `mis2/themes' alist.

If cannot find theme, or THEME is nil, returns all faces for
default theme: `:default'.
"
  ;; our alist cells are lists, not conses, so drop the outer list we get from
  ;; alist-get so we just have value, not (value).
  ;; We want (<face-key> <face-val> ...), not ((<face-key> <face-val> ...)).
  (or (first (alist-get theme mis2/themes))
      (first (alist-get :default mis2/themes))))
;; (mis2//themes/get/faces-all :default)


(defun mis2//themes/get/face (face theme)
  "Get specific FACE from THEME in `mis2/themes' data.
THEME should be alist key keyword symbol in `mis2/themes' alist.
FACE should be a plist key keyword symbol for THEME plist.

Returns face property or nil.
"
  (plist-get (mis2//themes/get/faces-all theme) face))
;; (mis2//themes/get/face :title :default)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-themes)
