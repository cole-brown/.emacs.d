;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------Do we have a song?-------------------------------
;;--                             We have Themes.                              --
;;----------------------------Why not a Theme Song?-----------------------------

(require 'dash)


;; NOTE:
;;   "emface"   - emacs face property
;;
;;   "misface" - mis2 face keyword (:title)
;;   "theme"   - mis2 keyword (:default) for a collection of misfaces

;; NOTE:
;;   Special Types:
;;     - :message - entire mis2 message line
;;     - :text    - part of mis2 message that is not line/box/special/etc.

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

               :message     font-lock-builtin-face       ;; white/bold

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
  (:theme-keyword misfaces-list)

Each faces-list should be a plist of:
  (:misface-keyword defined-face-symbol)

See 'M-x list-faces-display' for all defined faces."
  :group 'mis2
  :type '(alist :key-type list :value-type list))


;;------------------------------------------------------------------------------
;; Theme Adder
;;------------------------------------------------------------------------------

(defun mis2/themes/add (name faces-plist)
  "Add theme NAME with FACES-PLIST to mis2/themes.

NAME should be a symbol keyword.
 - e.g. :my-theme

FACES-PLIST should be a keyword/face-property PLIST.
 - e.g. '(:borders font-lock-comment-delimiter-face)

This sets theme NAME to FACES-PLIST, overwriting any existing
theme of that name.
"
  (setq mis2/themes (plist-put mis2/themes name faces-plist)))


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------


(defun mis2//themes/get/theme (plist)
  "Get the theme from the mis2 PLIST. theme is a keyword symbol alist
key for the `mis2/themes' alist.

If cannot find a theme, returns default theme: `:default'.
"
  ;; Get style, then get theme from style.
  (or (plist-get (plist-get plist :mis2//settings) :theme)
      ;; Fallback to default if none found.
      :default))
;; (mis2//themes/get/theme '(:mis2//settings (:theme 'jeff)))


(defun mis2//themes/get/misfaces-all (theme)
  "Get specific THEME faces from `mis2/themes' data. THEME is a
keyword symbol alist key for the `mis2/themes' alist.

If cannot find theme, or THEME is nil, returns all faces for
default theme: `:default'.

Returns plist of key/values: (misface0 emface0 ... misfaceN emfaceN)
"
  ;; (message "get/misfaces-all-: %S" theme)
  ;; (message "      mis2/themes: %S" mis2/themes)
  ;; (message "   theme-misfaces: %S" (-first-item (alist-get theme mis2/themes)))
  ;; (message "  default-misfaces: %S" (-first-item (alist-get :default mis2/themes)))

  ;; our alist cells are lists, not conses, so drop the outer list we get from
  ;; alist-get so we just have value, not (value).
  ;; We want (<face-key> <face-val> ...), not ((<face-key> <face-val> ...)).
  (or (-first-item (alist-get theme mis2/themes))
      (-first-item (alist-get :default mis2/themes))))
;; (mis2//themes/get/misfaces-all :default)


(defun mis2//themes/get/emface (misface theme)
  "Get specific MISFACE from THEME in `mis2/themes' data.
THEME should be alist key keyword symbol in `mis2/themes' alist.
MISFACE should be a plist key keyword symbol for THEME plist.

Returns emface (emacs face property) or nil.
"
  ;; (message "get/emface-----: %S %S" misface theme)
  ;; (message "   all-misfaces: %S" (mis2//themes/get/misfaces-all theme))
  ;; (message "         emface: %S" (plist-get (mis2//themes/get/misfaces-all theme) misface))
  (plist-get (mis2//themes/get/misfaces-all theme) misface))
;; (mis2//themes/get/emface :title :default)


(defun mis2//themes/get/misface/by-type (type plist)
  "Get misface (e.g. :text) from THEME by TYPE (e.g. :borders).

Returns misface or nil.
"
  ;; Get TYPE's face keyword from style's `:faces' plist.
  ;; This is still the mis2 face (e.g. `:borders'), not the
  ;; actual property (e.g. `font-lock-comment-delimiter-face').
  (plist-get
   ;; Get `:faces' plist from style data from mis2 plist.
   (plist-get (plist-get plist :mis2//style) :faces)
   type))


(defun mis2//themes/get/misface/from-style (plist)
  "Get misface (e.g. :text) from :face from :mis2//style in PLIST.

Returns misface or nil.
"
   ;; Get `:face' plist from style data from mis2 plist.
   (plist-get (plist-get plist :mis2//style) :face))


(defun mis2//themes/get/misface/smart-type (type plist)
  "Get misface (e.g. :text) from THEME by TYPE (e.g. :borders).

If TYPE is _A_, checks for _A_ with fallback to _B_:
  A) :text
    B) :message
  A)
    B)
Otherwise just look for TYPE, no fallback.

Returns misface or nil.
"
  ;; :text should check :message as fallback
  (cond ((eq type :text)
         (or (mis2//themes/get/misface/by-type :text    plist)
             (mis2//themes/get/misface/by-type :message plist)))

        ;; Default: No fallback to check.
        (t
         (mis2//themes/get/misface/by-type type plist))))


(defun mis2//themes/misface (type theme plist)
  "Checks various places for the correct misface for this type.

1) PLIST -> :mis2//style -> :faces -> TYPE -> misface
2) PLIST -> :mis2//style -> :face -> misface
3) THEME -> TYPE --exists?-> TYPE (type == misface in this instance)
4) THEME -> :default --exists?-> :default
5) nil
"
  (or
   ;; Check for match by type in :mis2//style :faces.
   (mis2//themes/get/misface/by-type type plist)

   ;; Else check for :mis2//style :face.
   (mis2//themes/get/misface/from-style plist)

   ;; Else check for match by type in theme.
   (when (mis2//themes/get/emface type theme)
     ;; Have match; type is also misface now.
     type)

   ;; Else check for... :default in theme?
   (when (mis2//themes/get/emface :default theme)
     :default)))


(defun mis2//themes/emface (type plist)
  "Go from mis2 PLIST and TYPE, all the way to emface (emacs face property).
Checks :theme, :faces vs :face, falls back to default values, etc. Everything
necessary to get from PLIST to emface (emacs face property).
"
  ;; (message "all-themes: %S" mis2/themes)
  ;; (message "themes/emface: %S %S" type plist)
  ;; (message "        theme: %S" (mis2//themes/get/theme plist))
  ;; (message "      misface: %S" (mis2//themes/misface type (mis2//themes/get/theme plist) plist))
  ;; (message "       emface: %S" (mis2//themes/get/emface
  ;;                               (mis2//themes/misface type (mis2//themes/get/theme plist) plist)
  ;;                               (mis2//themes/get/theme plist)))

  ;; Get theme (e.g. :default) from plist.
  (let* ((theme (mis2//themes/get/theme plist))
         ;; misface keyword (e.g. :title, :highlight, :attention) for:
         ;;   - specific content type (e.g. :margins, :message...)
         ;;   - or use type.
         ;;   - or fallback to the general mis2//style :face.
         (misface (mis2//themes/misface type theme plist)))
    ;; Get actual emacs face from the theme misface.
    (mis2//themes/get/emface misface theme)))


(defun mis2//themes/emface/from-style (styles-plist mis2-plist)
  "Get emface (emacs face property) from :face in STYLES-PLIST.

Returns emface or nil.
"
  (when (and styles-plist mis2-plist)
    ;; Get `:face' (misface) from style plist, then translate to emface for
    ;; this theme.
    (mis2//themes/get/emface
     (plist-get styles-plist :face) ;; misface getter
     (mis2//themes/get/theme mis2-plist)))) ;; theme getter


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis2-themes)
