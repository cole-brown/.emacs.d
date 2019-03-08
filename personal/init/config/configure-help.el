;; -*- emacs-lisp -*-

;; (Help) Not just anybody.

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Always give help window focus?
;; Can remove/set back to nil if annoying.
(setq help-window-select t)


;;------------------------------------------------------------------------------
;; Guide-Key
;;------------------------------------------------------------------------------

;; TODO: Switch to which-key. It's massively more popular and looks to have
;; a nicer layout maybe?
;;   - https://github.com/justbur/emacs-which-key/issues/29
;; Start with this config:
;;   http://nhoffman.github.io/.emacs.d/#orgb37c770


;; TODO: Doesn't work til turned off/back on. Need debugged?
;; It's hard to remember keyboard shortcuts. The guide-key package pops up help after a short delay. 
;;
;; Currently only set for:
;;   1) Rectangles    "C-x r"
;;   2) Window frames "C-x 4"
;;   3) Mode specific "C-c"
(use-package guide-key
  :delight guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence
          '("C-x r"     ; rectangle editing - I use parts of it but not all
            "C-x 4"     ; some window commands I never use
            "C-c"       ; God only knows. Minor modes, user keys, the moon...
            (projectile-mode "C-c p") ; Projectile's keymap
            (vlf-mode "C-c C-v")))    ; VLF (Very Large Files) keymap
    (guide-key-mode 1)))  ; Enable guide-key-mode
;; Trial: [2019-01-17]
;; TODO: any way to mark things what they are?
;;   Like `C-c C-v' are for VLF and "Prefix Command" isn't 100% useful.
;;   Also `C-c p' is Projectile, but shows up as "??" first then "Prefix Command".
;;   Meh.

;; (Help) You know I need someone.


;;------------------------------------------------------------------------------
;; Helpful Hydras
;;------------------------------------------------------------------------------

;; helm-apropos is bound to "C-h a" right now, and it combines apropos sources
;; too... So in the interest of learning helm, I'm holding off on this for now.
;; But it does seem neat.
;;   helm-apropos: https://tuhdo.github.io/helm-intro.html#orgheadline17
;;   these hydras: https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
;;
;; (require 'hydra nil 'noerror)
;; (require 'bind-key nil 'noerror)
;; (when (and (featurep 'hydra)
;;            (featurep 'bind-key))
;;
;;   (defhydra hydra-apropos (:color blue)
;;     "Apropos"
;;     ("a" apropos "apropos")
;;     ("c" apropos-command "cmd")
;;     ("d" apropos-documentation "doc")
;;     ("e" apropos-value "val")
;;     ("l" apropos-library "lib")
;;     ("o" apropos-user-option "opt")
;;     ("v" apropos-variable "var")
;;     ("i" info-apropos "info")
;;     ("t" tags-apropos "tags")
;;     ("z" hydra-customize-apropos/body "customize"))
;;
;;   (defhydra hydra-customize-apropos (:color blue)
;;     "Apropos (customize)"
;;     ("a" customize-apropos "apropos")
;;     ("f" customize-apropos-faces "faces")
;;     ("g" customize-apropos-groups "groups")
;;     ("o" customize-apropos-options "options"))
;;
;;   (defhydra hydra-info (:color blue)
;;     "Info"
;;     ("e" (funcall (info-display-topic "emacs")) "Emacs")
;;     ("l" (funcall (info-display-topic "elisp")) "Elisp")
;;     ("m" (funcall (info-display-topic "magit")) "Magit")
;;     ("o" (funcall (info-display-topic "org")) "Org Mode")
;;     ("s" (funcall (info-display-topic "screen")) "Screen")
;;     ("S" (funcall (info-display-topic "sicp")) "SICP"))
;;
;;   ;; Key Bindings
;;   (bind-key "C-h a" #'hydra-apropos/body)
;;
;;   ;; TODO: 
;;   ;; (define-key custom-keys-mode-prefix-map (kbd "i") #'hydra-info/body)
;;   )

;; Variables


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-help)
