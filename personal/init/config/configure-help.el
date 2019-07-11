;; -*- emacs-lisp -*-

;; (Help) Not just anybody.

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Always give help window focus?
;; Can remove/set back to nil if annoying.
(setq help-window-select t)


;;------------------------------------------------------------------------------
;; Which-Key
;;------------------------------------------------------------------------------

;; Which Key:
;;   https://github.com/justbur/emacs-which-key
;; Which-Key vs Guide-Key:
;;   https://github.com/justbur/emacs-which-key/issues/29
;; Settings From:
;;   https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p
;; Trial: [2019-03-15 Fri]
(use-package which-key
  :delight

  :config
  ;; (which-key-setup-side-window-right-bottom)
  ;; (setq which-key-sort-order 'which-key-key-order-alpha
  ;;   which-key-side-window-max-width 0.33
  ;;   which-key-idle-delay 0.05)
  (which-key-mode))

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
;;   (defhydra spydez/hydra/apropos (:color blue)
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
;;   (defhydra spydez/hydra/customize-apropos (:color blue)
;;     "Apropos (customize)"
;;     ("a" customize-apropos "apropos")
;;     ("f" customize-apropos-faces "faces")
;;     ("g" customize-apropos-groups "groups")
;;     ("o" customize-apropos-options "options"))
;;
;;   (defhydra spydez/hydra/info (:color blue)
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
