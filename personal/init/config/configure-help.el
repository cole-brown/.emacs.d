;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; (Help) Not just anybody.

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Always give help window focus?
;; Can remove/set back to nil if annoying.
;; TRIAL [2019-10-02]: Changed: t -> nil.
;; Can set back to t/'always or try 'other if annoying.
(customize-set-variable 'help-window-select nil)

;; Got this buffer a few times and don't know why. Auto-complete help should be
;; amply provided for by other packages/features I have.
;; TRIAL [2019-10-02]: Can set back to 't (needs 1 fail to give help), or try
;; 'lazy (needs 2 fails).
(customize-set-variable 'completion-auto-help nil)


;;------------------------------------------------------------------------------
;; Spydez Helpful Helpers for Issues
;;------------------------------------------------------------------------------

;; It'd be nice to have one that could do both when eval'd, but I need to think
;; about how to do that correctly...
;; Idea 1:
;;   (spydez/help/issue "whitespace-and-butler" "move-to-column.org")
;;   - be at end
;;     -     "C-x C-e" -> calls spydez/help/issue/path
;;     - "C-u C-x C-e" -> calls spydez/help/issue/visit
;;     - But this is bad because I want the arg supplied in code and "C-u C-x
;;       C-e" just prints out results of sexpr eval. Would take some shenanigans
;;       about "eval-last-sexp" to get it done that way.
;; Idea 2:
;;   - Some sort of specially formatted link text, like system org-mode has in
;;     place. But that gets out of the relm of 'KISS' I wanted to stay in...
;; Idea 3:
;;   - Another param like...
;;     (spydez/help/issue "bad-thing" "notes.org" 'visit)
;;     (spydez/help/issue "bad-thing" "notes.org" 'path t) ;; (still have QUIET)
;;     (spydez/help/issue "bad-thing" "notes.org" 'both)
;;   - §-TODO-§ [2019-10-02]: Try out this one?

(defun spydez/help/issue/path (issue-dir file-name &optional quiet)
  "Returns a string which is fully expanded path to issue file.
Also copies it to the clipboard and puts it in *Messages*, unless
quiet arg is non-nil."
  (let ((path (spydez/path/to-file spydez/dir/docs/issues issue-dir file-name)))
    (unless quiet
      (with-temp-buffer
        (insert path)
        (clipboard-kill-region (point-min) (point-max)))
      (message "%s" path))
    path))

(defun spydez/help/issue/visit (issue-dir file-name)
  "Visits the issue file."
  (find-file (spydez/help/issue/path issue-dir file-name 'quiet)))

;; (spydez/help/issue/path "whitespace-and-butler" "move-to-column.org")
;; (spydez/help/issue/path "whitespace-and-butler" "move-to-column.org" t)
;; (spydez/help/issue/visit "whitespace-and-butler" "move-to-column.org")


;;------------------------------------------------------------------------------
;; Which-Key
;;------------------------------------------------------------------------------

;; Which Key:
;;   https://github.com/justbur/emacs-which-key
;; Which-Key vs Guide-Key:
;;   https://github.com/justbur/emacs-which-key/issues/29
;; Settings From:
;;   https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p
;;
;; Super useful. Shows what all can be done from the buttons you just pressed.
;; E.g. "C-h" and pausing for a sec will bring up which-key with all the
;; commands in the help-command map.
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
;; Helpful - "A better Emacs *help* buffer"
;;------------------------------------------------------------------------------
(use-package helpful
  :after helm
  ;; replace emacs' default help bindings with helpful's


  ;;---
  :bind ;; global
  ;;---
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h o" . helpful-symbol)
   ;; replaces `finder-by-keyword' but I've never used that...
   ("C-h p" . helpful-at-point)
   ;; replaces `describe-coding-system' but I've never used that...
   ("C-h C" . helpful-command))

  ;;---
  :bind ;; helpful-mode-map
  ;;---
  (:map helpful-mode-map
        ;; kill-this-buffer instead of quit (bury)
        ;; TRIAL: [2019-10-28]
        ;; kill-or-quit instead of original quit-or-kill?
        ;;  - "quit" as in bury...
        ("q" . spydez/window/kill-or-quit))


  ;;---
  :custom
  ;;---
  (helpful-max-buffers 5 ;; [2019-10-28]: 2->5
    "If this or more, kill oldest when opening another.")
  (helm-describe-function-function 'helpful-callable
    "Have Helm use Helpful for e.g. helm-apropos.")
  (helm-describe-variable-function 'helpful-variable
    "Have Helm use Helpful for e.g. helm-apropos."))


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
