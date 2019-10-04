;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; TODO: check this out for stuff:
;;   https://www.emacswiki.org/emacs/CSharpMode

;; TODO: these might belong in their own file...

;; TODO: check this for any useful things?
;;   https://www.gnu.org/software/emacs/manual/html_mono/efaq-w32.html#DevStudio

;; TODO: flycheck? Here? configure-cpp? configure-prog (prog-mode)?
;;   https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el


;;------------------------------------------------------------------------------
;; C#
;;------------------------------------------------------------------------------

;; C# isn't very popular as an emacs mode. Who'd've thunk.
;; From https://github.com/cbaggers/dotemacs/blob/master/init/csharp.el

;; Some from old emacs config, some from someone else's.
(defun spydez/hook/csharp-mode ()
;;; <my old emacs>

  ;; Use BSD in C, C++. Used to use it in C#.
  ;; But now C# shares a style with Java, so see if that's better.
  ;; Or go back to BSD if I hate it.
  ;; (c-set-style "bsd") ;; See c-style-alist for supported types
  ;; Trial [2019-02-15]
  (c-set-style "C#") ;; See c-style-alist for supported types

  (setq c-basic-offset spydez/dev-env/tab/normal)
  (c-set-offset 'innamespace 0) ; Don't indent namespace - waste of indent level
  (c-set-offset 'case-label '+) ; indent case labels by c-indent-level, too

  ;; Have this set globally.
  ;; (setq indent-tabs-mode nil)

  (setq c-indent-level spydez/dev-env/tab/normal)

  ;; electric-indent-mode is true and might take care of this?
  ;; (local-set-key [return] 'newline-and-indent)

  ;; TRIAL [2019-10-03]: fill-column bumped out to 'long'?
  (setq fill-column spydez/dev-env/fill-column/long)
  (message "fill-column: %s" fill-column)

  ;; line numbers already on globally

;;; </my old emacs>

  ;; TODO: I probably want most or all of these, or some competing package/feature
  ;; (paredit-mode 1)
  ;; (omnisharp-mode 1)
  ;; (my/disable-paredit-spaces-before-paren)
  ;; (company-mode 1)
  ;; (yas-minor-mode 1)
  ;; (flycheck-mode 1)

  ;; TODO: check out these ideas from: https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el
  ;; TODO: enable/disable in c-common-hook as well.

  ;; Separate camel-case into separate words.
  ;; (subword-mode t)
  )

;; TODO: check this https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/csharp.el
(use-package csharp-mode
  ;;---
  :hook
  ;;---
  (csharp-mode . spydez/hook/csharp-mode)


  ;; TODO: and... again with the do-i-want-this-i-don't-know-yet
  ;; :bind
  ;; (("{" 'paredit-open-curly csharp-mode-map)
  ;;  ("}" 'paredit-close-curly csharp-mode-map))

  ;;---
  :config
  ;;---
  (bind-keys :map csharp-mode-map
             ;; `comment-or-uncomment-region' instead of just `comment-region'
             ("C-c C-c" . comment-or-uncomment-region)))

;; IDE helper, language server, auto-complete, jump-to-defs, etc.
;; Doesn't use LSP-Mode as of [2019-09-30].
;;  - http://www.omnisharp.net/
;;  - https://github.com/OmniSharp/omnisharp-emacs
;; Config initially from:
;;  - https://github.com/OmniSharp/omnisharp-emacs/issues/339#issuecomment-335077125
(use-package omnisharp
  :after csharp-mode

  ;;---
  :preface
  ;;---
  (defun spydez/hook/csharp-mode/omnisharp ()
    (omnisharp-mode)
    (add-to-list 'company-backends #'company-omnisharp)
    (company-mode))

  ;; ;;---
  ;; :custom
  ;; ;;---
  ;; Can't do this cuz... custom happens before it knows about omnisharp-expected-server-version? :|
  ;; (omnisharp-server-executable-path
  ;;  (spydez/path/to-file omnisharp-cache-directory
  ;;                       "server"
  ;;                       (concat "v" omnisharp-expected-server-version)
  ;;                       "OmniSharp.exe")
  ;;  (concat "Path to OmniSharp executable. Package and server should be "
  ;;          "lockstep? Or help for `omnisharp-expected-server-version' "
  ;;          "seems to indicate so."))

  ;;---
  :hook
  ;;---
  (csharp-mode . spydez/hook/csharp-mode/omnisharp)

  ;;---
  :config
  ;;---

  (with-feature 'hydra
    (defhydra spydez/hydra/csharp (:color blue ;; default exit heads
                                   :idle 0.75)  ;; no help for this many seconds
                                   ;; :hint nil)  ;; no hint - just fancy docstr
      "C# Mode"

      ("c"  spydez/dev-env/visual-studio/compile "compile")
      ("b" (spydez/buffer/bury-visible "*compilation*") "bury compile")
      ;; ("c"  recompile "compile") ;; Doesn't work? Calls 'make'...

      ("8" spydez/dev-env/visual-studio/unit-test "unit test")
      ("9" (spydez/buffer/bury-visible
            (spydez/buffer/special-name "Unit Tests"))
       "bury unit test")

      ("r"  omnisharp-run-code-action-refactoring "refactor action...")
      ("\\" omnisharp-go-to-definition "goto def")
      ("="  omnisharp-find-usages "find usages")
      ("/"  omnisharp-find-implementations "find impls")
      ("u"  omnisharp-fix-usings "fix 'using's")

      ;; Just use some awkward letters for start/stop?
      ("Y"  omnisharp-start-omnisharp-server "Start OmniSharp")
      ("F"  omnisharp-stop-server "Stop OmniSharp")

      ("q"  nil "cancel" :color blue))

    (bind-keys :map csharp-mode-map
               ("C-c r"   . spydez/hydra/csharp/body)
               ("C-c C-r" . spydez/hydra/csharp/body)
               ;; F7 is what my VS2010 has as "build" so it's kinda reflex...
               ("<f7>" . spydez/dev-env/visual-studio/compile)))

  ;; use-package is getting annoying... Can't do these in ':bind' or it won't
  ;; ever hook into C# mode... Trying a hydra anyways.
  ;; (bind-keys :map omnisharp-mode-map
  ;;            ("C-c r c"   . recompile) ;; csharp-mode-map???
  ;;            ("C-c r r" . omnisharp-run-code-action-refactoring)
  ;;            ("M-." 'omnisharp-go-to-definition csharp-mode-map)

  ;; TODO: other binds? like...
  ;; (bind-key "M-," 'pop-tag-mark csharp-mode-map)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd "g d")
  ;;                  (lambda() (interactive)
  ;;                    (evil-jumper--set-jump)
  ;;                    (omnisharp-go-to-definition)))
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", cf")
  ;;                  'omnisharp-code-format)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", nm")
  ;;                  'omnisharp-rename-interactively)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", fu")
  ;;                  'omnisharp-helm-find-usages)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", fs")
  ;;                  'omnisharp-helm-find-symbols)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd "<M-RET>")
  ;;                  'omnisharp-run-code-action-refactoring)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", x")
  ;;                  'omnisharp-fix-code-issue-at-point)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", fx")
  ;;                  'omnisharp-fix-usings)
  ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", o")
  ;;                  'omnisharp-auto-complete-overrides)

  (customize-set-variable 'omnisharp-server-executable-path
   (spydez/path/to-file omnisharp-cache-directory
                        "server"
                        (concat "v" omnisharp-expected-server-version)
                        "OmniSharp.exe")
   (concat "Path to OmniSharp executable. Package and server should be "
           "lockstep? Or help for `omnisharp-expected-server-version' "
           "seems to indicate so.")))

;; (use-package omnisharp
;;   :after csharp-mode)

;; https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/csharp.el
;; (use-package omnisharp
;;   :config
;;   (validate-setq
;;    omnisharp-server-executable-path
;;    (path-join *user-omnisharp-path* "omnisharp")))

;; dunno where I got this one...
;;  (use-package omnisharp
;;    :ensure t
;;    :config
;;    (setq omnisharp-server-executable-path "/home/baggers/Programs/omnisharp/myrun.sh")
;;    (bind-key "M-." 'omnisharp-go-to-definition csharp-mode-map)
;;    (bind-key "M-," 'pop-tag-mark csharp-mode-map)
;;    (bind-key "C-c C-w C-c" 'omnisharp-find-usages csharp-mode-map)
;;    (add-to-list 'company-backends 'company-omnisharp))



;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO: some sanity check for things-I-want-in-mode-X-but-are-set-globally?
;;   - On the one hand, they're set globally, so it's likely I want a bunch of
;;     things that aren't even in this file or on my mind.
;;   - On the other hand, I don't like throwing away things that were useful
;;     before and might be again.
;;   - On the gripping hand, might as well do everything I can to catch things
;;     that may go wrong when I'm not familiar with the inner workings of
;;     all my init files?

;; TODO: CEDET?
;;   - https://github.com/tuhdo/tuhdo.github.io/blob/master/emacs-tutor/cedet.org


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-csharp)
