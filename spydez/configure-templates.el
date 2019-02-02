;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; TODO: General Settings?
;;------------------------------------------------------------------------------

;; http://ergoemacs.org/emacs/emacs_templates.html

;; TODO:  Check yasnippet settings:
;;   https://github.com/manuel-uberti/.emacs.d/blob/master/lisp/mu-completion.el


;;------------------------------------------------------------------------------
;; Yasnippets
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
;; https://www.emacswiki.org/emacs/Yasnippet
;; https://joaotavora.github.io/yasnippet/snippet-expansion.html
(use-package yasnippet
  :diminish yas-minor-mode
  
  :init
  ;; why are there /three/ calls to yas-global-mode though..?
  (yas-global-mode)
  
  :config
  (progn
    ;; TODO: this seems real janky so I'm disabling most of it to start debugging...

    ;; todo: why both calls into global mode?
    ;; (yas-global-mode)

    ;; TODO: what does this do?
    ;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
    
    (add-to-list 'yas-snippet-dirs spydez/dir/yasnippets) ;; want this one - mine - at front of list
    
    ;; (setq yas-expand-only-for-last-commands nil)
    
    ;; todo: why both calls into global mode?
    ;; (yas-global-mode 1)

    ;; todo: do I want \t bound for yas? (yes??) Do I want it bound to hippie-expand?..
    ;; ...let's find out.
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)
    (add-to-list 'yas-prompt-functions 'spydez/yas/helm-prompt)
    ))


;;------------------------------------------------------------------------------
;; Helm/Yasnippets
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org656616c
;; Orig: shk-yas/helm-prompt
(defun spydez/yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))


;;------------------------------------------------------------------------------
;; Default/External Snippets
;;------------------------------------------------------------------------------
;; "Official Collection of Snippets"
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; https://github.com/manuel-uberti/.emacs.d/blob/master/lisp/mu-completion.el
;;   Looks like no real setup - just make sure my snippets are ahead of these
;;   in the list `yas-snippet-dirs'.
;; Some people have this inside of (use-package yasnippet)
(use-package yasnippet-snippets)


;;------------------------------------------------------------------------------
;; Disabled/Broken: Yasnippet Insert Indicators on Cursor
;;------------------------------------------------------------------------------
;; (setq spydez/default-cursor-color "gray")
;; (setq spydez/yasnippet-can-fire-cursor-color "purple")
;; 
;; ;; It will test whether it can expand, if yes, cursor color -> can-fire-cursor-color.
;; (defun spydez/yasnippet-can-fire-p (&optional field)
;;   (interactive)
;;   (setq yas--condition-cache-timestamp (current-time))
;;   (let (templates-and-pos)
;;     (unless (and yas-expand-only-for-last-commands
;;                  (not (member last-command yas-expand-only-for-last-commands)))
;;       (setq templates-and-pos (if field
;;                                   (save-restriction
;;                                     (narrow-to-region (yas--field-start field)
;;                                                       (yas--field-end field))
;;                                     (yas--templates-for-key-at-point))
;;                                 (yas--templates-for-key-at-point))))
;;     (and templates-and-pos (first templates-and-pos))))
;; 
;; (defun spydez/change-cursor-color-when-can-expand (&optional field)
;;   (interactive)
;;   (when (eq last-command 'self-insert-command)
;;     (set-cursor-color (if (spydez/can-expand)
;;                           spydez/yasnippet-can-fire-cursor-color
;;                         spydez/default-cursor-color))))
;; 
;; (defun spydez/can-expand ()
;;   "Return true if right after an expandable thing."
;;   (or (abbrev--before-point) (spydez/yasnippet-can-fire-p)))

;; DISABLED:
;; So this is neat and all but it needs way better condition checking for resetting
;; cursor color (or way stupider (e.g. always reset to normal, set to purp after if can-expand)).
;; Easy way to break it: `up' is tag for `use-package' elisp snippet.
;;   `up C-<backspace>' breaks this.
;; Will have to hook into more stuff, or improve the predicate.
;; Disabled for now.
;; (add-hook 'post-command-hook 'spydez/change-cursor-color-when-can-expand)
;; NOTE: See this for a library that does cursor changes:
;;   https://www.emacswiki.org/emacs/ChangingCursorDynamically


;;------------------------------------------------------------------------------
;; Yasnippet -> Hydra/Key-Chord/Hippie
;;------------------------------------------------------------------------------
;; Stuff Yasnippet into more things.

;; used in config hydra
(defun spydez/insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (spydez/hippie-expand-maybe nil) (insert "  "))))

;; Modify the behaviour of hippie-expand so that it doesn't ding so much...
;; todo: is this function used in my init (of hippie?)?

(defun spydez/hippie-expand-maybe (arg)
  "Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument],
undoes the expansion."
  (interactive "P")
  (require 'hippie-exp)
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn (setq he-num -1) nil)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list)))))
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Undoing expansions"))))))


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; TODO: check this for anything useful:
;;   https://www.reddit.com/r/emacs/comments/8vdhb4/tip_how_to_integrate_snippets_with_yasnippets/
;; TODO: Grab old snippets?
;; TODO: Some new snippets for C# or Django or whatever? As needed maybe?
;; TODO: Just noticed auto-complete (hippie?) for elisp is a bit overzealous...
;;   - `(use-package yas<tab>' was completing to the full sexpr of the up above.


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-templates)
