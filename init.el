;; -*- emacs-lisp -*-

;; init.el - non-computer-specific emacs init

;; can 'literate programming' do multiple files?
;; http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html

;;------------------------------------------------------------------------------
;; Initial vars setup.
;;------------------------------------------------------------------------------

;;---
;; Setup some very basics, so we can get moving...
;;---
;; These are the defaults. Override with setq later if needed.

;; defconst vs defvar vs setq
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html

;; cross-platform dir and file names:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html#Directory-Names

(defconst spydez/setup-domain-name "work"
  "A domain/folder for setups similar to this. E.g. work vs personal.")
(defconst spydez/setup-comp-name (system-name)
  "This specific computer's setup folder.")
(defconst spydez/setup-root-dir (expand-file-name user-emacs-directory)
  ;; user-init-file and user-emacs-directory can be helpful here
  "Probably don't need this?")
;; todo: need these?
;(defconst spydez/setup-libs (concat spydez/setup-root-dir "libs/"))
(defconst spydez/setup-personal-dir (expand-file-name "personal" spydez/setup-root-dir)
  "All of my own personal/custom setup code/vars/definitions...")
(defconst spydez/setup-domain-specific-dir (expand-file-name spydez/setup-domain-name spydez/setup-personal-dir)
  "Anything that has to be domain specific. Tab widths or whatnot.")
(defconst spydez/setup-comp-specific-dir (expand-file-name spydez/setup-comp-name spydez/setup-domain-specific-dir)
  "Anything that has to be computer specific. Overriding tab widths or whatnot.")

;; todo: move to an end script or something?
;; auto-open file list
(defvar spydez/auto-open-list
  '("c:/home/documents/work.org"))

;; TODO: user-full-name user-mail-address ??

;; TODO: custom-file
; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

;;---
;; Add stuff to our load path.
;;---
;; Setting overrides towards front of load-path (add-to-list does this for us).
;;
;; Don't use .emacs.d. 
;; https://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path
;; (add-to-list 'load-path spydez/setup-root-dir)
(add-to-list 'load-path spydez/setup-personal-dir)
(add-to-list 'load-path spydez/setup-domain-specific-dir)
(add-to-list 'load-path spydez/setup-comp-specific-dir)

;;---
;; Custom file
;;---
;; Some packages want to write to our custom file, so set that up first.
(setq custom-file "custom.el")
(load custom-file t)

;;------------------------------------------------------------------------------
;; Bootstrap.
;;------------------------------------------------------------------------------

;; Init use-package so we can use use-package for the rest of the packages we use.
(load "init-package.el")

;; todo: check these out?
;(load-file (concat kooru/emacs-libs "bootstrap.el"))
;(bootstrap-init kooru/comp-domain kooru/comp-name)

;; todo: inhibit startup stuff
;; todo: initial-buffer-choice vs spydez/auto-open-list???
;ptions affect some aspects of the startup sequence.
;- User Option: inhibit-startup-screen
;
;    This variable, if non-nil, inhibits the startup screen. In that case, Emacs typically displays the *scratch* buffer; but see initial-buffer-choice, below.
;
;    Do not set this variable in the init file of a new user, or in a way that affects more than one user, as that would prevent new users from receiving information about copyleft and basic Emacs usage.
;
;    inhibit-startup-message and inhibit-splash-screen are aliases for this variable. 
;
;- User Option: initial-buffer-choice
;
;    If non-nil, this variable is a string that specifies a file or directory for Emacs to display after starting up, instead of the startup screen. If its value is a function, Emacs calls that function which must return a buffer which is then displayed. If its value is t, Emacs displays the *scratch* buffer. 
;
;- User Option: inhibit-startup-echo-area-message
;
;    This variable controls the display of the startup echo area message. You can suppress the startup echo area message by adding text with this form to your init file: 

;; todo: utf-8

;; todo: move to wherever the end is? Or leave here because this is the end?
;; https://github.com/zzamboni/dot-emacs/blob/master/init.org
(defun spydez/startup-hook ()
  (message "Emacs ready in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time
        (time-subtract after-init-time before-init-time)))
    gcs-done))
(add-hook 'emacs-startup-hook 'spydez/startup-hook)
;;fin
