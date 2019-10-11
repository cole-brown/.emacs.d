;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Greedy File Opening
;;------------------------------------------------------------------------------

;; small speed-up by nuking all the regexs in file-name-handler-alist
(defvar spydez/file-name-handler-alist/orig (copy-alist file-name-handler-alist))
(setq file-name-handler-alist nil)


;; but also set them back when emacs is done with startup
;; TODO: Complain if not nil when setting back? Or add orig to end of list?
;;   - Probably complain first - a package is probably adding something.
;;     Probably to the front, maybe to the back - who knows?
;; Testing startup aka (emacs-init-time)
;;   without this:   ~5.5 sec
;;   with:         -> 4.6 sec
(spydez/hook/defun-and-hooker
    spydez/hook-runner/finalize/boot-and-config nil
    "spydez/file-name-handler-alist/revert"
    nil
    "init/zeroth/zeroth-garbage.el"
  "Reverts file-name-handler-alist to original value. Will try to
merge if junk was added meanwhile."
  (cond
   ;; both null - who cares
   ((and (null file-name-handler-alist)
         (null spydez/file-name-handler-alist/orig))
    (ignore))

   ;; null 'official' list right now, revert it to original value
   ((null file-name-handler-alist)
      (setq file-name-handler-alist spydez/file-name-handler-alist/orig))

   ;; null 'saved off' of orig... eh? Whatever.
   ((null spydez/file-name-handler-alist/orig)
    (ignore))

    ;; else, something in both... complain and merge
   (t
    ;; Complain...
    (spydez/message/warning
     nil nil
     (concat "Merging `file-name-handler-alist' instead of reverting back "
             "to default. It is no longer null: %s\n\norig: %s")
     file-name-handler-alist
     spydez/file-name-handler-alist/orig)

    ;; ...and merge.
    ;; TODO [2019-09-21]: Re-test this and see if it speeds up anything...
    ;; Filename alist is tiny on home pc?
    (seq-uniq (seq-concatenate 'list
                               file-name-handler-alist
                               spydez/file-name-handler-alist/orig)))))


;;------------------------------------------------------------------------------
;; Garbage Collection
;;------------------------------------------------------------------------------
;; Adjust garbage collection thresholds.

;; Testing startup aka (emacs-init-time)
;;   with defaults:            6.5 sec (~50 GCs)
;;   with 0.5 and 100 MB:   -> 5.8 sec (4 GCs)
;;   with 0.5 and 1000 MB:  -> 5.8 sec (4 GCs)
;;   with 0.75 and 1000 MB: -> 5.7 sec (4 GCs)
;;   with biggest thresh:   -> 5.8 sec (4 GCs)
;; (setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-percentage 0.75)
;; (setq gc-cons-threshold (* 1000 1000 1000))
;; (setq gc-cons-percentage 0.5)
;; (setq gc-cons-threshold (* 100 1000 1000)) ;; up to 100 MB from default (800k)

(defconst spydez/gc-cons-threshold/normal (* 20 1000 1000)
  "Upped to 20MB for GC threshold.")

;; In init, have big gc so it doesn't get hit much.
;; In normal running, have smaller gc so you don't get affected by lag too much
;; when it does go off on you.
;; Could consider messing with gc-cons-percentage as well.
;; (default thresh is 800k, percent is 0.1)
;; https://github.com/purcell/emacs.d/commit/6d9439b74153f91f614bba48b734a43e8cacf57e

(let ((spydez/gc-cons-threshold/init (* 256 1000 1000)) ;; most-positive-fixnum)
      ;; TODO: mess with gc-cons-percentage?
      )
  (setq gc-cons-threshold spydez/gc-cons-threshold/init)
  (add-hook 'emacs-startup-hook
            (lambda ()  (setq gc-cons-threshold spydez/gc-cons-threshold/normal))))

;; Note that here:
;;   https://github.com/syl20bnr/spacemacs/issues/3011#issuecomment-140536210
;; an argument against messing with this is presented, but saving most of a
;; second and dozens of gc cycles is worthwile.

;; See also configure-emacs.el.


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-garbage)
