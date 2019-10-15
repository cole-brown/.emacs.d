;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------One Step, Two Step------------------------------
;;--                         Red Step, Blue Step...                          --
;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; General Functions
;;-----------------------------------------------------------------------------

;; Need this here - super early - so not a great place to put...
(defun spydez/consp-and-not-listp (var)
  "Apparently lists qualify as conses so... fucking lisp, yeah?
...and I can't `listp' or `length' a cons because it's not a list...?!?!

Jesus fuck lisp. I just want to check for dotted-pair conses...

https://emacs.stackexchange.com/questions/10489/predicate-function-for-dotted-pairs
Pretty sure this is just a hack to check length 2, but fuck it;
it works."
  (and (cdr var) (atom (cdr var))))

;;-----------------------------------------------------------------------------
;; Progress of Steps
;;-----------------------------------------------------------------------------

(defvar spydez/init/step/completed nil
  "Should be a list of 2 items: (major minor) steps recently completed. See
`spydez/init/step/major' and
spydez/init/step/{zeroth,bootstarp,configure,finalize} for acceptable values
and approximate sequence. Compare with `spydez/init/step-at',
`spydez/init/step/past', `spydez/init/step/past-major',
`spydez/init/step/past-minor'.")


;; (defun spydez/init/step/current ()
;;   "Should return a list of (major minor) of its guess of current step
;; based on what is set as `spydez/init/step/completed'. See
;; `spydez/init/step/major' and
;; spydez/init/step/{zeroth,bootstarp,configure,finalize} for acceptable values
;; and approximate sequence. Compare with `spydez/init/step-at'"

;;   ;; find indices for completed, or complain.
;;   (let* ((major-i (or (seq-position spydez/init/step/major-seq
;;                                     (car spydez/init/step/completed))
;;                       (__/startup/message
;;                        "%s: Unknown major step: '%s'"
;;                        "spydez/init/step/current"
;;                        (car spydez/init/step/completed))))
;;          (minor-list (alist-get major spydez/init/step/minor-alist nil))
;;          (minor-i (or (seq-position minor-list
;;                                     (cdr spydez/init/step/completed))
;;                       (__/startup/message
;;                        "%s: Unknown minor step: '%s'"
;;                        "spydez/init/step/current"
;;                        (cdr spydez/init/step/completed)))))
;;     (when (and major-i minor-i)
;;       ;; can we increment in this?

;;       ;; hm... '(system default) vs '(system specific) wrenches this...
;;       ;; §-TODO-§ [2019-10-09]: TBD for now.
;;       )))


;;------------------------------------------------------------------------------
;; Define Steps
;;------------------------------------------------------------------------------
;; NOTE: all of these are in order.
;; These must be in an order that `spydez/init/step-past' understands. A more
;; complete step must be after a less.
;; (e.g. bootstrap's '(system default) and '(system specific))


;;----------
;; Minor Steps of Each Major Step
;;----------

(defconst spydez/init/step/none-seq
  '(none)
  "There is no spoon.")

;;---
;; Zeroth
;;---
(defconst spydez/init/step/zeroth-seq
  '(none
    post   ;; "power on self test", no any of those other posts...
    zero   ;; pre-pre-basics?
    debug  ;; now have my debug helpers and such
    system ;; system setup
    complete)
  "Steps for early-init.el and init/zeroth/*.el")

;;---
;; Bootstrap
;;---
(defconst spydez/init/step/bootstrap-seq
  '(none
    early

    ;; system setup
    ;; §-TODO-§ [2019-10-09]: "or" these two to make figuring out 'current' step
    ;; from completed easier?
    (system default)
    (system specific)

    ;; system done
    (system finalized)

    ;; significant steps
    package

    ;; finished
    complete)
  "Steps for init.el's bootstrap and init/bootstrap/*.el")

;;---
;; Configure
;;---
(defconst spydez/init/step/config-seq
  '(none
    early
    libraries

    ;; emacs, os, daemons, etc
    backend

    ;; significant step
    package

    ;; finished
    complete)
  "Steps for init.el's configuration and init/config/*.el")

;;---
;; Finalize
;;---
(defconst spydez/init/step/finalize-seq
  '(none
    early
    sanity

    ;; hooks, domain, other things put off in other steps until finalize
    system

    ;; finished
    complete)
  "Steps for init.el's finalization and init/finalize/*.el")


;;---
;; Runnnig
;;---
(defconst spydez/init/step/running-seq
  '(none)
  "Init steps for... post-init, normal emacs?")


;;----------
;; Major Steps Of Majority (and also minor alist)
;;----------
(defconst spydez/init/step/major-seq
  '(none zeroth bootstrap config finalize running)
  "Major steps in my init...")
(defconst spydez/init/step/minor-alist
  (list
   (cons 'none      spydez/init/step/none-seq)
   (cons 'zeroth    spydez/init/step/zeroth-seq)
   (cons 'bootstrap spydez/init/step/bootstrap-seq)
   (cons 'config    spydez/init/step/config-seq)
   (cons 'finalize  spydez/init/step/finalize-seq)
   (cons 'running   spydez/init/step/running-seq))
  "Alist of '(major . minor-list) steps.")


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------


(defun __/startup/message (message &rest args)
  "Don't use this, please...
Always returns nil so if/and/or shenanigans can shenan."
  (let* ((type (spydez/init/step/to-type spydez/init/step/completed
                                            'step-missing)))
    (if (functionp 'spydez/message/warning)
        (apply #'spydez/message/warning
               type
               :warning
               message args)
      (lwarn type
             :warning
             message args)))
  ;; always return nil
  nil)


(defun spydez/init/step/to-type (step &rest extras)
  "Turns list/cons STEP (or spydez/init/step/completed list if nil) into a
'type' list appropriate for `spydez/message/*' and `lwarn'
functions. Appends any EXTRAS onto end."
  (let ((type (or step
                  ;; if defaulting, tack 'spydez' on front.
                  (append '(spydez) spydez/init/step/completed))))
    (cond
     ((spydez/consp-and-not-listp type)
      ;; convert to list... without mucking up any variables looking at step...
      (message "hm... %s" (list (car type) (cdr type)))
      (setq type (list (car type) (cdr type))))

     ;; Apparently dotted-pair cons cells qualify as listp so, check after consp
     ((listp type)
      ;; Ok; good as is... Nothing to do here.
      )

     (t
      ;; Dunno what to do with it so make sure we catch it as error.
      (setq type nil)))

    (if (null type)
        (progn
          ;; Bad. Yell and return nil.
          (__/startup/message "%s: Can't convert step? %s %s" step extras)
          nil)
      ;; Add any extras and return list.
      (append type extras))))
;; (spydez/init/step/to-type '(1 . 2) 'jeff 'rob)
;; (spydez/init/step/to-type nil 'jeff 'rob)
;; (spydez/init/step/to-type '(1 . 2))
;; (spydez/init/step/to-type '(1 2))


(require 'seq)
(defun spydez/init/step/set-completed (major minor)
  "Sets step completed to (MAJOR MINOR)."
  (let* ((major-valid (seq-contains spydez/init/step/major-seq major))
         (minor-list (alist-get major spydez/init/step/minor-alist nil))
         (minor-valid (seq-contains minor-list minor)))

    (when (and
           ;; print warning if not valid
           (or major-valid
               (__/startup/message "%s: Unknown major step: '%s'"
                                   "spydez/init/step/set-completed"
                                   major))
           ;; print warning only if major valid but minor not valid
           (or (and major-valid minor-valid)
               (__/startup/message "%s: Unknown minor step: '%s' in %s's %s"
                                   "spydez/init/step/set-completed"
                                   minor major minor-list)))

      ;; ok; set step to major/minor
      (when (functionp 'spydez/message/init-step/done)
        ;; We'll miss out on an early one or two, but ok.
        (spydez/message/init-step/done spydez/init/step/completed
                                       (list major minor)))
      (setq spydez/init/step/completed (list major minor))))
  spydez/init/step/completed)
;; (spydez/init/step/set-completed 'none 'none)
;; (spydez/init/step/set-completed 'jeff 'jeff)
;; (spydez/init/step/set-completed 'none 'jeff)


(defun spydez/init/step/at (expected-major expected-minor)
  "Checks if startup is at expected step of the process (exactly)."
  (let ((expected (list expected-major expected-minor)))
    (if (equal spydez/init/step/completed expected)
        ;; Ok! All's well.
        t

      ;; Nopers.
      (__/startup/message
       (concat "%s: Sanity check failed. Expected to be at step "
               "'%s' but we're on '%s' instead.")
       "spydez/init/step/at"
       expected
       spydez/init/step/completed)
      ;; Return nil on noperses.
      nil)))
;; (spydez/init/step/set-completed 'config 'package)
;; (spydez/init/step/at 'jeff 'jeff)
;; (spydez/init/step/at 'config 'package)


(defun spydez/sequence/compare (current min-desired seq compare-fn)
  "Checks if CURRENT in SEQ is greater than or equal to MIN-DESIRED (also in SEQ).
Returns only t or nil - doesn't error for not-in-sequences."
  ;; curr pos is either found in list, or very big
  (let ((curr-index (or (seq-position seq current)
                        most-negative-fixnum))
        ;; min-desired pos is either found in list, or very big
        (min-desired-index (or (seq-position seq min-desired)
                               most-positive-fixnum)))
    ;; (message "seq/compare? %s (%s) %s %s: ci: %s qi: %s"
    ;;          current compare-fn min-desired seq curr-index min-desired-index)

    ;; So if one of those isn't found, we should have...
    ;;   min-desired: +lots >=     N -> false
    ;;   current:         N >= -lots -> false
    ;; Yeah? Ok.
    (funcall compare-fn curr-index min-desired-index)))
;; jeff >= 2? t:   (spydez/sequence/compare 'jeff 2 '(0 1 2 jeff 3) #'>=)
;; jeff >= 3? nil: (spydez/sequence/compare 'jeff 3 '(0 1 2 jeff 3) #'>=)
;;    0 >= 0? t:   (spydez/sequence/compare 0 0 '(0 1 2 jeff 3) #'>=)
;;     DNE? nil:  (spydez/sequence/compare 'james 3 '(0 1 2 jeff 3) #'>=)
;;     DNE? nil:  (spydez/sequence/compare 3 'james '(0 1 2 jeff 3) #'>=)


(defun spydez/init/step/past-major (minimum)
  "Checks if startup is /past/ the MINIMUM major step of the process."
  ;; minimum has to be in the list.
  (if (not (seq-contains spydez/init/step/major-seq minimum))
      (__/startup/message "%s: Unknown major step: '%s'"
                          "spydez/init/step/past-major"
                          major)
    (spydez/sequence/compare (car spydez/init/step/completed)
                             minimum
                             spydez/init/step/major-seq
                             #'>)))
;; (spydez/init/step/set-completed 'config 'package)
;; t:   (spydez/init/step/past-major 'none)
;; t:   (spydez/init/step/past-major 'bootstrap)
;; nil: (spydez/init/step/past-major 'finalize)


(defun spydez/init/step/past-minor (major minimum)
  "Checks if startup is at major step (exactly) and /past/ the
MINIMUM minor step of the process."
  ;; Major has to match, minor has to be in the list.
  (if (not (eq (car spydez/init/step/completed) major))
      (__/startup/message "%s: Not on major step: '%s'. On %s."
                          "spydez/init/step/past-minor"
                          major
                          spydez/init/step/completed)

    (let ((minor-list (alist-get major spydez/init/step/minor-alist nil)))
      (or (seq-contains minor-list minimum)
          (__/startup/message "%s: Unknown minor step '%s' in '%s' list %s"
                              "spydez/init/step/past-minor"
                              minimum
                              major
                              minor-list))
      (spydez/sequence/compare (cadr spydez/init/step/completed)
                               minimum
                               minor-list
                               #'>=))))
;; (spydez/init/step/set-completed 'config 'package)
;; nil: (spydez/init/step/past-minor 'none 'none)
;; t:   (spydez/init/step/past-minor 'config 'package)
;; nil: (spydez/init/step/past-minor 'config 'complete)


(defun spydez/init/step/past (major minor)
  "Checks if startup is at or past minimum (MAJOR MINOR) step of the process."
  ;; (message "Hello. %s past %s?" (list major minor)
  ;;          spydez/init/step/completed)
  ;; (message "step/past %s past %s? M: %s m: %s"
  ;;          spydez/init/step/completed (list major minor)
  ;;          (spydez/init/step/past-major major)
  ;;          (spydez/init/step/past-minor major minor))

  ;; if past major, everything's fine, so check it first.
  (if (spydez/init/step/past-major major)
      t
    (spydez/init/step/past-minor major minor)))
;; (spydez/init/step/set-completed 'config 'package)
;; t:   (spydez/init/step/past 'none 'none)
;; t:   (spydez/init/step/past 'config 'package)
;; nil: (spydez/init/step/past 'finalize 'none)
;; (spydez/init/step/set-completed 'config 'backend)
;; t:   (spydez/init/step/past 'bootstrap 'package)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(spydez/init/step/set-completed 'zeroth 'post)
(provide 'zeroth-steps)
