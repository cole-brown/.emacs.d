;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Strings
;;------------------------------------------------------------------------------

(defun spydez/list-join (join prefix &rest args)
  (let ((full-list (cons prefix args)))
    (mapconcat (function (lambda (x) (format "%s" x)))
               full-list
               join)))


;;------------------------------------------------------------------------------
;; Hashes
;;------------------------------------------------------------------------------

(defconst spydez/hash/default 'sha512
  "Default hashing function to use for spydez/hash-input.")
(defconst spydez/hash/slice 4
  "Default hashing slice size to use for spydez/hash-and-reduce.")
(defconst spydez/hash/join "-"
  "Default hashing slice size to use for spydez/hash-and-reduce.")
(defconst spydez/hash/prefix "hash"
  "Default hashing slice size to use for spydez/hash-and-reduce.")

;; Wanted to use dash for this but its way before packages have been bootstrapped.
;; Well, even more now that it's in early-init.el...
(defun spydez/hash-input (input &optional hash)
  "Returns a hash string of input. Hard-coded to sha512 atm."
  (let ((hash (or hash spydez/hash/default))) ;; set hash to default if unspecified
    (if (member hash (secure-hash-algorithms)) ;; make sure it exists as a supported algorithm
        (secure-hash hash input)
      (error "Unknown hash: %s" hash))
    ))

(defun spydez/hash-and-reduce (input &optional prefix hash slice join)
  (let* ((hash-full (spydez/hash-input input hash))
         ;; (hash-len (length hash-full)) ;; negative numbers work in substring, yay
         (slice (or slice spydez/hash/slice))
         (join (or join spydez/hash/join))
         (prefix (or prefix spydez/hash/prefix)))
    ;; (mis/debug "hashed: %s %s %s %s" input prefix hash slice join)
    (spydez/list-join join
                      prefix
                      (substring hash-full 0 slice)
                      (substring hash-full (- slice) nil))
    ))


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'zeroth-funcs)
