;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------------who?---------------------------------------
;;--                 Dates, Times, Datetimes, Timedates...                    --
;;------------------------------------------------------------------------------

;; best format
(setq spydez/datetime/format/yyyy-mm-dd "%Y-%m-%d")

;; pretty ok format & ISO standard
(setq spydez/datetime/format/ISO-8601 "%Y-%m-%dT%T%z")

;; pretty much worst format
(setq spydez/datetime/format/dd-mon-yy "%d %b %y")

;; And... the terrible USA formats.
(setq spydez/datetime/format/mm-dd-yyyy "%%m-%d-%Y")
(setq spydez/datetime/format/mm-dd-yy "%%m-%d-%y")


;;------------------------------------------------------------------------------
;; Timestamp Functions (& Datestamp functions?)
;;------------------------------------------------------------------------------

;; Was used in an old weekly-status template
(defun spydez/datetime/next-friday (format)
  (let ((today (nth 6 (decode-time (current-time)))))
    (format-time-string 
     format
     (time-add 
      (current-time) 
      (days-to-time 
       (if (eq 5 today) ; saturday is only day bigger than friday
           6
         (- 5 today)))))))

;; Insert ISO 8601 format timestamp.
(defun spydez/datetime/timestamp-ISO ()
  "Produces and inserts a full ISO 8601 format timestamp of NOW."
  (interactive)
  (insert (format-time-string spydez/datetime/format/ISO-8601)))

;; Insert an org-mode inactive format timestamp (e.g. '[2019-04-24 Wed]')
(defalias 'spydez/datetime/timestamp-ORG 'org-time-stamp-inactive
  "Produces and inserts an abbreviated timestamp using org-time-stamp-inactive.")


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'date-and-time)
