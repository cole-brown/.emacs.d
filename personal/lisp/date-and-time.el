;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-----------------------------------who?---------------------------------------
;;--                 Dates, Times, Datetimes, Timedates...                    --
;;------------------------------------------------------------------------------

;; best format
(defcustom spydez/datetime/format/yyyy-mm-dd "%Y-%m-%d"
  "My favorite, and the best, date format."
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/yyyy-mm-dd)

;; best format w/ time
(defcustom spydez/datetime/format/yyyy-mm-dd_hh-mm-ss "%Y-%m-%d %H:%M:%S"
  "My favorite, and the best, date & time format."
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/yyyy-mm-dd)

(defcustom spydez/datetime/format/org-inactive-derivative "[%Y-%m-%d]"
  "org-time-stamp-inactive e.g.: [2019-09-19 Thu]
mine e.g.: [2019-09-19]"
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/org-inactive-derivative)

;; pretty ok format & ISO standard
(defcustom spydez/datetime/format/ISO-8601 "%Y-%m-%dT%T%z"
  "ISO format, used in logs and such..."
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/ISO-8601)

;; other
(defcustom spydez/datetime/format/yyyymmdd "%Y%m%d"
  "Why would you use this?!"
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/yyyymmdd)

;; pretty much worst format
(defcustom spydez/datetime/format/dd-mon-yy "%d-%b-%y"
  "Bad, ye olde human format. Trash."
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/dd-mon-yy)

;; And... the terrible USA formats.
(defcustom spydez/datetime/format/mm-dd-yyyy "%m-%d-%Y"
  "Bad, US American format. I am ashamed."
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/mm-dd-yyyy)
(defcustom spydez/datetime/format/mm-dd-yy "%m-%d-%y"
  "Extra bad, US American format. I am ashamed."
  :group 'spydez/group
  :type 'string)
;;(format-time-string spydez/datetime/format/mm-dd-yy)


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
;; (spydez/datetime/next-friday spydez/datetime/format/yyyy-mm-dd)


;; Insert ISO 8601 format timestamp.
(defun spydez/datetime/timestamp-ISO ()
  "Produces and inserts a full ISO 8601 format timestamp of NOW."
  (interactive)
  (insert (format-time-string spydez/datetime/format/ISO-8601)))


;; Insert an org-mode inactive format...ish timestamp (e.g. '[2019-04-24]')
(defun spydez/datetime/timestamp-ORG ()
  "Produces and inserts a timestamp of [yyyy-mm-dd], similar to
inactive ORG timestamp."
  (interactive)
  (insert (format-time-string spydez/datetime/format/org-inactive-derivative)))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'date-and-time)
