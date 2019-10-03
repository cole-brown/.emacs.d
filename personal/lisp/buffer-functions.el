;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;----------------------------------Buffers------------------------------------
;;--                    And Things to Make Them Better.                      --
;;-----------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-10-02]: move these spydez/buffer/* things into... a better file.
(defcustom spydez/buffer/format/bookend-normal
  '("§-" "-§")
  "Start/end strings for special-name formats."
  :group 'spydez/group
  :type '(list string string))


(defcustom spydez/buffer/format/bookend-high
  '("§!" "!§")
  "Start/end strings for special-name formats."
  :group 'spydez/group
  :type '(list string string))


(defcustom spydez/buffer/format/priorities
  '((low    . spydez/buffer/format/bookend-normal) ;; no actual low right now
    (medium . spydez/buffer/format/bookend-normal)
    (high   . spydez/buffer/format/bookend-high))
  "Priority (for `spydez/buffer/special-name') to bookend consts."
  :group 'spydez/group
  :type '(alist :key-type symbol :value-type symbol))


(defcustom spydez/buffer/regexp/bookend
  (rx string-start "§" (one-or-more printing) "§" string-end)
  "Regexp for matching a bookended buffer name string."
  :group 'spydez/group
  :type 'regexp)


;;-----------------------------------------------------------------------------
;; Naming Functions
;;-----------------------------------------------------------------------------

(defun spydez/buffer/special-name (title &optional desc priority)
  "Format string for `spydez/buffer/special-name' with description."

  ;; PRIORITY is either known or forced to medium
  (let ((priority (if (memq priority '(low medium high)) priority 'medium))
        ;; look for bookends in list, default if fail/nil
        (bookends (or (symbol-value
                       (cdr (assoc priority spydez/buffer/format/priorities)))
                      spydez/buffer/format/bookend-normal))
        ;; "title" or "title: desc"
        (inner-fmt (if (null desc) "%s" "%s: %s")))

    ;; inner format: "title" or "title: desc", as decided above by `inner-fmt'
    (format
     ;; outer format: "<bookend> <inner-format-%s> <bookend>"
     (format "%s %s %s"
             (nth 0 bookends)
             inner-fmt
             (nth 1 bookends))
     title desc)))
;; (spydez/buffer/special-name "jeff")
;; (spydez/buffer/special-name "jeff" "is here")
;; (spydez/buffer/special-name "jeff" nil 'high)
;; (spydez/buffer/special-name "jeff" "is here" 'high)





;;------------------------------------------------------------------------------
;; Bury Functions
;;------------------------------------------------------------------------------

(defun spydez/buffer/bury-visible (buffer-or-name)
  "Bury BUFFER-OR-NAME buffer, whatever window has it right now."
  (let ((curr-buff (current-buffer))
        (curr-name (buffer-name)))
    (save-excursion
      (pop-to-buffer buffer-or-name)
      (bury-buffer))
    ;; bury left me in the wrong window, probably?
    (unless (string-equal curr-name buffer-or-name)
      (pop-to-buffer (current-buffer)))))


;;------------------------------------------------------------------------------
;; Kill Functions
;;------------------------------------------------------------------------------

;; Like `kill-buffer-ask' but no confirmation for unmodified buffers.
(defun spydez/buffer/kill-ask (buffer &optional delete-process)
  "Kill BUFFER if confirmed. No confirm given for unmodified
buffers; just kill.

Returns buffer-name on kill, nil on no kill."
  ;; so... kill?
  (if (or
         ;; just kill it if not modified
         (not (buffer-modified-p buffer))
         ;; or ask first if modded
         (yes-or-no-p (format "Buffer '%s' HAS BEEN EDITED.  Kill? "
                                 (buffer-name buffer))))
    ;; ok - kill
    (prog1
        ;; return name when killed
        (buffer-name buffer)
      (when delete-process (spydez/buffer/delete-buffer-process buffer))
      (kill-buffer buffer))
    ;; else, ret nil when no kill
    nil))


(defun spydez/buffer/kill-special (buffer-or-name-regexp)
  "Kills my special(ly named) buffers, deleting any process they
may have running.

If BUFFER-OR-NAME-REGEXP is a string, the
`spydez/buffer/regexp/bookend' is checked to see if the regexp is
'correctly' guarded by them, adding them in if needed."
  (if (stringp buffer-or-name-regexp)
      ;; We have a string. Make sure it's formatted as "special-buffer",
      ;; and then try to kill it without confirmation.
      (let ((buffer-or-name-regexp
             (or (string-match-p
                  spydez/buffer/regexp/bookend buffer-or-name-regexp)
                 (spydez/buffer/special-name buffer-or-name-regexp))))
        (spydez/buffer/kill-matching buffer-or-name-regexp nil t t t))
    ;; else we have a buffer, probably?
    (spydez/buffer/delete-buffer-process buffer-or-name-regexp)
    (kill-buffer buffer-or-name-regexp)))


(defun spydez/buffer/delete-buffer-process (buffer-or-name)
  "Gets buffer, gets buffer's process (if any), and ends/deletes/kills/SIGKILLs
it. BUFFER-OR-NAME must be exact."
  (let ((proc (get-buffer-process (get-buffer buffer-or-name))))
    (if (not (process-live-p proc))
        (message "No live process in '%s'?" buffer-or-name)
      (delete-process proc))))


;;------------------------------------------------------------------------------
;; Better Kill-Matching-Buffer
;;------------------------------------------------------------------------------

;; I could just grub my meaty hooks into kill-matching-buffers, but... eh.
;; I'd probably just forget the function that way.
(defun spydez/buffer/kill-matching (regexp &optional
                                           internal-too
                                           no-ask
                                           delete-process
                                           quiet)
  "Kill buffers whose name matches the specified REGEXP.

Ignores buffers whose name starts with a space (internal
buffers), unless optional prefix argument INTERNAL-TOO is
non-nil.

Asks before killing each buffer if it is modified,
unless NO-ASK is non-nil.

Deletes buffer's process (if any) if DELETE-PROCESS is non-nil."
  (interactive "sKill buffers matching regex: ")

  ;; TODO: find all matching buffers and do a "does this look about right?"
  ;; prompt?

  (let ((killed-names ()))
    ;; for all open buffers...
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        ;; (message "kill-match plz name:%s e?%s it?%s regex?%s ok?%s"
        ;;          ;; has a name
        ;;          name
        ;;          ;; not empty
        ;;          (not (string-equal name ""))
        ;;          ;; internal-too or not internal
        ;;          (or internal-too (/= (aref name 0) ?\s))
        ;;          ;; matches regex
        ;;          (string-match regexp name)
        ;;          ;; total
        ;;          (and name (not (string-equal name ""))
        ;;               (or internal-too (/= (aref name 0) ?\s))
        ;;               (string-match regexp name)))

        ;; check name obeys `internal-too' type restriction and matches regex
        (when (and name (not (string-equal name ""))
                   (or internal-too (/= (aref name 0) ?\s))
                   (string-match regexp name))
          ;; and kill it maybe
          (if no-ask
              ;; either just kill it...
              (progn
                (push name killed-names)
                (when delete-process
                  (spydez/buffer/delete-buffer-process buffer))
                (kill-buffer buffer))
            ;; ...or probably kill it? Save the name if so.
            (let ((maybe-kill-name (spydez/buffer/kill-ask buffer
                                                           delete-process)))
              (unless (null maybe-kill-name)
                (push maybe-kill-name killed-names)))))))
    (if quiet
        ;; return number of buffers killed if quiet Mode
        (length (or killed-names '()))
      ;; And finally, give some goddamn output (looking at you,
      ;; kill-matching-buffers).
      (cond
       ((null killed-names)
        (message "No buffers killed matching '%s'."
                 regexp))
       ((>= (length killed-names) 10)
        (message "Killed %s buffers matching '%s'."
                 (length killed-names)
                 regexp))
       (t
        (message "Killed %s buffers matching '%s': %s"
                 (length killed-names)
                 regexp
                 killed-names))))))


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'buffer-functions)
