;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Link-related Convenience Functions

;; TODO: configure-text, configure-point-and-mark, configure-dev-env, configure-files-and-folders...
;; ...configure-links...
;; check to see if they've got the correct pieces

;; https://github.com/ahungry/md4rd
;;   https://www.reddit.com/r/emacs/comments/apu0ii/browse_reddit_in_emacs/
;; https://www.reddit.com/r/linux/comments/aot1ga/benno_rice_the_tragedy_of_systemd_linuxconfau_2019/

;;------------------------------------------------------------------------------
;; M-x goto-address-mode
;;------------------------------------------------------------------------------

;; todo: make a note or something about M-x goto-address-mode?
;;   tags for when I want to find this again but can't: link click url at point at-point fuuuuuuuu ur mom
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html
;;   Its neat but I'll probably not often use it.


;;------------------------------------------------------------------------------
;; M-x org-time-stamp-inactive
;;------------------------------------------------------------------------------
;; For stamps like: [2019-01-30]
;; Has been aliased to `spydez/timestamp-ORG' as I can never remember it.

;; Use `M-x org-time-stamp-inactive' or `M-x spydez/timestamp-ORG' for inserting
;; `[2019-01-29 Tue]' type stamps into current non-org-mode buffer.

;; Or use `C-c !' for org-mode buffer.
;; https://orgmode.org/manual/Creating-timestamps.html


;;------------------------------------------------------------------------------
;; From Sacha.org
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html

;; TODO: put these in a namespace i.e. spydez/foo -> spydez/url/foo

;; a test url: (spydez/resolve-redirect "http://bit.ly/tdotco")
;; orig-name: kensanata/resolve-redirect
;; Trial [2019-01-30]
;; TODO: interactive? Does this even work (on windows)?
;;  Oh, derp, next func uses.
(defun spydez/resolve-redirect (url)
  "Resolve shortened URL by launching `curl --head' and parsing the result."
  (let* ((curl (shell-command-to-string
                (format "curl --silent --head %s" url)))
         (location (when (and (string-match "^HTTP/1\.1 301" curl)
                              (string-match "^Location: \\(.*\\)" curl))
                     (match-string 1 curl))))
    (or location url)))

;; TODO: Does this even work (on windows)?
;; Trial [2019-01-30]
(defun spydez/resolve-urls-in-region (beg end)
  "Expand URLs between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (replace-match (save-match-data (spydez/resolve-redirect
                                         (match-string 1))) t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward org-link-re-with-space nil t)
        (replace-match (save-match-data (spydez/resolve-redirect
                                         (match-string 0))) t t nil)))))

;; Trial [2019-01-30]
(defun spydez/open-urls-in-region (beg end)
  "Open URLs between BEG and END.
TODO: Get better at detecting and opening all URLs"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-any-link-re nil t)
        (save-excursion
          (backward-char)
          (org-open-at-point))))))


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-links)
