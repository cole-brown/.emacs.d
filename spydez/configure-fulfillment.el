;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; XKCD
;;------------------------------------------------------------------------------
(use-package xkcd)


;;------------------------------------------------------------------------------
;; reddit
;;------------------------------------------------------------------------------

;; TODO: move that reddit thing to here? (my various reddit rss/json thing)
;; TODO: find/make a function for getting my subscribed reddits (and formatting to org-mode list?)

;; https://www.reddit.com/r/emacs/comments/6w5xav/is_there_a_reddit_viewer_for_emacs_ala_orgmode/
;;  - actual general reddit browsing emacs packages


;;------------------------------------------------------------------------------
;; Weather
;;------------------------------------------------------------------------------
;; These are old and bad:
;;   http://pages.sachachua.com/.emacs.d/Sacha.html#orgd9057e7
;;   https://github.com/syohex/forecast.el
;; Melpa uses this one, and it has different vars and setup.
;;   https://github.com/cadadr/elisp/blob/devel/forecast.el
(use-package forecast
  :defer t
  :config
  (load spydez/file/secrets)
  (setq forecast-api-key secret/api-key/darksky
        calendar-latitude secret/forecast/latitude
        calendar-longitude secret/forecast/longitude
        calendar-location-name secret/forecast/city
        forecast-units 'us
        )
  )

;; TODO: this for rain and moon:
;;   "See also the docstring for the face `forecast-moon-phase', which
;;   governs the face for the moon phase visualisation.  Most fonts will
;;   not have defined the necessary characters, thus one may need to
;;   install a special font, e.g. Quivira (http://quivira-font.com/)."

;; TODO: I had a todo somewhere for trying out always loading many/most/all packages.
;; The secret keyword for use-package is ":demand".


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-fulfillment)
