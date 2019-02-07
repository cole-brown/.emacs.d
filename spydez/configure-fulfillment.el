;; -*- emacs-lisp -*-


;;------------------------------------------------------------------------------
;; XKCD
;;------------------------------------------------------------------------------
(use-package xkcd)


;;------------------------------------------------------------------------------
;; reddit
;;------------------------------------------------------------------------------

;; TODO: move that reddit thing to here?
;; TODO: find/make a function for getting my subscribed reddits (and formatting to org-mode list?)


;;------------------------------------------------------------------------------
;; Weather
;;------------------------------------------------------------------------------
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgd9057e7
;; https://github.com/syohex/forecast.el
;; TODO: get this working (get dev key?)
;; (use-package forecast
;;   :config
;;   (setq forecast-city "Toronto"
;;         forecast-latitude 43.6486
;;         forecast-longitude -79.3853
;;         )
;;   )


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-fulfillment)
