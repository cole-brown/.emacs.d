;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; REST: restclient
;;------------------------------------------------------------------------------
;; https://github.com/pashky/restclient.el
;; Lots of helpful usage there.

;; Found restclient via:
;;   https://emacs.stackexchange.com/questions/2427/how-to-test-rest-api-with-emacs

;; Bare essentials blog post:
;;   https://jakemccrary.com/blog/2014/07/04/using-emacs-to-explore-an-http-api/

;; Once installed, you can prepare a text file with queries.
;;
;; restclient-mode is a major mode which does a bit of highlighting and supports a few additional keypresses:
;;
;;     C-c C-c: runs the query under the cursor, tries to pretty-print the response (if possible)
;;     C-c C-r: same, but doesn't do anything with the response, just shows the buffer
;;     C-c C-v: same as C-c C-c, but doesn't switch focus to other window
;;     C-c C-p: jump to the previous query
;;     C-c C-n: jump to the next query
;;     C-c C-.: mark the query under the cursor
;;     C-c C-u: copy query under the cursor as a curl command
;;     C-c C-g: start a helm session with sources for variables and requests (if helm is available, of course)
;;     C-c n n: narrow to region of current request (including headers)
;;     TAB: hide/show current request body, only if
;;     C-c C-a: show all collapsed regions
(use-package restclient
  ;;---
  :mode
  ;;---
  ;; Set some file extensions to use restclient in
  (("\\.http\\'" . restclient-mode)
   ("\\.restclient\\'" . restclient-mode)
   ;; restclient puts the response buffer into html mode automatically,
   ;; but sometimes I save that response as this extension.
   ("\\.restresponse\\'" . html-mode)))

;; See (spydez/path/to-file spydez/dir/personal/docs "example.restclient")
;; for an example file.
;; TODO: move that and unicode.txt into references if we get rid of other people's emacs files...

;; auto-completion for company in restclient mode
(use-package company-restclient
  :after (restclient know-your-http-well company)

  ;;---
  :init
  ;;---
  (add-to-list 'company-backends 'company-restclient))


;;------------------------------------------------------------------------------
;; Info/Help: know-your-http-well
;;------------------------------------------------------------------------------

;; https://github.com/for-GET/know-your-http-well
(use-package know-your-http-well
  :commands (http-header http-method http-relation http-status-code))
;; M-x http-header ;; content-type
;; M-x http-method ;; post | POST
;; M-x http-relation ;; describedby
;; M-x http-status-code ;; 500
;; M-x http-status-code ;; not_found | NOT_FOUND


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-web)
