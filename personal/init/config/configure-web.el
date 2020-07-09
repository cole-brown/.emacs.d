;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; Web Bookmarks
;;-----------------------------------------------------------------------------

;; §-TODO-§ [2019-10-14]: get web bookmarks working...

;;---
;; Emacs Code
;;---

;; org-protocol set up in configure-org-mode.el

(defconst spydez/file/org/web-bookmarks
  (spydez/path/to-file spydez/dir/org-docs-secrets "web-bookmarks.org")
  "Org-Mode file for web bookmarksfile.")

;; Does this work now?
;; https://orgmode.org/worg/org-contrib/org-protocol.html
;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
(add-to-list 'org-capture-templates
             `("w" "Web Bookmark Capture" entry
               (file+headline spydez/file/org/web-bookmarks "INBOX")
               ,(concat "* %a" ;; title line: org-link of URL/Title
                       "\n\n"  ;; blank line

                       "%U"    ;; inactive date & time stamp
                       "\n\n"  ;; blank line before title/link

                       "%?"    ;; put mark here in front of title when done
                       "%:description" ;; title
                       "\n"
                       "%:link"    ;; URL
                       "\n\n"      ;; blank line before text

                       "%:initial" ;; any selected text
                       "\n\n")     ;; blank before next entry

               ;; just be done - no need to edit before inserting
               :immediate-finish t))

;; Mostly works... Doesn't like a lot of unicode from the web. But whatever.


;;---
;; Browser Bookmark
;;---

;; Something like this?
;;   https://orgmode.org/worg/org-contrib/org-protocol.html#orgeda0362
;;
;; javascript:location.href='org-protocol://capture://w/'+
;;       encodeURIComponent(location.href)+'/'+
;;       encodeURIComponent(document.title||"[untitled page]")+'/'+
;;       encodeURIComponent(window.getSelection())
;;
;; Ok. That's wrong (says outdated), but I got that from the org docs. :| WTF.

;; This one's from org-protocol.el in emacs26, so... Maybe?
;; javascript:location.href='org-protocol://capture?template=w&url='+
;;       encodeURIComponent(location.href)+'&title='+
;;       encodeURIComponent(document.title||"[untitled page]")+'&body='+
;;       encodeURIComponent(window.getSelection())
;; Sigh. No. Or yes and org-capture is fucking it up. IDK.

;; alphapapa's org-protocol-capture-html's bookmarklets:
;;   https://github.com/alphapapa/org-protocol-capture-html#firefox
;; Nope.

;;---
;; Working Browser Bookmark
;;--
;;--                                Use This!
;;--
;; It's "DEPRECATED". It  throws a *Warning*:
;;   "Warning (emacs): Please update your Org Protocol handler to deal with
;;   new-style links."
;; But at least it works?
;;
;; javascript:location.href='org-protocol://capture://w/' +
;; encodeURIComponent(location.href) + '/' + encodeURIComponent(document.title)
;; + '/' + encodeURIComponent(window.getSelection())
;;
;; Dunno why the "correct" way doesn't work. I'mma guess windows paths.
;;---


;;---
;; OS Integration
;;---

;; To setup OS integration, see:
;;   https://orgmode.org/worg/org-contrib/org-protocol.html#orgb0bf7e6
;;
;; Windows:
;;   https://orgmode.org/worg/org-contrib/org-protocol.html#orgf93bb1b
;;
;;   Windows users may register the "org-protocol" once for all by adjusting the
;;   following to their facts, save it as *.reg file and double-click it. This
;;   worked for me on Windows-XP Professional and the emasc23 from
;;   ourcomments.org (http://ourcomments.org/cgi-bin/emacsw32-dl-latest.pl). I'm
;;   no Windows user though and enhancements are more than welcome on the
;;   org-mode mailinglist. The original file is from
;;   http://kb.mozillazine.org/Register_protocol.
;;
;;     REGEDIT4
;;
;;     [HKEY_CLASSES_ROOT\org-protocol]
;;     @="URL:Org Protocol"
;;     "URL Protocol"=""
;;     [HKEY_CLASSES_ROOT\org-protocol\shell]
;;     [HKEY_CLASSES_ROOT\org-protocol\shell\open]
;;     [HKEY_CLASSES_ROOT\org-protocol\shell\open\command]
;;     @="\"C:\\Programme\\Emacs\\emacs\\bin\\emacsclientw.exe\" \"%1\""
;;
;; Or here is the file:
;; (spydez/path/to-file spydez/dir/org-docs-secrets
;;                      "setup-org-bookmark-protocol.reg")
;; NOTE: Open that and update path to emacs executable!!!


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
   ("\\.restresponse\\'" . html-mode))

  ;; §-TODO-§ [2019-11-04]: Look into both:
  ;;   - restclient-helm  - for the helm integration?
  ;;   - ob-restclient    - for putting the restclient calls into a better notes file?
  )

;; See (spydez/path/to-file (spydez/dirky/path :emacs :docs) "example.restclient")
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
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-web)
