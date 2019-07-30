;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Links
;;------------------------------------------------------------------------------
;; Most/all of this (for now) comes from this blog post:
;;   https://writequit.org/articles/working-with-logs-in-emacs.html
;; by Elasticsearch contributor Lee Hinman (lee@elastic.co)


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; M-x auto-revert-tail-mode
;;------------------------------------------------------------------------------
;; This can tail follow a file like `tail -f'.

;; StackOverflow has some downsides for auto-revert-tail-mode:
;;   https://emacs.stackexchange.com/a/15213 (see sub-comment for rename buffer)
;; So sometimse tail with an actual `tail -f' is good(er).

;;   "auto-revert-tail-mode is great, but it has its limits. Therefore I prefer
;; to use an asynchronous shell command. Open the remote directory in dired,
;; position the cursor to the file you want to watch, and apply ! tail -f * &.
;;
;;   "If you want to suppress Tramp messages, decrease the verbosity. (setq
;; tramp-verbose 1) shall be sufficient."


;;------------------------------------------------------------------------------
;; View Large Files
;;------------------------------------------------------------------------------
;; https://github.com/m00natic/vlfi
;; https://elpa.gnu.org/packages/vlf.html
(use-package vlf
  ;; diminish: probably don't want to try until I see the modeline.
  ;; It has chunk size, total file size.

  :config
  (require 'vlf-setup)
  ;; C-c C-v prefix by default. Can be changed.
  )

;; Basic Moving Around
;;
;; Scrolling automatically triggers move to previous or next chunk at the
;; beginning or end respectively of the current one.
;;
;; C-c C-v n and C-c C-v p: move batch by batch. With positive prefix argument
;; they move prefix number of batches. With negative - append prefix number of
;; batches.
;;
;; C-c C-v SPC: displays batch starting from current point.
;;
;; C-c C-v [ and C-c C-v ]: take you to the beginning and end of file
;; respectively.
;;
;; C-c C-v j: jump to particular batch number.


;;------------------------------------------------------------------------------
;; follow-mode?
;;------------------------------------------------------------------------------
;; M-x follow-mode

;; "Sometimes when viewing a file, you don't have enough vertical space. To
;; alleviate this, Emacs has a feature called follow-mode. If you split the
;; buffer vertically with C-x 3 and then invoke M-x follow-mode Emacs will treat
;; the pane on the left as the "top" of the buffer and the pane on the right as
;; the "bottom", making the viewing part essentially twice as high. If you
;; scroll in one pane the other pane will follow. It's great for viewing very
;; large files as well as large programming methods!"
;;   - https://writequit.org/articles/working-with-logs-in-emacs.html


;;------------------------------------------------------------------------------
;; Highlight Line Mode: hl-line-mode
;;------------------------------------------------------------------------------

;; Makes some lag/slow-down? At least, the 14 MB file in VLF mode on 2nd chunk,
;; when in hl-line-mode, would get slower and jankier the longer I
;; did/held-down C-n.

;; But it is nice for seeing what all in this log a line is.


;;------------------------------------------------------------------------------
;; View Mode: view-mode
;;------------------------------------------------------------------------------

;; View mode is a minor mode that lets you scan a buffer by sequential
;; screenfuls. It provides commands for scrolling through the buffer
;; conveniently but not for changing it. Apart from the usual Emacs cursor
;; motion commands, you can type <SPC> to scroll forward one windowful, S-<SPC>
;; or <DEL> to scroll backward, and s to start an incremental search.
;;   - https://www.gnu.org/software/emacs/manual/html_node/emacs/View-Mode.html

;; Can do (more) view/move keybinds for view-mode-map to help out too.
;; Or even make a hydra. See:
;;   https://writequit.org/articles/working-with-logs-in-emacs.html


;;------------------------------------------------------------------------------
;; Narrow/Widen
;;------------------------------------------------------------------------------

;; https://writequit.org/articles/working-with-logs-in-emacs.html
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html


;;------------------------------------------------------------------------------
;; JSON
;;------------------------------------------------------------------------------

;; Some stuff emacs already has:
;;   M-x json-pretty-print         - expand selected region to pretty print
;;   M-x json-pretty-print-ordered - like above, also sort keys
;;   and there are json-pretty-print-buffer, json-pretty-print-buffer-ordered

;; json-mode and json-reformat if more viewing/editting of json needed.
;;
;; Whole big thing: JSON Navigator
;;   https://github.com/DamienCassou/json-navigator


;;------------------------------------------------------------------------------
;; Hiding JSON w/ Hideshow
;;------------------------------------------------------------------------------
;; Enable hs-minor-mode to prog-modes...
;; TODO: Move into there if this is useful there.
;; TODO: Enable for logs... How? VLF mode hook? Make a new, simple Log Mode?
;; TRIAL: [2019-03-05]
(use-package hideshow
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))


;;------------------------------------------------------------------------------
;; Helm
;;------------------------------------------------------------------------------

;; Helm-occur and helm-swoop have usefulness in searching logs... They're
;; already in helm or in configure-completion.el


;;------------------------------------------------------------------------------
;; Silver Searcher?
;;------------------------------------------------------------------------------

;; An external search tool that can be used by emacs.
;; Have notes to try it elsewhere. Probably helm-ag.


;;------------------------------------------------------------------------------
;; keep-lines / flush-lines
;;------------------------------------------------------------------------------

;; If you do a
;;   M-x keep-lines RET memory\|gc\|error
;; the log will then become filtered down to just those lines matched.
;; This happens from point to end of file.

;; There is also the inverse:
;;   M-x flush-lines
;; e.g.
;;   M-x flush-lines RET ^org\|.+at[[:space:]]


;;------------------------------------------------------------------------------
;; Number of Occurances
;;------------------------------------------------------------------------------

;; M-x how-many RET failed to send ping

;;   "If you only care about how many occurrences of something is in the file,
;; you can use how-many, so if I wanted to know how many failed pings there were
;; in the file, I would do M-x how-many and then type failed to send ping, and
;; then I'll get back in the modeline: 10 occurances
;;   - https://writequit.org/articles/working-with-logs-in-emacs.html


;;------------------------------------------------------------------------------
;; TODOs
;;------------------------------------------------------------------------------

;; Pop up a shell?
;; `eshell-here' and `shell-here' functions here:
;;   - https://writequit.org/articles/working-with-logs-in-emacs.html


;;------------------------------------------------------------------------------
;; Provide this.
;;------------------------------------------------------------------------------
(provide 'configure-logs)
