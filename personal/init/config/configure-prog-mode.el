;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;-------------------------------mode, not rock.-------------------------------
;;--                      General Programming Mode Stuff                     --
;;-----------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

(bind-key "C-c f" 'find-function)


;;------------------------------------------------------------------------------
;; Align Regex Helpers
;;------------------------------------------------------------------------------
;; `Align-Regex' helper function idea came from this nice chap:
;; http://pragmaticemacs.com/emacs/aligning-text/

(defun spydez/align-before (start end text)
  "Align columns by whitespace before TEXT. E.g. with text \"+=\" and region:
  Jeff.Jet(jeff.it).onClick += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;
becomes
  Jeff.Jet(jeff.it).onClick      += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;

Align currently selected region (if interactive), or region
indicated by START and END.
"
  (interactive "r\nsAlign Before: ")

  (let ((regexp (rx-to-string `(sequence
                                ;; target group: whitespace before input text
                                (group (zero-or-more whitespace))
                                ,text))))
    ;; (message "%S %S %S %S" start end text regexp)
    (align-regexp start end
                  regexp
                  ;; target group 1, min spacing 1, no repeat.
                  1 1 nil)))


(defun spydez/align-after (start end text)
  "Align columns by whitespace after TEXT. E.g. with text \"+=\" and region:
  Jeff.Jet(jeff.it).onClick += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;
becomes
  Jeff.Jet(jeff.it).onClick +=      OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;

Align currently selected region (if interactive), or region
indicated by START and END.
"
  (interactive "r\nsAlign After: ")

  (let ((regexp (rx-to-string `(sequence
                                ,text
                                ;; target group: whitespace after input text
                                (group (zero-or-more whitespace))))))
    ;; (message "%S %S %S %S" start end text regexp)
    (align-regexp start end
                  regexp
                  ;; target group 1, min spacing 1, no repeat.
                  1 1 nil)))


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'configure-prog-mode)
