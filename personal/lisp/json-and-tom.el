;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;--------------------------------JSON and TOML---------------------------------
;;--                          Sitting in a tree...                            --
;;------------------------------------KISS--------------------------------------



;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defconst spydez/json/string-placeholder "�"
  "Placeholder for another character while munging JSON strings.

Character is Unicode Specials code block:
   U+FFFD
   �
   REPLACEMENT CHARACTER
     - Used to replace an unknown, unrecognized or unrepresentable character.")

;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

;; Python 2 and 3 both do this for a dict:
;; >>> foo = {"hi":"hello I'm here"}
;; >>> foo
;; {'hi': "hello I'm here"}
;; >>> foo[2] = "hello ' \""
;; >>> foo
;; {2: 'hello \' "', 'hi': "hello I'm here"}
;;
;; So only /need/ to make sure I don't turn escaped singles into doubles. But a
;; better solution would be better.
(defun spydez/json/pp/py2el (from to &optional ordered)
  "Turns a python printed dictionary (JSON) into real JSON capable of being
formatted/used by json.el. Then calls json-pretty-print(-ordered) on it.

Defaults to `json-pretty-print-ordered' unless prefix arg set.
"
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (region-beginning)
                       (region-end)
                       ;; Yes ordered if no prefix arg.
                       (null current-prefix-arg))))

  (let* (;; (start-point (point-marker))
        ;; (start-bol (line-beginning-position))
        ;; (start-eol (line-end-position))
        (conversion-start (min from to))
        (conversion-end (max from to))
        (region (string-trim (delete-and-extract-region conversion-start
                                                        conversion-end)))
        (pp-start conversion-start)
        pp-end
        (pp-func (if ordered
                     #'json-pretty-print-ordered
                   #'json-pretty-print)))

    ;; Insert back into buffer.
    (insert
     ;; Convert Python's long (1234L) into JSON number (1234).
     (replace-regexp-in-string
      ;; Start match at word-boundry...
      (rx word-start
          ;; Caption optional starting whitespace and then our number...
          (group (optional space) (one-or-more digit))
          ;; L should be the final char in the word.
          "L"
          word-end
          ;; Must be the end of the JSON thing (array, dict, key, value).
          ;; Capture this in group 2.
          ;; (Not sure if I need this requirement, really...)
          (group (any "]},")))
      ;; Our replacement is just both captured groups.
      (rx (backref 1) (backref 2))

      ;; True -> true
      (replace-regexp-in-string
       "True" "true"

       ;; False -> false
       (replace-regexp-in-string
        "False" "false"

        ;; ' -> ", but not already-quoted ones (we hid those below).
        (replace-regexp-in-string
         "'" "\""

         ;; u' -> '
         (replace-regexp-in-string
          "u'" "'"

          ;; Remove trailing comma?
          (string-trim-right
           ;; Hide escaped quotes first so we don't stomp on
           ;; them because I don't have the regex-fu to
           ;; solve that garbage without negative
           ;; lookbehinds.
           (replace-regexp-in-string
            "\\\\'"
            spydez/json/string-placeholder
            region)
           ",")
          ))
        t) ;; Do not "Smart Case" replace "false".
       t)))  ;; Do not "Smart Case" replace "true".

    ;; `insert' moved our point to the end of the inserted text...
    (setq pp-end (point))

    ;; Use start and end points of new text to call json-pretty-print.
    (funcall-interactively pp-func pp-start pp-end)

    (mis/message/propertize t :default :text
                            "py->JSON pretty print (%s)"
                            (if ordered "ordered" "unordered"))))

;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'json-and-tom)
