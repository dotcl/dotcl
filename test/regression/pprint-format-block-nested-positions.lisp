;;; A FORMAT logical block nested in another logical block (one opened by
;;; PPRINT-LOGICAL-BLOCK or by an enclosing ~<...~:>) after some text. The
;;; outer block buffers the conditional newlines as positions in its own
;;; output, and the nested block used to record its newlines as positions in a
;;; separate buffer: the break landed inside a word (a line ending in "hhh"
;;; and the next starting with "h"), or, under a prefix, never happened. The
;;; expected strings are SBCL 2.6.8's.

(defvar *pfbnp-ctl* "~@<aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk~:@>")

(defun pfbnp-print (fn)
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* 40))
      (funcall fn s))))

(deftest pprint-format-block-nested-positions.after-text
  (pfbnp-print (lambda (s)
                 (pprint-logical-block (s nil)
                   (write-string "- " s)
                   (format s *pfbnp-ctl*))))
  "- aaaa bbbb cccc dddd eeee ffff gggg
  hhhh iiii jjjj kkkk")

(deftest pprint-format-block-nested-positions.per-line-prefix
  (pfbnp-print (lambda (s)
                 (pprint-logical-block (s nil :per-line-prefix "  ")
                   (write-string "- " s)
                   (format s *pfbnp-ctl*))))
  "  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk")

(deftest pprint-format-block-nested-positions.prefix
  (pfbnp-print (lambda (s)
                 (pprint-logical-block (s nil :prefix "  ")
                   (write-string "- " s)
                   (format s *pfbnp-ctl*))))
  "  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk")

(deftest pprint-format-block-nested-positions.in-format-block
  (pfbnp-print (lambda (s)
                 (format s "~@<- ~@<aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk~:@>~:>")))
  "- aaaa bbbb cccc dddd eeee ffff gggg
  hhhh iiii jjjj kkkk")

;; A literal newline inside the nested block starts the next line with the
;; per-line prefix, and the fill newline after it breaks (the section before it
;; did not fit on one line).
(deftest pprint-format-block-nested-positions.literal-newline
  (with-output-to-string (s)
    (let ((*print-pretty* t))
      (pprint-logical-block (s nil)
        (format s "~@<~S in check:~:@_~:@>" :foo)
        (pprint-logical-block (s nil :per-line-prefix "  ")
          (format s "~@<FORMAT-ARGS~A with new line.~:@>" (format nil "~%"))))))
  ":FOO in check:
  FORMAT-ARGS

  with new line.")
