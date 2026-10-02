;;; Output to a string stream that has nothing to do with the current logical
;;; block (a WITH-OUTPUT-TO-STRING opened inside the block) must not move the
;;; block's column or get its per-line prefix. The text written there used to
;;; count as written to the block, so a later line of the block was indented by
;;; its length. The expected strings are SBCL 2.6.8's.

(defun pus-run (build)
  (let ((*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (s nil)
        (write-string "abc:" s)
        (pprint-newline :mandatory s)
        (pprint-logical-block (s nil :per-line-prefix "  ")
          (format s (funcall build)))))))

(deftest pprint-unrelated-stream.write-string
  (pus-run (lambda ()
             (with-output-to-string (o)
               (write-string "~@<one~@:_two.~:@>" o))))
  "abc:
  one
  two.")

(deftest pprint-unrelated-stream.write-char-terpri
  (pus-run (lambda ()
             (let ((junk (with-output-to-string (o)
                           (write-char #\x o) (terpri o) (write-string "yy" o)
                           (fresh-line o))))
               (declare (ignore junk))
               (copy-seq "~@<one~@:_two.~:@>"))))
  "abc:
  one
  two.")

(deftest pprint-unrelated-stream.inner-text-unprefixed
  (let ((*print-pretty* t) (inner nil))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :per-line-prefix ";; ")
        (setq inner (with-output-to-string (o)
                      (write-string "a" o) (terpri o) (write-string "b" o)))))
    inner)
  "a
b")
