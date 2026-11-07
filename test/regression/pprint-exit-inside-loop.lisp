;;; PPRINT-EXIT-IF-LIST-EXHAUSTED and PPRINT-POP leave the logical block, not the
;;; innermost NIL block: inside a LOOP or DOLIST they used to leave only that.

(deftest pprint-exit-inside-loop.loop
  (with-output-to-string (o)
    (pprint-logical-block (o '(a b) :prefix "(" :suffix ")")
      (loop (pprint-exit-if-list-exhausted)
            (write (pprint-pop) :stream o)
            (pprint-exit-if-list-exhausted)
            (write-string " " o))
      (write-string "TAIL" o)))
  "(A B)")

(deftest pprint-exit-inside-loop.rest-of-body-skipped
  (with-output-to-string (o)
    (pprint-logical-block (o '(a b) :prefix "(" :suffix ")")
      (loop (pprint-exit-if-list-exhausted)
            (write (pprint-pop) :stream o)
            (pprint-exit-if-list-exhausted)
            (write-string " " o))
      (write-string "TAIL" o)
      (pprint-newline :mandatory o)))
  "(A B)")

(deftest pprint-exit-inside-loop.print-length
  (let ((*print-length* 2))
    (with-output-to-string (o)
      (pprint-logical-block (o '(a b c d) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream o)
              (write-string " " o)))))
  "(A B ...)")

(deftest pprint-exit-inside-loop.plain-body-and-return
  (list (with-output-to-string (o)
          (pprint-logical-block (o '(a b) :prefix "[" :suffix "]")
            (pprint-exit-if-list-exhausted)
            (write (pprint-pop) :stream o)
            (write-string "-" o)
            (write (pprint-pop) :stream o)))
        (block nil
          (with-output-to-string (o)
            (pprint-logical-block (o '(a) :prefix "(" :suffix ")")
              (return :escaped)))))
  ("[A-B]" :escaped))
