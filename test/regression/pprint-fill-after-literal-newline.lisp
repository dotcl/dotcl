;;; A fill newline after a section that holds a literal newline.
;;;
;;; PPRINT-NEWLINE :FILL also breaks when the preceding section was not
;;; printed on a single line. A literal newline (~&, ~%, TERPRI, a newline in
;;; a string) is not a conditional newline, so it does not end the section,
;;; and the next fill newline must break. A mandatory newline (~@:_) is a
;;; conditional newline and starts a new section. The pretty printer could not
;;; tell the two apart and never broke at such a fill newline. The expected
;;; strings are SBCL 2.6.8's.

(deftest pfaln-fresh-line
  (format nil "~@<A~&B. ~:_C~:>")
  "A
B.
C")

(deftest pfaln-mandatory-does-not-break
  (format nil "~@<A~@:_B. ~:_C~:>")
  "A
B. C")

(deftest pfaln-fresh-line-twice
  (format nil "~@<A~&B. ~:_C:~&D~:>")
  "A
B.
C:
D")

(deftest pfaln-only-first-fill-breaks
  (format nil "~@<A~%B ~:_C ~:_D~:>")
  "A
B
C D")

(deftest pfaln-mandatory-resets
  (format nil "~@<A~&B ~:_C~@:_D ~:_E~:>")
  "A
B
C
D E")

(deftest pfaln-literal-after-first-fill
  (format nil "~@<A ~:_B~%C ~:_D ~:_E~:>")
  "A B
C
D E")

(deftest pfaln-per-line-prefix
  (format nil "~@<> ~@;A~&B. ~:_C~:>")
  "> A
> B.
> C")

(deftest pfaln-pprint-newline-terpri
  (with-output-to-string (s)
    (let ((*print-pretty* t))
      (pprint-logical-block (s nil)
        (write-string "A" s)
        (terpri s)
        (write-string "B. " s)
        (pprint-newline :fill s)
        (write-string "C" s))))
  "A
B.
C")

(deftest pfaln-pprint-newline-mandatory
  (with-output-to-string (s)
    (let ((*print-pretty* t))
      (pprint-logical-block (s nil)
        (write-string "A" s)
        (pprint-newline :mandatory s)
        (write-string "B. " s)
        (pprint-newline :fill s)
        (write-string "C" s))))
  "A
B. C")

(deftest pfaln-no-newline-unchanged
  (format nil "~@<A B ~:_C~:>")
  "A B C")
