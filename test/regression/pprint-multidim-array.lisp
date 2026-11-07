;;; A rank >= 2 array printed with *print-pretty* is laid out as nested logical
;;; blocks, breaking lines at the right margin like a vector does.

(deftest pprint-multidim-array.rank2-breaks
  (let ((*print-pretty* t) (*print-right-margin* 40))
    (write-to-string (make-array '(2 2) :initial-element 12345678901234567890)))
  "#2A((12345678901234567890
     12345678901234567890)
    (12345678901234567890
     12345678901234567890))")

(deftest pprint-multidim-array.rank2-fill
  (let ((*print-pretty* t) (*print-right-margin* 40))
    (write-to-string (make-array '(2 3) :initial-element 123456789012)))
  "#2A((123456789012 123456789012
     123456789012)
    (123456789012 123456789012
     123456789012))")

(deftest pprint-multidim-array.rank3
  (let ((*print-pretty* t) (*print-right-margin* 40))
    (write-to-string (make-array '(2 2 2) :initial-element 12345678901234567)))
  "#3A(((12345678901234567
      12345678901234567)
     (12345678901234567
      12345678901234567))
    ((12345678901234567
      12345678901234567)
     (12345678901234567
      12345678901234567)))")

(deftest pprint-multidim-array.fits-on-a-line
  (let ((*print-pretty* t))
    (list (write-to-string (make-array '(2 2) :initial-contents '((1 2) (3 4))))
          (write-to-string (make-array '(2 0)))
          (write-to-string (make-array '(0 3)))
          (let ((*print-pretty* nil)) (write-to-string (make-array '(2 2) :initial-element 7)))))
  ("#2A((1 2) (3 4))" "#2A(() ())" "#2A()" "#2A((7 7) (7 7))"))

(deftest pprint-multidim-array.print-length-and-level
  (let ((*print-pretty* t))
    (list (let ((*print-length* 1)) (write-to-string (make-array '(2 2) :initial-element 5)))
          (let ((*print-level* 1)) (write-to-string (make-array '(2 2) :initial-element 5)))))
  ("#2A((5 ...) ...)" "#2A(# #)"))
