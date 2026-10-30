;;; Standard objects written inside a logical block share its XP buffer.
;;; Expected values are SBCL 2.6.8's output.

(deftest ppxb-newline-linear
  (let ((*print-right-margin* 8) (*print-pretty* t) (*print-miser-width* 40))
    (with-output-to-string (s)
      (pprint-logical-block (s '(a (b (c d)) e f) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (write-string " " s)
              (pprint-newline :linear s)))))
  "(A
 (B
  (C
   D))
 E
 F
 )")

(deftest ppxb-newline-fill
  (let ((*print-right-margin* 16) (*print-pretty* t) (*print-miser-width* 10))
    (with-output-to-string (s)
      (pprint-logical-block (s '((a b) (c d) (e f)) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (write-string " " s)
              (pprint-newline :fill s)))))
  "((A B) (C D)
 (E F) )")

(deftest ppxb-per-line-prefix
  (let ((*print-right-margin* 8) (*print-pretty* t) (*print-miser-width* 20))
    (with-output-to-string (s)
      (pprint-logical-block (s '(x (y z) (w (v (u))) t)
                               :per-line-prefix "x " :suffix "")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (write-string " " s)
              (pprint-newline :linear s)))))
  "x X
x (Y Z)
x (W
x  (V
x   (U)))
x T
x ")

(deftest ppxb-per-line-prefix-suffix
  (let ((*print-right-margin* 16) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s '((a b) (c d) (e f))
                               :per-line-prefix "  " :suffix " end")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (write-string " " s)
              (pprint-newline :fill s)))))
  "  (A B) (C D)
  (E F)  end")

(deftest ppxb-outer-suffix
  (let ((*print-right-margin* 16) (*print-pretty* t) (*print-miser-width* 40))
    (with-output-to-string (s)
      (pprint-logical-block (s '(a (b (c d)) e f) :suffix ">")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (write-string " " s)
              (pprint-newline :linear s)))))
  "A
(B (C D))
E
F
>")

(deftest ppxb-format-fill
  (let ((*print-right-margin* 30) (*print-pretty* t) (*print-miser-width* 20))
    (format nil "~@<~a~:_~a~:>"
            '(this-is-a-long-symbol another-long-symbol and-a-third-long-symbol)
            '(a b . tail)))
  "(THIS-IS-A-LONG-SYMBOL
 ANOTHER-LONG-SYMBOL
 AND-A-THIRD-LONG-SYMBOL)
(A B . TAIL)")

(deftest ppxb-current-indent
  (let ((*print-right-margin* 12) (*print-pretty* t) (*print-miser-width* 5))
    (with-output-to-string (s)
      (pprint-logical-block (s '(x (y z) (w (v (u))) t) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (pprint-indent :current 0 s)
              (pprint-newline :mandatory s)))))
  "(X
  (Y Z)
       (W
        (V
         (U)))
              T
               )")
