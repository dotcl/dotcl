;;; PPRINT-POP labels a circular remainder and still prints the prefix.
;;; Expected value is SBCL 2.6.8's output.

(deftest pprint-pop-circle-logical-block.circular
  (let ((*print-circle* t) (*print-pretty* t))
    (let ((x (list 'aaa 'bbb 'ccc 'ddd)))
      (setf (cdddr x) x)
      (with-output-to-string (s)
        (pprint-logical-block (s x :prefix "(" :suffix ")")
          (loop (pprint-exit-if-list-exhausted)
                (write (pprint-pop) :stream s)
                (write-string " " s))))))
  "(AAA . #1=(BBB CCC AAA . #1#))")

(deftest pprint-pop-circle-logical-block.shared
  (let* ((*print-circle* t) (*print-pretty* t)
         (v1 '(8)) (val (list v1 v1)))
    (with-output-to-string (os)
      (pprint-logical-block (os val :prefix "(" :suffix ")")
        (pprint-logical-block (os (car val) :prefix "(" :suffix ")")
          (write (caar val) :stream os))
        (write-char #\Space os)
        (pprint-logical-block (os (cadr val) :prefix "(" :suffix ")")
          (write (caadr val) :stream os)))))
  "(#1=(8) #1#)")

(deftest pprint-pop-circle-logical-block.circular-margin
  (let ((*print-circle* t) (*print-right-margin* 20)
        (*print-pretty* t) (*print-miser-width* nil))
    (let ((x (list 'aaa 'bbb 'ccc 'ddd)))
      (setf (cdddr x) x)
      (with-output-to-string (s)
        (pprint-logical-block (s x :prefix "(" :suffix ")")
          (loop (pprint-exit-if-list-exhausted)
                (write (pprint-pop) :stream s :pretty t)
                (write-string " " s))))))
  "(AAA . #1=(BBB CCC
           AAA
           . #1#))")

(deftest pprint-pop-circle-logical-block.circular-newline
  (let ((*print-circle* t) (*print-right-margin* 20)
        (*print-pretty* t) (*print-miser-width* nil))
    (let ((x (list 'aaa 'bbb 'ccc 'ddd)))
      (setf (cdddr x) x)
      (with-output-to-string (s)
        (pprint-logical-block (s x :prefix "(" :suffix ")")
          (loop (pprint-exit-if-list-exhausted)
                (write (pprint-pop) :stream s :pretty t)
                (write-string " " s)
                (pprint-newline :linear s))))))
  "(AAA
 . #1=(BBB CCC AAA
       . #1#))")

(deftest pprint-pop-circle-logical-block.circular-not-pretty
  (let ((*print-circle* t) (*print-pretty* nil) (*print-right-margin* 20))
    (let ((x (list 'aaa 'bbb 'ccc 'ddd)))
      (setf (cdddr x) x)
      (with-output-to-string (s)
        (pprint-logical-block (s x :prefix "(" :suffix ")")
          (loop (pprint-exit-if-list-exhausted)
                (write (pprint-pop) :stream s)
                (write-string " " s))))))
  "(AAA . #1=(BBB CCC AAA . #1#))")
