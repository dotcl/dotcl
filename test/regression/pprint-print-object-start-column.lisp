;;; A PRINT-OBJECT method is handed a buffer that the printer later writes to
;;; the destination. A logical block the method opens starts at the column the
;;; object is printed at, so its continuation lines are indented to that column,
;;; whether the object is printed by PRIN1 / PRINC / WRITE or by FORMAT's ~S / ~A.
;;; The buffer used to start at column 0. The expected strings are SBCL 2.6.8's.

(defclass ppsc-thing () ())
(defmethod print-object ((x ppsc-thing) stream)
  (format stream "~@<aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk~:@>"))

(defun ppsc-print (fn)
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* 40))
      (write-string "  - " s)
      (funcall fn (make-instance 'ppsc-thing) s))))

(deftest pprint-print-object-start-column.prin1-princ-write
  (list (ppsc-print (lambda (x s) (prin1 x s)))
        (ppsc-print (lambda (x s) (princ x s)))
        (ppsc-print (lambda (x s) (write x :stream s))))
  ("  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk"
   "  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk"
   "  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk"))

(deftest pprint-print-object-start-column.format
  (list (ppsc-print (lambda (x s) (format s "~S" x)))
        (ppsc-print (lambda (x s) (format s "~A" x)))
        (ppsc-print (lambda (x s) (format s "* ~S" x))))
  ("  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk"
   "  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk"
   "  - * aaaa bbbb cccc dddd eeee ffff
      gggg hhhh iiii jjjj kkkk"))

;;; The same when the method opens the block with PPRINT-LOGICAL-BLOCK rather
;;; than FORMAT's ~<...~:>: the block used to start at column 0, so it both
;;; broke too late and indented its continuation lines to column 0.
(defclass ppsc-block-thing () ())
(defmethod print-object ((x ppsc-block-thing) stream)
  (pprint-logical-block (stream nil)
    (format stream "~@<aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk~:@>")))

(deftest pprint-print-object-start-column.logical-block
  (list (with-output-to-string (s)
          (let ((*print-pretty* t) (*print-right-margin* 40))
            (write-string "  - " s)
            (prin1 (make-instance 'ppsc-block-thing) s)))
        (with-output-to-string (s)
          (let ((*print-pretty* t) (*print-right-margin* 40))
            (format s "  - ~S" (make-instance 'ppsc-block-thing)))))
  ("  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk"
   "  - aaaa bbbb cccc dddd eeee ffff gggg
    hhhh iiii jjjj kkkk"))
