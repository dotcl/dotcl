;;; With *PRINT-READABLY* true a circular list cannot be printed without
;;; *PRINT-CIRCLE*: "..." does not read back as the list. PRINT-NOT-READABLE is
;;; signalled (as it already was for a circular vector or structure), whether or not
;;; *PRINT-PRETTY* is on. With *PRINT-CIRCLE* true the list prints as #1=(... . #1#),
;;; and with *PRINT-READABLY* nil the "..." form is unchanged.

(defun %prcl-circular ()
  (let ((x (list 'a 'b 'c))) (setf (cdddr x) x) x))

(defun %prcl-print (readably circle pretty)
  (let ((*print-readably* readably) (*print-circle* circle) (*print-pretty* pretty)
        (x (%prcl-circular)))
    (handler-case (prin1-to-string x)
      (print-not-readable () :print-not-readable))))

(deftest print-readably-circular-list.signals
  (list (%prcl-print t nil t) (%prcl-print t nil nil))
  (:print-not-readable :print-not-readable))

(deftest print-readably-circular-list.circle-prints-labels
  (list (%prcl-print t t t) (%prcl-print t t nil))
  ("#1=(A B C . #1#)" "#1=(A B C . #1#)"))

(deftest print-readably-circular-list.not-readably-unchanged
  (list (%prcl-print nil nil t) (%prcl-print nil nil nil))
  ("(A B C A B ...)" "(A B C A B ...)"))

(deftest print-readably-circular-list.nested
  (let ((*print-readably* t) (*print-circle* nil) (x (%prcl-circular)))
    (handler-case (prin1-to-string (list 1 x)) (print-not-readable () :print-not-readable)))
  :print-not-readable)

(deftest print-readably-circular-list.acyclic-lists-unaffected
  (let ((*print-readably* t) (*print-circle* nil) (shared (list 1 2)))
    (list (prin1-to-string (list shared shared)) (prin1-to-string '(1 2 . 3))))
  ("((1 2) (1 2))" "(1 2 . 3)"))
