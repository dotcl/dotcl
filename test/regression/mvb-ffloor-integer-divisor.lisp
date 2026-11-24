;;; (MULTIPLE-VALUE-BIND (Q R) (FFLOOR V 3) ...) with V declared a float and the
;;; divisor an integer: the divisor converts to V's format.

(defmacro %mfi-def (name op ty)
  `(defun ,name (v) (declare (type ,ty v)) (multiple-value-bind (q r) (,op v 3) (list q r))))
(%mfi-def %mfi-ffloor-s ffloor single-float)
(%mfi-def %mfi-fceiling-s fceiling single-float)
(%mfi-def %mfi-ftruncate-s ftruncate single-float)
(%mfi-def %mfi-fround-s fround single-float)
(%mfi-def %mfi-ffloor-d ffloor double-float)
(%mfi-def %mfi-fceiling-d fceiling double-float)
(%mfi-def %mfi-ftruncate-d ftruncate double-float)
(%mfi-def %mfi-fround-d fround double-float)

(defun %mfi-typed-divisor (v n) (declare (single-float v) (fixnum n))
  (multiple-value-bind (q r) (ffloor v n) (list q r)))

(deftest mvb-ffloor-integer-divisor.values
  (list (%mfi-ffloor-s 7f0) (%mfi-fceiling-s 7f0) (%mfi-ftruncate-s -7f0) (%mfi-fround-s 7.5f0)
        (%mfi-ffloor-d 7d0) (%mfi-fceiling-d -7d0) (%mfi-ftruncate-d 1d15) (%mfi-fround-d 4.5d0)
        (%mfi-typed-divisor 10f0 4) (%mfi-typed-divisor -10f0 4))
  ((2.0 1.0) (3.0 -2.0) (-2.0 -1.0) (2.0 1.5) (2.0d0 1.0d0) (-2.0d0 -1.0d0)
   (3.33333333333333d14 1.0d0) (2.0d0 -1.5d0) (2.0 2.0) (-3.0 2.0)))
