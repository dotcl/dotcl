;;; Infinities and NaN print in a form that reads back.
;;;
;;; The printer emitted #.SINGLE-FLOAT-POSITIVE-INFINITY: a #. form naming a
;;; symbol nobody defined, and unqualified, so reading it back signalled
;;; UNBOUND-VARIABLE. The constants now exist in DOTCL and the printed form
;;; names them with the package prefix, so it reads from any package.

(defun %fip-inf-d () (/ 1.0d0 0.0d0))
(defun %fip-inf-s () (/ 1.0f0 0.0f0))

(deftest float-infinity-print.double-form
  (prin1-to-string (%fip-inf-d))
  "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY")

(deftest float-infinity-print.single-negative-form
  (prin1-to-string (- (%fip-inf-s)))
  "#.DOTCL:SINGLE-FLOAT-NEGATIVE-INFINITY")

(deftest float-infinity-print.nan-form
  (prin1-to-string (- (%fip-inf-d) (%fip-inf-d)))
  "#.DOTCL:DOUBLE-FLOAT-NAN")

;;; The point of the exercise: print/read round-trips.

(deftest float-infinity-print.roundtrip
  (list (eql (%fip-inf-d) (read-from-string (prin1-to-string (%fip-inf-d))))
        (eql (- (%fip-inf-s)) (read-from-string (prin1-to-string (- (%fip-inf-s))))))
  (t t))

(deftest float-infinity-print.constants-are-the-values
  (list (eql (%fip-inf-d) dotcl:double-float-positive-infinity)
        (eql (- (%fip-inf-d)) dotcl:double-float-negative-infinity)
        (eql (%fip-inf-s) dotcl:single-float-positive-infinity))
  (t t t))

;;; The NaN constants print as NaN too (a NaN is not EQL to itself, so the
;;; printed form is the observable check).
(deftest float-infinity-print.nan-constants
  (list (prin1-to-string dotcl:single-float-nan)
        (prin1-to-string dotcl:double-float-nan))
  ("#.DOTCL:SINGLE-FLOAT-NAN" "#.DOTCL:DOUBLE-FLOAT-NAN"))

;;; With *READ-EVAL* NIL the #. form would not read, so the printer says so
;;; rather than lying: an unreadable form normally, an error when the caller
;;; demanded readable output.

(deftest float-infinity-print.read-eval-nil-is-unreadable-form
  (let ((*read-eval* nil)) (prin1-to-string (%fip-inf-d)))
  "#<DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY>")

(deftest float-infinity-print.print-readably-signals
  (handler-case (let ((*read-eval* nil) (*print-readably* t))
                  (prin1-to-string (%fip-inf-d)))
    (print-not-readable () :print-not-readable)
    (error (e) (list :other (type-of e))))
  :print-not-readable)

;;; Ordinary floats are untouched.

(deftest float-infinity-print.finite-floats-unchanged
  (list (prin1-to-string 1.5d0) (prin1-to-string 1.5f0) (prin1-to-string 0.0d0))
  ("1.5d0" "1.5" "0.0d0"))

;;; One expression, one answer. `(/ 1.0d0 0.0d0)` inside a DEFUN compiles to a
;;; raw IL div and yields an infinity, but the same division through #'/ used to
;;; signal DIVISION-BY-ZERO -- so (mapcar #'/ ...) disagreed with (/ ...), and
;;; the emit-free build, which has only the function, disagreed with every other
;;; build (it could not even load this file: the infinity above was built by
;;; dividing). Float division by zero now follows IEEE everywhere; rational
;;; division by zero still signals.

(defun %fip-div (a b) (funcall #'/ a b))

(deftest float-infinity-print.function-division-matches-compiled
  (list (prin1-to-string (%fip-div 1.0d0 0.0d0))
        (prin1-to-string (%fip-inf-d)))
  ("#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY" "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"))

(deftest float-infinity-print.mixed-type-division-is-ieee
  (prin1-to-string (%fip-div 1 0.0d0))
  "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY")

(deftest float-infinity-print.zero-over-zero-is-nan
  (prin1-to-string (%fip-div 0.0d0 0.0d0))
  "#.DOTCL:DOUBLE-FLOAT-NAN")

(deftest float-infinity-print.rational-division-by-zero-still-signals
  (list (handler-case (%fip-div 1 0) (division-by-zero () :signalled))
        (handler-case (%fip-div 1/2 0) (division-by-zero () :signalled)))
  (:signalled :signalled))
