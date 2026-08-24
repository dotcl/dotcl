;;; A declared float comparison answers what the undeclared one answers.
;;;
;;; NaN is unordered: <, >, <= and >= are all false against it, and /= is the
;;; only one that is true. The native comparison emitted for declared floats
;;; computed <= as "not greater" and >= as "not less", which are true for a NaN
;;; -- so (<= x y) answered T with the declaration and NIL without it, on the
;;; same values. They now negate the unordered comparison instead.
;;;
;;; The undeclared path is the reference here rather than another implementation:
;;; SBCL folds (- inf inf) to 0.0d0, so it cannot be asked this question directly.

(defun %fnc-declared-double (a b)
  (declare (double-float a b))
  (list (if (> a b) t nil) (if (< a b) t nil) (if (>= a b) t nil)
        (if (<= a b) t nil) (if (= a b) t nil) (if (/= a b) t nil)))

(defun %fnc-declared-single (a b)
  (declare (single-float a b))
  (list (if (> a b) t nil) (if (< a b) t nil) (if (>= a b) t nil)
        (if (<= a b) t nil) (if (= a b) t nil) (if (/= a b) t nil)))

(defun %fnc-undeclared (a b)
  (list (if (> a b) t nil) (if (< a b) t nil) (if (>= a b) t nil)
        (if (<= a b) t nil) (if (= a b) t nil) (if (/= a b) t nil)))

(defun %fnc-nan-double () (let ((inf (/ 1.0d0 0.0d0))) (- inf inf)))
(defun %fnc-nan-single () (let ((inf (/ 1.0 0.0))) (- inf inf)))

(deftest float-nan-comparison.nan-is-unordered
  (let ((nan (%fnc-nan-double)))
    (list (%fnc-declared-double nan 1.0d0)
          (%fnc-declared-double 1.0d0 nan)
          (%fnc-declared-double nan nan)
          (%fnc-declared-single (%fnc-nan-single) 1.0)))
  ((nil nil nil nil nil t) (nil nil nil nil nil t)
   (nil nil nil nil nil t) (nil nil nil nil nil t)))

;;; The declaration must not change any answer, ordered operands included.
(deftest float-nan-comparison.declared-agrees-with-undeclared
  (let ((nan (%fnc-nan-double)) (inf (/ 1.0d0 0.0d0)))
    (mapcar (lambda (p)
              (equal (%fnc-declared-double (first p) (second p))
                     (%fnc-undeclared (first p) (second p))))
            (list (list nan 1.0d0) (list 1.0d0 nan) (list nan nan)
                  (list 1.0d0 2.0d0) (list 2.0d0 2.0d0) (list 2.0d0 1.0d0)
                  (list inf 1.0d0) (list (- inf) inf)
                  (list 0.0d0 -0.0d0))))
  (t t t t t t t t t))

;;; Ordinary comparisons keep working.
(deftest float-nan-comparison.ordered-values
  (list (%fnc-declared-double 1.0d0 2.0d0) (%fnc-declared-double 2.0d0 2.0d0)
        (%fnc-declared-single 1.0 2.0) (%fnc-declared-single 2.0 2.0))
  ((nil t nil t nil t) (nil nil t t t nil)
   (nil t nil t nil t) (nil nil t t t nil)))
