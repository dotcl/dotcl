;;; The functions that report on a float's format have to look at the format.
;;;
;;; FLOAT-DIGITS answered 53 for everything, FLOAT-PRECISION did the same, and
;;; FLOAT-SIGN always built its answer as a double -- so (float-sign 1.5) came
;;; back as 1.0d0. All three also accepted an integer, which has no format to
;;; report on. Expected values from SBCL.

(deftest float-format-introspection.digits
  (list (float-digits 1.5) (float-digits 1.5d0)
        (float-digits least-positive-single-float)
        (float-digits least-positive-double-float))
  (24 53 24 53))

;;; PRECISION is about the value, not just the format: a denormal's leading
;;; zeros are not digits it carries, and a zero carries none.
(deftest float-format-introspection.precision
  (list (float-precision 1.5) (float-precision 1.5d0)
        (float-precision 0.0) (float-precision 0.0d0)
        (float-precision least-positive-normalized-single-float)
        (float-precision least-positive-single-float)
        (float-precision (* least-positive-single-float 8))
        (float-precision least-positive-double-float)
        (float-precision (* least-positive-double-float 8)))
  (24 53 0 0 24 1 4 1 4))

;;; The sign comes back in the argument's own format.
(deftest float-format-introspection.sign-format
  (list (float-sign 1.5) (float-sign -1.5) (float-sign -0.0)
        (float-sign 1.5d0) (float-sign -1.5d0) (float-sign -0.0d0))
  (1.0 -1.0 -1.0 1.0d0 -1.0d0 -1.0d0))

;;; With two arguments the answer is the first one's sign on the second one's
;;; magnitude -- a product, so a double on either side wins.
(deftest float-format-introspection.sign-two-arguments
  (list (float-sign 1.5 2.0) (float-sign -1.5 2.0)
        (float-sign 1.5 2.0d0) (float-sign -1.5d0 2.0)
        (float-sign 1.5d0 -3.5d0) (float-sign -1.5 -3.5))
  (2.0 -2.0 2.0d0 -2.0d0 3.5d0 -3.5))

;;; A non-float has no format to report.
(deftest float-format-introspection.requires-a-float
  (flet ((errs (f) (handler-case (progn (funcall f) :no-error)
                     (type-error () :type-error) (error () :other))))
    (list (errs (lambda () (float-digits 1)))
          (errs (lambda () (float-precision 1)))
          (errs (lambda () (float-sign 1)))
          (errs (lambda () (float-sign 1.5 2)))
          (errs (lambda () (float-sign 1/2)))))
  (:type-error :type-error :type-error :type-error :type-error))

;;; Non-finite values report their format and their sign.
(deftest float-format-introspection.non-finite
  (let ((inf (/ 1.0d0 0.0d0)) (infs (/ 1.0 0.0)))
    (list (float-precision inf) (float-digits inf)
          (float-precision infs) (float-digits infs)
          (float-sign inf) (float-sign infs)))
  (53 53 24 24 1.0d0 1.0))
