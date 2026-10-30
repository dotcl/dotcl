;;; Reader edge cases: out-of-range float literals, skipped #., and nested #C parts.

;;; CLHS 2.3.1.1: an out-of-range float token is a reader error.
(deftest reader-edge.float-overflow-is-reader-error
  (list (signals-error (read-from-string "1e400") reader-error)
        (signals-error (read-from-string "1d400") reader-error)
        (signals-error (read-from-string "1s999") reader-error)
        (signals-error (read-from-string "1f999") reader-error)
        (signals-error (read-from-string "-1e400") reader-error))
  (t t t t t))

(deftest reader-edge.float-overflow-follows-default-format
  (let ((*read-default-float-format* 'double-float))
    (list (signals-error (read-from-string "1e999") reader-error)
          (= (read-from-string "1e308") 1d308)))
  (t t))

(deftest reader-edge.float-overflow-long-exponent
  (signals-error (read-from-string "1.0e999999999999999999999") reader-error)
  t)

(deftest reader-edge.float-underflow-and-in-range-still-read
  (list (= (read-from-string "1e-999") 0.0)
        (= (read-from-string "1.7976931348623157d308") 1.7976931348623157d308))
  (t t))

;;; CLHS 2.4.8.11: both #C parts must be reals.
(deftest reader-edge.complex-rejects-complex-parts
  (list (handler-case (progn (read-from-string "#C(#C(1 2) 3)") :no-error)
          (type-error () :type-error)
          (error () :other-error))
        (handler-case (progn (read-from-string "#C(1 #C(2 3))") :no-error)
          (type-error () :type-error)
          (error () :other-error)))
  (:type-error :type-error))

(deftest reader-edge.complex-real-parts-still-read
  (let ((z (read-from-string "#C(1 2)")))
    (list (realpart z) (imagpart z)))
  (1 2))

;;; CLHS *read-suppress*: a skipped form is parsed but not evaluated.
(deftest reader-edge.skipped-read-eval-nil-plus
  (let ((*read-eval* nil))
    (values (read-from-string "#+(or) #.(error \"must not run\") 7")))
  7)

(deftest reader-edge.skipped-read-eval-nil-minus
  (let ((*read-eval* nil))
    (values (read-from-string "#-(and) #.(error \"must not run\") 7")))
  7)

(deftest reader-edge.unsuppressed-read-eval-nil-still-rejected
  (let ((*read-eval* nil))
    (signals-error (read-from-string "#.(+ 1 2)") reader-error))
  t)
