;;; An inlined local function checks its parameters' type declarations like the
;;; out-of-line call.

(setf dotcl:*save-sil* t)

(defun %liad-call (fn)
  (handler-case (funcall fn)
    (type-error (e) (list 'type-error (type-error-datum e) (type-error-expected-type e)))))

;;; Required parameters with a (fixnum ...).
(defun %liad-inline (n) (flet ((f (k) (declare (fixnum k)) (list k))) (declare (inline f)) (f n)))
(defun %liad-outline (n) (flet ((f (k) (declare (fixnum k)) (list k))) (declare (notinline f)) (f n)))

;;; A (type ...) declaration.
(defun %liad-type-inline (n) (flet ((g (k) (declare (type (integer 0 10) k)) k)) (declare (inline g)) (g n)))
(defun %liad-type-outline (n) (flet ((g (k) (declare (type (integer 0 10) k)) k)) (declare (notinline g)) (g n)))

;;; Several names in one declaration.
(defun %liad-names-inline (n) (flet ((h (a b) (declare (fixnum a b)) (list (+ a b)))) (declare (inline h)) (h n 2)))
(defun %liad-names-outline (n) (flet ((h (a b) (declare (fixnum a b)) (list (+ a b)))) (declare (notinline h)) (h n 2)))

;;; LABELS.
(defun %liad-labels-inline (n) (labels ((f (k) (declare (fixnum k)) (1+ k))) (declare (inline f)) (f n)))
(defun %liad-labels-outline (n) (labels ((f (k) (declare (fixnum k)) (1+ k))) (declare (notinline f)) (f n)))

;;; A body that assigns its parameter: the entry check still runs first.
(defun %liad-mutated-inline (n) (flet ((f (k) (declare (fixnum k)) (setq k (1+ k)) k)) (declare (inline f)) (f n)))
(defun %liad-mutated-outline (n) (flet ((f (k) (declare (fixnum k)) (setq k (1+ k)) k)) (declare (notinline f)) (f n)))

(deftest d1.1
  (%liad-call (lambda () (%liad-inline "x")))
  (type-error "x" fixnum))

(deftest liad-inline-and-outline-agree.1
  (%liad-call (lambda () (%liad-outline "x")))
  (type-error "x" fixnum))

(deftest liad-type-decl-signals.1
  (%liad-call (lambda () (%liad-type-inline 99)))
  (type-error 99 (integer 0 10)))

(deftest liad-type-decl-and-outline-agree.1
  (%liad-call (lambda () (%liad-type-outline 99)))
  (type-error 99 (integer 0 10)))

(deftest liad-names-decl-signals.1
  (%liad-call (lambda () (%liad-names-inline "x")))
  (type-error "x" fixnum))

(deftest liad-names-decl-and-outline-agree.1
  (%liad-call (lambda () (%liad-names-outline "x")))
  (type-error "x" fixnum))

(deftest liad-labels-signals.1
  (%liad-call (lambda () (%liad-labels-inline "x")))
  (type-error "x" fixnum))

(deftest liad-labels-and-outline-agree.1
  (%liad-call (lambda () (%liad-labels-outline "x")))
  (type-error "x" fixnum))

(deftest liad-mutated-param-signals.1
  (%liad-call (lambda () (%liad-mutated-inline "x")))
  (type-error "x" fixnum))

(deftest liad-mutated-param-and-outline-agree.1
  (%liad-call (lambda () (%liad-mutated-outline "x")))
  (type-error "x" fixnum))

;;; The right-type argument still gets the fast code: the substitution stays,
;;; and an argument whose value sits in a checked raw slot needs no second
;;; check.
(defun %liad-loop-inline (n)
  (flet ((sq (k) (declare (fixnum k)) (* k k)))
    (declare (inline sq))
    (let ((sum 0))
      (dotimes (i n sum) (incf sum (sq i)))
      sum)))

(defun %liad-boxed-inline (n)
  (flet ((f (k) (declare (fixnum k)) (list k)))
    (declare (inline f))
    (f n)))

(defun %liad-u64-lane (w)
  (declare (type (simple-array (unsigned-byte 64) (4)) w))
  (flet ((m+ (a b) (declare (type (unsigned-byte 64) a b))
          (ldb (byte 64 0) (+ a b))))
    (declare (inline m+))
    (setf (aref w 2) (m+ (aref w 0) (aref w 1)))
    w))

(deftest liad-loop-values.1
  (%liad-loop-inline 4)
  14)

(deftest liad-lane-values.1
  (coerce (%liad-u64-lane
           (make-array 4 :element-type '(unsigned-byte 64)
                          :initial-contents (list #xFFFFFFFFFFFFFFFF 2 0 0)))
          'list)
  (18446744073709551615 2 1 0))

(defun %liad-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (if q (progn (incf k) (setq p (1+ q))) (return k))))))

;;; Loop counts are raw Int64 locals: their values fit the declaration on
;;; arrival, so the substitution keeps the count-free code.
(deftest-compiled-only liad.fast-loop-no-check
  (list (%liad-sil-count #'%liad-loop-inline "CheckArgFixnum")
        (%liad-sil-count #'%liad-loop-inline "LispFunction.Invoke"))
  (0 0))

;;; A boxed argument is checked at the substitution itself, a static call on
;;; the value it evaluates -- no closure and no call.
(deftest-compiled-only liad.boxed-arg-checked-no-call
  (list (plusp (%liad-sil-count #'%liad-boxed-inline "Runtime.CheckArgFixnum"))
        (%liad-sil-count #'%liad-boxed-inline "LispFunction.Invoke"))
  (t 0))

;;; (unsigned-byte 64) parameters of a substituted body read the raw Int64[]:
;;; the bits were exact on arrival, no second box or check, no generic copy.
(deftest-compiled-only liad.u64-lane-no-copy
  (list (%liad-sil-count #'%liad-u64-lane "Runtime.Add")
        (%liad-sil-count #'%liad-u64-lane "Runtime.BoxU64")
        (%liad-sil-count #'%liad-u64-lane "Runtime.CheckArgU64")
        (%liad-sil-count #'%liad-u64-lane "LispFunction.Invoke"))
  (0 0 0 0))

(setf dotcl:*save-sil* nil)
