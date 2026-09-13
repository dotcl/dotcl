;;; A two-value call in single-value position does not build the value wrapper.
;;;
;;; GETHASH, FLOOR, TRUNCATE, CEILING and ROUND publish two values and return an
;;; MvReturn carrying them. In single-value position the very next instruction is
;;; RUNTIME.UNWRAPMV, which takes the primary back out and collapses the thread
;;; state to that one value -- so the 40-byte wrapper is dead the instant it is
;;; made, and these are among the most-called functions in real library code.
;;;
;;; The peephole (P12) now rewrites the pair to a twin entry that publishes the
;;; primary and returns it directly. The two entries share one core in the
;;; runtime, so what the twin computes cannot drift from what the original does.

(defparameter *mpt-h* (let ((h (make-hash-table)))
                        (setf (gethash :a h) 1)
                        h))

;;; What the call site in MULTIPLE-VALUE position sees is unchanged: the rewrite
;;; must fire only where the second value is already being discarded.

(deftest mv-primary-twin.gethash-hit
  (multiple-value-list (gethash :a *mpt-h*))
  (1 t))

(deftest mv-primary-twin.gethash-miss
  (multiple-value-list (gethash :z *mpt-h*))
  (nil nil))

(deftest mv-primary-twin.gethash-default
  (multiple-value-list (gethash :z *mpt-h* 99))
  (99 nil))

(deftest mv-primary-twin.floor
  (list (multiple-value-list (floor 7 2))
        (multiple-value-list (floor -7 2))
        (multiple-value-list (floor 7.5d0)))
  ((3 1) (-4 1) (7 0.5d0)))

(deftest mv-primary-twin.truncate-ceiling-round
  (list (multiple-value-list (truncate -7 2))
        (multiple-value-list (ceiling 7 2))
        (multiple-value-list (round 7 2)))
  ((-3 -1) (4 -1) (4 -1)))

;;; MULTIPLE-VALUE-CALL puts two of them in MV position in one form: if the
;;; rewrite fired there, the second pair would arrive as a single value.
(deftest mv-primary-twin.multiple-value-call
  (multiple-value-call #'list (floor 7 2) (floor 9 4))
  (3 1 2 1))

;;; Tail position propagates values to the caller, so it must not be rewritten.
(deftest mv-primary-twin.tail-position
  (multiple-value-list (funcall (lambda () (floor 7 2))))
  (3 1))

;;; Single-value position: the primary is the value, and the errors the original
;;; entry signals are signalled by the twin as well.

(deftest mv-primary-twin.single-value-results
  (list (gethash :a *mpt-h*) (floor 7 2) (nth-value 0 (round 7 2)))
  (1 3 4))

(deftest mv-primary-twin.errors-still-signalled
  (list (handler-case (gethash :a 5) (type-error () :type-error) (error () :other))
        (handler-case (floor 1 0) (division-by-zero () :div0) (error () :other)))
  (:type-error :div0))

;;; The point of the change: no per-call allocation in single-value position.
;;;
;;; Measured the way FIXNUM-SLOT-ARITH measures, as the difference between two
;;; loop lengths so that anything else alive in the image cancels out. The loop
;;; body is in a DEFUN because that is where the calls being measured live.

(defun %mpt-gethash (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (gethash :a *mpt-h*)))))

(defun %mpt-floor (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (floor 7 2)))))

;; Compiled-only: this is a statement about emitted code. An emit-free build
;; interprets the call and has no peephole to run.
(deftest-compiled-only mv-primary-twin.gethash-allocates-nothing
  (< (bytes-per-op #'%mpt-gethash) 1)
  t)

(deftest-compiled-only mv-primary-twin.floor-allocates-nothing
  (< (bytes-per-op #'%mpt-floor) 1)
  t)

;;; A written (VALUES A B) is the same call pair: RUNTIME.VALUES2 followed by the
;;; unwrap. It is also the shape every two-value function ends in, so the entry
;;; carries more traffic than the five above put together -- but only where the
;;; VALUES form is itself in single-value position. A function whose tail is
;;; (VALUES A B) still builds the wrapper, because there the value is the
;;; caller's to interpret.

(deftest mv-primary-twin.values-single-value-position
  (list (let ((x (values 1 2))) x)
        (+ (values 3 4) 10)
        (list (values 5 6)))
  (1 13 (5)))

(deftest mv-primary-twin.values-mv-position
  (list (multiple-value-list (values 1 2))
        (multiple-value-bind (a b) (values 1 2) (list a b))
        (nth-value 1 (values 1 2))
        (multiple-value-call #'list (values 1 2) (values 3 4)))
  ((1 2) (1 2) 2 (1 2 3 4)))

;;; Tail position propagates to the caller, so the rewrite must not fire there.
(defun %mpt-two () (values 7 8))

(deftest mv-primary-twin.values-tail-position
  (list (multiple-value-list (%mpt-two))
        (multiple-value-list (funcall (lambda () (values 9 10))))
        (%mpt-two))
  ((7 8) (9 10) 7))

;;; Both arguments are still evaluated, and in order: the rewrite drops the
;;; second VALUE, never the form that computes it.
(defparameter *mpt-order* nil)

(defun %mpt-note (x) (push x *mpt-order*) x)

(deftest mv-primary-twin.values-evaluates-both-in-order
  (let ((*mpt-order* nil))
    (list (let ((x (values (%mpt-note :a) (%mpt-note :b)))) x)
          (reverse *mpt-order*)))
  (:a (:a :b)))

(defun %mpt-values (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (values 1 2)))))

(deftest-compiled-only mv-primary-twin.values-allocates-nothing
  (< (bytes-per-op #'%mpt-values) 1)
  t)
