;;; Declared-fixnum loops whose arithmetic is not provably fixnum no longer box each result:
;;; (setf s (+ s (* i i))), (setf s (mod (+ (* s 31) i) m)) and (evenp i) on an unboxed loop
;;; variable allocated a Fixnum (22 bytes) or more per iteration. Results are the generic ones,
;;; including when an intermediate leaves int64.

(defun fnb-sum-squares (n) (declare (type fixnum n))
  (let ((s 0)) (declare (type fixnum s))
    (dotimes (i n) (setf s (+ s (* i i))) (when (> s 1000000000) (setf s 0))) s))
(defun fnb-hash (n) (declare (type fixnum n))
  (let ((s 0)) (declare (type fixnum s))
    (dotimes (i n) (setf s (mod (+ (* s 31) i) 1000003))) s))
(defun fnb-count-even (n) (declare (type fixnum n))
  (let ((c 0)) (declare (type fixnum c))
    (dotimes (i n) (when (evenp i) (incf c)) (when (oddp i) (decf c 2))) c))
(defun fnb-w10 (n) (declare (type fixnum n))
  (let ((s 0)) (declare (type fixnum s))
    (dotimes (i n) (declare (type fixnum i)) (setf s (mod (+ s (* i i)) 1000003))) s))
(defun fnb-w10-ref (n)
  (let ((s 0)) (dotimes (i n) (setf s (mod (+ s (* i i)) 1000003))) s))
(defun fnb-mod-big (a b m) (declare (type fixnum a b m))
  (mod (+ (* a b) a) m))
(defun fnb-overflow (a) (declare (type fixnum a))
  (let ((s 0)) (declare (type fixnum s)) (setf s (+ s (* a a))) s))

(deftest fixnum-loop-no-boxing.results
  (list (fnb-sum-squares 100000) (fnb-hash 100000) (fnb-count-even 1001)
        (= (fnb-w10 100000) (fnb-w10-ref 100000))
        (fnb-mod-big 4611686018427387904 3 1000007)
        (fnb-mod-big most-positive-fixnum most-positive-fixnum 1000003)
        (fnb-overflow 12345))
  (0 933429 -499 t 919788 825416 152399025))

;; The tree-walk interpreter does not enforce the fixnum declaration, so it answers with the
;; bignum; compiled code signals when the value cannot be stored in the declared variable.
(deftest-compiled-only fixnum-loop-no-boxing.declared-overflow-signals
  (handler-case (fnb-overflow most-positive-fixnum) (error () :error))
  :error)

(deftest-compiled-only fixnum-loop-no-boxing.allocation
  (progn
    (fnb-sum-squares 10) (fnb-hash 10) (fnb-count-even 10) (fnb-w10 10)
    (list (< (min-allocated-bytes (lambda () (fnb-w10 100000)) 3) 20000) (< (min-allocated-bytes (lambda () (fnb-sum-squares 100000)) 3) 20000)
          (< (min-allocated-bytes (lambda () (fnb-hash 100000)) 3) 20000)
          (< (min-allocated-bytes (lambda () (fnb-count-even 100000)) 3) 20000)))
  (t t t t))
