;;; A FIXNUM key reaches an EQL table without a key box.

(defun %ghfn-table (test)
  (let ((h (make-hash-table :test test)))
    (dotimes (i 1000) (setf (gethash (+ i 100000) h) i))
    h))

(defun %ghfn-hit-loop (h n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n)
      (incf s (the fixnum (gethash (+ i 100000) h -1))))
    s))

(defun %ghfn-miss-loop (h n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n)
      (incf s (the fixnum (gethash (+ i 200000) h 1))))
    s))

(deftest gethash-fixnum-no-box.hit
  (mapcar (lambda (test) (%ghfn-hit-loop (%ghfn-table test) 1000))
          '(eq eql equal equalp))
  (499500 499500 499500 499500))

(deftest gethash-fixnum-no-box.miss-with-default
  (let ((h (%ghfn-table 'eql)))
    (list (gethash (+ 200000 17) h :missing)
          (gethash (+ 100000 17) h :missing)))
  (:missing 17))

(deftest gethash-fixnum-no-box.miss-without-default-two-values
  (let ((h (%ghfn-table 'eql)))
    (multiple-value-list (gethash (+ 200000 17) h)))
  (nil nil))

(deftest gethash-fixnum-no-box.hit-two-values
  (let ((h (%ghfn-table 'eql)))
    (multiple-value-list (gethash (+ 100000 17) h)))
  (17 t))

;;; EQUALP compares a fixnum with another number type by value.
(deftest gethash-fixnum-no-box.equalp-cross-type
  (let ((h (make-hash-table :test 'equalp)))
    (setf (gethash 100000.0d0 h) :hit)
    (list (gethash 100000 h :miss) (gethash 100000.0d0 h :miss)))
  (:hit :hit))

(defun %ghfn-overflow-add (h a b)
  (declare (fixnum a b))
  (setf (gethash (+ a b) h) :add)
  (gethash (+ a b) h :miss))

(defun %ghfn-overflow-sub (h a b)
  (declare (fixnum a b))
  (setf (gethash (- a b) h) :sub)
  (gethash (- a b) h :miss))

(defun %ghfn-overflow-mul (h a b)
  (declare (fixnum a b))
  (setf (gethash (* a b) h) :mul)
  (gethash (* a b) h :miss))

(deftest gethash-fixnum-no-box.overflowing-key-is-a-bignum
  (let ((h (make-hash-table :test 'eql)))
    (list (%ghfn-overflow-add h most-positive-fixnum 1)
          (%ghfn-overflow-sub h most-negative-fixnum 1)
          (%ghfn-overflow-mul h most-positive-fixnum 2)
          (hash-table-count h)))
  (:add :sub :mul 3))

(deftest gethash-fixnum-no-box.synchronized
  (let ((h (make-hash-table :test 'eql :synchronized t)))
    (dotimes (i 1000) (setf (gethash (+ i 100000) h) i))
    (list (gethash (+ 100000 42) h :miss)
          (gethash (+ 200000 0) h :miss)))
  (42 :miss))

(deftest gethash-fixnum-no-box.other-table-kinds
  (let ((weak (make-hash-table :test 'eql :weakness :value))
        (hashed (make-hash-table :test 'eql :hash-function (lambda (k) (mod k 7)))))
    (setf (gethash 100000 weak) :weak
          (gethash 100000 hashed) :hashed)
    (list (gethash 100000 weak :miss) (gethash 100000 hashed :miss)))
  (:weak :hashed))

(defun %ghfn-form ()
  '(let ((h (make-hash-table :test 'eql)) (s 0))
     (declare (fixnum s))
     (dotimes (i 1000) (setf (gethash (+ i 100000) h) i))
     (dotimes (i 5000) (incf s (the fixnum (gethash (+ i 200000) h 1))))
     (list s (gethash 100000 h) (gethash 200000 h :miss))))

(deftest-compiled-only gethash-fixnum-no-box.compiled-and-evaluated
  (let ((compiled (funcall (compile nil `(lambda () ,(%ghfn-form))))))
    (list compiled
          (let ((dotcl:*evaluator-mode* :compile)) (eval (%ghfn-form)))
          (let ((dotcl:*evaluator-mode* :interpret)) (eval (%ghfn-form)))))
  ((5000 0 :miss) (5000 0 :miss) (5000 0 :miss)))

(deftest-compiled-only gethash-fixnum-no-box.no-per-call-allocation
  (let ((h (%ghfn-table 'eql)))
    (%ghfn-hit-loop h 1000)
    (%ghfn-miss-loop h 1000)
    (list (< (min-allocated-bytes
              (lambda () (dotimes (k 100) (%ghfn-hit-loop h 1000))) 3)
             20000)
          (< (min-allocated-bytes (lambda () (%ghfn-miss-loop h 100000)) 3)
             20000)))
  (t t))

;; The fixnum index of a plain table is created with its first fixnum key: lookups before
;; that, after non-fixnum keys only, after CLRHASH and after REMHASH all agree with the
;; boxed path.
(defun %gfnb-lazy (k ht) (declare (type fixnum k)) (multiple-value-list (gethash k ht :none)))

(deftest gethash-fixnum-no-box.lazy-index
  (let ((ht (make-hash-table :test 'eql)))
    (list (%gfnb-lazy 5 ht)
          (progn (setf (gethash "str" ht) 1 (gethash 2.5d0 ht) 2) (%gfnb-lazy 5 ht))
          (progn (setf (gethash 5 ht) :five) (%gfnb-lazy 5 ht))
          (progn (remhash 5 ht) (%gfnb-lazy 5 ht))
          (progn (setf (gethash 5 ht) :again) (clrhash ht) (%gfnb-lazy 5 ht))
          (progn (setf (gethash 5 ht) :third) (%gfnb-lazy 5 ht))
          (hash-table-count ht)))
  ((:none nil) (:none nil) (:five t) (:none nil) (:none nil) (:third t) 1))
