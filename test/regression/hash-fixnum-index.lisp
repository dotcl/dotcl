;;; The fixnum lookup index of an EQL / EQUAL / EQUALP hash table is built by
;;; the first fixnum GETHASH, not on every store; every answer is the same
;;; whether the index exists yet or not.

(defun %hfi-put-only ()
  (let ((h (make-hash-table)))
    (dotimes (i 200000) (setf (gethash i h) (* i 2)))
    h))

(defun %hfi-probe (h)
  (list (gethash 1 h) (gethash 2 h) (gethash 3 h) (gethash 0 h) (gethash -1 h)
        (gethash (expt 2 61) h) (gethash (- (expt 2 61)) h) (gethash (expt 2 70) h)
        (gethash 1.0 h) (gethash 0.0 h) (gethash -0.0 h) (gethash 1/2 h)
        (hash-table-count h)))

(defun %hfi-fill (h)
  (setf (gethash 1 h) :one (gethash 2 h) :two (gethash 0 h) :zero (gethash -1 h) :neg
        (gethash (expt 2 61) h) :big61 (gethash (- (expt 2 61)) h) :negbig61
        (gethash (expt 2 70) h) :bignum (gethash 1.0 h) :float1 (gethash 0.0 h) :fzero
        (gethash -0.0 h) :nfzero (gethash 1/2 h) :ratio)
  h)

(deftest-emitting-only hash-fixnum-index.put-only-allocation
  (progn
    (%hfi-put-only)
    (< (min-allocated-bytes #'%hfi-put-only) 30000000))
  t)

(deftest hash-fixnum-index.eql-before-and-after-index
  (let ((h (%hfi-fill (make-hash-table))))
    ;; first probe builds the index; then stores, removal, reinsertion keep it right
    (let ((a (%hfi-probe h)))
      (setf (gethash 3 h) :three)
      (remhash 1 h)
      (let ((b (%hfi-probe h)))
        (setf (gethash 1 h) :one-again (gethash 2 h) :two-new)
        (clrhash h)
        (let ((c (%hfi-probe h)))
          (setf (gethash 5 h) :five)
          (list a b c (gethash 5 h) (hash-table-count h))))))
  ((:ONE :TWO NIL :ZERO :NEG :BIG61 :NEGBIG61 :BIGNUM :FLOAT1 :FZERO :NFZERO :RATIO 11)
   (NIL :TWO :THREE :ZERO :NEG :BIG61 :NEGBIG61 :BIGNUM :FLOAT1 :FZERO :NFZERO :RATIO 11)
   (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 0)
   :FIVE 1))

(deftest hash-fixnum-index.store-before-first-lookup
  (let ((h (make-hash-table)))
    (setf (gethash 7 h) :a)
    (setf (gethash 7 h) :b)
    (setf (gethash 8 h) :c)
    (remhash 8 h)
    (setf (gethash 9 h) :d)
    (list (gethash 7 h) (gethash 8 h) (gethash 9 h) (hash-table-count h)))
  (:B NIL :D 2))

(deftest hash-fixnum-index.maphash-order
  (let ((h (make-hash-table)) (keys '()))
    (dotimes (i 10) (setf (gethash (* i 3) h) i))
    (gethash 3 h)
    (setf (gethash 100 h) :late)
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) h)
    (nreverse keys))
  (0 3 6 9 12 15 18 21 24 27 100))

(deftest hash-fixnum-index.other-tests
  (let ((e (make-hash-table :test 'equal)) (p (make-hash-table :test 'equalp))
        (s (make-hash-table :test 'eql :synchronized t))
        (w (make-hash-table :weakness :key)))
    (dolist (h (list e p s w))
      (setf (gethash 1 h) :one (gethash 2 h) :two)
      (gethash 1 h)
      (setf (gethash 3 h) :three)
      (remhash 2 h))
    (setf (gethash 1.0 p) :f)
    (list (mapcar (lambda (h) (list (gethash 1 h) (gethash 2 h) (gethash 3 h) (hash-table-count h)))
                  (list e p s w))
          (gethash 1.0 e)))
  (((:ONE NIL :THREE 2) (:F NIL :THREE 2) (:ONE NIL :THREE 2) (:ONE NIL :THREE 2)) NIL))
