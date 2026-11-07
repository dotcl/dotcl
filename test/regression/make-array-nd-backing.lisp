;;; A LET variable bound to (MAKE-ARRAY (LIST a b [c]) :ELEMENT-TYPE <numeric>)
;;; that the body does not assign holds a rank-2 or rank-3 simple array. Its
;;; element buffer is fetched once and each AREF computes the row-major index
;;; itself, checking every subscript against its own dimension.

(defun %manb-fill-sum (n m)
  (declare (fixnum n m))
  (let ((a (make-array (list n m) :element-type '(integer 0 1000) :initial-element 3))
        (b (make-array (list n m) :element-type '(integer 0 2000))))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref b i j) (+ (aref a i j) i j))))
    (let ((s 0))
      (dotimes (i n) (dotimes (j m) (incf s (aref b i j))))
      s)))

(deftest make-array-nd.2d-sum
  (list (%manb-fill-sum 4 5) (%manb-fill-sum 1 1) (%manb-fill-sum 0 3))
  (;; 20 cells * 3, plus sum(i) * 5 = 30, plus sum(j) * 4 = 40
   130 3 0))

(defun %manb-3d (n m k)
  (declare (fixnum n m k))
  (let ((a (make-array (list n m k) :element-type 'fixnum :initial-element 0)))
    (dotimes (i n) (dotimes (j m) (dotimes (l k) (setf (aref a i j l) (+ (* 100 i) (* 10 j) l)))))
    (list (aref a 0 0 0) (aref a (1- n) (1- m) (1- k)) (aref a 1 2 3))))

(deftest make-array-nd.3d-roundtrip
  (%manb-3d 2 3 4)
  (0 123 123))

;; Each subscript is checked on its own: (aref a 0 5) on a 3x3 array is out of
;; range even though row-major 5 is inside the storage.
(defun %manb-ref (n i j)
  (declare (fixnum n i j))
  (let ((a (make-array (list n n) :element-type 'fixnum :initial-element 7)))
    (dotimes (r n) (dotimes (c n) (setf (aref a r c) (+ (* 10 r) c))))
    (aref a i j)))

(deftest make-array-nd.in-range
  (list (%manb-ref 3 2 2) (%manb-ref 3 0 0))
  (22 0))

(deftest make-array-nd.second-subscript-out-of-range
  (handler-case (%manb-ref 3 0 5) (error () :error))
  :error)

(deftest make-array-nd.first-subscript-out-of-range
  (handler-case (%manb-ref 3 3 0) (error () :error))
  :error)

(deftest make-array-nd.negative-subscript
  (list (handler-case (%manb-ref 3 -1 0) (error () :error))
        (handler-case (%manb-ref 3 0 -1) (error () :error)))
  (:error :error))

(defun %manb-set (n i j)
  (declare (fixnum n i j))
  (let ((a (make-array (list n n) :element-type '(unsigned-byte 8) :initial-element 1)))
    (setf (aref a i j) 9)
    (aref a i j)))

(deftest make-array-nd.set-out-of-range
  (list (%manb-set 3 1 2)
        (handler-case (%manb-set 3 1 3) (error () :error))
        (handler-case (%manb-set 3 3 0) (error () :error)))
  (9 :error :error))

(defun %manb-store-range (v)
  (let ((a (make-array (list 2 2) :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref a 1 1) v)
    (aref a 1 1)))

(deftest make-array-nd.store-element-type-still-checked
  (list (%manb-store-range 255)
        (handler-case (%manb-store-range 256) (error () :error)))
  (255 :error))

;; Not hoisted: the variable is assigned, the array is adjustable, the rank is
;; not known statically, or the subscript count differs from the rank.
(defun %manb-assigned (n)
  (let ((a (make-array (list n n) :element-type 'fixnum :initial-element 1)))
    (setq a (make-array (list n n) :element-type 'fixnum :initial-element 2))
    (aref a (1- n) (1- n))))

(defun %manb-adjustable (n)
  (let ((a (make-array (list n n) :element-type 'fixnum :initial-element 5 :adjustable t)))
    (adjust-array a (list (1+ n) (1+ n)) :initial-element 8)
    (list (aref a 0 0) (aref a n n))))

(defun %manb-dims-var (dims)
  (let ((a (make-array dims :element-type 'fixnum :initial-element 4)))
    (list (array-rank a) (array-total-size a))))

(deftest make-array-nd.not-hoisted-forms
  (list (%manb-assigned 3) (%manb-adjustable 2) (%manb-dims-var '(2 3)) (%manb-dims-var 5))
  (2 (5 8) (2 6) (1 5)))

(defun %manb-rank-mismatch (n)
  (let ((a (make-array (list n n) :element-type 'fixnum :initial-element 1)))
    (handler-case (aref a 1) (error () :error))))

(deftest make-array-nd.subscript-count-differs-from-rank
  (%manb-rank-mismatch 3)
  :error)

(defun %manb-literal-dims ()
  (let ((a (make-array '(2 3) :element-type '(unsigned-byte 16) :initial-element 6))
        (b (make-array (list 2 3) :element-type 'double-float :initial-element 1.5d0)))
    (setf (aref a 1 2) 60000)
    (setf (aref b 1 2) (+ (aref b 0 0) 1d0))
    (list (aref a 1 2) (aref a 0 0) (aref b 1 2) (aref b 0 1))))

(deftest make-array-nd.literal-and-float
  (%manb-literal-dims)
  (60000 6 2.5d0 1.5d0))

(defun %manb-closure (n)
  (let ((a (make-array (list n n) :element-type 'fixnum :initial-element 0)))
    (let ((f (lambda (i j v) (setf (aref a i j) v))))
      (funcall f 1 1 42)
      (aref a 1 1))))

(deftest make-array-nd.captured-by-closure
  (%manb-closure 3)
  42)
