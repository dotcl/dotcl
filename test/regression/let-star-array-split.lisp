;;; A LET* whose variable is declared a typed simple array and whose later inits read it with
;;; AREF: the reads use the array's raw backing store like the reads in the body do.

(setf dotcl:*save-sil* t)

(defstruct (%lsa-ctx (:constructor %lsa-make-ctx ()))
  (key (make-array 9 :element-type '(unsigned-byte 64) :initial-contents '(1 2 3 4 5 6 7 8 9))
   :type (simple-array (unsigned-byte 64) (9))))

(defun %lsa-sum (c)
  (declare (type %lsa-ctx c) (optimize (speed 3) (safety 0)))
  (let* ((key (%lsa-ctx-key c)) (k0 (aref key 0)) (k1 (aref key 1)) (k8 (aref key 8)))
    (declare (type (simple-array (unsigned-byte 64) (9)) key)
             (type (unsigned-byte 64) k0 k1 k8)
             (ignorable key))
    (ldb (byte 64 0) (+ (* k0 1000000) (* k1 1000) k8))))

(defun %lsa-checked (c)
  (declare (type %lsa-ctx c))
  (let* ((n (aref (%lsa-ctx-key c) 0)) (a (list n)) (k (car a)))
    (declare (type fixnum n) (type list a))
    (list n a k)))

(defun %lsa-wrong-type (v)
  ;; the declared type of the array variable is still checked
  (let* ((arr v) (x (aref arr 0)))
    (declare (type (simple-array (unsigned-byte 64) (*)) arr))
    x))

(defun %lsa-shadow (c)
  (let* ((key (%lsa-ctx-key c)) (x (aref key 1)) (key (%lsa-ctx-key c)) (y (aref key 2)))
    (declare (type (simple-array (unsigned-byte 64) (9)) key) (type (unsigned-byte 64) x y))
    (+ x y)))

(deftest let-star-array-split.value
  (%lsa-sum (%lsa-make-ctx))
  1002009)

(deftest let-star-array-split.mixed-declarations
  (%lsa-checked (%lsa-make-ctx))
  (1 (1) 1))

(deftest-emitting-only let-star-array-split.wrong-type-still-errors
  (handler-case (%lsa-wrong-type (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
    (error () :error))
  :error)

(deftest let-star-array-split.shadowed-name
  (%lsa-shadow (%lsa-make-ctx))
  5)

(defun %lsa-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(deftest-compiled-only let-star-array-split.no-generic-aref
  (%lsa-count "Runtime.ArefL" (%flat-text (dotcl:function-sil #'%lsa-sum)))
  0)
