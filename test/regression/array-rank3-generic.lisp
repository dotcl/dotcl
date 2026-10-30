;;; Rank-3 and generic multi-subscript array accesses that no other test reached:
;;; Runtime.ArefNum3DD / ArefSetNum3DD (a (simple-array double-float (* * *))
;;; parameter), ArefU64Bits3D (a rank-3 (unsigned-byte 64) DEFCONSTANT table and
;;; a THE form), and Aref3D / ArefSet2D / ArefSet3D (subscripts or an array the
;;; compiler knows nothing about). Values and error classes agree with SBCL 2.6.8.

(setf dotcl:*save-sil* t)

(defun %da3-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %da3-ref-d (a i j k)
  (declare (type (simple-array double-float (* * *)) a) (fixnum i j k) (optimize (speed 1) (safety 1) (debug 0)))
  (aref a i j k))
(defun %da3-set-d (a i j k x)
  (declare (type (simple-array double-float (* * *)) a) (fixnum i j k) (double-float x) (optimize (speed 1) (safety 1) (debug 0)))
  (setf (aref a i j k) x))
(defconstant +da3-u64+ (make-array '(2 2 2) :element-type '(unsigned-byte 64)
  :initial-contents '(((0 1) (#x8000000000000000 #xFFFFFFFFFFFFFFFF)) ((7 8) (9 #x7FFFFFFFFFFFFFFF)))))
(defun %da3-ref-u64 (i j k) (declare (fixnum i j k)) (aref +da3-u64+ i j k))
(defun %da3-ref-u64-xor (i j k) (declare (fixnum i j k)) (logxor (aref +da3-u64+ i j k) 3))
(defun %da3-the-u64 (a i j k) (declare (fixnum i j k))
  (logxor (aref (the (simple-array (unsigned-byte 64) (* * *)) a) i j k) 3))
(defun %da3-ref3 (a i j k) (aref a i j k))
(defun %da3-set2 (a i j x) (setf (aref a i j) x))
(defun %da3-set3 (a i j k x) (setf (aref a i j k) x))
(defun %da3-te (thunk) (handler-case (progn (funcall thunk) :no-error) (type-error () :type-error) (error () :error)))
(defun %da3-d3 () (let ((a (make-array '(2 2 2) :element-type 'double-float :initial-element 0d0))) a))

(deftest array-rank3-generic.double-3d
  (let ((a (%da3-d3)))
    (list (%da3-set-d a 0 0 0 -0d0) (%da3-set-d a 1 1 1 1d300) (%da3-set-d a 0 1 0 -2.5d0)
          (%da3-ref-d a 0 0 0) (%da3-ref-d a 1 1 1) (%da3-ref-d a 0 1 0) (%da3-ref-d a 1 0 1)
          (row-major-aref a 2)))
  (-0.0d0 1.0d300 -2.5d0 -0.0d0 1.0d300 -2.5d0 0.0d0 -2.5d0))

(deftest array-rank3-generic.double-3d-errors
  (let ((a (%da3-d3)))
    (list (%da3-te (lambda () (%da3-ref-d a 2 0 0)))
          (%da3-te (lambda () (%da3-ref-d a 0 0 -1)))
          (%da3-te (lambda () (%da3-set-d a 0 2 0 1d0)))
          (%da3-te (lambda () (%da3-ref-d (make-array '(1 1 1) :element-type 'single-float) 0 0 0)))
          (%da3-te (lambda () (%da3-ref-d (make-array '(1 1) :element-type 'double-float) 0 0 0)))
          (%da3-te (lambda () (%da3-set-d (make-array 1) 0 0 0 1d0)))))
  (:type-error :type-error :type-error :type-error :type-error :type-error))

(deftest array-rank3-generic.u64-constant-table
  (list (%da3-ref-u64 0 0 0) (%da3-ref-u64 0 1 0) (%da3-ref-u64 0 1 1) (%da3-ref-u64 1 1 1)
        (%da3-ref-u64 1 0 1) (%da3-ref-u64-xor 0 1 1) (%da3-ref-u64-xor 1 0 0)
        (%da3-te (lambda () (%da3-ref-u64 2 0 0)))
        (%da3-te (lambda () (%da3-ref-u64 0 0 2))))
  (0 9223372036854775808 18446744073709551615 9223372036854775807 8
   18446744073709551612 4 :type-error :type-error))

(deftest array-rank3-generic.u64-the-form
  (list (%da3-the-u64 +da3-u64+ 0 1 1) (%da3-the-u64 +da3-u64+ 1 1 0)
        (%da3-te (lambda () (%da3-the-u64 +da3-u64+ 0 0 2))))
  (18446744073709551612 10 :type-error))

(deftest array-rank3-generic.unknown-array
  (let ((a (make-array '(2 2 2) :initial-element nil))
        (b (make-array '(2 2 2) :element-type '(unsigned-byte 8) :initial-element 0))
        (c (make-array '(2 2 2) :element-type 'double-float :initial-element 0d0))
        (d (make-array '(2 2) :initial-element 0))
        (e (make-array '(2 2) :element-type '(signed-byte 32) :initial-element 0)))
    (list (%da3-set3 a 1 0 1 :x) (%da3-ref3 a 1 0 1) (%da3-ref3 a 0 0 0)
          (%da3-set3 b 1 1 1 255) (%da3-ref3 b 1 1 1)
          (%da3-te (lambda () (%da3-set3 b 0 0 0 256)))
          (%da3-te (lambda () (%da3-set3 b 0 0 0 -1))) (%da3-ref3 b 0 0 0)
          (%da3-set3 c 0 1 1 3d0) (%da3-ref3 c 0 1 1)
          (%da3-te (lambda () (%da3-set3 c 0 0 0 1)))
          (%da3-te (lambda () (%da3-set3 a 2 0 0 1)))
          (%da3-te (lambda () (%da3-ref3 a 0 0 2)))
          (%da3-set2 d 1 0 "s") (aref d 1 0)
          (%da3-set2 e 0 1 -2147483648) (aref e 0 1)
          (%da3-te (lambda () (%da3-set2 e 0 0 2147483648)))
          (%da3-te (lambda () (%da3-set2 d 2 0 1)))
          (%da3-te (lambda () (%da3-set2 d 0 -1 1)))))
  (:x :x nil 255 255 :type-error :type-error 0 3.0d0 3.0d0 :type-error
   :type-error :type-error "s" "s" -2147483648 -2147483648 :type-error
   :type-error :type-error))

(deftest-emitting-only array-rank3-generic.uses-the-helpers
  (flet ((n (needle fn) (plusp (%da3-count needle (%flat-text (dotcl:function-sil fn))))))
    (list (n "Runtime.ArefNum3DD" #'%da3-ref-d) (n "Runtime.ArefSetNum3DD" #'%da3-set-d)
          (n "Runtime.ArefU64Bits3D" #'%da3-ref-u64) (n "Runtime.ArefU64Bits3D" #'%da3-the-u64)
          (n "Runtime.Aref3D" #'%da3-ref3) (n "Runtime.ArefSet2D" #'%da3-set2)
          (n "Runtime.ArefSet3D" #'%da3-set3)))
  (t t t t t t t))
