;;; Any-rank SIMPLE-ARRAY declarations fetch their buffer through Runtime.BackingIf*.

(setf dotcl:*save-sil* t)

(defun %aar-u16 (a n)
  (declare (type (simple-array (unsigned-byte 16) *) a) (fixnum n))
  (let ((s 0)) (declare (fixnum s)) (dotimes (i n s) (incf s (aref a i)))))

(defun %aar-i32 (a n)
  (declare (type (simple-array (signed-byte 32) *) a) (fixnum n))
  (let ((s 0)) (declare (fixnum s)) (dotimes (i n s) (incf s (aref a i)))))

(defun %aar-u64 (a n)
  (declare (type (simple-array (unsigned-byte 64) *) a) (fixnum n))
  (let ((s 0)) (declare (fixnum s)) (dotimes (i n s) (incf s (aref a i)))))

(defun %aar-f32 (a n)
  (declare (type (simple-array single-float *) a) (fixnum n))
  (let ((s 0.0f0)) (declare (single-float s)) (dotimes (i n s) (incf s (aref a i)))))

(defun %aar-f64 (a n)
  (declare (type (simple-array double-float *) a) (fixnum n))
  (let ((s 0d0)) (declare (double-float s)) (dotimes (i n s) (incf s (aref a i)))))

(defun %aar-has (fn text)
  (and (search text (%flat-text (dotcl:function-sil fn))) t))

(deftest array-any-rank-backing.values
  (list (%aar-u16 (make-array 3 :element-type '(unsigned-byte 16)
                              :initial-contents '(1 2 3)) 3)
        (%aar-i32 (make-array 3 :element-type '(signed-byte 32)
                              :initial-contents '(-1 2 3)) 3)
        (%aar-u64 (make-array 3 :element-type '(unsigned-byte 64)
                              :initial-contents '(1 2 3)) 3)
        (%aar-f32 (make-array 3 :element-type 'single-float
                              :initial-contents '(1f0 2f0 3f0)) 3)
        (%aar-f64 (make-array 3 :element-type 'double-float
                              :initial-contents '(1d0 2d0 3d0)) 3))
  (6 4 6 6.0 6.0d0))

(deftest array-any-rank-backing.errors
  (flet ((e (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (type-error () :type-error)
             (error () :error))))
    (list (e (lambda () (%aar-u16 (make-array 3 :element-type '(unsigned-byte 16)
                                               :adjustable t) 1)))
          (e (lambda () (%aar-u16 (vector 1 2 3) 1)))
          (e (lambda () (%aar-f64 (make-array 3 :element-type '(unsigned-byte 16)) 1)))
          (e (lambda () (%aar-u16 (make-array 3 :element-type '(unsigned-byte 16)) 1.5)))))
  (:type-error :type-error :type-error :type-error))

(deftest-emitting-only array-any-rank-backing.shape
  (list (%aar-has #'%aar-u16 "Runtime.BackingIfU16)")
        (%aar-has #'%aar-i32 "Runtime.BackingIfI32)")
        (%aar-has #'%aar-u64 "Runtime.BackingIfU64)")
        (%aar-has #'%aar-f32 "Runtime.BackingIfF32)")
        (%aar-has #'%aar-f64 "Runtime.BackingIfF64)"))
  (t t t t t))
