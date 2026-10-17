;;; AREF and (SETF AREF) on a local declared (SIMPLE-ARRAY <elt> (D0 D1 [D2]))
;;; read and write the element storage directly: the subscripts are combined
;;; into the row-major index (constant dimensions fold into the strides) and
;;; checked per axis above (safety 0). A declared * dimension is read once.
;;; An array that is not the declared one is a TYPE-ERROR at every safety.

(setf dotcl:*save-sil* t)

(defun %mda-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %mda-u64-2d (a i j)
  (declare (type (simple-array (unsigned-byte 64) (8 256)) a)
           (type (integer 0 7) i) (type (integer 0 255) j))
  (setf (aref a i j) (logxor (aref a i j) (aref a 7 255)))
  (aref a i j))

(defun %mda-u64-3d (a b)
  (declare (type (simple-array (unsigned-byte 64) (16 2 2)) a)
           (type bit b)
           (optimize (speed 3) (safety 0)))
  (let ((z 0))
    (declare (type (unsigned-byte 64) z))
    (dotimes (i 16 z)
      (setf z (logxor z (aref a i b 0) (aref a i b 1))))))

(defun %mda-u32-2d (a)
  (declare (type (simple-array (unsigned-byte 32) (33 4)) a))
  (dotimes (i 33)
    (dotimes (j 4)
      (setf (aref a i j) (+ (* i 4) j #xFFFFFF00))))
  (list (aref a 0 0) (aref a 32 3) (aref a 10 (1+ 1))))

(defun %mda-df-2d (a n)
  (declare (type (simple-array double-float (3 3)) a) (type fixnum n))
  (let ((s 0d0))
    (declare (type double-float s))
    (dotimes (k n s)
      (dotimes (i 3)
        (setf (aref a i i) (+ (aref a i i) 1d0))
        (incf s (aref a i (- 2 i)))))))

(defun %mda-star (a)
  (declare (type (simple-array fixnum (* *)) a))
  (let ((s 0))
    (declare (type fixnum s))
    (dotimes (i (array-dimension a 0) s)
      (dotimes (j (array-dimension a 1))
        (setf (aref a i j) (* i j))
        (incf s (aref a i j))))))

(defun %mda-ref (a i j)
  (declare (type (simple-array (unsigned-byte 8) (4 4)) a))
  (aref a i j))

(defun %mda-ref-fast (a i j)
  (declare (type (simple-array (unsigned-byte 8) (4 4)) a)
           (optimize (safety 0)))
  (aref a i j))

(defun %mda-u32-2d-store (a v)
  (declare (type (simple-array (unsigned-byte 32) (33 4)) a))
  (setf (aref a 1 1) v))

(deftest mda-u64-2d-values
  (let ((a (make-array '(8 256) :element-type '(unsigned-byte 64) :initial-element 0)))
    (setf (aref a 7 255) #xFFFFFFFFFFFFFFFF
          (aref a 3 100) #x8000000000000001)
    (list (%mda-u64-2d a 3 100) (aref a 3 100) (aref a 3 101)))
  (#x7FFFFFFFFFFFFFFE #x7FFFFFFFFFFFFFFE 0))

(deftest mda-u64-3d-values
  (let ((a (make-array '(16 2 2) :element-type '(unsigned-byte 64) :initial-element 0)))
    (dotimes (i 16)
      (setf (aref a i 1 0) (ash 1 (+ i 48))
            (aref a i 1 1) (ash 1 i)
            (aref a i 0 0) 12345))
    (list (%mda-u64-3d a 1) (%mda-u64-3d a 0)))
  (#xFFFF00000000FFFF 0))

(deftest mda-u32-2d-values
  (let ((a (make-array '(33 4) :element-type '(unsigned-byte 32))))
    (%mda-u32-2d a))
  (#xFFFFFF00 #xFFFFFF83 #xFFFFFF2A))

(deftest mda-df-2d-values
  (let ((a (make-array '(3 3) :element-type 'double-float :initial-element 0.5d0)))
    (list (%mda-df-2d a 2) (aref a 1 1) (aref a 0 1)))
  (6.0d0 2.5d0 0.5d0))

(deftest mda-star-values
  (let ((a (make-array '(3 5) :element-type 'fixnum)))
    (list (%mda-star a) (aref a 2 4)))
  (30 8))

(deftest mda-axis-out-of-range-signals
  (let ((a (make-array '(4 4) :element-type '(unsigned-byte 8))))
    (list (handler-case (%mda-ref a 0 4) (error () :error))
          (handler-case (%mda-ref a 4 0) (error () :error))
          (handler-case (%mda-ref a -1 1) (error () :error))
          (%mda-ref a 3 3)))
  (:error :error :error 0))

(deftest mda-u32-store-out-of-type-signals
  (let ((a (make-array '(33 4) :element-type '(unsigned-byte 32))))
    (handler-case (progn (%mda-u32-2d-store a (ash 1 32)) :no-error)
      (error () :error)))
  :error)

(deftest-emitting-only mda-wrong-shape-signals-above-safety-0
  (let ((a (make-array '(2 8) :element-type '(unsigned-byte 8))))
    (handler-case (progn (%mda-ref a 0 0) :no-error)
      (type-error () :type-error)))
  :type-error)

(deftest mda-non-simple-signals-above-safety-0
  (let ((a (make-array '(4 4) :element-type '(unsigned-byte 8) :adjustable t)))
    (handler-case (progn (%mda-ref a 0 0) :no-error)
      (type-error () :type-error)))
  :type-error)

(deftest-emitting-only mda-wrong-shape-signals-at-safety-0
  (list (handler-case (%mda-ref-fast (make-array '(2 8) :element-type '(unsigned-byte 8)) 1 2)
          (type-error () :type-error))
        (handler-case (%mda-ref-fast (make-array '(4 4) :element-type '(unsigned-byte 8)
                                                        :adjustable t)
                                     2 3)
          (type-error () :type-error))
        (handler-case (%mda-ref-fast (make-array '(4 4)) 2 3)
          (type-error () :type-error)))
  (:type-error :type-error :type-error))

(defun %mda-generic-count (fn)
  (loop for name in '("Aref" "ArefL" "Aref2D" "Aref2DL" "Aref3D" "Aref3DL" "ArefMulti"
                      "ArefSet" "ArefSetL" "ArefSet2D" "ArefSet2DL" "ArefSet3D"
                      "ArefSet3DL" "ArefSetMulti")
        sum (%mda-sil-count fn (format nil "Runtime.~a)" name))))

(deftest-compiled-only mda-no-generic-aref
  (mapcar #'%mda-generic-count
          (list #'%mda-u64-2d #'%mda-u64-3d #'%mda-u32-2d #'%mda-df-2d #'%mda-star))
  (0 0 0 0 0))

(deftest-compiled-only mda-reads-storage
  (list (plusp (%mda-sil-count #'%mda-u64-3d "LDELEM-I8"))
        (%mda-sil-count #'%mda-u64-3d "AxisIndexL")
        (plusp (%mda-sil-count #'%mda-ref "AxisIndexL")))
  (t 0 t))
