;;; DOTCL-SIMD array access: X-AREF, X-ROW-MAJOR-AREF and their SETF functions,
;;; for each lane layout and for the storage shapes an array can have.

(require "dotcl-simd")

(defun simd-array-roundtrip (name element-type values)
  (let* ((n (length values))
         (a (make-array n :element-type element-type :initial-contents values))
         (aref (find-symbol (concatenate 'string name "-AREF") :dotcl-simd))
         (setter (fdefinition (list 'setf aref)))
         (lanes (multiple-value-list
                 (funcall (find-symbol (concatenate 'string name "-VALUES") :dotcl-simd)
                          (funcall aref a 0))))
         (b (make-array n :element-type element-type :initial-element (first values))))
    (funcall setter (funcall aref a 0) b 0)
    (list (equal lanes (subseq values 0 (length lanes)))
          (equalp (subseq b 0 (length lanes)) (subseq a 0 (length lanes))))))

(deftest dotcl-simd-array.every-layout
  (list (simd-array-roundtrip "F64.2" 'double-float '(1d0 2d0 3d0 4d0))
        (simd-array-roundtrip "F32.4" 'single-float '(1f0 2f0 3f0 4f0 5f0))
        (simd-array-roundtrip "S64.2" '(signed-byte 64) '(-1 2 3 4))
        (simd-array-roundtrip "U64.2" '(unsigned-byte 64) (list (1- (expt 2 64)) 2 3 4))
        (simd-array-roundtrip "S32.4" '(signed-byte 32) '(-1 2 3 4 5))
        (simd-array-roundtrip "U32.4" '(unsigned-byte 32) '(4294967295 2 3 4 5))
        (simd-array-roundtrip "S16.8" '(signed-byte 16) '(-1 2 3 4 5 6 7 8 9))
        (simd-array-roundtrip "U16.8" '(unsigned-byte 16) '(65535 2 3 4 5 6 7 8 9))
        (simd-array-roundtrip "S8.16" '(signed-byte 8) '(-128 2 3 4 5 6 7 8 9 10 11 12 13 14 15 127 17))
        (simd-array-roundtrip "U8.16" '(unsigned-byte 8) '(255 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest dotcl-simd-array.offsets-and-store
  (let ((a (make-array 8 :element-type 'double-float :initial-contents '(0d0 1d0 2d0 3d0 4d0 5d0 6d0 7d0))))
    (setf (dotcl-simd:f64.2-aref a 3) (dotcl-simd:make-f64.2 30d0 40d0))
    (list (coerce a 'list)
          (multiple-value-list (dotcl-simd:f64.2-values (dotcl-simd:f64.2-aref a 6)))
          (multiple-value-list (dotcl-simd:f64.2-values (dotcl-simd:f64.2-row-major-aref a 2)))
          (multiple-value-list (dotcl-simd:f64.2-values
                                (setf (dotcl-simd:f64.2-row-major-aref a 0) (dotcl-simd:f64.2 9d0))))
          (coerce a 'list)))
  ((0d0 1d0 2d0 30d0 40d0 5d0 6d0 7d0)
   (6d0 7d0) (2d0 30d0) (9d0 9d0)
   (9d0 9d0 2d0 30d0 40d0 5d0 6d0 7d0)))

(deftest dotcl-simd-array.bounds-and-types
  (let ((a (make-array 4 :element-type 'double-float :initial-element 0d0)))
    (list (signals-error (dotcl-simd:f64.2-aref a 3) error)
          (signals-error (dotcl-simd:f64.2-aref a -1) error)
          (signals-error (dotcl-simd:f64.2-aref a 4) error)
          (signals-error (setf (dotcl-simd:f64.2-aref a 3) (dotcl-simd:f64.2 1d0)) error)
          (signals-error (dotcl-simd:f64.2-aref (make-array 4) 0) type-error)
          (signals-error (dotcl-simd:f64.2-aref (make-array 4 :element-type 'single-float) 0) type-error)
          (signals-error (dotcl-simd:f64.2-aref 5 0) type-error)
          (signals-error (setf (dotcl-simd:f64.2-aref a 0) (dotcl-simd:f32.4 1f0)) type-error)
          (signals-error (dotcl-simd:f64.2-aref a 0 0) error)
          (type-of (dotcl-simd:f64.2-aref a 2))))
  (t t t t t t t t t dotcl-simd::f64.2))

(deftest dotcl-simd-array.shapes
  (let* ((m (make-array '(2 4) :element-type '(unsigned-byte 8) :initial-element 0))
         (fp (make-array 8 :element-type 'double-float :fill-pointer 2 :adjustable t :initial-element 1d0))
         (base (make-array 8 :element-type 'double-float :initial-contents '(0d0 1d0 2d0 3d0 4d0 5d0 6d0 7d0)))
         (disp (make-array 4 :element-type 'double-float :displaced-to base :displaced-index-offset 2)))
    (dotimes (i 8) (setf (row-major-aref m i) i))
    (list (multiple-value-list (dotcl-simd:u8.16-values (dotcl-simd:u8.16-row-major-aref (make-array 16 :element-type '(unsigned-byte 8) :initial-contents (loop for i below 16 collect i)) 0)))
          (signals-error (dotcl-simd:u8.16-aref m 0 0) error)
          (multiple-value-list (dotcl-simd:f64.2-values (dotcl-simd:f64.2-aref fp 6)))
          (multiple-value-list (dotcl-simd:f64.2-values (dotcl-simd:f64.2-aref disp 1)))
          (progn (setf (dotcl-simd:f64.2-aref disp 0) (dotcl-simd:make-f64.2 -2d0 -3d0))
                 (coerce base 'list))))
  ((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
   t
   (1d0 1d0)
   (3d0 4d0)
   (0d0 1d0 -2d0 -3d0 4d0 5d0 6d0 7d0)))

(deftest dotcl-simd-array.two-dimensional
  (let ((m (make-array '(2 2) :element-type 'double-float :initial-contents '((1d0 2d0) (3d0 4d0)))))
    (list (multiple-value-list (dotcl-simd:f64.2-values (dotcl-simd:f64.2-aref m 1 0)))
          (signals-error (dotcl-simd:f64.2-aref m 1 1) error)
          (progn (setf (dotcl-simd:f64.2-aref m 0 0) (dotcl-simd:make-f64.2 8d0 9d0))
                 (aref m 0 1))))
  ((3d0 4d0) t 9d0))

(deftest-emitting-only dotcl-simd-array.same-named-function-is-an-ordinary-call
  (let* ((pkg (or (find-package "DSA-OTHER") (make-package "DSA-OTHER" :use nil)))
         (sym (intern "%SIMD-STORE" pkg)))
    (setf (symbol-function sym) (lambda (a b c) (list a b c)))
    (funcall (compile nil `(lambda (x) (,sym 'dotcl-simd:f64.2-aref x 3))) 1))
  (dotcl-simd:f64.2-aref 1 3))
