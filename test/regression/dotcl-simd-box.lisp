;;; DOTCL-SIMD: the boxed 128-bit pack types, their constructors and lane access.

(require "dotcl-simd")

(deftest dotcl-simd-box.types
  (list (type-of (dotcl-simd:make-f64.2 1d0 2d0))
        (type-of (dotcl-simd:u8.16 7))
        (typep (dotcl-simd:u8.16 7) 'dotcl-simd:u8.16)
        (typep (dotcl-simd:u8.16 7) 'dotcl-simd:s8.16)
        (typep 1 'dotcl-simd:f64.2)
        (dotcl-simd:u8.16p (dotcl-simd:u8.16 7))
        (dotcl-simd:f32.4p 1.0))
  (dotcl-simd::f64.2 dotcl-simd::u8.16 t nil nil t nil))

(deftest dotcl-simd-box.broadcast-and-values
  (list (multiple-value-list (dotcl-simd:f64.2-values (dotcl-simd:f64.2 1.5d0)))
        (multiple-value-list (dotcl-simd:s16.8-values (dotcl-simd:make-s16.8 1 -2 3 -4 5 -6 7 -8)))
        (multiple-value-list (dotcl-simd:u64.2-values (dotcl-simd:make-u64.2 0 (1- (expt 2 64)))))
        (multiple-value-list (dotcl-simd:s8.16-values (dotcl-simd:s8.16 -128))))
  ((1.5d0 1.5d0)
   (1 -2 3 -4 5 -6 7 -8)
   (0 18446744073709551615)
   (-128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128)))

(deftest dotcl-simd-box.reinterpret
  (list (multiple-value-list (dotcl-simd:f32.4-values (dotcl-simd:f32.4! (dotcl-simd:u32.4 #x3f800000))))
        (multiple-value-list (dotcl-simd:u32.4-values (dotcl-simd:u32.4! (dotcl-simd:make-u16.8 1 2 3 4 5 6 7 8))))
        (multiple-value-list (dotcl-simd:u64.2-values (dotcl-simd:u64.2! (dotcl-simd:f64.2 1d0)))))
  ((1.0 1.0 1.0 1.0)
   (131073 262147 393221 524295)
   (4607182418800017408 4607182418800017408)))

(deftest dotcl-simd-box.lane-errors
  (list (signals-error (dotcl-simd:u8.16 256) type-error)
        (signals-error (dotcl-simd:s8.16 -129) type-error)
        (signals-error (dotcl-simd:f64.2 1) type-error)
        (signals-error (dotcl-simd:f32.4 1d0) type-error)
        (signals-error (dotcl-simd:make-f64.2 1d0) program-error)
        (signals-error (dotcl-simd:f64.2 (dotcl-simd:u8.16 1)) type-error))
  (t t t t t t))

(deftest dotcl-simd-box.identity
  (let ((h (make-hash-table :test 'eql))
        (a (dotcl-simd:make-u32.4 1 2 3 4)))
    (setf (gethash a h) :found)
    (list (eql a (dotcl-simd:make-u32.4 1 2 3 4))
          (eql a (dotcl-simd:make-u32.4 1 2 3 5))
          (eql (dotcl-simd:u32.4 0) (dotcl-simd:s32.4 0))
          (equal a (dotcl-simd:make-u32.4 1 2 3 4))
          (equalp a (dotcl-simd:make-u32.4 1 2 3 4))
          (gethash (dotcl-simd:make-u32.4 1 2 3 4) h)
          (eql (sxhash a) (sxhash (dotcl-simd:make-u32.4 1 2 3 4)))
          (prin1-to-string (dotcl-simd:make-f64.2 1d0 -0d0))))
  (t nil nil t t :found t "#<F64.2 1.0d0 -0.0d0>"))
