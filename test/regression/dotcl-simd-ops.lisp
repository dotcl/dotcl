;;; DOTCL-SIMD operations: the scalar reference implementation against the
;;; Vector128 one on random lanes, then fixed edge cases for each operation.

(require "dotcl-simd")
(require "dotcl-float")

(defvar *simd-names* '("F64.2" "F32.4" "S64.2" "U64.2" "S32.4" "U32.4" "S16.8" "U16.8" "S8.16" "U8.16"))
(defvar *simd-float-names* '("F64.2" "F32.4"))

(defun simd-sym (&rest parts)
  (find-symbol (apply #'concatenate 'string parts) :dotcl-simd))

(defun simd-fn (&rest parts)
  (fdefinition (apply #'simd-sym parts)))

(defun simd-lane-count (name)
  (cdr (assoc name '(("F64.2" . 2) ("F32.4" . 4) ("S64.2" . 2) ("U64.2" . 2) ("S32.4" . 4)
                     ("U32.4" . 4) ("S16.8" . 8) ("U16.8" . 8) ("S8.16" . 16) ("U8.16" . 16))
              :test #'string=)))

(defun simd-lane-bits (name)
  (floor 128 (simd-lane-count name)))

(defun simd-signed-p (name) (char= (char name 0) #\S))

(defvar *simd-doubles*
  (list 0d0 -0d0 1d0 -1d0 2.5d0 3.5d0 -2.5d0 0.5d0 -0.5d0 1d300 1d-300 1d20 -1d20
        dotcl-float:+double-float-positive-infinity+ dotcl-float:+double-float-negative-infinity+
        dotcl-float:+double-float-nan+ 4d0 9d0 2147483648d0 -2147483649d0 4294967296d0
        18446744073709551616d0))

(defvar *simd-singles*
  (list 0f0 -0f0 1f0 -1f0 2.5f0 3.5f0 -2.5f0 0.5f0 -0.5f0 1f20 1f-30 -1f20
        dotcl-float:+single-float-positive-infinity+ dotcl-float:+single-float-negative-infinity+
        dotcl-float:+single-float-nan+ 4f0 9f0 2147483648f0 -2147483904f0 4294967296f0))

(defun simd-random-lane (name state)
  (let ((bits (simd-lane-bits name)))
    (cond ((string= name "F64.2")
           (if (zerop (random 3 state))
               (nth (random (length *simd-doubles*) state) *simd-doubles*)
               (- (random 200d0 state) 100d0)))
          ((string= name "F32.4")
           (if (zerop (random 3 state))
               (nth (random (length *simd-singles*) state) *simd-singles*)
               (- (random 200f0 state) 100f0)))
          (t
           (let* ((u (case (random 6 state)
                       (0 0) (1 1) (2 (1- (expt 2 bits))) (3 (expt 2 (1- bits)))
                       (4 (1- (expt 2 (1- bits))))
                       (t (random (expt 2 bits) state)))))
             (if (and (simd-signed-p name) (>= u (expt 2 (1- bits))))
                 (- u (expt 2 bits))
                 u))))))

(defun simd-random-pack (name state)
  (apply (simd-fn "MAKE-" name)
         (loop repeat (simd-lane-count name) collect (simd-random-lane name state))))

(defun simd-lanes (pack)
  (let ((name (string (type-of pack))))
    (multiple-value-list (funcall (simd-fn name "-VALUES") pack))))

(defun simd-same-lane-p (a b)
  (or (eql a b)
      (and (floatp a) (floatp b) (dotcl-float:float-nan-p a) (dotcl-float:float-nan-p b))))

(defun simd-pack-p (x)
  (and (not (numberp x))
       (symbolp (type-of x))
       (eq (symbol-package (type-of x)) (find-package :dotcl-simd))))

(defun simd-same-p (a b)
  (if (simd-pack-p a)
      (and (eq (type-of a) (type-of b))
           (every #'simd-same-lane-p (simd-lanes a) (simd-lanes b)))
      (simd-same-lane-p a b)))

(defun simd-both (thunk)
  (dotcl::%simd-force-scalar t)
  (let ((scalar (unwind-protect (funcall thunk) (dotcl::%simd-force-scalar nil))))
    (values scalar (funcall thunk))))

(defun simd-differences (name suffix arity iterations &key (arg-fn nil))
  (let ((state (make-random-state t))
        (fn (simd-fn name suffix))
        (bad '()))
    (dotimes (i iterations)
      (let ((args (if arg-fn
                      (funcall arg-fn state)
                      (loop repeat arity collect (simd-random-pack name state)))))
        (multiple-value-bind (s v) (simd-both (lambda () (apply fn args)))
          (unless (simd-same-p s v)
            (push (list name suffix args s v) bad)))))
    (subseq bad 0 (min 3 (length bad)))))

(defun simd-all-differences (names suffix arity &key arg-fn (iterations 60))
  (loop for name in names
        append (simd-differences name suffix arity iterations :arg-fn (and arg-fn (funcall arg-fn name)))))

(deftest dotcl-simd-ops.differential-binary
  (loop for suffix in '("+" "-" "*" "-MIN" "-MAX" "-AND" "-OR" "-XOR" "-ANDC1")
        append (simd-all-differences *simd-names* suffix 2))
  nil)

(deftest dotcl-simd-ops.differential-float-binary
  (simd-all-differences *simd-float-names* "/" 2)
  nil)

(deftest dotcl-simd-ops.differential-unary
  (append
   (simd-all-differences *simd-names* "-NOT" 1)
   (loop for suffix in '("-SQRT" "-ROUND" "-FLOOR" "-CEILING" "-TRUNCATE")
         append (simd-all-differences *simd-float-names* suffix 1)))
  nil)

(deftest dotcl-simd-ops.differential-compare
  (loop for suffix in '("=" "/=" "<" "<=" ">" ">=")
        append (simd-all-differences *simd-names* suffix 2))
  nil)

(deftest dotcl-simd-ops.differential-horizontal
  (loop for suffix in '("-HORIZONTAL+" "-HORIZONTAL-MIN" "-HORIZONTAL-MAX")
        append (simd-all-differences *simd-names* suffix 1))
  nil)

(deftest dotcl-simd-ops.differential-if
  (simd-all-differences
   *simd-names* "-IF" 3
   :arg-fn (lambda (name)
             (let ((mask-name (concatenate 'string "U" (subseq name 1))))
               (lambda (state)
                 (list (simd-random-pack mask-name state)
                       (simd-random-pack name state)
                       (simd-random-pack name state))))))
  nil)

(deftest dotcl-simd-ops.differential-shift
  (let ((int-names (remove-if (lambda (n) (member n *simd-float-names* :test #'string=)) *simd-names*)))
    (loop for suffix in '("-SHIFTL" "-SHIFTR")
          append (simd-all-differences
                  int-names suffix 2
                  :arg-fn (lambda (name)
                            (lambda (state)
                              (list (simd-random-pack name state)
                                    (random (simd-lane-bits name) state)))))))
  nil)

(deftest dotcl-simd-ops.differential-convert
  (loop for (to from) in '(("F32.4" "S32.4") ("F32.4" "U32.4") ("S32.4" "F32.4") ("U32.4" "F32.4")
                           ("F64.2" "S64.2") ("F64.2" "U64.2") ("S64.2" "F64.2") ("U64.2" "F64.2")
                           ("F64.2" "F32.4") ("F32.4" "F64.2"))
        append (let ((suffix (concatenate 'string "-FROM-" from)))
                 (let ((state (make-random-state t)) (bad '()) (fn (simd-fn to suffix)))
                   (dotimes (i 60)
                     (let ((a (simd-random-pack from state)))
                       (multiple-value-bind (s v) (simd-both (lambda () (funcall fn a)))
                         (unless (simd-same-p s v) (push (list to from a s v) bad)))))
                   (subseq bad 0 (min 3 (length bad))))))
  nil)

(defun simd-vals (pack) (simd-lanes pack))

(deftest dotcl-simd-ops.integer-wrap
  (list (simd-vals (dotcl-simd:u8.16+ (dotcl-simd:u8.16 255) 1))
        (simd-vals (dotcl-simd:s8.16+ (dotcl-simd:s8.16 127) 1))
        (simd-vals (dotcl-simd:u64.2+ (dotcl-simd:u64.2 (1- (expt 2 64))) 1))
        (simd-vals (dotcl-simd:s64.2- (dotcl-simd:s64.2 (- (expt 2 63))) 1))
        (simd-vals (dotcl-simd:s32.4* (dotcl-simd:s32.4 65536) 65536))
        (simd-vals (dotcl-simd:u16.8- (dotcl-simd:u16.8 0) 1)))
  ((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (-128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 -128)
   (0 0)
   (9223372036854775807 9223372036854775807)
   (0 0 0 0)
   (65535 65535 65535 65535 65535 65535 65535 65535)))

(deftest dotcl-simd-ops.min-max-nan-and-zero
  (let* ((nan dotcl-float:+double-float-nan+)
         (r (simd-vals (dotcl-simd:f64.2-min (dotcl-simd:make-f64.2 -0d0 nan)
                                             (dotcl-simd:make-f64.2 0d0 1d0))))
         (m (simd-vals (dotcl-simd:f64.2-max (dotcl-simd:make-f64.2 -0d0 1d0)
                                             (dotcl-simd:make-f64.2 0d0 nan)))))
    (list (eql (first r) -0d0) (dotcl-float:float-nan-p (second r))
          (eql (first m) 0d0) (dotcl-float:float-nan-p (second m))))
  (t t t t))

(deftest dotcl-simd-ops.shift
  (list (simd-vals (dotcl-simd:u8.16-shiftl (dotcl-simd:u8.16 1) 7))
        (simd-vals (dotcl-simd:u8.16-shiftl (dotcl-simd:u8.16 #xFF) 4))
        (simd-vals (dotcl-simd:s32.4-shiftr (dotcl-simd:s32.4 -8) 1))
        (simd-vals (dotcl-simd:u32.4-shiftr (dotcl-simd:u32.4 #xFFFFFFFF) 28))
        (simd-vals (dotcl-simd:s64.2-shiftr (dotcl-simd:s64.2 (- (expt 2 63))) 63))
        (signals-error (dotcl-simd:u8.16-shiftl (dotcl-simd:u8.16 1) 8) type-error)
        (signals-error (dotcl-simd:u32.4-shiftr (dotcl-simd:u32.4 1) 32) type-error)
        (signals-error (dotcl-simd:s16.8-shiftl (dotcl-simd:s16.8 1) -1) type-error))
  ((128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128)
   (240 240 240 240 240 240 240 240 240 240 240 240 240 240 240 240)
   (-4 -4 -4 -4)
   (15 15 15 15)
   (-1 -1)
   t t t))

(deftest dotcl-simd-ops.compare-and-select
  (let* ((nan dotcl-float:+double-float-nan+)
         (a (dotcl-simd:make-f64.2 1d0 nan))
         (b (dotcl-simd:make-f64.2 1d0 nan)))
    (list (simd-vals (dotcl-simd:f64.2= a b))
          (simd-vals (dotcl-simd:f64.2/= a b))
          (simd-vals (dotcl-simd:f64.2< a (dotcl-simd:f64.2 2d0)))
          (type-of (dotcl-simd:s32.4< (dotcl-simd:s32.4 1) (dotcl-simd:s32.4 2)))
          (simd-vals (dotcl-simd:s8.16< (dotcl-simd:s8.16 -1) (dotcl-simd:s8.16 0)))
          (simd-vals (dotcl-simd:u8.16< (dotcl-simd:u8.16 255) (dotcl-simd:u8.16 0)))
          (simd-vals (dotcl-simd:s32.4-if (dotcl-simd:make-u32.4 #xFFFFFFFF 0 #xFFFFFFFF 0)
                                          (dotcl-simd:make-s32.4 1 2 3 4)
                                          (dotcl-simd:make-s32.4 10 20 30 40)))))
  ((18446744073709551615 0)
   (0 18446744073709551615)
   (18446744073709551615 0)
   dotcl-simd::u32.4
   (255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (1 20 3 40)))

(deftest dotcl-simd-ops.horizontal
  (list (dotcl-simd:f32.4-horizontal+ (dotcl-simd:make-f32.4 1f8 1f0 -1f8 1f0))
        (dotcl-simd:u8.16-horizontal+ (dotcl-simd:u8.16 255))
        (dotcl-simd:s32.4-horizontal-min (dotcl-simd:make-s32.4 3 -7 5 0))
        (dotcl-simd:f64.2-horizontal-max (dotcl-simd:make-f64.2 -1d0 4d0)))
  (0.0 240 -7 4d0))

(deftest dotcl-simd-ops.float-arithmetic-and-rounding
  (list (simd-vals (dotcl-simd:f64.2/ (dotcl-simd:make-f64.2 1d0 9d0) (dotcl-simd:make-f64.2 4d0 3d0)))
        (simd-vals (dotcl-simd:f64.2-sqrt (dotcl-simd:make-f64.2 4d0 2.25d0)))
        (simd-vals (dotcl-simd:f64.2-round (dotcl-simd:make-f64.2 2.5d0 3.5d0)))
        (simd-vals (dotcl-simd:f64.2-round (dotcl-simd:make-f64.2 -2.5d0 -0.4d0)))
        (simd-vals (dotcl-simd:f64.2-floor (dotcl-simd:make-f64.2 -1.5d0 1.5d0)))
        (simd-vals (dotcl-simd:f64.2-ceiling (dotcl-simd:make-f64.2 -1.5d0 1.5d0)))
        (simd-vals (dotcl-simd:f32.4-truncate (dotcl-simd:make-f32.4 -1.5f0 1.5f0 2.9f0 -2.9f0))))
  ((0.25d0 3d0) (2d0 1.5d0) (2d0 4d0) (-2d0 -0d0) (-2d0 1d0) (-1d0 2d0) (-1f0 1f0 2f0 -2f0)))

(deftest dotcl-simd-ops.bit-ops
  (list (simd-vals (dotcl-simd:u8.16-and (dotcl-simd:u8.16 #xF0) #x3C))
        (simd-vals (dotcl-simd:u8.16-or (dotcl-simd:u8.16 #xF0) #x0F))
        (simd-vals (dotcl-simd:u8.16-xor (dotcl-simd:u8.16 #xFF) #x0F))
        (simd-vals (dotcl-simd:u8.16-andc1 (dotcl-simd:u8.16 #xF0) #xFF))
        (simd-vals (dotcl-simd:u16.8-not (dotcl-simd:u16.8 1)))
        (simd-vals (dotcl-simd:u64.2! (dotcl-simd:f64.2-and (dotcl-simd:f64.2 -1d0) (dotcl-simd:f64.2! (dotcl-simd:u64.2 #x7FFFFFFFFFFFFFFF))))))
  ((48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48)
   (255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
   (240 240 240 240 240 240 240 240 240 240 240 240 240 240 240 240)
   (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
   (65534 65534 65534 65534 65534 65534 65534 65534)
   (4607182418800017408 4607182418800017408)))

(deftest dotcl-simd-ops.conversions
  (let ((nan dotcl-float:+single-float-nan+))
    (list (simd-vals (dotcl-simd:f32.4-from-s32.4 (dotcl-simd:make-s32.4 1 -2 16777217 0)))
          (simd-vals (dotcl-simd:s32.4-from-f32.4 (dotcl-simd:make-f32.4 1.9f0 -1.9f0 1f20 nan)))
          (simd-vals (dotcl-simd:u32.4-from-f32.4 (dotcl-simd:make-f32.4 -5f0 1f20 3.5f0 nan)))
          (simd-vals (dotcl-simd:f64.2-from-s64.2 (dotcl-simd:make-s64.2 -3 4)))
          (simd-vals (dotcl-simd:s64.2-from-f64.2 (dotcl-simd:make-f64.2 -1d20 2.9d0)))
          (simd-vals (dotcl-simd:u64.2-from-f64.2 (dotcl-simd:make-f64.2 1d30 -2d0)))
          (simd-vals (dotcl-simd:f64.2-from-f32.4 (dotcl-simd:make-f32.4 1.5f0 2.5f0 3f0 4f0)))
          (simd-vals (dotcl-simd:f32.4-from-f64.2 (dotcl-simd:make-f64.2 1.5d0 -2d0)))))
  ((1f0 -2f0 1.6777216f7 0f0)
   (1 -1 2147483647 0)
   (0 4294967295 3 0)
   (-3d0 4d0)
   (-9223372036854775808 2)
   (18446744073709551615 0)
   (1.5d0 2.5d0)
   (1.5f0 -2f0 0f0 0f0)))
