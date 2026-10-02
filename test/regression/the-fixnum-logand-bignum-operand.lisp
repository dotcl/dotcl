;;; (THE FIXNUM (LOGAND X M)) asserts only that the result is a fixnum. X may
;;; be a bignum: with a non-negative fixnum mask the result is a fixnum anyway.
;;; The native int64 lowering of the bitwise ops used to unbox X as a fixnum
;;; and fail with a .NET cast error on a bignum. cl-base64's
;;; INTEGER-TO-BASE64-STRING indexes its table with (the fixnum (logand int #x3f))
;;; and failed for any integer of 63 bits or more.

(defun tflb-index (int tbl)
  (declare (simple-string tbl))
  (schar tbl (the fixnum (logand int #x3f))))

(defun tflb-index-speed (int tbl)
  (declare (integer int) (simple-string tbl) (optimize (speed 3) (safety 1)))
  (schar tbl (the fixnum (logand int #x3f))))

(defun tflb-sum (int)
  (declare (integer int))
  (+ 1 (the fixnum (logand int #xff))))

(deftest the-fixnum-logand-bignum-operand
  (let ((tbl "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+/"))
    (list (tflb-index (+ (expt 2 70) 37) tbl)
          (tflb-index-speed (+ (expt 2 64) 10) tbl)
          (tflb-index 5 tbl)
          (tflb-sum (+ (expt 10 30) 1))
          (tflb-sum 255)))
  (#\B #\a #\5 2 256))

(deftest the-fixnum-logand-bignum-encode-loop
  ;; The shape of cl-base64's encoder: take 6 bits at a time off an integer.
  (let ((out '()))
    (do ((int (+ (expt 2 64) 63) (ash int -6)))
        ((zerop int) out)
      (declare (integer int))
      (push (the fixnum (logand int #x3f)) out)))
  (16 0 0 0 0 0 0 0 0 0 63))

(defun tflb-xor (x y) (the fixnum (logxor x y)))
(defun tflb-ior-mixed (x y)
  (declare (fixnum y))
  (the fixnum (logior x y)))

(deftest the-fixnum-logxor-logior-bignum-operands
  ;; Both operands unknown (logxor of two bignums with a fixnum result), and a
  ;; declared-fixnum operand next to a bignum one (logior with a negative
  ;; bignum). The fixnum operands take the unboxed path.
  (list (tflb-xor (+ (expt 2 70) 5) (+ (expt 2 70) 3))
        (tflb-xor 12 10)
        (tflb-ior-mixed (- (expt 2 70)) -1)
        (tflb-ior-mixed 8 3))
  (6 6 -1 11))
