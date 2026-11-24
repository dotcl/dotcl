;;; The signed reading of the low N bits of an integer, as portable code writes it
;;;   (let ((g X)) (logior (ldb (byte N-1 0) g) (- (mask-field (byte 1 N-1) g))))
;;; runs in the 64-bit lane. The value is the two's-complement reading of the low N bits of X for
;;; every integer X: boundaries of 32 and 64 bits, negative fixnums and bignums, and a million
;;; pseudo-random words are compared with the same value taken through MOD and a comparison,
;;; which the lane does not recognise.

(setf dotcl:*save-sil* t)

(defmacro %swl (n x)
  (let ((g (gensym "G")))
    `(let ((,g ,x))
       (logior (ldb (byte ,(1- n) 0) ,g) (- (mask-field (byte 1 ,(1- n)) ,g))))))

(defun %swl-ref (x n)
  (let ((m (mod x (ash 1 n))))
    (if (>= m (ash 1 (1- n))) (- m (ash 1 n)) m)))

;; X as an untyped argument, a signed 64-bit argument and an unsigned 64-bit one.
(defun %swl-64-any (x) (%swl 64 x))
(defun %swl-32-any (x) (%swl 32 x))
(defun %swl-64-s (x) (declare (type (signed-byte 64) x)) (%swl 64 x))
(defun %swl-32-s (x) (declare (type (signed-byte 64) x)) (%swl 32 x))
(defun %swl-64-u (x) (declare (type (unsigned-byte 64) x)) (%swl 64 x))
(defun %swl-32-u (x) (declare (type (unsigned-byte 64) x)) (%swl 32 x))
;; The operand an arithmetic expression: a product, a sum, a shift.
(defun %swl-64-mul (a b) (declare (type (signed-byte 64) a b)) (%swl 64 (+ (* a 31) b)))
(defun %swl-32-mul (a b) (declare (type (signed-byte 64) a b)) (%swl 32 (+ (* a 31) b)))
(defun %swl-64-shr (a) (declare (type (signed-byte 64) a)) (%swl 64 (ash (ldb (byte 64 0) a) -3)))
;; Nested, with typed locals between the steps.
(defun %swl-chain (a b)
  (declare (type (signed-byte 64) a b))
  (let ((x (%swl 64 (+ (* a 31) b))))
    (declare (type (signed-byte 64) x))
    (let ((y (%swl 64 (ash (ldb (byte 64 0) x) -3))))
      (declare (type (signed-byte 64) y))
      (let ((z (%swl 32 (+ (* y 7) x))))
        (declare (type (signed-byte 32) z))
        (+ z (%swl 64 (logxor x (ash y 5))))))))
;; A 32-bit reading inside a 64-bit modular sum.
(defun %swl-mod64 (a b)
  (declare (type (unsigned-byte 64) a b))
  (ldb (byte 64 0) (+ (* (%swl 32 a) 31) (%swl 64 b))))

(defun %swl-mask (a) (declare (type (unsigned-byte 64) a)) (logand (%swl 32 a) #xFFFFFFFFFFFF))
(defun %swl-shl (a) (declare (type (unsigned-byte 64) a)) (ldb (byte 64 0) (ash (%swl 32 a) 3)))
(defun %swl-shr (a) (declare (type (unsigned-byte 64) a)) (ash (logand (%swl 32 a) #xFFFFFFFFFFFFFFF) -2))

(defun %swl-chain-ref (a b)
  (let* ((x (%swl-ref (+ (* a 31) b) 64))
         (y (%swl-ref (ash (ldb (byte 64 0) x) -3) 64))
         (z (%swl-ref (+ (* y 7) x) 32)))
    (+ z (%swl-ref (logxor x (ash y 5)) 64))))
(defun %swl-mod64-ref (a b)
  (ldb (byte 64 0) (+ (* (%swl-ref a 32) 31) (%swl-ref b 64))))

(setf dotcl:*save-sil* nil)

(defparameter *swl-bounds*
  (let ((base (list 0 1 2 3 #x7f #x80 #xff #x100 #x7fff #x8000 #xffff #x10000
                    (1- (ash 1 31)) (ash 1 31) (1+ (ash 1 31)) (1- (ash 1 32)) (ash 1 32)
                    (1+ (ash 1 32)) (1- (ash 1 62)) (ash 1 62) (1- (ash 1 63)) (ash 1 63)
                    (1+ (ash 1 63)) (1- (ash 1 64)) (ash 1 64) (1+ (ash 1 64))
                    (+ (ash 1 70) 5) (1- (ash 1 100)))))
    (remove-duplicates (append base (mapcar #'- base)))))

(defun %swl-check (fn ref-n values)
  "NIL when FN gives the reference reading on every value, else the first value that differs."
  (dolist (v values nil)
    (unless (eql (funcall fn v) (%swl-ref v ref-n))
      (return (list v (funcall fn v) (%swl-ref v ref-n))))))

(defun %swl-in-range (signed)
  (remove-if-not (lambda (v) (if signed
                                 (typep v '(signed-byte 64))
                                 (typep v '(unsigned-byte 64))))
                 *swl-bounds*))

(deftest sign-wrap-lane.64-bit-untyped
  (%swl-check #'%swl-64-any 64 *swl-bounds*)
  nil)
(deftest sign-wrap-lane.32-bit-untyped
  (%swl-check #'%swl-32-any 32 *swl-bounds*)
  nil)
(deftest sign-wrap-lane.64-bit-signed
  (%swl-check #'%swl-64-s 64 (%swl-in-range t))
  nil)
(deftest sign-wrap-lane.32-bit-signed
  (%swl-check #'%swl-32-s 32 (%swl-in-range t))
  nil)
(deftest sign-wrap-lane.64-bit-unsigned
  (%swl-check #'%swl-64-u 64 (%swl-in-range nil))
  nil)
(deftest sign-wrap-lane.32-bit-unsigned
  (%swl-check #'%swl-32-u 32 (%swl-in-range nil))
  nil)

(deftest sign-wrap-lane.a-non-integer-is-a-type-error
  (list (handler-case (%swl-64-any 1.5d0) (type-error () :type-error))
        (handler-case (%swl-32-any "x") (type-error () :type-error))
        (handler-case (%swl-64-any nil) (type-error () :type-error)))
  (:type-error :type-error :type-error))

(deftest sign-wrap-lane.the-operand-is-evaluated-once
  (let ((n 0))
    (list (%swl 64 (progn (incf n) (ash 1 63)))
          (%swl 32 (progn (incf n) (ash 1 31)))
          n))
  (-9223372036854775808 -2147483648 2))

(defun %swl-pairs ()
  (let ((vals (remove-if-not (lambda (v) (typep v '(signed-byte 64))) *swl-bounds*)))
    (loop for a in vals append (loop for b in vals collect (cons a b)))))

(deftest sign-wrap-lane.arithmetic-operands-on-the-boundaries
  (let ((bad nil))
    (dolist (p (%swl-pairs))
      (let ((a (car p)) (b (cdr p)))
        (unless (and (eql (%swl-64-mul a b) (%swl-ref (+ (* a 31) b) 64))
                     (eql (%swl-32-mul a b) (%swl-ref (+ (* a 31) b) 32))
                     (eql (%swl-chain a b) (%swl-chain-ref a b)))
          (push p bad))))
    (subseq bad 0 (min 3 (length bad))))
  nil)

(deftest sign-wrap-lane.shift-operand-on-the-boundaries
  (let ((bad nil))
    (dolist (a (remove-if-not (lambda (v) (typep v '(signed-byte 64))) *swl-bounds*))
      (unless (eql (%swl-64-shr a) (%swl-ref (ash (ldb (byte 64 0) a) -3) 64))
        (push a bad)))
    bad)
  nil)

(deftest sign-wrap-lane.modular-sum-with-a-32-bit-reading
  (let ((bad nil)
        (vals (remove-if-not (lambda (v) (typep v '(unsigned-byte 64))) *swl-bounds*)))
    (dolist (a vals)
      (dolist (b vals)
        (unless (eql (%swl-mod64 a b) (%swl-mod64-ref a b))
          (push (cons a b) bad))))
    (subseq bad 0 (min 3 (length bad))))
  nil)

;; A million words from a linear congruential generator, spread over the shapes above.
(deftest sign-wrap-lane.a-million-random-words
  (let ((x 88172645463325252) (bad nil))
    (dotimes (i 1000000)
      (setq x (ldb (byte 64 0) (+ (* x 6364136223846793005) 1442695040888963407)))
      (let* ((u x)
             (s (if (logbitp 63 x) (- x (ash 1 64)) x))
             (w (if (zerop (mod i 5)) (* s (+ 1 (mod i 7))) s)))
        (unless (and (eql (%swl-64-u u) (%swl-ref u 64))
                     (eql (%swl-32-u u) (%swl-ref u 32))
                     (eql (%swl-64-s s) (%swl-ref s 64))
                     (eql (%swl-32-s s) (%swl-ref s 32))
                     (eql (%swl-64-any w) (%swl-ref w 64))
                     (eql (%swl-32-any w) (%swl-ref w 32)))
          (when (< (length bad) 3) (push (list u s w) bad)))))
    bad)
  nil)

(deftest sign-wrap-lane.random-pairs-through-the-chain
  (let ((x 1234567891011121314) (bad nil))
    (dotimes (i 200000)
      (setq x (ldb (byte 64 0) (+ (* x 6364136223846793005) 1442695040888963407)))
      (let* ((a (if (logbitp 63 x) (- x (ash 1 64)) x))
             (b (- (ldb (byte 64 0) (* x 3)) (ash 1 63))))
        (unless (and (eql (%swl-chain a b) (%swl-chain-ref a b))
                     (eql (%swl-mod64 (ldb (byte 64 0) a) (ldb (byte 64 0) b))
                          (%swl-mod64-ref (ldb (byte 64 0) a) (ldb (byte 64 0) b))))
          (when (< (length bad) 3) (push (cons a b) bad)))))
    bad)
  nil)

;; The chain no longer builds a bignum for the mask-field value or its negation.
(deftest-emitting-only sign-wrap-lane.no-generic-negate-in-the-chain
  (let ((sil (%flat-text (dotcl:function-sil #'%swl-chain))))
    (list (and (search "Runtime.Negate" sil) t)
          (and (search "Runtime.Logior2" sil) t)
          (and (search "Runtime.BoxU64" sil) t)))
  (nil nil nil))

(deftest sign-wrap-lane.a-32-bit-reading-under-a-mask-and-a-shift
  (let ((bad nil))
    (dolist (a (%swl-in-range nil))
      (let ((r (%swl-ref a 32)))
        (unless (and (eql (%swl-mask a) (logand r #xFFFFFFFFFFFF))
                     (eql (%swl-shl a) (ldb (byte 64 0) (ash r 3)))
                     (eql (%swl-shr a) (ash (logand r #xFFFFFFFFFFFFFFF) -2)))
          (push a bad))))
    bad)
  nil)
