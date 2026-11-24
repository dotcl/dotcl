;;; The high 64 bits of a 128-bit product, (ldb (byte 64 64) (* x y)) and (ash (* x y) -64), are
;;; one machine multiply when both factors are PROVED to be (unsigned-byte 64) (the unsigned
;;; high word) or, for the masked field, both (signed-byte 64) (the signed high word). Factors
;;; that are not proved take the generic path. Every case is compared with the same value taken
;;; through FLOOR and MOD, which the lane does not recognise: boundaries of both signs
;;; (negative times positive, the most negative value times -1, 0, the largest values) and a
;;; million pseudo-random pairs.

(defun %mhl-ref (x y)
  "(ldb (byte 64 64) (* x y)) as FLOOR and MOD."
  (mod (floor (* x y) (ash 1 64)) (ash 1 64)))

(setf dotcl:*save-sil* t)

;; Proved unsigned: the three spellings.
(defun %mhl-u-ldb (x y) (declare (type (unsigned-byte 64) x y)) (ldb (byte 64 64) (* x y)))
(defun %mhl-u-ash (x y) (declare (type (unsigned-byte 64) x y)) (ash (* x y) -64))
(defun %mhl-u-and (x y)
  (declare (type (unsigned-byte 64) x y))
  (logand (ash (* x y) -64) #xFFFFFFFFFFFFFFFF))
;; Proved signed (the masked field only).
(defun %mhl-s-ldb (x y) (declare (type (signed-byte 64) x y)) (ldb (byte 64 64) (* x y)))
(defun %mhl-s-and (x y)
  (declare (type (signed-byte 64) x y))
  (logand (ash (* x y) -64) #xFFFFFFFFFFFFFFFF))
;; The bare shift: signed when a factor is negative (an operand that is no (unsigned-byte 64) takes
;; the generic product, unmasked).
(defun %mhl-bare-any (x y) (ash (* x y) -64))
(defun %mhl-bare-s (x y) (declare (type (signed-byte 64) x y)) (ash (* x y) -64))
(defun %mhl-bare-mixed (x y) (declare (type (unsigned-byte 64) x) (type (signed-byte 64) y)) (ash (* x y) -64))
;; Not proved: untyped, and one factor of each kind.
(defun %mhl-any (x y) (ldb (byte 64 64) (* x y)))
(defun %mhl-mixed (x y)
  (declare (type (unsigned-byte 64) x) (type (signed-byte 64) y))
  (ldb (byte 64 64) (* x y)))
;; An expression as a factor, and a use of the result in a 64-bit modular sum.
(defun %mhl-expr (a b c)
  (declare (type (unsigned-byte 64) a b c))
  (ldb (byte 64 0) (+ (ldb (byte 64 64) (* (logxor a c) (logior b 1))) (* a 3))))
(defun %mhl-expr-ref (a b c)
  (ldb (byte 64 0) (+ (%mhl-ref (logxor a c) (logior b 1)) (* a 3))))

(setf dotcl:*save-sil* nil)

(defparameter *mhl-bounds*
  (let ((base (list 0 1 2 3 #x7f #x80 #xff #x100 #xffff #x10000
                    (1- (ash 1 31)) (ash 1 31) (1- (ash 1 32)) (ash 1 32) (1+ (ash 1 32))
                    (1- (ash 1 62)) (ash 1 62) (1- (ash 1 63)) (ash 1 63) (1+ (ash 1 63))
                    (1- (ash 1 64)) #x5555555555555555 #xAAAAAAAAAAAAAAAA #x0123456789ABCDEF)))
    (remove-duplicates (append base (mapcar #'- base)))))

(defun %mhl-values (kind)
  (remove-if-not (lambda (v) (typep v (if (eq kind :u) '(unsigned-byte 64) '(signed-byte 64))))
                 *mhl-bounds*))

(defun %mhl-check (fn kind)
  "NIL when FN agrees with the reference on every pair of boundary values of KIND."
  (let ((vals (%mhl-values kind)) (bad nil))
    (dolist (x vals)
      (dolist (y vals)
        (unless (eql (funcall fn x y) (%mhl-ref x y))
          (when (< (length bad) 3) (push (list x y (funcall fn x y) (%mhl-ref x y)) bad)))))
    bad))

(deftest mulhi-lane.unsigned-ldb (%mhl-check #'%mhl-u-ldb :u) nil)
(deftest mulhi-lane.unsigned-and (%mhl-check #'%mhl-u-and :u) nil)
(deftest mulhi-lane.unsigned-bare-ash
  (let ((bad nil) (vals (%mhl-values :u)))
    (dolist (x vals)
      (dolist (y vals)
        (unless (eql (%mhl-u-ash x y) (floor (* x y) (ash 1 64)))
          (push (list x y) bad))))
    (subseq bad 0 (min 3 (length bad))))
  nil)
(deftest mulhi-lane.signed-ldb (%mhl-check #'%mhl-s-ldb :s) nil)
(deftest mulhi-lane.signed-and (%mhl-check #'%mhl-s-and :s) nil)
(deftest mulhi-lane.untyped (%mhl-check #'%mhl-any :s) nil)
(deftest mulhi-lane.untyped-unsigned-and-bignums
  (let ((bad nil)
        (vals (append (%mhl-values :u) (list (ash 1 64) (ash 1 70) (- (ash 1 80))))))
    (dolist (x vals)
      (dolist (y vals)
        (unless (eql (%mhl-any x y) (%mhl-ref x y)) (push (list x y) bad))))
    (subseq bad 0 (min 3 (length bad))))
  nil)
(deftest mulhi-lane.mixed-kinds
  (let ((bad nil))
    (dolist (x (%mhl-values :u))
      (dolist (y (%mhl-values :s))
        (unless (eql (%mhl-mixed x y) (%mhl-ref x y)) (push (list x y) bad))))
    (subseq bad 0 (min 3 (length bad))))
  nil)
(deftest mulhi-lane.the-corner-cases
  (list (%mhl-s-ldb (- (ash 1 63)) -1)
        (%mhl-s-ldb (- (ash 1 63)) (- (ash 1 63)))
        (%mhl-s-ldb -1 1)
        (%mhl-s-ldb -1 -1)
        (%mhl-s-ldb (1- (ash 1 63)) (1- (ash 1 63)))
        (%mhl-u-ldb (1- (ash 1 64)) (1- (ash 1 64)))
        (%mhl-u-ldb 0 (1- (ash 1 64))))
  (0 4611686018427387904 18446744073709551615 0 4611686018427387903 18446744073709551614 0))

(deftest mulhi-lane.factors-that-are-expressions
  (let ((bad nil) (vals (%mhl-values :u)))
    (dolist (a vals)
      (dolist (b vals)
        (dolist (c '(0 1 #xFFFFFFFFFFFFFFFF #x8000000000000000))
          (unless (eql (%mhl-expr a b c) (%mhl-expr-ref a b c)) (push (list a b c) bad)))))
    (subseq bad 0 (min 3 (length bad))))
  nil)

(deftest mulhi-lane.a-million-random-pairs
  (let ((x 88172645463325252) (bad nil))
    (dotimes (i 500000)
      (setq x (ldb (byte 64 0) (+ (* x 6364136223846793005) 1442695040888963407)))
      (let* ((u1 x)
             (u2 (ldb (byte 64 0) (* x 2862933555777941757)))
             (s1 (if (logbitp 63 u1) (- u1 (ash 1 64)) u1))
             (s2 (if (logbitp 63 u2) (- u2 (ash 1 64)) u2)))
        (unless (and (eql (%mhl-u-ldb u1 u2) (%mhl-ref u1 u2))
                     (eql (%mhl-u-ash u1 u2) (floor (* u1 u2) (ash 1 64)))
                     (eql (%mhl-s-ldb s1 s2) (%mhl-ref s1 s2))
                     (eql (%mhl-s-and s1 s2) (%mhl-ref s1 s2))
                     (eql (%mhl-any s1 u2) (%mhl-ref s1 u2))
                     (eql (%mhl-mixed u1 s2) (%mhl-ref u1 s2)))
          (when (< (length bad) 3) (push (list u1 u2 s1 s2) bad)))))
    bad)
  nil)

;; The signed form is one multiply with no generic product at all; the unsigned ones use the
;; machine multiply on the fast path and keep the generic product only for a value that fails
;; the run-time check.
(deftest-emitting-only mulhi-lane.machine-multiply-is-used
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Runtime.MulHiU64" #'%mhl-u-ldb)
          (has "Runtime.MulHiU64" #'%mhl-u-ash)
          (has "Runtime.MulHiU64" #'%mhl-any)
          (has "Runtime.MulHiS64" #'%mhl-s-ldb) (has "Runtime.Multiply" #'%mhl-s-ldb)
          (has "Runtime.MulHiS64" #'%mhl-s-and) (has "Runtime.Multiply" #'%mhl-s-and)))
  (t t t t nil t nil))

;; (ash (* x y) -64) is FLOOR of the product by 2^64 -- negative for a negative product -- for
;; every kind of factor; only the masked forms are reduced to [0, 2^64).
(deftest mulhi-lane.the-bare-shift-is-signed-for-negative-factors
  (let ((bad nil)
        (vals (append *mhl-bounds* (list (ash 1 64) (ash 1 70) (- (ash 1 80))))))
    (dolist (x vals)
      (dolist (y vals)
        (let ((ref (floor (* x y) (ash 1 64))))
          (unless (eql (%mhl-bare-any x y) ref) (push (list :any x y) bad))
          (when (and (typep x '(signed-byte 64)) (typep y '(signed-byte 64)))
            (unless (eql (%mhl-bare-s x y) ref) (push (list :s x y) bad)))
          (when (and (typep x '(unsigned-byte 64)) (typep y '(signed-byte 64)))
            (unless (eql (%mhl-bare-mixed x y) ref) (push (list :mixed x y) bad))))))
    (subseq bad 0 (min 3 (length bad))))
  nil)

(deftest mulhi-lane.the-reported-values
  (list (%mhl-bare-any -1 2) (%mhl-bare-any -3 18446744073709551615)
        (%mhl-bare-any 3 18446744073709551615) (%mhl-bare-s -1 2) (%mhl-bare-mixed 5 -7))
  (-1 -3 2 -1 -1))
