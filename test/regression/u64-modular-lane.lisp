;;; Integer expressions whose value is known to lie in [0, 2^64) and that are
;;; built from operations exact modulo 2^64 -- (ldb (byte 64 0) (+ a b)),
;;; (logand x #xFFFFFFFFFFFFFFFF), a 64-bit rotate -- are computed in a raw
;;; int64 instead of through bignums at every step. The answers have to be
;;; exactly the generic ones: for every value, for values that break the
;;; declarations, and for non-integers.

(defun %u64l-add (a b) (declare (type (unsigned-byte 64) a b)) (ldb (byte 64 0) (+ a b)))
(defun %u64l-sub (a b) (declare (type (unsigned-byte 64) a b)) (ldb (byte 64 0) (- a b)))
(defun %u64l-mul (a b) (declare (type (unsigned-byte 64) a b)) (ldb (byte 64 0) (* a b)))
(defun %u64l-rol (a s)
  (declare (type (unsigned-byte 64) a) (type (integer 0 64) s))
  (logior (ldb (byte 64 0) (ash a s)) (ash a (- s 64))))
(defun %u64l-mix (a b c)
  (logand (logxor (+ a (* b 3)) (lognot c) (ash a 7)) #xFFFFFFFFFFFFFFFF))
(defun %u64l-shr (a) (declare (type (unsigned-byte 64) a)) (logxor (ash a -32) (ash a -64) (ash a -1)))

(defun %u64l-check (n)
  (let ((rs (make-random-state nil)) (m (expt 2 64)) (bad nil))
    (dotimes (i n bad)
      (let ((a (random m rs)) (b (random m rs)) (s (random 65 rs)))
        (unless (and (eql (%u64l-add a b) (mod (+ a b) m))
                     (eql (%u64l-sub a b) (mod (- a b) m))
                     (eql (%u64l-mul a b) (mod (* a b) m))
                     (eql (%u64l-rol a s) (mod (logior (ash a s) (ash a (- s 64))) m))
                     (eql (%u64l-mix a b s) (mod (logxor (+ a (* b 3)) (lognot s) (ash a 7)) m))
                     (eql (%u64l-shr a) (logxor (ash a -32) (ash a -64) (ash a -1))))
          (push (list a b s) bad))))))

(deftest u64-modular-lane.random-values
  (%u64l-check 3000)
  nil)

;; Declarations that do not hold give the generic answers: a negative value,
;; one past 2^64, a value whose high bits a right shift reads.
(deftest u64-modular-lane.declarations-broken
  (list (%u64l-add -1 3)
        (%u64l-rol -1 4)
        (%u64l-rol (expt 2 70) 3)
        (%u64l-shr -8)
        (%u64l-shr (expt 2 70))
        (%u64l-mix -5 (expt 2 80) 7))
  (2 -1 512 #.(logxor (ash -8 -32) (ash -8 -64) (ash -8 -1))
   #.(logxor (ash (expt 2 70) -32) (ash (expt 2 70) -64) (ash (expt 2 70) -1))
   #.(ldb (byte 64 0) (logxor (+ -5 (* (expt 2 80) 3)) (lognot 7) (ash -5 7)))))

(deftest u64-modular-lane.not-an-integer
  (list (handler-case (%u64l-add 1.5d0 2) (type-error () :type-error))
        (handler-case (%u64l-mix 1 #\a 3) (type-error () :type-error)))
  (:type-error :type-error))

;; Large words do not become a bignum at each step: a million rotate-and-adds
;; on values at or above 2^63 allocate at most one boxed result each.
(defun %u64l-bytes () (nth 4 (dotcl:gc-stats)))
(deftest-compiled-only u64-modular-lane.no-intermediate-bignums
  (let ((a (- (expt 2 64) 12345)) (b (- (expt 2 63) 7)) (acc 0))
    (%u64l-add (%u64l-rol a 13) b)
    (let ((b0 (%u64l-bytes)))
      (dotimes (i 100000) (setf acc (%u64l-add (%u64l-rol a 13) b)))
      (list (< (/ (- (%u64l-bytes) b0) 100000) 120)
            (eql acc (mod (+ (mod (logior (ash a 13) (ash a -51)) (expt 2 64)) b) (expt 2 64))))))
  (t t))
