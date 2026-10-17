;;; A call to an INLINE function whose body is 64-bit word arithmetic is
;;; planned through in the 64-bit lane together with the inline calls in its
;;; arguments: (mod64+ a (mod64* (logand a m) b)) is one expression boxed once,
;;; not one box (often a bignum) per call. The answers have to be the ones the
;;; calls give one by one: for every value, for values that break the
;;; declarations, and with arguments evaluated once each, left to right.

(declaim (inline %u64i-add %u64i-mul %u64i-rol %u64i-ror %u64i-pick %u64i-early %u64i-twice))
(defun %u64i-add (a b) (declare (type (unsigned-byte 64) a b)) (ldb (byte 64 0) (+ a b)))
(defun %u64i-mul (a b) (declare (type (unsigned-byte 64) a b)) (ldb (byte 64 0) (* a b)))
(defun %u64i-rol (a s)
  (declare (type (unsigned-byte 64) a) (type (integer 0 64) s))
  (logior (ldb (byte 64 0) (ash a s)) (ash a (- s 64))))
(defun %u64i-ror (a s)
  (declare (type (unsigned-byte 64) a) (type (integer 0 64) s))
  (%u64i-rol a (- 64 s)))
;; Bodies the lane cannot take: a conditional, a RETURN-FROM, a parameter used
;; twice. They stay calls (or are planned through) with the same answers.
(defun %u64i-pick (a b) (if (> a b) (ldb (byte 64 0) (- a b)) (ldb (byte 64 0) (- b a))))
(defun %u64i-early (a b)
  (when (zerop b) (return-from %u64i-early a))
  (ldb (byte 64 0) (+ a b)))
(defun %u64i-twice (a) (logxor (ldb (byte 64 0) (* a a)) (ash a -3)))

;; The same functions, not inline: the reference answers.
(defun %u64r-add (a b) (mod (+ a b) (expt 2 64)))
(defun %u64r-mul (a b) (mod (* a b) (expt 2 64)))
(defun %u64r-rol (a s) (mod (logior (ash a s) (ash a (- s 64))) (expt 2 64)))
(defun %u64r-ror (a s) (%u64r-rol a (- 64 s)))

;; The shape of one step of BLAKE2b / Argon2's G.
(defun %u64i-g (a b d)
  (let* ((a (%u64i-add a (%u64i-add b (%u64i-mul 2 (%u64i-mul (logand a #xffffffff)
                                                              (logand b #xffffffff))))))
         (d (%u64i-ror (logxor d a) 32)))
    (list a d)))
(defun %u64r-g (a b d)
  (let* ((a (%u64r-add a (%u64r-add b (%u64r-mul 2 (%u64r-mul (logand a #xffffffff)
                                                              (logand b #xffffffff))))))
         (d (%u64r-ror (logxor d a) 32)))
    (list a d)))

(defun %u64i-check (n)
  (let ((rs (make-random-state nil)) (m (expt 2 64)) (bad nil))
    (dotimes (i n bad)
      (let ((a (random m rs)) (b (random m rs)) (d (random m rs)) (s (random 65 rs)))
        (unless (and (equal (%u64i-g a b d) (%u64r-g a b d))
                     (eql (%u64i-add (%u64i-rol a s) b) (%u64r-add (%u64r-rol a s) b))
                     (eql (%u64i-mul (%u64i-ror a 24) (%u64i-ror b 63))
                          (%u64r-mul (%u64r-ror a 24) (%u64r-ror b 63)))
                     (eql (%u64i-add (%u64i-pick a b) d)
                          (%u64r-add (abs (- a b)) d))
                     (eql (%u64i-add (%u64i-early a (logand b 1)) d)
                          (%u64r-add (if (zerop (logand b 1)) a (+ a 1)) d))
                     (eql (%u64i-add (%u64i-twice a) b)
                          (%u64r-add (logxor (mod (* a a) m) (ash a -3)) b)))
          (push (list a b d s) bad))))))

(deftest u64-inline-lane.random-values
  (%u64i-check 2000)
  nil)

;; An argument outside (unsigned-byte 64) signals a TYPE-ERROR from (safety 1)
;; up, also when it comes from a variable. Under (safety 0) the declarations are
;; trusted and the calls compute as if they were not there.
(defvar *u64i-2^70* (expt 2 70))
(defvar *u64i--1* -1)
(defvar *u64i-2^64* (expt 2 64))
(defmacro %u64i-te (form) `(handler-case ,form (type-error () :type-error)))
(defun %u64i-broken-safe ()
  (list (%u64i-te (%u64i-add (%u64i-mul *u64i--1* 3) 1))
        (%u64i-te (%u64i-add (%u64i-ror *u64i-2^70* 3) 0))
        (%u64i-te (%u64i-add *u64i-2^64* 0))
        (%u64i-te (%u64i-g -5 *u64i-2^70* 7))
        (%u64i-te (%u64i-add (%u64i-mul -1 3) 1))
        (%u64i-te (%u64i-add (%u64i-mul 1.5d0 2) 1))
        (%u64i-te (%u64i-add (%u64i-mul *u64i-2^64* 0) 0))))
(defun %u64i-broken-unsafe ()
  (declare (optimize (safety 0)))
  (list (%u64i-add (%u64i-mul *u64i--1* 3) 1)
        (%u64i-add (%u64i-ror *u64i-2^70* 3) 0)
        (%u64i-add -5 (%u64i-add *u64i-2^70* 7))))
;; The emit-free interpreter does not check declared parameter types.
(deftest-emitting-only u64-inline-lane.declarations-broken
  (%u64i-broken-safe)
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))
(deftest-emitting-only u64-inline-lane.declarations-broken-safety-0
  (%u64i-broken-unsafe)
  (18446744073709551614
   #.(mod (logior (ash (expt 2 70) 61) (ash (expt 2 70) -3)) (expt 2 64))
   2))

;; Arguments are evaluated once each, left to right.
(deftest u64-inline-lane.argument-order
  (let ((log nil))
    (flet ((v (x) (push x log) x))
      (list (%u64i-add (v 1) (%u64i-mul (v 2) (%u64i-rol (v 3) 4)))
            (reverse log))))
  (97 (1 2 3)))

;; A chain of such calls boxes its result once. Four steps of G on words at or
;; above 2^63 allocated one box (mostly bignums) per call: ~820 B a round.
(defun %u64i-bytes () (nth 4 (dotcl:gc-stats)))
(defun %u64i-round (w)
  (declare (type (simple-array (unsigned-byte 64) (4)) w))
  (let ((a (aref w 0)) (b (aref w 1)) (c (aref w 2)) (d (aref w 3)))
    (declare (type (unsigned-byte 64) a b c d))
    (setf a (%u64i-add a (%u64i-add b (%u64i-mul 2 (%u64i-mul (logand a #xffffffff)
                                                               (logand b #xffffffff)))))
          d (%u64i-ror (logxor d a) 32)
          c (%u64i-add c (%u64i-add d (%u64i-mul 2 (%u64i-mul (logand c #xffffffff)
                                                               (logand d #xffffffff)))))
          b (%u64i-ror (logxor b c) 24))
    (setf (aref w 0) a (aref w 1) b (aref w 2) c (aref w 3) d)
    nil))
(deftest-compiled-only u64-inline-lane.one-box-per-result
  (let ((w (make-array 4 :element-type '(unsigned-byte 64)
                         :initial-contents (list (- (expt 2 64) 1) (- (expt 2 64) 7919)
                                                 (- (expt 2 64) 15838) (- (expt 2 64) 23757)))))
    (%u64i-round w)
    (let ((b0 (%u64i-bytes)))
      (dotimes (i 100000) (%u64i-round w))
      (< (/ (- (%u64i-bytes) b0) 100000) 400)))
  t)
