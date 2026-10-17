;;; A LET variable declared (unsigned-byte 64) keeps its value's bits in an
;;; Int64 slot: the 64-bit lane reads it as an operand already in [0, 2^64),
;;; an assignment stores bits, and a read where the type is not known boxes it
;;; (a bignum at or above 2^63). The answers are the generic path's, the
;;; declaration is checked at the binding and at every assignment, and the
;;; shape of a BLAKE2b round on such variables allocates nothing.

(declaim (inline %u64v-ror))
(defun %u64v-ror (x s)
  (declare (type (unsigned-byte 64) x) (type (integer 0 64) s))
  (logior (ldb (byte 64 0) (ash x (- 64 s))) (ash x (- s))))

;; The G function of BLAKE2b, on four LET variables, N times.
(defun %u64v-mix (a b c d n)
  (let ((a a) (b b) (c c) (d d))
    (declare (type (unsigned-byte 64) a b c d))
    (dotimes (i n)
      (setf a (ldb (byte 64 0) (+ a b))
            d (%u64v-ror (logxor d a) 32)
            c (ldb (byte 64 0) (+ c d))
            b (%u64v-ror (logxor b c) 24)
            a (ldb (byte 64 0) (+ a b))
            d (%u64v-ror (logxor d a) 16)
            c (ldb (byte 64 0) (+ c d))
            b (%u64v-ror (logxor b c) 63)))
    (list a b c d)))

;; The same with no declarations: the generic path.
(defun %u64v-mix-generic (a b c d n)
  (flet ((ror (x s) (mod (logior (ash x (- 64 s)) (ash x (- s))) (expt 2 64))))
    (dotimes (i n)
      (setf a (mod (+ a b) (expt 2 64))
            d (ror (logxor d a) 32)
            c (mod (+ c d) (expt 2 64))
            b (ror (logxor b c) 24)
            a (mod (+ a b) (expt 2 64))
            d (ror (logxor d a) 16)
            c (mod (+ c d) (expt 2 64))
            b (ror (logxor b c) 63)))
    (list a b c d)))

(deftest u64-let-slot.matches-generic
  (let ((rs 1) (bad nil))
    (flet ((next () (setf rs (mod (+ (* rs 6364136223846793005) 1442695040888963407)
                                  (expt 2 64)))))
      (dotimes (k 200 bad)
        (let ((a (next)) (b (next)) (c (next)) (d (next)))
          (unless (equal (%u64v-mix a b c d 3) (%u64v-mix-generic a b c d 3))
            (push (list a b c d) bad))))))
  nil)

;; The expected value is what SBCL gives for the same source. The emit-free
;; build interprets the loop, where the slots this is about do not exist, and
;; runs fewer rounds; the answer for those is SBCL's too.
(deftest u64-let-slot.checksum
  (let ((n (emit-free-scale 100000 10000)))
    (= (reduce #'logxor (%u64v-mix 1 (expt 2 63) (1- (expt 2 64)) 12345 n))
       (if (= n 100000) 2167232548050145232 6360510883765741510)))
  t)

;; The declaration is checked where the slot is written: the binding and each
;; assignment, whatever computed the value. A check of compiled code: the
;; emit-free build's interpreter does not act on declarations.
(defun %u64v-bind (x)
  (let ((y x))
    (declare (type (unsigned-byte 64) y))
    y))
(defun %u64v-incf (x)
  (let ((y x))
    (declare (type (unsigned-byte 64) y))
    (incf y)
    (list y
          (handler-case (progn (incf y (expt 2 64)) :stored) (type-error () :type-error))
          (handler-case (progn (setq y (- y (expt 2 64))) :stored) (type-error () :type-error))
          (handler-case (progn (setq y 'a) :stored) (type-error () :type-error))
          y)))

(deftest-emitting-only u64-let-slot.declaration-checked
  (list (%u64v-bind 5)
        (%u64v-bind (expt 2 63))
        (handler-case (%u64v-bind -1) (type-error () :type-error))
        (handler-case (%u64v-bind (expt 2 64)) (type-error () :type-error))
        (handler-case (%u64v-bind 1.0) (type-error () :type-error))
        (%u64v-incf (1- (expt 2 63))))
  (5 9223372036854775808 :type-error :type-error :type-error
   (9223372036854775808 :type-error :type-error :type-error 9223372036854775808)))

;; LET* (a later init reads an earlier binding), a value read where its type
;; is not known, a shadowing binding of the same name that is not declared, and
;; a variable captured by a closure (kept boxed).
(defun %u64v-mixed (x)
  (let* ((y x)
         (z (logxor y #xFFFFFFFFFFFFFFFF)))
    (declare (type (unsigned-byte 64) y z))
    (setq y (ldb (byte 64 0) (* z 3)))
    (let ((captured y))
      (declare (type (unsigned-byte 64) captured))
      (let ((f (lambda () (setq captured (ldb (byte 64 0) (1+ captured))))))
        (funcall f)
        (list y z (format nil "~d" y)
              (let ((y "shadow")) y)
              captured
              (eql y (expt 2 63)))))))

(deftest u64-let-slot.mixed
  (list (%u64v-mixed 0) (%u64v-mixed (expt 2 63)))
  ((18446744073709551613 18446744073709551615 "18446744073709551613" "shadow"
    18446744073709551614 nil)
   (9223372036854775805 9223372036854775807 "9223372036854775805" "shadow"
    9223372036854775806 nil)))

;; Four variables updated 8 times a round, 100,000 rounds: the words at or
;; above 2^63 were a bignum per assignment (tens of MB).
(defun %u64v-bytes () (nth 4 (dotcl:gc-stats)))
(deftest-compiled-only u64-let-slot.no-allocation
  (progn
    (%u64v-mix 1 2 3 4 10)
    (let ((b0 (%u64v-bytes)))
      (%u64v-mix (1- (expt 2 64)) (expt 2 63) 3 4 100000)
      ;; Under a byte an iteration: other threads (the JIT's own) can add a
      ;; few KB to the process-wide count while this runs.
      (< (- (%u64v-bytes) b0) 100000)))
  t)

;; A variable that holds bits in an Int64 slot and is also known to be a
;; fixnum: its init has a small range (a loop counter, an argument declared
;; (integer 0 10)), or it is declared FIXNUM as well. A fixnum operation on it,
;; (logand n #xff), read the raw slot as a boxed Fixnum: unverifiable IL that
;; threw NullReferenceException for 0 and read a wild address otherwise.
(defun %u64v-ranged (k)
  (declare (type (integer 0 10) k) (optimize (speed 3) (safety 0)))
  (let ((n k))
    (declare (type (unsigned-byte 64) n))
    (ldb (byte 64 0) (ash (logxor #x70 (logand n #xff)) 56))))

(defun %u64v-ranged-fixnum (k)
  (declare (type (integer 0 10) k))
  (let ((n k))
    (declare (type (unsigned-byte 64) n) (fixnum n))
    (ldb (byte 64 0) (+ (ash (logand n #xff) 60) (1+ n)))))

(declaim (inline %u64v-round))
(defun %u64v-round (x n)
  (declare (type (simple-array (unsigned-byte 64) (*)) x) (type (unsigned-byte 64) n)
           (optimize (speed 3) (safety 0)))
  (macrolet ((unrolled ((var limit) &body body)
               `(progn ,@(loop for k below limit
                               collect `(symbol-macrolet ((,var ,k)) ,@body)))))
    (unrolled (i 4)
      (setf (aref x i)
            (ldb (byte 64 0)
                 (+ (aref x i)
                    (ldb (byte 64 0) (ash (logxor (* 16 i) (logand n #xff)) 56)))))))
  (values))

;; The shape of ironclad's Kupyna transform: the loop counter, now a ranged
;; integer, is passed to an inline function whose parameter is (unsigned-byte 64).
(defun %u64v-rounds ()
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (let ((x (make-array 4 :element-type '(unsigned-byte 64) :initial-element 0)))
    (loop for r from 0 below 10 by 2 do (%u64v-round x r) (%u64v-round x (1+ r)))
    (coerce x 'list)))

(deftest u64-let-slot.fixnum-ranged
  (list (%u64v-ranged 0) (%u64v-ranged 3) (%u64v-ranged 10)
        (%u64v-ranged-fixnum 0) (%u64v-ranged-fixnum 9)
        (%u64v-rounds))
  (8070450532247928832 8286623314361712640 8791026472627208192
   1 10376293541461622794
   (3242591731706757120 14771806777775226880 7854277750134145024
    936748722493063168)))
