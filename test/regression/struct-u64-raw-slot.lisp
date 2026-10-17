;;; A structure slot declared (unsigned-byte 64), directly or through a
;;; DEFTYPE, holds the value's bits: values at or above 2^63 read back exact,
;;; arithmetic in the 64-bit lane reads and stores it without a bignum, and a
;;; value outside the type is still an error.

(deftype %su-u64 () '(unsigned-byte 64))

(defstruct (%su-state (:constructor %make-su-state (&optional (state 0) (inc 1))))
  (state 0 :type %su-u64)
  (inc 1 :type (unsigned-byte 64))
  (tag nil))

(declaim (inline %su-mod64))
(defun %su-mod64 (n) (mod n (expt 2 64)))

(defun %su-step (s)
  (setf (%su-state-state s)
        (%su-mod64 (+ (* (%su-state-state s) 6364136223846793005) (%su-state-inc s))))
  nil)

(defun %su-top-bits (s)
  (let ((x (%su-state-state s)))
    (logior (ash (ldb (byte 5 59) x) 32) (ldb (byte 32 0) x))))

(deftest struct-u64-raw-slot.round-trip
  (let ((s (%make-su-state (1- (expt 2 64)) (expt 2 63))))
    (list (%su-state-state s) (%su-state-inc s)
          (progn (setf (%su-state-state s) (+ (expt 2 63) 5)) (%su-state-state s))
          (progn (setf (%su-state-inc s) 0) (%su-state-inc s))))
  (18446744073709551615 9223372036854775808 9223372036854775813 0))

(deftest struct-u64-raw-slot.lane-update
  (let ((s (%make-su-state 12345 54321))
        (ref 12345))
    (dotimes (i 50)
      (%su-step s)
      (setq ref (mod (+ (* ref 6364136223846793005) 54321) (expt 2 64))))
    (list (= (%su-state-state s) ref)
          (= (%su-top-bits s)
             (logior (ash (ldb (byte 5 59) ref) 32) (ldb (byte 32 0) ref)))))
  (t t))

(deftest struct-u64-raw-slot.out-of-type
  (let ((s (%make-su-state 1 1)))
    (list (handler-case (progn (setf (%su-state-state s) -1) :stored)
            (type-error () :type-error))
          (handler-case (progn (setf (%su-state-inc s) (expt 2 64)) :stored)
            (type-error () :type-error))
          (%su-state-state s) (%su-state-inc s)))
  (:type-error :type-error 1 1))

(deftest struct-u64-raw-slot.copy-equalp-print
  (let* ((s (%make-su-state (expt 2 63) 7))
         (c (copy-%su-state s)))
    (setf (%su-state-tag c) :copy)
    (list (%su-state-state c) (equalp s (%make-su-state (expt 2 63) 7))
          (let ((*package* (symbol-package '%su-state)))
            (prin1-to-string s))
          (handler-case (progn (%make-su-state (expt 2 64) 0) :made)
            (type-error () :type-error))))
  (9223372036854775808 t "#S(%SU-STATE :STATE 9223372036854775808 :INC 7 :TAG NIL)"
   :type-error))

;; The update in the lane reads and stores the slot as bits: no allocation.
(defun %su-step-loop (n)
  (declare (fixnum n))
  (let ((s (%make-su-state (+ (expt 2 63) 1) 1442695040888963407)))
    (dotimes (i n s) (%su-step s))))

(deftest-compiled-only struct-u64-raw-slot.update-no-alloc
  (< (bytes-per-op #'%su-step-loop) 8)
  t)

;; A LET bound to such a slot and read only by the lane's operators holds the
;; bits: taking fields of a state at or above 2^63 allocates nothing.
(defun %su-low-bits (s)
  (let ((x (%su-state-state s)))
    (logxor (ldb (byte 5 59) x) (ldb (byte 5 0) x))))
(defun %su-top-bits-loop (n)
  (declare (fixnum n))
  (let ((s (%make-su-state (+ (expt 2 63) 12345) 1)) (r 0))
    (dotimes (i n r) (setq r (%su-low-bits s)))))

(deftest-compiled-only struct-u64-raw-slot.let-bits-no-alloc
  (< (bytes-per-op #'%su-top-bits-loop) 8)
  t)

;; One used elsewhere keeps the boxed value.
(defun %su-state-pair (s)
  (let ((x (%su-state-state s)))
    (list x (ldb (byte 8 0) x))))

(deftest struct-u64-raw-slot.let-other-use
  (%su-state-pair (%make-su-state (+ (expt 2 63) 255) 1))
  (9223372036854776063 255))
