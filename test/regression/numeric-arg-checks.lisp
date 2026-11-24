;;; Argument checks of FLOAT-RADIX, CONJUGATE, DIGIT-CHAR, PEEK-CHAR, FLOAT, CIS,
;;; SIGNUM, and the sign of PHASE on a negative zero. Expected values from SBCL.

(deftest numeric-arg-checks.float-radix
  (list (float-radix 1.0) (float-radix 1d0)
        (handler-case (float-radix 1) (type-error () :type-error))
        (handler-case (float-radix 'a) (type-error () :type-error))
        (handler-case (float-radix #c(1.0 2.0)) (type-error () :type-error)))
  (2 2 :type-error :type-error :type-error))

(deftest numeric-arg-checks.conjugate
  (list (conjugate 5) (conjugate #c(1 2))
        (handler-case (conjugate 'a) (type-error () :type-error))
        (handler-case (conjugate nil) (type-error () :type-error)))
  (5 #c(1 -2) :type-error :type-error))

(deftest numeric-arg-checks.digit-char
  (list (digit-char 5) (digit-char 11 16)
        (handler-case (digit-char 5 'a) (type-error () :type-error))
        (handler-case (digit-char 5 1.5) (type-error () :type-error)))
  (#\5 #\B :type-error :type-error))

(deftest numeric-arg-checks.peek-char
  (let ((s (make-string-input-stream "ab")))
    (list (peek-char nil s) (peek-char #\b s)
          (handler-case (peek-char 1 s) (type-error () :type-error))
          (handler-case (peek-char 'a s) (type-error () :type-error))
          (read-char s)))
  (#\a #\b :type-error :type-error #\b))

(deftest numeric-arg-checks.float-complex
  (list (handler-case (float #c(1 2)) (type-error () :type-error))
        (handler-case (float #c(1.5d0 2)) (type-error () :type-error))
        (handler-case (cis #c(1 2)) (type-error () :type-error))
        (handler-case (signum 'a) (type-error () :type-error))
        (handler-case (signum nil) (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest numeric-arg-checks.phase-negative-zero
  (list (phase -0.0) (phase -0.0d0) (phase 0.0) (phase 0.0d0) (phase -1.0) (phase 2) (phase -3))
  (3.1415927 3.141592653589793d0 0.0 0.0d0 3.1415927 0.0 3.1415927))

(deftest numeric-arg-checks.byte
  (list (byte 3 4)
        (handler-case (byte -1 0) (type-error () :type-error))
        (handler-case (byte 1 -1) (type-error () :type-error))
        (handler-case (byte 'a 1) (type-error () :type-error))
        (handler-case (byte 1.5 0) (type-error () :type-error))
        (ldb (byte 8 4) 4080))
  ((3 . 4) :type-error :type-error :type-error :type-error 255))

(defun nac-byte (n p) (byte n p))
(defun nac-ldb (n p x) (ldb (byte n p) x))
(defun nac-dpb (n p x) (dpb 5 (byte n p) x))
(defun nac-ldb-fix (n p x) (declare (fixnum n p x)) (ldb (byte n p) x))

(deftest numeric-arg-checks.byte-variable-arguments
  (list (nac-byte 3 4)
        (nac-byte 0 0)
        (nac-byte (expt 2 70) 0)
        (handler-case (nac-byte -1 0) (type-error () :type-error))
        (handler-case (nac-byte 0 -1) (type-error () :type-error))
        (handler-case (nac-byte 'a 0) (type-error () :type-error))
        (handler-case (nac-byte 0 1.5) (type-error () :type-error))
        (nac-ldb 8 4 4080)
        (nac-ldb 0 4 4080)
        (handler-case (nac-ldb -1 0 5) (type-error () :type-error))
        (nac-dpb 4 4 0)
        (handler-case (nac-dpb 4 -1 0) (type-error () :type-error))
        (nac-ldb-fix 8 4 4080)
        (handler-case (nac-ldb-fix -1 0 5) (type-error () :type-error)))
  ((3 . 4) (0 . 0) (#.(expt 2 70) . 0) :type-error :type-error :type-error :type-error
   255 0 :type-error 80 :type-error 255 :type-error))
