;;; A variable declared (SIMPLE-ARRAY <integer type> (*)) fetches its element
;;; buffer once. Above (safety 0) that fetch matched only the .NET storage, so
;;; a FIXNUM vector passed for an (UNSIGNED-BYTE 32) one (both are kept in an
;;; Int64[]) and an (UNSIGNED-BYTE 5) vector for an (UNSIGNED-BYTE 8) one (both
;;; in a byte[]). It now matches the vector's element type as well, so a false
;;; declaration is a TYPE-ERROR, as in SBCL, and the elements of a declared
;;; array are known to be in its element type: the XOR of two elements of
;;; declared (UNSIGNED-BYTE 32) arrays is stored without a store check.
;;; (safety 0) trusts the declaration as before.

(setf dotcl:*save-sil* t)

(defun %detf-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %detf-xor1 (x y)
  (declare (type (simple-array (unsigned-byte 32) (*)) x y)
           (optimize (speed 3) (safety 1)))
  (dotimes (i (length x))
    (setf (aref x i) (logxor (aref x i) (aref y i))))
  x)

(defun %detf-xor0 (x y)
  (declare (type (simple-array (unsigned-byte 32) (*)) x y)
           (optimize (speed 3) (safety 0)))
  (dotimes (i (length x))
    (setf (aref x i) (logxor (aref x i) (aref y i))))
  x)

;; Can leave [0, 2^32 - 1]: the check stays.
(defun %detf-sum1 (x y)
  (declare (type (simple-array (unsigned-byte 32) (*)) x y)
           (optimize (speed 3) (safety 1)))
  (setf (aref x 0) (+ (aref x 0) (aref y 0)))
  x)

(defun %detf-u8 (x)
  (declare (type (simple-array (unsigned-byte 8) (*)) x)
           (optimize (speed 3) (safety 1)))
  (setf (aref x 0) (logand (aref x 1) 15))
  x)

(defun %detf-s16 (x)
  (declare (type (simple-array (signed-byte 16) (*)) x)
           (optimize (speed 3) (safety 1)))
  (setf (aref x 0) (aref x 1))
  x)

(defun %detf-fixnum (x)
  (declare (type (simple-array fixnum (*)) x)
           (optimize (speed 3) (safety 1)))
  (setf (aref x 0) (- (aref x 1)))
  x)

(deftest-emitting-only declared-element-type-fetch.sil
  (list (%detf-count "Runtime.CheckStore" #'%detf-xor1)
        (%detf-count "Runtime.BackingI64Narrow" #'%detf-xor1)
        (%detf-count "Runtime.CheckStore" #'%detf-sum1)
        (%detf-count "Runtime.BackingI64Narrow" #'%detf-xor0)
        (%detf-count "Runtime.CheckStore" #'%detf-xor0))
  (0 2 1 0 0))

(defun %detf-vec (type &rest vals)
  (make-array (length vals) :element-type type :initial-contents vals))

(defun %detf-try (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-expected-type e)))))

;; True declarations, and the range of the values stored.
(deftest declared-element-type-fetch.values
  (list (coerce (%detf-xor1 (%detf-vec '(unsigned-byte 32) #xffffffff 0 #x12345678)
                            (%detf-vec '(unsigned-byte 32) 0 #xffffffff #x0f0f0f0f))
                'list)
        (coerce (%detf-xor0 (%detf-vec '(unsigned-byte 32) 1 2)
                            (%detf-vec '(unsigned-byte 32) 3 4))
                'list)
        (aref (%detf-sum1 (%detf-vec '(unsigned-byte 32) 1) (%detf-vec '(unsigned-byte 32) 2)) 0)
        (car (%detf-try (lambda ()
                          (%detf-sum1 (%detf-vec '(unsigned-byte 32) #xffffffff)
                                      (%detf-vec '(unsigned-byte 32) 1)))))
        (aref (%detf-u8 (%detf-vec '(unsigned-byte 8) 0 255)) 0)
        (aref (%detf-s16 (%detf-vec '(signed-byte 16) 0 -32768)) 0)
        (aref (%detf-fixnum (%detf-vec 'fixnum 0 most-positive-fixnum)) 0))
  ((#xffffffff #xffffffff #x1d3b5977) (2 6) 3
   :type-error 15 -32768 #.(- most-positive-fixnum)))

;; False declarations: a TYPE-ERROR above (safety 0), from the fetch, also when
;; every element happens to be in the declared type. The interpreter may
;; ignore declarations (CLHS 3.3.1), so these hold for compiled code only.
(deftest-emitting-only declared-element-type-fetch.false-declaration
  (list (%detf-try (lambda ()
                     (%detf-xor1 (%detf-vec '(unsigned-byte 32) 1 2)
                                 (%detf-vec 'fixnum 3 4))))
        (%detf-try (lambda ()
                     (%detf-xor1 (%detf-vec 'fixnum 1 2)
                                 (%detf-vec '(unsigned-byte 32) 3 4))))
        (%detf-try (lambda () (%detf-u8 (%detf-vec '(unsigned-byte 5) 0 31))))
        (%detf-try (lambda () (%detf-s16 (%detf-vec '(signed-byte 8) 0 1))))
        (%detf-try (lambda () (%detf-fixnum (%detf-vec '(unsigned-byte 32) 0 1))))
        ;; (safety 0) trusts the declaration: the storage is the same, so the
        ;; values come out right.
        (coerce (%detf-xor0 (%detf-vec 'fixnum 1 2)
                            (%detf-vec '(unsigned-byte 32) 3 4))
                'list))
  ((:type-error (simple-array (unsigned-byte 32) (*)))
   (:type-error (simple-array (unsigned-byte 32) (*)))
   (:type-error (simple-array (unsigned-byte 8) (*)))
   (:type-error (simple-array (signed-byte 16) (*)))
   (:type-error (simple-array fixnum (*)))
   (2 6)))

(setf dotcl:*save-sil* nil)
