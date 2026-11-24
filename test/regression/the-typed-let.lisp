;;; A LET / LET* / DO variable bound to (THE T X), with T a simple-array type and the variable
;;; never assigned, is typed T like a declared one: its element reads and writes take the typed
;;; path. THE keeps checking X where the init is evaluated, so a value of another type still
;;; signals a TYPE-ERROR, and an assigned variable stays untyped (it may hold anything later).

(setf dotcl:*save-sil* t)

(defvar *ttl-table* (let ((a (make-array 256 :element-type '(unsigned-byte 32))))
                      (dotimes (n 256 a)
                        (let ((c n))
                          (dotimes (k 8) (setq c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
                          (setf (aref a n) c)))))
(declaim (notinline ttl-table))
(defun ttl-table () *ttl-table*)

(defun ttl-crc (octets)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (let ((crc #xFFFFFFFF))
    (declare (type (unsigned-byte 32) crc))
    (do ((i 0 (1+ i))
         (table (the (simple-array (unsigned-byte 32) (256)) (ttl-table))))
        ((>= i (length octets)) (logxor crc #xFFFFFFFF))
      (setf crc (logxor (aref table (logand (logxor crc (aref octets i)) #xff))
                        (ldb (byte 32 0) (ash crc -8)))))))
(defun ttl-crc-let (octets)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (let ((crc #xFFFFFFFF)
        (table (the (simple-array (unsigned-byte 32) (256)) (ttl-table))))
    (declare (type (unsigned-byte 32) crc))
    (dotimes (i (length octets) (logxor crc #xFFFFFFFF))
      (setf crc (logxor (aref table (logand (logxor crc (aref octets i)) #xff))
                        (ldb (byte 32 0) (ash crc -8)))))))
(defun ttl-crc-let* (octets)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (let* ((crc #xFFFFFFFF)
         (table (the (simple-array (unsigned-byte 32) (256)) (ttl-table)))
         (n (length octets)))
    (declare (type (unsigned-byte 32) crc))
    (dotimes (i n (logxor crc #xFFFFFFFF))
      (setf crc (logxor (aref table (logand (logxor crc (aref octets i)) #xff))
                        (ldb (byte 32 0) (ash crc -8)))))))
;; The same loop with an untyped table: the reference value.
(defun ttl-crc-ref (octets)
  (let ((crc #xFFFFFFFF) (table (ttl-table)))
    (dotimes (i (length octets) (logxor crc #xFFFFFFFF))
      (setf crc (logxor (aref table (logand (logxor crc (aref octets i)) #xff))
                        (ldb (byte 32 0) (ash crc -8)))))))

;; Other element types and a variable that is assigned.
(defun ttl-sum-double (a)
  (let ((v (the (simple-array double-float (*)) a)) (s 0d0))
    (declare (type double-float s))
    (dotimes (i (length v) s) (incf s (aref v i)))))
(defun ttl-fill-fixnum (n)
  (let ((v (the (simple-array fixnum (*)) (make-array n :element-type 'fixnum))))
    (dotimes (i n) (setf (aref v i) (* i i)))
    (let ((s 0)) (dotimes (i n s) (incf s (aref v i))))))
(defun ttl-assigned (a b flag)
  (let ((v (the (simple-array fixnum (*)) a)))
    (when flag (setq v b))
    (aref v 0)))
(defun ttl-wrong (x)
  (let ((v (the (simple-array (unsigned-byte 8) (*)) x)))
    (aref v 0)))
(defun ttl-wrong-safe (x)
  (declare (optimize (safety 3)))
  (let ((v (the (simple-array (unsigned-byte 8) (*)) x)))
    (aref v 0)))

(setf dotcl:*save-sil* nil)

(defun %ttl-octets (n)
  (let ((a (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n a) (setf (aref a i) (logand 255 (+ (* i 37) 11))))))

(deftest the-typed-let.crc-agrees-with-the-untyped-loop
  (let ((bad nil))
    (dolist (n '(0 1 2 7 255 256 257 1000 4096))
      (let* ((o (%ttl-octets n)) (r (ttl-crc-ref o)))
        (unless (and (eql (ttl-crc o) r) (eql (ttl-crc-let o) r) (eql (ttl-crc-let* o) r))
          (push n bad))))
    bad)
  nil)

(deftest the-typed-let.a-known-crc
  (list (ttl-crc (map '(simple-array (unsigned-byte 8) (*)) #'char-code "123456789"))
        (ttl-crc (make-array 0 :element-type '(unsigned-byte 8))))
  (#xCBF43926 0))

(deftest the-typed-let.other-element-types
  (list (ttl-sum-double (make-array 5 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0 5d0)))
        (ttl-fill-fixnum 10) (ttl-fill-fixnum 0))
  (15d0 285 0))

(deftest the-typed-let.an-assigned-variable-takes-the-other-array
  (let ((a (make-array 3 :element-type 'fixnum :initial-element 1))
        (b (make-array 3 :element-type 'fixnum :initial-element 2)))
    (list (ttl-assigned a b nil) (ttl-assigned a b t)))
  (1 2))

(deftest the-typed-let.the-wrong-type-still-signals
  (list (handler-case (ttl-wrong (list 1 2 3)) (type-error () :type-error) (error () :other))
        (handler-case (ttl-wrong-safe (list 1 2 3)) (type-error () :type-error) (error () :other))
        (handler-case (ttl-wrong (make-array 3 :element-type 'fixnum)) (type-error () :type-error) (error () :other))
        (handler-case (ttl-wrong "abc") (type-error () :type-error) (error () :other))
        (ttl-wrong (%ttl-octets 4)))
  (:type-error :type-error :type-error :type-error 11))

(deftest-emitting-only the-typed-let.the-table-reads-are-typed
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Runtime.ArefL" #'ttl-crc) (has "Runtime.Logxor2" #'ttl-crc)
          (has "Runtime.ArefL" #'ttl-crc-let) (has "Runtime.Logxor2" #'ttl-crc-let)
          (has "Runtime.ArefL" #'ttl-crc-let*) (has "Runtime.Logxor2" #'ttl-crc-let*)))
  (nil nil nil nil nil nil))
