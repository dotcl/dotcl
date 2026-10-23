;;; A statement that only stores an (unsigned-byte 64) word into a local or an
;;; array leaves the values channel untouched, so the body it sits in needs no
;;; MultipleValues.Reset after it, also when it is wrapped in SYMBOL-MACROLET
;;; or reached through a MACROLET place (ironclad's Keccak round).

(setf dotcl:*save-sil* t)

(defmacro %smn-unrolled ((var limit) &body body)
  (loop for x from 0 below limit
        collect `(symbol-macrolet ((,var ,x)) ,@body) into forms
        finally (return `(progn ,@forms))))

(defun %smn-round (v n)
  (declare (type (simple-array (unsigned-byte 64) (4)) v)
           (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (let ((a (aref v 0)) (b (aref v 1)))
    (declare (type (unsigned-byte 64) a b))
    (macrolet ((sa (x &environment e) (if (eql (macroexpand x e) 0) 'a 'b)))
      (dotimes (i n)
        (%smn-unrolled (x 2)
          (setf (sa x) (logxor (sa x) (ldb (byte 64 0) (ash (sa 1) 1)) #x8000000000000001)))))
    (setf (aref v 0) a (aref v 1) b)
    (values a b)))

(defun %smn-progn-value (a b)
  (declare (type (unsigned-byte 64) a b))
  (list (progn (setq a (logxor a b)) (setq b (lognot-u64 a)))
        a b))

(defun lognot-u64 (x) (ldb (byte 64 0) (lognot x)))

(defun %smn-ref (a b n)
  (dotimes (i n (list a b))
    (setq a (logxor a (ldb (byte 64 0) (ash b 1)) #x8000000000000001))
    (setq b (logxor b (ldb (byte 64 0) (ash b 1)) #x8000000000000001))))

(deftest stmt-u64-mv-neutral.values
  (let ((v (make-array 4 :element-type '(unsigned-byte 64)
                         :initial-contents '(#xFFFFFFFFFFFFFFFF 3 0 0))))
    (list (multiple-value-list (%smn-round v 5))
          (coerce v 'list)
          (%smn-ref #xFFFFFFFFFFFFFFFF 3 5)
          (%smn-progn-value #xF0F0F0F0F0F0F0F0 #xFF)))
  ((9223372036854775728 9223372036854775884)
   (9223372036854775728 9223372036854775884 0 0)
   (9223372036854775728 9223372036854775884)
   (1085102592571150320 17361641481138401295 1085102592571150320)))

(defun %smn-two () (values 1 2))

(defun %smn-after-statements (a)
  (declare (type (unsigned-byte 64) a))
  (multiple-value-call #'list
    (progn (%smn-two) (setq a (logxor a 1)) a)))

(deftest stmt-u64-mv-neutral.no-leaked-values
  (%smn-after-statements #xFFFFFFFFFFFFFFFF)
  (18446744073709551614))

(defun %smn-plain (v n)
  (declare (type (simple-array (unsigned-byte 64) (4)) v)
           (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (let ((a (aref v 0)) (b (aref v 1)))
    (declare (type (unsigned-byte 64) a b))
    (dotimes (i n)
      (setq a (logxor a (ldb (byte 64 0) (ash b 1)) #x8000000000000001))
      (setq b (logxor b (ldb (byte 64 0) (ash b 1)) #x8000000000000001)))
    (setf (aref v 0) a (aref v 1) b)
    (values a b)))

;; As many resets as the same round written with plain SETQs (none in the loop).
(deftest-emitting-only stmt-u64-mv-neutral.no-reset-in-round
  (flet ((resets (f) (count '(:call "MultipleValues.Reset") (dotcl:function-sil f)
                            :test #'equal)))
    (- (resets #'%smn-round) (resets #'%smn-plain)))
  0)
