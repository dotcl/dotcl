;;; (THE (SIGNED-BYTE 64) E) or (THE FIXNUM E) put into an int64 local lowered
;;; E's operands to int64 too, though only E's value is asserted: a constant
;;; outside int64 stopped the compile ("Expected integer, got ..."), a bignum
;;; operand signalled "Unable to cast object of type 'DotCL.Bignum'". Expected
;;; values are SBCL's.

(defmacro %tibo (name expected form &rest args)
  `(deftest-emitting-only ,(intern (format nil "THE-INT64-OF-BIGNUM-OPERANDS.~a" name))
     (let ((*error-output* (make-broadcast-stream)))
       (funcall (compile nil ',form) ,@args))
     ,expected))

(%tibo "SETQ" 5
  (lambda (x)
    (let ((y 0)) (declare (type (signed-byte 64) y))
      (setq y (the (signed-byte 64) (- x 9223372036854775808)))
      y))
  9223372036854775813)

(%tibo "LET" 5
  (lambda (x)
    (let ((y (the (signed-byte 64) (- x 9223372036854775808))))
      (declare (type (signed-byte 64) y))
      y))
  9223372036854775813)

(%tibo "FIXNUM" 5
  (lambda (x)
    (let ((y 0)) (declare (type fixnum y))
      (setq y (the fixnum (- x 9223372036854775808)))
      y))
  9223372036854775813)

(%tibo "PLUS-NEGATIVE" 4
  (lambda (x)
    (let ((y 0)) (declare (type (signed-byte 64) y))
      (setq y (the (signed-byte 64) (+ x -9223372036854775809)))
      y))
  9223372036854775813)

(%tibo "LDB-OPERAND" 5
  (lambda (v)
    (let ((y 0)) (declare (type (signed-byte 64) y))
      (setf y (the (signed-byte 64)
                   (- (ldb (byte 64 0) (+ v 9223372036854775808)) 9223372036854775808)))
      y))
  5)

(%tibo "FIXNUM-OPERANDS" 9
  (lambda (a b)
    (declare (fixnum a b))
    (let ((y 0)) (declare (fixnum y))
      (setq y (the fixnum (+ a b)))
      y))
  4 5)
