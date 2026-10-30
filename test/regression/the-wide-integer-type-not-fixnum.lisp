;;; (THE (INTEGER LO HI) E) was taken as a fixnum result whatever the bounds,
;;; so a product of fixnums under a type wider than int64 was computed with the
;;; fixnum overflow check (a TYPE-ERROR "does not fit the declared FIXNUM type"
;;; from safety 1) or wrapped (safety 0). Expected values are SBCL's.

(defmacro %twit (name expected form &rest args)
  `(deftest-emitting-only ,(intern (format nil "THE-WIDE-INTEGER-TYPE-NOT-FIXNUM.~a" name))
     (let ((*error-output* (make-broadcast-stream)))
       (funcall (compile nil ',form) ,@args))
     ,expected))

(%twit "BOUNDED" -33696413345969625000
  (lambda (c) (declare (fixnum c))
    (the (integer -471516570247932809625 -1) (* 36375 c)))
  -926361878927000)

(%twit "BOUNDED-SAFETY-0" -33696413345969625000
  (lambda (c) (declare (optimize (safety 0)) (fixnum c))
    (the (integer -471516570247932809625 -1) (* 36375 c)))
  -926361878927000)

(%twit "HALF-OPEN" -33696413345969625000
  (lambda (c) (declare (fixnum c)) (the (integer * -1) (* 36375 c)))
  -926361878927000)

(%twit "POSITIVE" 33696413345969625000
  (lambda (c) (declare (fixnum c))
    (the (integer 1 471516570247932809625) (* 36375 c)))
  926361878927000)

(%twit "RANGED-VARIABLE" -32685520945590536250
  (lambda (c) (declare (type (integer 0 133635617032751) c))
    (the (integer -471516570247932809625 -1755689899256625) (* 36375 (* -97 c))))
  9263618789270)

(%twit "REM-OF-UB64" 1
  (lambda (p) (declare (type (unsigned-byte 64) p))
    (the (integer 0 1) (rem (the (integer 0 *) p) 2)))
  18446744073709551615)

(%twit "FIXNUM-STILL-FIXNUM" 10
  (lambda (c) (declare (fixnum c)) (the (integer 0 *) (+ c 7)))
  3)
