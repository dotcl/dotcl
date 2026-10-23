;;; A typed function that calls nothing but checked arithmetic is entered by
;;; typed callers without linking a frame; the frame is linked only when one of
;;; those operations signals. A handler must still see the function, with its
;;; arguments, on top of the backtrace, and the frame list must be right after.

(declaim (notinline %tle-add %tle-sub %tle-mul %tle-floor %tle-truncate %tle-mod %tle-rem %tle-4))
(declaim (ftype (function (fixnum fixnum) fixnum)
                %tle-add %tle-sub %tle-mul %tle-floor %tle-truncate %tle-mod %tle-rem))
(defun %tle-add (a b) (+ a b))
(defun %tle-sub (a b) (- a b))
(defun %tle-mul (a b) (* a b))
(defun %tle-floor (a b) (values (floor a b)))
(defun %tle-truncate (a b) (values (truncate a b)))
(defun %tle-mod (a b) (mod a b))
(defun %tle-rem (a b) (rem a b))
(declaim (ftype (function (double-float fixnum fixnum fixnum) fixnum) %tle-4))
(defun %tle-4 (d a b c) (declare (ignore d)) (if (> a b) (- a c) (+ b c)))

(defun %tle-top-frame (thunk)
  (block out
    (handler-bind ((error (lambda (c)
                            (return-from out
                              (let ((s (with-output-to-string (o) (dotcl:print-backtrace o))))
                                (list (type-of c)
                                      (string-trim " " (subseq s (1+ (position #\: s))
                                                               (position #\Newline s)))))))))
      (funcall thunk))))

(defun %tle-depth () (length (dotcl:backtrace)))

(defun %tle-c-add (a b) (declare (fixnum a b)) (list (%tle-add a b)))
(defun %tle-c-sub (a b) (declare (fixnum a b)) (list (%tle-sub a b)))
(defun %tle-c-mul (a b) (declare (fixnum a b)) (list (%tle-mul a b)))
(defun %tle-c-floor (a b) (declare (fixnum a b)) (list (%tle-floor a b)))
(defun %tle-c-truncate (a b) (declare (fixnum a b)) (list (%tle-truncate a b)))
(defun %tle-c-mod (a b) (declare (fixnum a b)) (list (%tle-mod a b)))
(defun %tle-c-rem (a b) (declare (fixnum a b)) (list (%tle-rem a b)))
(defun %tle-c-4 (d a b c) (declare (double-float d) (fixnum a b c)) (list (%tle-4 d a b c)))

(deftest typed-leaf-entry.values
  (list (%tle-c-add 1 2) (%tle-c-sub 1 2) (%tle-c-mul 3 -4)
        (%tle-c-floor -7 2) (%tle-c-floor 7 -1) (%tle-c-truncate -7 2) (%tle-c-truncate 7 -1)
        (%tle-c-mod -7 2) (%tle-c-mod 7 -1) (%tle-c-rem -7 2) (%tle-c-rem 7 -1)
        (%tle-c-4 1d0 3 2 10) (%tle-c-4 1d0 2 3 10))
  ((3) (-1) (-12) (-4) (-7) (-3) (-7) (1) (0) (-1) (0) (-7) (13)))

(deftest-emitting-only typed-leaf-entry.backtrace-on-signal
  (let ((base (%tle-depth)))
    (list (%tle-top-frame (lambda () (%tle-c-add most-positive-fixnum 1)))
          (%tle-top-frame (lambda () (%tle-c-sub most-negative-fixnum 1)))
          (%tle-top-frame (lambda () (%tle-c-mul most-positive-fixnum 2)))
          (%tle-top-frame (lambda () (%tle-c-floor 5 0)))
          (%tle-top-frame (lambda () (%tle-c-truncate 5 0)))
          (%tle-top-frame (lambda () (%tle-c-mod 5 0)))
          (%tle-top-frame (lambda () (%tle-c-rem 5 0)))
          (%tle-top-frame (lambda () (%tle-c-4 1.5d0 3 2 most-negative-fixnum)))
          (- (%tle-depth) base)))
  ((type-error "(%TLE-ADD 9223372036854775807 1)")
   (type-error "(%TLE-SUB -9223372036854775808 1)")
   (type-error "(%TLE-MUL 9223372036854775807 2)")
   (division-by-zero "(%TLE-FLOOR 5 0)")
   (division-by-zero "(%TLE-TRUNCATE 5 0)")
   (division-by-zero "(%TLE-MOD 5 0)")
   (division-by-zero "(%TLE-REM 5 0)")
   (type-error "(%TLE-4 1.5d0 3 2 -9223372036854775808)")
   0))

;;; Range checks of declared integer types (CheckLongRange) and MIN / MAX keep a
;;; body a leaf; a failing check links the frame the same way.
(declaim (notinline %tle-u8r %tle-clamp))
(declaim (ftype (function (fixnum) (unsigned-byte 8)) %tle-u8r))
(defun %tle-u8r (x) (logand x 511))
(declaim (ftype (function ((unsigned-byte 32)) (unsigned-byte 8)) %tle-red))
(defun %tle-red (c) (ldb (byte 8 16) c))
(declaim (ftype (function (fixnum fixnum fixnum) fixnum) %tle-clamp))
(defun %tle-clamp (x lo hi) (max (min x hi) lo))
(defun %tle-c-u8r (x) (declare (fixnum x)) (list (%tle-u8r x)))
(defun %tle-c-red (x) (declare (fixnum x)) (list (%tle-red x)))
(defun %tle-c-clamp (x) (declare (fixnum x)) (list (%tle-clamp x 0 10)))

(deftest typed-leaf-entry.range-and-min-max
  (list (%tle-c-u8r 200) (%tle-c-red #xff123456) (%tle-c-clamp -5) (%tle-c-clamp 5) (%tle-c-clamp 50)
        (handler-case (%tle-c-red -1) (type-error () :type-error)))
  ((200) (#x12) (0) (5) (10) :type-error))

(deftest-emitting-only typed-leaf-entry.range-backtrace-on-signal
  (let ((base (%tle-depth)))
    (list (%tle-top-frame (lambda () (%tle-c-u8r 300)))
          (- (%tle-depth) base)))
  ((type-error "(%TLE-U8R 300)") 0))
