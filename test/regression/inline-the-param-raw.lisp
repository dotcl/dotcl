;;; An undeclared accumulator passed to an inline function whose parameter is only read as
;;; (THE FIXNUM PARAM) stays a raw integer: no Fixnum is built per iteration.

(declaim (inline %itp-stir))
(defun %itp-stir (a b)
  (the fixnum (logxor (the fixnum (logand (the fixnum a) 1152921504606846975)) (the fixnum b))))

(declaim (inline %itp-inc))
(defun %itp-inc (a) (the fixnum (+ (the fixnum a) 1)))

(defun %itp-hash (n)
  (declare (fixnum n))
  (let ((res 70000))
    (dotimes (i n) (setf res (%itp-stir res (mod i 65536))))
    res))

(defun %itp-count (n)
  (declare (fixnum n))
  (let ((res 70000))
    (dotimes (i n) (setf res (%itp-inc res)))
    res))

(deftest inline-the-param-raw.values
  (list (%itp-count 10) (%itp-hash 3))
  (70010 #.(let ((r 70000)) (dotimes (i 3 r) (setf r (logxor (logand r 1152921504606846975) i))))))

(deftest-compiled-only inline-the-param-raw.no-allocation
  (progn (%itp-hash 10) (%itp-count 10)
         (list (< (min-allocated-bytes (lambda () (%itp-hash 100000))) 100000)
               (< (min-allocated-bytes (lambda () (%itp-count 100000))) 100000)))
  (t t))
