;;; (FORMAT stream "~D" fixnum) writes the digits of a small integer to a plain stream
;;; without building a string, and the digits are the same as before at the edges of the
;;; cached range.

(defvar *fdsi-stream* (make-string-output-stream))

(defun %fdsi-write (stream n)
  (format stream "~D" n))

(defun %fdsi-loop (stream n)
  (declare (fixnum n))
  (dotimes (i 10000) (format stream "~D" n))
  nil)

(deftest format-d-small-integers.digits
  (list (with-output-to-string (s) (%fdsi-write s 0))
        (with-output-to-string (s) (%fdsi-write s 7))
        (with-output-to-string (s) (%fdsi-write s 1023))
        (with-output-to-string (s) (%fdsi-write s 1024))
        (with-output-to-string (s) (%fdsi-write s -1))
        (with-output-to-string (s) (%fdsi-write s -1024))
        (with-output-to-string (s) (%fdsi-write s most-positive-fixnum))
        (with-output-to-string (s) (%fdsi-write s most-negative-fixnum))
        (with-output-to-string (s) (%fdsi-write s (expt 10 30)))
        (let ((*print-base* 2)) (with-output-to-string (s) (%fdsi-write s 5)))
        (let ((*print-radix* t)) (with-output-to-string (s) (%fdsi-write s 5))))
  ("0" "7" "1023" "1024" "-1" "-1024"
   #.(princ-to-string most-positive-fixnum) #.(princ-to-string most-negative-fixnum)
   "1000000000000000000000000000000" "5" "5"))

(deftest format-d-small-integers.line-state
  (with-output-to-string (s)
    (write-string "ab" s)
    (%fdsi-write s 12)
    (format s "~&x")
    (%fdsi-write s 3))
  "ab12
x3")

(deftest format-d-small-integers.pretty-block
  (let ((*print-pretty* t))
    (format nil "~@<a~:_~D~:>" 42))
  "a42")

(deftest-compiled-only format-d-small-integers.no-allocation
  (let ((stream (make-string-output-stream)))
    (%fdsi-loop stream 7)
    (%fdsi-loop stream 700)
    (list (< (min-allocated-bytes (lambda () (%fdsi-loop stream 7))) (* 10000 16))
          (< (min-allocated-bytes (lambda () (%fdsi-loop stream 700))) (* 10000 16))))
  (t t))
