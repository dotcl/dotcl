;;; A structure slot kept unboxed (a FIXNUM, DOUBLE-FLOAT, SINGLE-FLOAT or (UNSIGNED-BYTE 64) slot)
;;; read or written through the generic accessor path (the structure not known at compile time,
;;; the value not known to be a fixnum) gives the values and errors the boxed slots give.

(defstruct (srs-a (:conc-name srs-a-)) (n 0 :type fixnum) (d 0d0 :type double-float)
  (f 0f0 :type single-float) (u 0 :type (unsigned-byte 64)) (any nil))
(defstruct (srs-b (:include srs-a) (:conc-name srs-b-)) (m 5 :type fixnum))

(defun %srs-read-n (s) (srs-a-n s))
(defun %srs-read-d (s) (srs-a-d s))
(defun %srs-read-f (s) (srs-a-f s))
(defun %srs-read-u (s) (srs-a-u s))
(defun %srs-write-n (s v) (setf (srs-a-n s) v))
(defun %srs-write-d (s v) (setf (srs-a-d s) v))
(defun %srs-write-u (s v) (setf (srs-a-u s) v))
(defun %srs-incf-n (s) (incf (srs-a-n s)))

(deftest struct-raw-slot.reads
  (let ((s (make-srs-a :n 42 :d 2.5d0 :f 1.5f0 :u 18446744073709551615 :any :x))
        (b (make-srs-b :n -7 :m 9)))
    (list (%srs-read-n s) (%srs-read-d s) (%srs-read-f s) (%srs-read-u s)
          (%srs-read-n b) (srs-b-m b) (srs-a-any s)
          (%srs-read-n (make-srs-a :n most-positive-fixnum))
          (%srs-read-n (make-srs-a :n most-negative-fixnum))
          (%srs-read-n (make-srs-a :n 100000))))
  (42 2.5d0 1.5f0 18446744073709551615 -7 9 :x #.most-positive-fixnum #.most-negative-fixnum 100000))

(deftest struct-raw-slot.writes-and-incf
  (let ((s (make-srs-a)) (b (make-srs-b)))
    (%srs-write-n s 7) (%srs-write-d s 0.25d0) (%srs-write-u s 123456789012345678)
    (%srs-write-n b 70000) (%srs-incf-n s) (%srs-incf-n s) (%srs-incf-n b)
    (list (srs-a-n s) (srs-a-d s) (srs-a-u s) (srs-a-n b)
          (%srs-write-n s most-positive-fixnum) (srs-a-n s)))
  (9 0.25d0 123456789012345678 70001 #.most-positive-fixnum #.most-positive-fixnum))

(deftest struct-raw-slot.type-errors
  (let ((s (make-srs-a :n 1 :d 1d0)))
    (list (handler-case (%srs-write-n s "x") (type-error () :type-error))
          (handler-case (%srs-write-n s 1.5) (type-error () :type-error))
          (handler-case (%srs-write-n s (1+ most-positive-fixnum)) (type-error () :type-error))
          (handler-case (%srs-write-d s 1) (type-error () :type-error))
          (handler-case (%srs-write-u s -1) (type-error () :type-error))
          (handler-case (%srs-read-n 5) (type-error () :type-error))
          (handler-case (%srs-read-n (make-srs-b)) (type-error () :type-error))
          (handler-case (progn (setf (srs-a-n s) most-positive-fixnum) (%srs-incf-n s)) (type-error () :type-error))
          (srs-a-n s)))
  (:type-error :type-error :type-error :type-error :type-error :type-error 0 :type-error #.most-positive-fixnum))

(deftest struct-raw-slot.copy-and-equalp
  (let* ((s (make-srs-a :n 3 :d 1d0)) (c (copy-srs-a s)))
    (incf (srs-a-n c) 4)
    (list (srs-a-n s) (srs-a-n c) (equalp s (copy-srs-a s)) (equalp s c)))
  (3 7 t nil))
