;;; LOGANDC1, LOGANDC2, LOGORC1, LOGORC2, LOGNAND, LOGNOR and two-argument
;;; LOGEQV are compiled as the LOGAND / LOGIOR / LOGXOR / LOGNOT forms they
;;; mean. Values must match the functions for every kind of integer, in typed
;;; code as well, the first argument is still evaluated first, and a
;;; non-integer is still a TYPE-ERROR.

(defun %lco-inline (a b)
  (list (logandc1 a b) (logandc2 a b) (logorc1 a b) (logorc2 a b)
        (lognand a b) (lognor a b) (logeqv a b)))

(defun %lco-generic (a b)
  (mapcar (lambda (f) (funcall f a b))
          (list #'logandc1 #'logandc2 #'logorc1 #'logorc2 #'lognand #'lognor #'logeqv)))

(defun %lco-u32 (x y z)
  (declare (type (unsigned-byte 32) x y z))
  (ldb (byte 32 0) (logxor y (logorc2 x z))))

(defun %lco-u64 (e g)
  (declare (type (unsigned-byte 64) e g))
  (logandc1 e g))

(deftest logical-complement-ops.matches-function
  (let ((vals (list 0 1 -1 255 #xFFFFFFFF #x80000000 -12345
                    most-positive-fixnum most-negative-fixnum
                    #xFFFFFFFFFFFFFFFF (expt 2 70) (- (expt 3 50)))))
    (loop for a in vals
          nconc (loop for b in vals
                      unless (equal (%lco-inline a b) (%lco-generic a b))
                        collect (list a b))))
  nil)

(deftest logical-complement-ops.typed
  (list (%lco-u32 #xFFFFFFFF 0 0) (%lco-u32 0 #x12345678 #xFFFFFFFF) (%lco-u32 #xF0F0F0F0 1 #x0F0F0F0F)
        (%lco-u64 #xFFFFFFFF00000000 #xFFFFFFFFFFFFFFFF) (%lco-u64 0 #x8000000000000000))
  (#xFFFFFFFF #x12345678 #xF0F0F0F1 #x00000000FFFFFFFF #x8000000000000000))

(deftest logical-complement-ops.evaluation-order
  (let ((log '()))
    (logorc2 (progn (push :a log) 1) (progn (push :b log) 2))
    (lognand (progn (push :c log) 1) (progn (push :d log) 2))
    (nreverse log))
  (:a :b :c :d))

(deftest logical-complement-ops.not-an-integer
  (list (handler-case (progn (logandc1 (the t (identity 1.5)) 3) :no-error)
          (type-error () :type-error))
        (handler-case (progn (logeqv 3 (the t (identity "x"))) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error))
