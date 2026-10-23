;;; A local function declared INLINE whose value is an (unsigned-byte 64) past
;;; fixnum range is not taken as a fixnum by the arithmetic around its call,
;;; and a closure nested two deep still reaches a local INLINE function.

(defun %fiw-inline (a b)
  (flet ((ror (x s) (ldb (byte 64 0) (logior (ash x (- s)) (ash x (- 64 s)))))
         (m+ (p q) (ldb (byte 64 0) (+ p q))))
    (declare (inline ror m+))
    (let ((x (m+ (logxor (ror a 14) (ror a 18) (ror a 41)) b)))
      (list (m+ x (logxor (ror b 28) (ror b 34) (ror b 39))) x))))

(defun %fiw-notinline (a b)
  (flet ((ror (x s) (ldb (byte 64 0) (logior (ash x (- s)) (ash x (- 64 s)))))
         (m+ (p q) (ldb (byte 64 0) (+ p q))))
    (declare (notinline ror m+))
    (let ((x (m+ (logxor (ror a 14) (ror a 18) (ror a 41)) b)))
      (list (m+ x (logxor (ror b 28) (ror b 34) (ror b 39))) x))))

(deftest flet-inline-wide-result.sha512-sigma
  (list (%fiw-inline #xFFFFFFFFFFFFFFFF 3)
        (equal (%fiw-inline #xFFFFFFFFFFFFFFFF 3) (%fiw-notinline #xFFFFFFFFFFFFFFFF 3))
        (equal (%fiw-inline #x6A09E667F3BCC908 #xBB67AE8584CAA73B)
               (%fiw-notinline #x6A09E667F3BCC908 #xBB67AE8584CAA73B)))
  ((209480318978 2) t t))

(defun %fiw-logxor (a)
  (flet ((ror (x) (ldb (byte 64 0) (logior (ash x -14) (ash x 50)))))
    (declare (inline ror))
    (list (logxor (ror a) (ror (ash a -1))) (+ 1 (logand (ror a) (ror a))))))

(deftest flet-inline-wide-result.logxor
  (%fiw-logxor #xFFFFFFFFFFFFFFFF)
  (#x2000000000000 #x10000000000000000))

(defun %fiw-nested (y)
  (flet ((dv () (make-array 2 :element-type 'double-float :initial-element (float y 1d0))))
    (declare (inline dv))
    (list (funcall (lambda () (funcall (lambda () (aref (dv) 0)))))
          (mapcar (lambda (f) (funcall f (lambda () (dv)))) (list #'funcall))
          (funcall (lambda () (funcall (lambda () (let ((y 7)) (declare (ignorable y)) (aref (dv) 1)))))))))

(deftest flet-inline-wide-result.nested-closure
  (let ((r (%fiw-nested 5)))
    (list (first r) (aref (first (second r)) 0) (third r)))
  (5d0 5d0 5d0))
