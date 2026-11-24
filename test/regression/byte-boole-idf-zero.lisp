;;; BYTE takes non-negative integer size and position (TYPE-ERROR otherwise);
;;; BOOLE with an operation outside 0..15 is a TYPE-ERROR; INTEGER-DECODE-FLOAT of a
;;; zero has exponent 0 (as SBCL), the sign still follows the float's sign.

(deftest byte-boole-idf-zero.byte-errors
  (flet ((o (f) (handler-case (funcall f) (type-error () :type-error) (error () :other))))
    (list (o (lambda () (byte -1 2))) (o (lambda () (byte 2 -1)))
          (o (lambda () (byte 'a 1))) (o (lambda () (byte 1 'a)))
          (byte 0 0) (byte 3 4)))
  (:type-error :type-error :type-error :type-error (0 . 0) (3 . 4)))

(deftest byte-boole-idf-zero.boole-op
  (flet ((o (f) (handler-case (funcall f) (type-error () :type-error) (error () :other))))
    (list (o (lambda () (boole 99 1 2))) (o (lambda () (boole -1 1 2))) (boole boole-and 12 10)))
  (:type-error :type-error 8))

(deftest byte-boole-idf-zero.idf
  (list (multiple-value-list (integer-decode-float 0.0))
        (multiple-value-list (integer-decode-float 0.0d0))
        (multiple-value-list (integer-decode-float -0.0))
        (multiple-value-list (integer-decode-float -0.0d0))
        (multiple-value-list (integer-decode-float least-positive-single-float))
        (multiple-value-list (integer-decode-float least-positive-double-float))
        (multiple-value-list (integer-decode-float 1.0d0)))
  ((0 0 1) (0 0 1) (0 0 -1) (0 0 -1) (1 -149 1) (1 -1074 1) (4503599627370496 -52 1)))

(deftest byte-boole-idf-zero.byte-variable-args
  (flet ((o (f) (handler-case (funcall f) (type-error () :type-error))))
    (let ((s -1) (p 2))
      (list (o (lambda () (byte s p))) (o (lambda () (byte p s))) (o (lambda () (byte 3 p))))))
  (:type-error :type-error (3 . 2)))
