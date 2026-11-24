;;; A size, dimension or width written in a type specifier that does not fit in 32 bits does not
;;; wrap onto a small real size: (vector t 4294967299) is not a type of a 3-element vector.

(defun %tsb-typep-all (n)
  (let ((v (vector 1 2 3)) (s (make-string 3 :initial-element #\a)) (b (make-array 3 :element-type 'bit :initial-element 0))
        (m (make-array '(3 3) :initial-element 0)))
    (list (typep v `(vector t ,n)) (typep v `(simple-vector ,n)) (typep s `(string ,n)) (typep s `(simple-string ,n))
          (typep b `(bit-vector ,n)) (typep b `(simple-bit-vector ,n)) (typep v `(array t (,n))) (typep v `(simple-array t (,n)))
          (typep m `(array t (,n 3))) (typep m `(array t (3 ,n))) (typep v `(array t ,n)))))

(deftest type-size-beyond-int.typep-sizes
  (list (%tsb-typep-all 3)
        (%tsb-typep-all (+ (expt 2 32) 3))
        (%tsb-typep-all (+ (expt 2 31) 3))
        (%tsb-typep-all (+ (expt 2 40) 3)))
  ((t t t t t t t t t t nil)
   (nil nil nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil nil nil)))

(deftest type-size-beyond-int.unsigned-byte-width
  (list (typep (expt 2 70) `(unsigned-byte ,(+ (expt 2 32) 3)))
        (typep (expt 2 70) `(unsigned-byte ,(expt 2 40)))
        (typep -1 `(unsigned-byte ,(+ (expt 2 32) 3)))
        (typep 200 `(unsigned-byte ,(+ (expt 2 32) 3)))
        (typep (- (expt 2 70)) `(signed-byte ,(+ (expt 2 32) 3)))
        (typep (expt 2 70) '(unsigned-byte 64))
        (typep (expt 2 70) '(unsigned-byte 71))
        (typep (expt 2 70) '(unsigned-byte 70)))
  (t t nil t t nil t nil))

(deftest type-size-beyond-int.coerce-and-concatenate
  (flet ((len-or-error (f) (handler-case (length (funcall f)) (error () :error))))
    (list (len-or-error (lambda () (coerce (list 1 2 3) '(vector t 3))))
          (len-or-error (lambda () (coerce (list 1 2 3) `(vector t ,(+ (expt 2 32) 3)))))
          (len-or-error (lambda () (coerce (list 1 2 3) `(simple-vector ,(+ (expt 2 40) 3)))))
          (len-or-error (lambda () (concatenate `(vector t ,(+ (expt 2 32) 3)) (list 1 2 3))))
          (len-or-error (lambda () (map `(vector t ,(+ (expt 2 32) 3)) #'identity (list 1 2 3))))
          (len-or-error (lambda () (coerce "abc" `(string ,(+ (expt 2 32) 3)))))
          (len-or-error (lambda () (concatenate '(vector t 3) (list 1 2) (list 3))))))
  (3 :error :error :error :error :error 3))
