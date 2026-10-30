;;; Shapes drawn by the widened random form generator: fill-pointer vectors,
;;; SVREF, and recursive LABELS entered through FUNCALL with a non-local exit.

(defun %rfws-fill-pointer ()
  (let ((v (make-array 2 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (declare (type (vector (unsigned-byte 8)) v))
    (dotimes (i 4) (vector-push-extend (* i 60) v))
    (list (fill-pointer v) (length v) (aref v 3))))

(defun %rfws-svref ()
  (let ((v (make-array 3 :initial-element 1)))
    (declare (type simple-vector v))
    (setf (svref v 1) 7)
    (+ (length v) (svref v 0) (svref v 2))))

(defun %rfws-recursive (x y z)
  (block out
    (labels ((g (n acc)
               (declare (type (integer 0 100) n) (type fixnum acc))
               (if (<= n 0)
                   (values acc n)
                   (progn
                     (when (and (> n 2) (> x y))
                       (return-from out (+ acc z)))
                     (funcall #'g (1- n) (logand (+ acc 31 z) 2305843009213693951)))))
             (rec (n)
               (declare (type (integer 0 100) n))
               (if (<= n 0) 5 (funcall #'rec (1- n)))))
      (multiple-value-bind (a b) (funcall #'g (mod x 6) (mod y 3))
        (+ a (or b 0) (funcall #'rec (mod z 4)))))))

(defun %rfws-typed (n)
  (let ((d (float n 1d0)))
    (declare (type double-float d))
    (+ (the fixnum (+ n 1))
       (nth-value 0 (floor (the double-float (* d 3)) 2)))))

(deftest random-forms-widened-shapes
  (list (%rfws-fill-pointer)
        (%rfws-svref)
        (%rfws-recursive 3 1 10)
        (%rfws-recursive 1 3 10)
        (%rfws-typed 5))
  ((4 4 180) 5 11 46 13))
