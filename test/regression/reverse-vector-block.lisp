;;; REVERSE and NREVERSE of a vector that owns its storage reverse the storage as a block.
;;; The result keeps the element type; a fill pointer limits the elements used; displaced and bit
;;; vectors take the element-by-element path. Every form answers T when it is right.

(defun rvb= (a b) (and (equalp a b) (equal (array-element-type a) (array-element-type b))))

(deftest reverse-vector-block.simple-vector
  (list (rvb= (reverse (vector 1 2 3 4 5)) (vector 5 4 3 2 1))
        (rvb= (reverse (vector)) (vector))
        (rvb= (reverse (vector 'a)) (vector 'a))
        (rvb= (reverse (vector 1 2)) (vector 2 1)))
  (t t t t))

(deftest reverse-vector-block.keeps-element-type
  (list (rvb= (reverse (make-array 4 :element-type 'fixnum :initial-contents '(1 2 3 4)))
              (make-array 4 :element-type 'fixnum :initial-contents '(4 3 2 1)))
        (rvb= (reverse (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
              (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(3 2 1)))
        (rvb= (reverse (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))
              (make-array 3 :element-type 'double-float :initial-contents '(3d0 2d0 1d0)))
        (rvb= (reverse (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1)))
              (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1)))
        (string= (reverse "abc") "cba"))
  (t t t t t))

(deftest reverse-vector-block.fresh-and-source-unchanged
  (let* ((v (vector 1 2 3)) (r (reverse v)))
    (list (equalp v (vector 1 2 3)) (equalp r (vector 3 2 1)) (eq v r)))
  (t t nil))

(deftest reverse-vector-block.fill-pointer
  (let ((v (make-array 6 :initial-contents '(1 2 3 4 5 6) :fill-pointer 4)))
    (list (equalp (reverse v) (vector 4 3 2 1)) (length (reverse v))
          (progn (nreverse v) (coerce v 'list))))
  (t 4 (4 3 2 1)))

(deftest reverse-vector-block.displaced
  (let* ((base (vector 1 2 3 4 5 6)) (d (make-array 3 :displaced-to base :displaced-index-offset 1)))
    (list (equalp (reverse d) (vector 4 3 2)) (progn (nreverse d) (coerce base 'list))))
  (t (1 4 3 2 5 6)))

(deftest nreverse-vector-block.in-place
  (let* ((v (vector 1 2 3 4)) (r (nreverse v)))
    (list (eq v r) (coerce v 'list)
          (rvb= (nreverse (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
                (make-array 5 :element-type 'fixnum :initial-contents '(5 4 3 2 1)))
          (equalp (nreverse (make-array 4 :element-type 'bit :initial-contents '(1 0 0 0)))
                  (make-array 4 :element-type 'bit :initial-contents '(0 0 0 1)))
          (equalp (nreverse (vector)) (vector))
          (string= (nreverse (copy-seq "abcd")) "dcba")))
  (t (4 3 2 1) t t t t))

(deftest reverse-vector-block.large
  (let* ((n 100000) (v (make-array n :element-type 'fixnum)))
    (dotimes (i n) (setf (aref v i) i))
    (let ((r (reverse v)))
      (list (aref r 0) (aref r (1- n)) (aref v 0) (every #'= r (nreverse (copy-seq v))))))
  (99999 0 0 t))
