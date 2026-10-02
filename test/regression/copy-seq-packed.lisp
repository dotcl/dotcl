;;; COPY-SEQ of a vector that owns packed storage (bit, integer, float,
;;; character) copies the storage, and EQUAL of two packed bit vectors compares
;;; a word at a time. These pin the edges: a fill pointer that ends inside a
;;; word (bits past it are not copied or compared), the result being a fresh
;;; simple vector of the same element type, and general vectors.

(deftest copy-seq-packed.bit-fill-pointer
  (let* ((a (make-array 130 :element-type 'bit :fill-pointer 70 :initial-element 1))
         (c (copy-seq a)))
    (list (length c) (count 1 c) (typep c 'simple-bit-vector)
          (equal c (make-array 70 :element-type 'bit :initial-element 1))
          (progn (setf (fill-pointer a) 130) (count 1 a))))
  (70 70 t t 130))

(deftest copy-seq-packed.bit-equal
  (let ((a (make-array 200 :element-type 'bit))
        (b (make-array 200 :element-type 'bit)))
    (setf (sbit a 199) 1)
    (list (equal a b)
          (progn (setf (sbit b 199) 1) (equal a b))
          (progn (setf (sbit b 3) 1) (equal a b))
          (equal (make-array 65 :element-type 'bit :fill-pointer 64 :initial-element 1)
                 (make-array 64 :element-type 'bit :initial-element 1))))
  (nil t nil t))

(deftest copy-seq-packed.fresh-and-typed
  (let* ((u (make-array 5 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3 4 5) :fill-pointer 3))
         (cu (copy-seq u))
         (d (make-array 3 :element-type 'double-float :initial-element 1.5d0))
         (cd (copy-seq d))
         (g (make-array 4 :initial-contents '(a b c d) :fill-pointer 2))
         (cg (copy-seq g)))
    (setf (aref cu 0) 9 (aref cd 0) 0d0 (aref cg 0) 'z)
    (list (coerce cu 'list) (aref u 0) (typep cu '(simple-array (unsigned-byte 8) (3)))
          (aref cd 0) (aref d 0) (array-element-type cd)
          (coerce cg 'list) (aref g 0) (simple-vector-p cg)))
  ((9 2 3) 1 t 0d0 1.5d0 double-float (z b) a t))
