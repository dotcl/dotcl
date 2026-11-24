;;; A store into a general (element-type T) array of any shape keeps the index check and
;;; stores any kind of object; the store skips the runtime's per-store array-type check
;;; but not the bounds check.

(defun %est-fill (v n)
  (declare (fixnum n))
  (dotimes (i n) (setf (aref v i) (case (mod i 6) (0 nil) (1 i) (2 "s") (3 (list i)) (4 #'car) (5 'sym))))
  v)

(deftest element-store.kinds-of-value
  (let ((v (%est-fill (make-array 12) 12)))
    (list (aref v 0) (aref v 1) (aref v 2) (aref v 3) (eq (aref v 4) #'car) (aref v 5)
          (aref v 6) (aref v 7)))
  (nil 1 "s" (3) t sym nil 7))

(deftest element-store.array-shapes
  (let ((adj (make-array 4 :adjustable t :initial-element 0))
        (fp (make-array 6 :fill-pointer 3 :initial-element 0))
        (base (make-array 8 :initial-element 0))
        (m (make-array '(3 4) :initial-element 0))
        (m3 (make-array '(2 2 2) :initial-element 0)))
    (let ((disp (make-array 4 :displaced-to base :displaced-index-offset 2)))
      (setf (aref adj 3) :a (aref fp 2) :f (aref disp 1) :d (aref m 2 3) :m (aref m3 1 1 1) :c
            (svref base 7) :s)
      (list (aref adj 3) (aref fp 2) (aref base 3) (aref disp 1) (aref m 2 3) (aref m3 1 1 1)
            (aref base 7) (aref m 0 0))))
  (:a :f :d :d :m :c :s 0))

(deftest element-store.bounds-still-checked
  (let ((v (make-array 5 :initial-element 0)) (n 5))
    (declare (fixnum n))
    (list (handler-case (setf (aref v n) 1) (error () :error))
          (handler-case (setf (aref v -1) 1) (error () :error))
          (handler-case (setf (svref v 5) 1) (error () :error))
          (handler-case (setf (aref v (1+ n)) 1) (error () :error))
          (handler-case (let ((fp (make-array 8 :fill-pointer 2 :initial-element 0))) (setf (aref fp 7) 1) (aref fp 7)) (error () :error))
          (coerce v 'list)))
  (:error :error :error :error 1 (0 0 0 0 0)))

(deftest element-store.typed-and-other-arrays-unaffected
  (let ((s (make-string 3 :initial-element #\a)) (b (make-array 4 :element-type 'bit :initial-element 0))
        (d (make-array 3 :element-type 'double-float :initial-element 0d0))
        (u (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref s 1) #\z (aref b 2) 1 (aref d 0) 2.5d0 (aref u 2) 200)
    (list s (coerce b 'list) (aref d 0) (aref u 2)
          (handler-case (setf (aref u 0) 300) (error () :error))
          (handler-case (setf (aref b 0) 2) (error () :error))
          (handler-case (setf (aref s 0) 5) (error () :error))))
  ("aza" (0 0 1 0) 2.5d0 200 :error :error :error))
