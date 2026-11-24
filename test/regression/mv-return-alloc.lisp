;;; A function that returns 3 to 8 values, bound by MULTIPLE-VALUE-BIND, read by NTH-VALUE or called for
;;; its primary value, builds no array and no MvReturn: the values go to the receiver's own buffer.

(defun %mra-v3 (a) (declare (fixnum a)) (values a (1+ a) (+ a 2)))
(defun %mra-v4 (a) (declare (fixnum a)) (values a (1+ a) (+ a 2) (+ a 3)))
(defun %mra-v6 (a) (declare (fixnum a)) (values a (1+ a) (+ a 2) (+ a 3) (+ a 4) (+ a 5)))
(defun %mra-v8 (a) (declare (fixnum a)) (values a (1+ a) (+ a 2) (+ a 3) (+ a 4) (+ a 5) (+ a 6) (+ a 7)))
(declaim (notinline %mra-v3 %mra-v4 %mra-v6 %mra-v8))
(defun %mra-wrap4 (a) (%mra-v4 a))
(declaim (notinline %mra-wrap4))

(defun %mra-mvb3 (n) (declare (fixnum n)) (let ((k 0)) (declare (fixnum k)) (dotimes (i n) (multiple-value-bind (a b c) (%mra-v3 (mod i 100)) (declare (fixnum a b c)) (incf k (the fixnum (+ a b c))))) k))
(defun %mra-mvb4 (n) (declare (fixnum n)) (let ((k 0)) (declare (fixnum k)) (dotimes (i n) (multiple-value-bind (a b c d) (%mra-v4 (mod i 100)) (declare (fixnum a b c d)) (incf k (the fixnum (+ a d))))) k))
(defun %mra-mvb6 (n) (declare (fixnum n)) (let ((k 0)) (declare (fixnum k)) (dotimes (i n) (multiple-value-bind (a b c d e f) (%mra-v6 (mod i 100)) (declare (fixnum a b c d e f)) (incf k (the fixnum (+ a f))))) k))
(defun %mra-mvb8 (n) (declare (fixnum n)) (let ((k 0)) (declare (fixnum k)) (dotimes (i n) (multiple-value-bind (a b c d e f g h) (%mra-v8 (mod i 100)) (declare (fixnum a b c d e f g h)) (incf k (the fixnum (+ a h))))) k))
(defun %mra-nth (n) (declare (fixnum n)) (let ((k 0)) (declare (fixnum k)) (dotimes (i n) (incf k (the fixnum (nth-value 3 (%mra-v4 (mod i 100)))))) k))
(defun %mra-primary (n) (declare (fixnum n)) (let ((k 0)) (declare (fixnum k)) (dotimes (i n) (incf k (the fixnum (%mra-v6 (mod i 100))))) k))
(defun %mra-tail (n) (declare (fixnum n)) (let ((k 0)) (declare (fixnum k)) (dotimes (i n) (multiple-value-bind (a b c d) (%mra-wrap4 (mod i 100)) (declare (fixnum a b c d)) (incf k (the fixnum (+ b d))))) k))

(deftest mv-return-alloc.values
  (list (%mra-mvb3 1000) (%mra-mvb4 1000) (%mra-mvb6 1000) (%mra-mvb8 1000) (%mra-nth 1000) (%mra-primary 1000) (%mra-tail 1000))
  (#.(loop for i below 1000 sum (let ((a (mod i 100))) (+ a (1+ a) (+ a 2))))
   #.(loop for i below 1000 sum (let ((a (mod i 100))) (+ a (+ a 3))))
   #.(loop for i below 1000 sum (let ((a (mod i 100))) (+ a (+ a 5))))
   #.(loop for i below 1000 sum (let ((a (mod i 100))) (+ a (+ a 7))))
   #.(loop for i below 1000 sum (let ((a (mod i 100))) (+ a 3)))
   #.(loop for i below 1000 sum (mod i 100))
   #.(loop for i below 1000 sum (let ((a (mod i 100))) (+ (1+ a) (+ a 3))))))

(deftest-compiled-only mv-return-alloc.no-allocation
  (progn (%mra-mvb3 100) (%mra-mvb4 100) (%mra-mvb6 100) (%mra-mvb8 100) (%mra-nth 100) (%mra-primary 100) (%mra-tail 100)
         (flet ((small (f) (< (min-allocated-bytes (lambda () (funcall f 100000))) 100000)))
           (list (small #'%mra-mvb3) (small #'%mra-mvb4) (small #'%mra-mvb6) (small #'%mra-mvb8)
                 (small #'%mra-nth) (small #'%mra-primary) (small #'%mra-tail))))
  (t t t t t t t))
