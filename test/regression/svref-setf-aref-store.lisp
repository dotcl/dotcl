;;; (SETF (SVREF V I) X) is the store (SETF AREF) makes: a subscript known to be a fixnum is not
;;; built into a Fixnum for it. Same values, same order of evaluation, same errors.

(deftest svref-setf.values-and-return
  (let ((v (make-array 6 :initial-element 0)) (r nil))
    (setf r (setf (svref v 2) :two))
    (setf (svref v 0) "s" (svref v 5) (list 1 2))
    (list r (svref v 0) (svref v 2) (svref v 5) (svref v 1)))
  (:two "s" :two (1 2) 0))

(deftest svref-setf.evaluation-order
  (let ((v (make-array 5 :initial-element nil)) (i 0) (log nil))
    (setf (svref v (progn (push :index log) (incf i))) (progn (push :value log) (incf i 10)))
    (list (reverse log) i (coerce v 'list)))
  ((:index :value) 11 (nil 11 nil nil nil)))

(deftest svref-setf.read-modify-write
  (let ((v (make-array 4 :initial-element 0)))
    (incf (svref v 1) 5) (decf (svref v 1)) (push :x (svref v 2)) (setf (svref v 3) (cons 1 (svref v 2)))
    (rotatef (svref v 0) (svref v 1))
    (coerce v 'list))
  (4 0 (:x . 0) (1 :x . 0)))

(deftest svref-setf.typed-index-and-bounds
  (let ((v (make-array 5 :initial-element 0)) (n 5))
    (declare (fixnum n))
    (dotimes (i n) (setf (svref v i) (* i i)))
    (list (coerce v 'list)
          (handler-case (setf (svref v n) 1) (error () :error))
          (handler-case (setf (svref v -1) 1) (error () :error))
          (handler-case (setf (svref v 1.5) 1) (error () :error))
          (handler-case (setf (svref v (expt 10 30)) 1) (error () :error))
          (coerce v 'list)))
  ((0 1 4 9 16) :error :error :error :error (0 1 4 9 16)))

(deftest svref-setf.large-index
  (let ((v (make-array 70000 :initial-element 0)))
    (setf (svref v 69999) :end (svref v 65536) :mid)
    (list (svref v 69999) (svref v 65536) (svref v 65535)))
  (:end :mid 0))
