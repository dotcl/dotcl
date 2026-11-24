;;; FILL-POINTER, (SETF FILL-POINTER), ARRAY-HAS-FILL-POINTER-P, ADJUSTABLE-ARRAY-P,
;;; VECTOR-POP, ARRAY-RANK, ARRAY-TOTAL-SIZE and ARRAY-DIMENSION are direct calls of the
;;; runtime, not a lookup of the function by symbol at each use. The answers and the
;;; errors stay the same.

(defun %fpd-fp (v) (fill-pointer v))
(defun %fpd-set (v n) (setf (fill-pointer v) n))
(defun %fpd-pop (v) (vector-pop v))
(defun %fpd-has (v) (array-has-fill-pointer-p v))
(defun %fpd-adj (v) (adjustable-array-p v))
(defun %fpd-rank (v) (array-rank v))
(defun %fpd-size (v) (array-total-size v))
(defun %fpd-dim (v n) (array-dimension v n))

(deftest fill-pointer-direct.fill-pointer
  (let ((v (make-array 10 :fill-pointer 4 :adjustable t :initial-element 0)))
    (list (%fpd-fp v) (%fpd-set v 7) (%fpd-fp v) (length v)
          (handler-case (%fpd-set v 11) (error () :error))
          (handler-case (%fpd-set v -1) (error () :error))
          (handler-case (%fpd-fp (make-array 3)) (type-error () :type-error))
          (handler-case (%fpd-fp '(1 2)) (type-error () :type-error))
          (%fpd-fp (make-array 5 :fill-pointer t))
          (%fpd-fp (make-array 5 :fill-pointer 0))
          (progn (setf (fill-pointer v) 10) (%fpd-fp v))))
  (4 7 7 7 :error :error :type-error :type-error 5 0 10))

(deftest fill-pointer-direct.vector-pop
  (let ((v (make-array 5 :fill-pointer 0 :adjustable t)))
    (vector-push 1 v) (vector-push-extend 2 v) (vector-push-extend 3 v)
    (list (%fpd-pop v) (%fpd-pop v) (%fpd-fp v) (%fpd-pop v)
          (handler-case (%fpd-pop v) (error () :error))
          (handler-case (%fpd-pop (make-array 3)) (type-error () :type-error))
          (handler-case (%fpd-pop '(1)) (type-error () :type-error))
          (handler-case (%fpd-pop (make-array 3 :element-type 'bit)) (type-error () :type-error))
          (let ((s (make-array 3 :element-type 'character :fill-pointer 2 :initial-contents "abc")))
            (list (%fpd-pop s) (%fpd-fp s)))
          (funcall #'vector-pop (make-array 2 :fill-pointer 2 :initial-contents '(8 9)))
          (mapcar #'vector-pop (list (make-array 1 :fill-pointer 1 :initial-element :x)))))
  (3 2 1 1 :error :type-error :type-error :type-error (#\b 1) 9 (:x)))

(deftest fill-pointer-direct.predicates-and-sizes
  (let ((v (make-array 10 :fill-pointer 4 :adjustable t)) (m (make-array '(2 3))) (s (make-array 5)))
    (list (%fpd-has v) (%fpd-has m) (%fpd-has s) (%fpd-has "abc") (%fpd-has (make-array 3 :fill-pointer t))
          (%fpd-adj v) (%fpd-adj m) (%fpd-adj s) (%fpd-adj "abc")
          (%fpd-rank v) (%fpd-rank m) (%fpd-rank "abc")
          (%fpd-size v) (%fpd-size m) (%fpd-size "abc") (%fpd-size (make-array 0))
          (%fpd-dim v 0) (%fpd-dim m 0) (%fpd-dim m 1) (%fpd-dim "abc" 0)
          (handler-case (%fpd-dim m 2) (error () :error))
          (handler-case (%fpd-dim m -1) (error () :error))
          (handler-case (%fpd-dim m 1.0) (type-error () :type-error))
          (handler-case (%fpd-dim m (expt 10 30)) (error () :error))
          (handler-case (%fpd-dim 5 0) (type-error () :type-error))
          (handler-case (%fpd-has 5) (type-error () :type-error))
          (handler-case (%fpd-rank 5) (type-error () :type-error))
          (handler-case (%fpd-size 'a) (type-error () :type-error))))
  (t nil nil nil t  t nil nil nil  1 2 1  10 6 3 0  10 2 3 3  :error :error :type-error :error :type-error
   :type-error :type-error :type-error))

(deftest fill-pointer-direct.local-function-shadows
  (flet ((fill-pointer (x) (list :mine x)))
    (declare (notinline fill-pointer))
    (fill-pointer 3))
  (:mine 3))
