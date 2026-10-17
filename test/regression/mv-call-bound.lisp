;;; MULTIPLE-VALUE-CALL with one values form calls the function on the values
;;; directly. Expected values are SBCL's.

(defun %mcb-n (n) (values-list (subseq '(1 2 3 4 5 6) 0 n)))
(defun %mcb-swap (a b) (values b a))
(defun %mcb-call (f n) (multiple-value-call f (%mcb-n n)))

(deftest mv-call-bound.counts
  (loop for n from 0 to 6 collect (%mcb-call #'list n))
  (() (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5) (1 2 3 4 5 6)))

(deftest mv-call-bound.designators
  (list (multiple-value-call 'list (%mcb-n 3))
        (multiple-value-call #'+ (floor 7 2))
        (multiple-value-call (lambda (a b) (list :l a b)) (values 1 2))
        (multiple-value-call #'list (values))
        (multiple-value-call #'list 5)
        (multiple-value-call #'list (gethash :zz (make-hash-table))))
  ((1 2 3) 4 (:l 1 2) () (5) (nil nil)))

;; The function's own values go to the caller, in each value mode.
(defun %mcb-swapped (a b) (multiple-value-call #'%mcb-swap (values a b)))
(deftest mv-call-bound.callee-values
  (list (multiple-value-list (%mcb-swapped 1 2))
        (multiple-value-bind (x y) (%mcb-swapped 1 2) (list x y))
        (nth-value 1 (%mcb-swapped 1 2))
        (%mcb-swapped 1 2)
        (multiple-value-list (multiple-value-call #'%mcb-swap (values 3 4))))
  ((2 1) (2 1) 1 2 (4 3)))

;; The function form is evaluated first, the values form after it.
(deftest mv-call-bound.order
  (let ((log nil) (f #'list))
    (list (multiple-value-call (progn (push :f log) f)
            (progn (push :a log) (values 1 2)))
          (reverse log)))
  ((1 2) (:f :a)))

(deftest mv-call-bound.errors
  (list (handler-case (multiple-value-call #'%mcb-swap (values 1)) (program-error () :program-error))
        (handler-case (multiple-value-call 'no-such-function-mcb (values 1 2))
          (undefined-function () :undefined))
        (handler-case (multiple-value-call 5 (values 1 2)) (type-error () :type-error)))
  (:program-error :undefined :type-error))

(defun %mcb-3 (a b c) (if c a b))
(defun %mcb-v3 (x) (values x x x))
(defun %mcb-v2 (x) (values x x))
(defun %mcb-loop (n)
  (declare (fixnum n))
  (let ((r nil) (o (list 1)))
    (dotimes (i n r)
      (setq r (multiple-value-call #'%mcb-3 (%mcb-v3 o)))
      (multiple-value-bind (a b) (multiple-value-call #'%mcb-swap (%mcb-v2 o)) (setq r (and a b))))))

(deftest-compiled-only mv-call-bound.allocates-nothing
  (< (bytes-per-op #'%mcb-loop) 1)
  t)
