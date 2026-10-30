;;; (VALUES X) where its values are read returns exactly one value, the primary
;;; value of X, whatever X itself returned and whatever the channel held before.

(defparameter *vo-h* (let ((h (make-hash-table))) (setf (gethash :a h) 1) h))

(defun %vo-two () (values (values 1 2)))
(defun %vo-none () (values (values)))
(defun %vo-hash (k) (values (gethash k *vo-h*)))
(defun %vo-floor (n) (values (floor n 2)))
;; The channel holds two values whose first is the value returned.
(defun %vo-stale (x) (values-list (list x 2)) (values x))
(defun %vo-stale-call (x) (multiple-value-call #'list (values x 2)) (values x))
(defun %vo-tail (x) (%vo-stale x))
(defun %vo-arg (x) (values (+ x 1)))

(deftest values-one-alone.multiple-value-list
  (list (multiple-value-list (%vo-two))
        (multiple-value-list (%vo-none))
        (multiple-value-list (%vo-hash :a))
        (multiple-value-list (%vo-hash :z))
        (multiple-value-list (%vo-floor 7))
        (multiple-value-list (%vo-stale 5))
        (multiple-value-list (%vo-stale-call 5))
        (multiple-value-list (%vo-tail 5))
        (multiple-value-list (%vo-arg 5)))
  ((1) (nil) (1) (nil) (3) (5) (5) (5) (6)))

(deftest values-one-alone.multiple-value-bind
  (list (multiple-value-bind (a b) (%vo-two) (list a b))
        (multiple-value-bind (a b) (%vo-stale 5) (list a b))
        (multiple-value-bind (a b) (funcall #'%vo-stale 5) (list a b))
        (multiple-value-bind (a b) (apply #'%vo-floor '(7)) (list a b)))
  ((1 nil) (5 nil) (5 nil) (3 nil)))

(deftest values-one-alone.readers
  (list (nth-value 1 (%vo-stale 5))
        (multiple-value-call #'list (%vo-stale 5) (%vo-two))
        (multiple-value-list (funcall (lambda (x) (values x)) (values 7 8))))
  (nil (5 1) (7)))
