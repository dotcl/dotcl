;;; VALUES-LIST answers the caller's value mode. Expected values are SBCL's.

(defun %mvl (l) (values-list l))
(defun %mvl-fn (l) (funcall #'values-list l))

(deftest mv-values-list-mode.values
  (loop for l in '(() (1) (1 2) (1 2 3) (1 2 3 4 5) (1 2 3 4 5 6 7 8 9 10))
        collect (list (multiple-value-list (%mvl l))
                      (multiple-value-bind (a b c) (%mvl l) (list a b c))
                      (nth-value 2 (%mvl l))
                      (%mvl l)
                      (let ((x (%mvl l))) x)
                      (multiple-value-list (%mvl-fn l))
                      (multiple-value-bind (a b) (%mvl-fn l) (list a b))
                      (multiple-value-list (apply #'values-list (list l)))))
  ((() (nil nil nil) nil nil nil () (nil nil) ())
   ((1) (1 nil nil) nil 1 1 (1) (1 nil) (1))
   ((1 2) (1 2 nil) nil 1 1 (1 2) (1 2) (1 2))
   ((1 2 3) (1 2 3) 3 1 1 (1 2 3) (1 2) (1 2 3))
   ((1 2 3 4 5) (1 2 3) 3 1 1 (1 2 3 4 5) (1 2) (1 2 3 4 5))
   ((1 2 3 4 5 6 7 8 9 10) (1 2 3) 3 1 1 (1 2 3 4 5 6 7 8 9 10) (1 2) (1 2 3 4 5 6 7 8 9 10))))

(deftest mv-values-list-mode.errors
  (list (handler-case (multiple-value-bind (a b) (%mvl '(1 . 2)) (list a b)) (type-error () :type-error))
        (handler-case (multiple-value-bind (a b) (%mvl 5) (list a b)) (type-error () :type-error))
        (handler-case (%mvl '(1 2 . 3)) (type-error () :type-error)))
  (:type-error :type-error :type-error))

(defun %mvl-loop (n)
  (declare (fixnum n))
  (let ((r nil) (l (list 1 2 3 4)))
    (dotimes (i n r)
      (multiple-value-bind (a b c d) (%mvl l) (setq r (and a b c d)))
      (setq r (nth-value 2 (%mvl l))))))

(deftest-compiled-only mv-values-list-mode.allocates-nothing
  (< (bytes-per-op #'%mvl-loop) 1)
  t)
