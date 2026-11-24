;;; (MULTIPLE-VALUE-BIND (V) FORM BODY) binds V to the first value of FORM, as a LET
;;; does; ROTATEF and SHIFTF, which expand into it, move values between places.

(defvar *mvbs-special* :global)
(defun %mvbs-read-special () *mvbs-special*)

(deftest mvb-single-variable.first-value
  (list (multiple-value-bind (a) (values 1 2 3) a)
        (multiple-value-bind (a) (values) a)
        (multiple-value-bind (a) 5 a)
        (multiple-value-bind (a) (floor 17 5) a)
        (multiple-value-bind (a) (gethash 'nokey (make-hash-table)) a)
        (multiple-value-list (multiple-value-bind (a) (values 1 2) (values a 9 10)))
        (multiple-value-bind (a) (values 1 2) (declare (fixnum a)) (+ a 1))
        (multiple-value-bind (a) (values 3 4) (declare (ignorable a)) :ok)
        (let ((a :outer)) (list (multiple-value-bind (a) (values :inner) a) a))
        (multiple-value-bind (a) (multiple-value-bind (b) (values 7 8) (1+ b)) (* a 2)))
  (1 nil 5 3 nil (1 9 10) 2 :ok (:inner :outer) 16))

(deftest mvb-single-variable.special-and-order
  (list (multiple-value-bind (*mvbs-special*) (values :bound :extra) (%mvbs-read-special))
        *mvbs-special*
        (let ((log nil))
          (multiple-value-bind (a) (progn (push :form log) (values 1 2))
            (push :body log) (list a (reverse log)))))
  (:bound :global (1 (:form :body))))

(deftest mvb-single-variable.closure-capture
  (let (fs)
    (dotimes (i 3) (multiple-value-bind (a) (values i :x) (push (lambda () a) fs)))
    (mapcar #'funcall (reverse fs)))
  (0 1 2))

(deftest mvb-single-variable.rotatef-shiftf
  (let ((v (vector 1 2 3 4)) (a 1) (b 2) (c 3) (l (list 1 2 3)) (n 0))
    (rotatef (aref v 0) (aref v 3))
    (rotatef a b c)
    (rotatef (car l) (cadr l))
    (let ((r (shiftf a b c 99)))
      (list (coerce v 'list) (list a b c) l r
            (rotatef (aref v (incf n)) (aref v (incf n))) n (coerce v 'list)
            (let ((x 1)) (rotatef x) x)
            (let ((p (cons 1 2))) (rotatef (car p) (cdr p)) p))))
  ((4 2 3 1) (3 1 99) (2 1 3) 2 nil 2 (4 3 2 1) 1 (2 . 1)))

(deftest mvb-single-variable.rotatef-evaluates-subforms-once
  (let ((cnt 0) (v (vector :a :b :c)))
    (rotatef (aref v (progn (incf cnt) 0)) (aref v (progn (incf cnt) 2)))
    (list cnt (coerce v 'list)))
  (2 (:c :b :a)))

(deftest mvb-single-variable.more-than-one-variable-still-works
  (list (multiple-value-bind (a b) (values 1 2) (list a b))
        (multiple-value-bind (a b c) (values 1 2) (list a b c))
        (multiple-value-bind () (values 1 2) :none))
  ((1 2) (1 2 nil) :none))
