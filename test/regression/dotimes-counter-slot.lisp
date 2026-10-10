;;; DOTIMES counter representation.
;;;
;;; The counter is declared a fixnum whatever the count form is, so it lives in
;;; a raw Int64 slot and costs no allocation per iteration. The count form is
;;; NOT restricted to fixnums by that: the loop test compares the raw counter
;;; against the boxed limit. These pin the semantics that has to survive.

(deftest dotimes-bignum-count-early-return
  (dotimes (i (expt 2 100))
    (when (= i 3) (return (list :early i))))
  (:early 3))

(deftest dotimes-bignum-count-value-type
  (dotimes (i (expt 2 100))
    (when (= i 2) (return (typep i 'fixnum))))
  t)

(deftest dotimes-float-count
  (let (r) (dotimes (i 3.5 (nreverse r)) (push i r)))
  (0 1 2 3))

(deftest dotimes-non-number-count
  (handler-case (dotimes (i :not-a-number) nil)
    (error () :error))
  :error)

(deftest dotimes-variable-count
  (let ((n 4) (r nil)) (dotimes (i n (nreverse r)) (push i r)))
  (0 1 2 3))

(deftest dotimes-zero-and-negative
  (list (dotimes (i 0 :zero)) (dotimes (i -5 :neg)))
  (:zero :neg))

(deftest dotimes-body-setq-of-counter
  (let (r) (dotimes (i 4) (push i r) (when (= i 1) (setq i 2))) (nreverse r))
  (0 1 3))

(deftest dotimes-counter-captured-by-closure
  (let (fs)
    (dotimes (i 3) (push (let ((j i)) (lambda () j)) fs))
    (mapcar #'funcall (nreverse fs)))
  (0 1 2))

(deftest dotimes-nested-variable-counts
  (let ((n 0) (a 3) (b 4))
    (dotimes (i a) (dotimes (j b) (incf n)))
    n)
  12)

;;; A count form whose name is rebound inside a DEFUN with a FIXNUM-declared
;;; parameter of that name. The expansion must follow the inner binding: the
;;; limit holds a bignum there, and treating it as a fixnum failed the unbox.
(defun dotimes-shadowed-param-let (n)
  (declare (fixnum n))
  (let ((n (expt 2 70)) (c 0))
    (dotimes (i n) (incf c) (when (> c 4) (return)))
    c))

(deftest dotimes-shadowed-fixnum-param-let
  (dotimes-shadowed-param-let 1)
  5)

(defun dotimes-shadowed-param-negative (n)
  (declare (fixnum n))
  (let ((n (- (expt 2 70))) (c 0))
    (dotimes (i n) (incf c))
    c))

(deftest dotimes-shadowed-fixnum-param-negative
  (dotimes-shadowed-param-negative 1)
  0)

(defun dotimes-shadowed-param-flet (n)
  (declare (fixnum n))
  (flet ((count-up (n)
           (let ((c 0)) (dotimes (i n) (incf c) (when (> c 4) (return))) c)))
    (list (count-up n) (count-up (expt 2 70)))))

(deftest dotimes-shadowed-fixnum-param-flet
  (dotimes-shadowed-param-flet 3)
  (3 5))

;;; The same DOTIMES form placed once where N is the FIXNUM parameter and once
;;; where it is rebound to a bignum.
(defmacro dotimes-twice-in-defun (name &body body)
  `(defun ,name (n)
     (declare (fixnum n))
     (list (let ((c 0)) ,@body c)
           (let ((n (expt 2 70)) (c 0)) ,@body c))))

(dotimes-twice-in-defun dotimes-same-form-two-bindings
  (dotimes (i n) (incf c) (when (> c 4) (return))))

(deftest dotimes-same-form-fixnum-and-bignum-binding
  (dotimes-same-form-two-bindings 3)
  (3 5))
