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
