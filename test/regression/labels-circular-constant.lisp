;;; LABELS whose body holds a circular constant.
;;;
;;; The LABELS path asks whether a definition ever names itself, so that one
;;; that does not can take the cheaper FLET route. That walk descended into
;;; quoted constants, where a symbol names nothing anyway -- and a literal read
;;; with #n= is circular, so the walk never came back. Defining a fiveam test
;;; whose body compared a circular literal was enough to blow the stack, which
;;; is what kept Eclector's test suite from loading.

(defun lcc-circular ()
  ;; #1=(A #1#) as data, built rather than read, so this file stays readable
  ;; under any reader.
  (let ((cell (list 'a nil)))
    (setf (second cell) cell)
    cell))

;;; The shape that used to hang: a circular literal inside a LABELS body.
(deftest lcc-labels-with-circular-constant
  (labels ((f () '(a #1=(b #1# c) d)))
    (let ((result (f)))
      (and (eq (first result) 'a)
           (eq (third result) 'd)
           ;; the inner list refers to itself
           (eq (second (second result)) (second result))
           t)))
  t)

(deftest lcc-labels-with-self-referential-constant
  (labels ((f () '#1=(a #1#)))
    (let ((result (f)))
      (eq (second result) result)))
  t)

;;; FLET and LAMBDA always worked; they are here so a future change that breaks
;;; them is not mistaken for this one.
(deftest lcc-flet-with-circular-constant
  (flet ((f () '#1=(a #1#)))
    (let ((result (f))) (eq (second result) result)))
  t)

(deftest lcc-lambda-with-circular-constant
  (let ((result (funcall (lambda () '#1=(a #1#)))))
    (eq (second result) result))
  t)

;;; The walk still has to do its real job. A self-recursive LABELS function
;;; keeps working...
(deftest lcc-labels-self-recursion-still-works
  (labels ((countdown (n) (if (> n 0) (countdown (1- n)) :done)))
    (countdown 5))
  :done)

;;; ...and mutual recursion too.
(deftest lcc-labels-mutual-recursion-still-works
  (labels ((evenp* (n) (if (zerop n) t (oddp* (1- n))))
           (oddp* (n) (if (zerop n) nil (evenp* (1- n)))))
    (list (evenp* 4) (oddp* 4)))
  (t nil))

;;; A symbol inside a quoted constant is data, not a reference to the function
;;; of the same name: the constant comes back as written.
(deftest lcc-name-inside-quoted-constant-is-data
  (labels ((f () '(f g)))
    (f))
  (f g))

;;; RETURN-FROM naming the function names its implicit block, and that still
;;; does not count as using the function as a value.
(deftest lcc-return-from-still-allowed
  (labels ((f (n) (when (> n 0) (return-from f :early)) :late))
    (list (f 1) (f 0)))
  (:early :late))

;;; A circular constant reached through a deeper form, which is how it arrived
;;; from fiveam: (labels ((f () (list 'x '(a #1=(b #1#))))) ...)
(deftest lcc-circular-constant-nested-in-a-call
  (labels ((f () (list 'x '(a #1=(b #1#) c))))
    (let* ((result (f))
           (inner (second (second result))))
      (eq (second inner) inner)))
  t)
