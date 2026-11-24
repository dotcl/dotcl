;;; COND, MACROLET and SYMBOL-MACROLET whose value is discarded compile their bodies for effect: the
;;; last form of a clause (an INCF of a fixnum loop variable) is not boxed for nobody.

(defun %ecf-cond (n)
  (let ((di 0) (a 0))
    (declare (fixnum di a))
    (dotimes (i n)
      (cond ((< (mod i 3) 1) (incf a) (incf di))
            ((< (mod i 3) 2) (incf a 2) (incf di 2))
            (t (incf a 3) (incf di 3))))
    (list di a)))

(defun %ecf-macrolet (n)
  (let ((di 0))
    (declare (fixnum di))
    (dotimes (i n)
      (macrolet ((step1 () '(incf di)))
        (cond ((evenp i) (step1) (step1))
              (t (step1)))))
    di))

(defun %ecf-symbol-macrolet (n)
  (let ((di 0) (v (make-array 3 :initial-element 0)))
    (declare (fixnum di))
    (dotimes (i n)
      (symbol-macrolet ((cell (svref v 1)))
        (cond ((evenp i) (incf cell) (incf di))
              (t (incf cell 2) (incf di 2)))))
    (list di (svref v 1))))

(deftest effect-context-forms.cond-values
  (list (%ecf-cond 10) (%ecf-macrolet 5) (%ecf-symbol-macrolet 4))
  ((19 19) 8 (6 6)))

(deftest effect-context-forms.cond-value-still-returned
  ;; The value of a COND in a value position is untouched, including a test-only clause.
  (list (let ((x 5)) (cond ((> x 9) :big) ((> x 3)) (t :small)))
        (let ((x 1)) (cond ((> x 9) :big) ((> x 3)) (t :small)))
        (let ((x 0)) (cond ((> x 9) :big)))
        (let ((x 0)) (cond ((> x 9) :big) (t)))
        (let ((c 0)) (cond ((incf c) (incf c 10) (incf c 100)) (t :no))))
  (t :small nil t 111))

(deftest effect-context-forms.cond-in-a-statement-keeps-the-order
  (let ((log '()))
    (dolist (x '(1 5 20))
      (cond ((< x 3) (push :small log) (push x log))
            ((< x 10) (push :mid log))
            (t (push :big log) (push x log) nil)))
    (nreverse log))
  (:small 1 :mid :big 20))

(deftest-compiled-only effect-context-forms.cond-body-allocates-nothing
  (progn (%ecf-cond 100) (%ecf-macrolet 100)
         (list (< (min-allocated-bytes (lambda () (%ecf-cond 100000))) 200000)
               (< (min-allocated-bytes (lambda () (%ecf-macrolet 100000))) 200000)))
  (t t))
