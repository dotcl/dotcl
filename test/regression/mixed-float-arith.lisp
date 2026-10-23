;;; +, -, * and / of a fixnum and a declared float compute in the float's format
;;; without boxing either operand, at the top of an expression as well as
;;; inside one, and give what the generic operation gives.

(setf dotcl:*save-sil* t)

(defun %mfa-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %mfa-s+ (i f) (declare (fixnum i) (single-float f)) (+ i f))
(defun %mfa-s- (i f) (declare (fixnum i) (single-float f)) (- f i))
(defun %mfa-s* (i f) (declare (fixnum i) (single-float f)) (* i f))
(defun %mfa-s/ (i f) (declare (fixnum i) (single-float f)) (/ i f))
(defun %mfa-s/r (i f) (declare (fixnum i) (single-float f)) (/ f i))
(defun %mfa-d+ (i f) (declare (fixnum i) (double-float f)) (+ f i))
(defun %mfa-d- (i f) (declare (fixnum i) (double-float f)) (- i f))
(defun %mfa-d* (i f) (declare (fixnum i) (double-float f)) (* f i))
(defun %mfa-d/ (i f) (declare (fixnum i) (double-float f)) (/ i f))
(defun %mfa-d-nest (i j f) (declare (fixnum i j) (double-float f)) (+ (* i f) (- j f)))
(defun %mfa-d-let (i f)
  (declare (fixnum i) (double-float f))
  (let ((x (* i f))) (+ x x)))

(defparameter *mfa-fns*
  (list (cons #'%mfa-s+ (lambda (i f) (+ i f))) (cons #'%mfa-s- (lambda (i f) (- f i)))
        (cons #'%mfa-s* (lambda (i f) (* i f))) (cons #'%mfa-s/ (lambda (i f) (/ i f)))
        (cons #'%mfa-s/r (lambda (i f) (/ f i)))
        (cons #'%mfa-d+ (lambda (i f) (+ f i))) (cons #'%mfa-d- (lambda (i f) (- i f)))
        (cons #'%mfa-d* (lambda (i f) (* f i))) (cons #'%mfa-d/ (lambda (i f) (/ i f)))))

(defun %mfa-generic (fn i f)
  ;; The operands reach the arithmetic through untyped variables.
  (funcall fn (identity i) (identity f)))

(deftest-compiled-only mixed-float-arith.native
  (list (mapcar (lambda (p) (%mfa-sil-count (car p) "Runtime.")) *mfa-fns*)
        (%mfa-sil-count #'%mfa-d-nest "Runtime.Add")
        (%mfa-sil-count #'%mfa-d-nest "Runtime.Multiply")
        (%mfa-sil-count #'%mfa-d-let "Runtime.Multiply"))
  ;; Runtime.CheckArg* for the two arguments only.
  ((2 2 2 2 2 2 2 2 2) 0 0 0))

(deftest mixed-float-arith.same-as-generic
  (let ((ints (list 0 1 -1 3 7 (expt 2 24) (1+ (expt 2 24)) (- (1+ (expt 2 24)))
                    (1+ (expt 2 53)) most-positive-fixnum most-negative-fixnum))
        (bad '()))
    (loop for (typed . generic) in *mfa-fns*
          for k from 0
          for floats = (if (< k 5)
                           (list 0.0f0 -0.0f0 1.5f0 -3.25f0 1f-30 3f38 0.1f0)
                           (list 0.0d0 -0.0d0 1.5d0 -3.25d0 1d-300 1d308 0.1d0))
          do (dolist (i ints)
               (dolist (f floats)
                 (let ((a (funcall typed i f))
                       (b (%mfa-generic generic i f)))
                   (unless (and (eql a b) (eq (type-of a) (type-of b)))
                     (push (list k i f a b) bad))))))
    bad)
  nil)

(deftest mixed-float-arith.nested
  (list (%mfa-d-nest 3 4 0.5d0) (%mfa-d-let 3 0.25d0)
        (%mfa-d-nest most-positive-fixnum 0 1d0))
  (5.0d0 1.5d0 #.(+ (* most-positive-fixnum 1d0) (- 0 1d0))))

(deftest mixed-float-arith.division-by-zero
  (list (%mfa-s/ 1 0.0f0) (%mfa-s/ -1 0.0f0) (%mfa-d/ 1 0.0d0) (%mfa-d/ 1 -0.0d0))
  (#.(/ 1 0.0f0) #.(/ -1 0.0f0) #.(/ 1 0.0d0) #.(/ 1 -0.0d0)))
