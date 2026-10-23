;;; A LABELS of one function whose body is one call of it, in tail position,
;;; runs the function as a loop in the enclosing method: no closure is made.
;;; Whatever that cannot express keeps the closure.

(setf dotcl:*save-sil* t)

(defun %lli-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(defun %lli-sum (n)
  (declare (fixnum n))
  (labels ((rec (i acc)
             (declare (fixnum i acc))
             (if (> i n) acc (rec (1+ i) (+ acc i)))))
    (rec 0 0)))

(defun %lli-values (n)
  (labels ((rec (i) (if (zerop i) (values :done n) (rec (1- i)))))
    (rec n)))

(defun %lli-return-from (list)
  (labels ((rec (l)
             "Find the first odd element."
             (cond ((null l) nil)
                   ((oddp (car l)) (return-from rec (list :odd (car l))))
                   (t (rec (cdr l))))))
    (rec list)))

(defun %lli-setq (n)
  (labels ((rec (i acc)
             (setq acc (cons i acc))
             (if (zerop i) acc (rec (1- i) acc))))
    (rec n nil)))

;;; Each call has its own bindings: a closure over a parameter keeps it.
(defun %lli-closures (n)
  (labels ((rec (i fs)
             (if (zerop i) fs (rec (1- i) (cons (lambda () i) fs)))))
    (mapcar #'funcall (rec n nil))))

;;; A self-call that is not a tail call, and #'REC, keep the closure.
(defun %lli-fib (n)
  (labels ((fib (k) (if (< k 2) k (+ (fib (- k 1)) (fib (- k 2))))))
    (fib n)))

(defun %lli-function (n)
  (labels ((rec (i) (if (zerop i) #'rec (rec (1- i)))))
    (functionp (rec n))))

;;; Not in tail position.
(defun %lli-not-tail (n)
  (1+ (labels ((rec (i acc) (if (zerop i) acc (rec (1- i) (+ acc 2)))))
        (rec n 0))))

(defvar *lli-special* :outer)

(defun %lli-special (n)
  (labels ((rec (*lli-special*)
             (if (eql *lli-special* 0) (symbol-value '*lli-special*) (rec (1- *lli-special*)))))
    (rec n)))

(defun %lli-type-error (x)
  (labels ((rec (d n)
             (declare (double-float d) (fixnum n))
             (if (zerop n) d (rec (+ d 1d0) (1- n)))))
    (rec x 3)))

(deftest labels-loop-inline.values
  (list (%lli-sum 100) (multiple-value-list (%lli-values 3))
        (%lli-return-from '(2 4 5 6)) (%lli-return-from '(2 4))
        (%lli-setq 3) (%lli-closures 3) (%lli-fib 15) (%lli-function 2)
        (%lli-not-tail 4) (%lli-special 2) (%lli-type-error 1d0)
        (handler-case (%lli-type-error 1) (type-error () :type-error)))
  (5050 (:done 3) (:odd 5) nil (0 1 2 3) (1 2 3) 610 t 9 0 4d0 :type-error))

(deftest-compiled-only labels-loop-inline.no-closure
  (mapcar (lambda (f) (and (%lli-has "LABELSLOOP" f) (not (%lli-has "MAKE-CLOSURE" f))))
          (list #'%lli-sum #'%lli-type-error #'%lli-values #'%lli-closures #'%lli-not-tail
                #'%lli-special))
  (t t t nil nil nil))

(setf dotcl:*save-sil* nil)
