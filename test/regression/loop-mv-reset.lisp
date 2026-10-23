;;; A loop that can neither publish values nor call out resets the values
;;; channel once before it starts instead of at each test. Values left by the
;;; code before the loop, or by an interrupt run inside it, must not become
;;; the loop's values.

(defvar *lmr-two* (lambda () (values :x 2)))

(defun %lmr-dolist (l)
  (let ((y (funcall *lmr-two*)))
    (dolist (x l y) x)))

(defun %lmr-do (l)
  (let ((y (funcall *lmr-two*)))
    (do ((p l (cdr p))) ((null p) y))))

(defun %lmr-nested (l)
  (let ((y (funcall *lmr-two*)))
    (dotimes (i 3) (dolist (x l) x))
    (do ((p l (cdr p))) ((null p) y))))

(deftest loop-mv-reset.dolist
  (multiple-value-list (%lmr-dolist '(1 2 3)))
  (:x))

(deftest loop-mv-reset.do
  (list (multiple-value-list (%lmr-do '(1 2 3)))
        (multiple-value-list (%lmr-do '())))
  ((:x) (:x)))

(deftest loop-mv-reset.nested
  (multiple-value-list (%lmr-nested '(1 2 3)))
  (:x))

(defun %lmr-spin (cell)
  (do ((p cell (cdr p))) ((null p) :end)))

(deftest loop-mv-reset.interrupt-values-do-not-leak
  (let* ((cell (list 1))
         (th (progn (setf (cdr cell) cell)
                    (dotcl:make-thread
                     (lambda () (multiple-value-list (%lmr-spin cell)))))))
    (sleep 0.2)
    (dotcl:interrupt-thread th (lambda () (setf (cdr cell) nil) (values :end 2)))
    (dotcl:thread-join th))
  (:end))

(setf dotcl:*save-sil* t)

;; Double-float arithmetic, a float literal, ABS and the boxes of the float
;; variables' SETQ values publish no values either.
(defun %lmr-float (c)
  (declare (double-float c))
  (let ((y (funcall *lmr-two*)) (z 0d0) (w 1d0))
    (declare (double-float z w) (ignorable y))
    (do ((i 0 (1+ i))) ((or (> (abs z) 4.0) (> i 50)) i)
      (setq w (* w -1d0)
            z (+ (* z 0.5d0) (- c) w)))))

(deftest loop-mv-reset.float
  (list (multiple-value-list (%lmr-float 0.5d0))
        (multiple-value-list (%lmr-float 3d0)))
  ((51) (3)))

(deftest-emitting-only loop-mv-reset.float-shape
  (let* ((s (%flat-text (dotcl:function-sil #'%lmr-float)))
         (n 0) (p (search "ConditionSystem.PollInterrupt" s)))
    (loop (let ((q (search "MultipleValues.Reset)" s :start2 p)))
            (if q (setq n (1+ n) p (1+ q)) (return))))
    n)
  1)
