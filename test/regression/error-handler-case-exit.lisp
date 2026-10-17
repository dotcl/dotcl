;;; ERROR transferring to a HANDLER-CASE clause, and the handler stack around it.

(define-condition ehe-a (error) ((v :initarg :v :reader ehe-v)))
(define-condition ehe-b (ehe-a) ())
(define-condition ehe-s (condition) ((v :initarg :v :reader ehe-sv)))

(deftest error-hc-exit.datum-shapes
  (list (handler-case (error 'ehe-b :v 1) (ehe-a (c) (list (type-of c) (ehe-v c))))
        (handler-case (error (make-condition 'ehe-a :v 2)) (ehe-a (c) (ehe-v c)))
        (handler-case (error "x ~A" 3)
          (simple-error (c) (apply #'format nil (simple-condition-format-control c)
                                   (simple-condition-format-arguments c))))
        (handler-case (error 'simple-error :format-control "y ~A" :format-arguments '(4))
          (error (c) (princ-to-string c))))
  ((ehe-b 1) 2 "x 3" "y 4"))

;;; Handler-bind handlers inside the handler-case run first, in order, and may
;;; decline; the clause then takes the condition.
(deftest error-hc-exit.bind-declines-first
  (let ((log '()))
    (list (handler-case
              (handler-bind ((ehe-a (lambda (c) (push (list :inner (ehe-v c)) log)))
                             (error (lambda (c) (declare (ignore c)) (push :inner2 log))))
                (handler-bind ((ehe-b (lambda (c) (declare (ignore c)) (push :innermost log))))
                  (error 'ehe-b :v 5)))
            (ehe-a (c) (list :caught (ehe-v c))))
          (reverse log)))
  ((:caught 5) (:innermost (:inner 5) :inner2)))

;;; While a handler runs its own cluster is not visible, so the ERROR inside it
;;; is not seen by it; afterwards the cluster is back for the next signal.
(deftest error-hc-exit.stack-around-handler
  (let ((seen '()))
    (handler-bind ((ehe-s (lambda (c) (push (list :outer (ehe-sv c)) seen)))
                   (ehe-a (lambda (c) (push (list :outer-error (ehe-v c)) seen))))
      (handler-case (error 'ehe-a :v 0) (ehe-a () nil))
      (handler-bind ((ehe-s (lambda (c)
                              (push (ehe-sv c) seen)
                              (push (handler-case (error 'ehe-b :v 9)
                                      (ehe-b (c2) (list :in (ehe-v c2))))
                                    seen))))
        (signal 'ehe-s :v 1)
        (signal 'ehe-s :v 2)))
    (reverse seen))
  (1 (:in 9) (:outer 1) 2 (:in 9) (:outer 2)))

;;; A handler-bind handler leaving by RETURN-FROM restores the stack too.
(deftest error-hc-exit.bind-nonlocal-exit
  (let ((n 0))
    (handler-bind ((condition (lambda (c) (declare (ignore c)) (incf n))))
      (block b
        (handler-bind ((ehe-a (lambda (c) (declare (ignore c)) (return-from b))))
          (error 'ehe-a :v 0)))
      (signal 'ehe-s :v 0))
    n)
  1)

;;; Restarts established around the ERROR are visible to a handler-bind handler,
;;; and a handler-case outside still gets what is not handled.
(deftest error-hc-exit.restart-and-unwind-protect
  (let ((cleanup nil))
    (list (handler-bind ((ehe-a (lambda (c) (invoke-restart 'use (ehe-v c)))))
            (restart-case (error 'ehe-a :v 7) (use (x) (* 2 x))))
          (handler-case (unwind-protect (error 'ehe-b :v 8) (setq cleanup t))
            (ehe-a (c) (ehe-v c)))
          cleanup))
  (14 8 t))

(defgeneric ehe-match (x))
(defmethod ehe-match ((x integer)) (error 'ehe-b :v x))
(defun ehe-pred (x)
  (handler-case (ehe-match x)
    (ehe-b (c) (error 'ehe-a :v (1+ (ehe-v c))))))

(deftest error-hc-exit.resignal-from-clause
  (loop for i below 3
        collect (handler-case (ehe-pred i) (ehe-b () :wrong) (ehe-a (c) (ehe-v c))))
  (1 2 3))

(deftest error-hc-exit.ignore-errors
  (multiple-value-bind (v c) (ignore-errors (error 'ehe-a :v 9))
    (list v (ehe-v c)))
  (nil 9))
