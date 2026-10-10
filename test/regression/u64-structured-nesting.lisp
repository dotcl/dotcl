;;; An INLINE call that is an operand of 64-bit word arithmetic (here LOGXOR)
;;; is compiled first to see whether its expansion ends in a word, and that
;;; code is dropped when it does not. The trial must not try the INLINE calls
;;; before the expansion's last form the same way once more: every level of
;;; nesting would compile its inner calls twice, so a chain of 20 nested INLINE
;;; functions took longer than any test runs. An INLINE call that is the whole
;;; expression (no arithmetic around it) is not tried at all. Both compile in
;;; well under a second here, with the values of the plain calls.

(defun %u64sn-define (prefix depth body)
  (let ((f0 (intern (format nil "~a0" prefix))))
    (eval `(declaim (inline ,f0)))
    (eval `(defun ,f0 (x) (car x))))
  (loop for i from 1 to depth
        for name = (intern (format nil "~a~d" prefix i))
        for prev = (intern (format nil "~a~d" prefix (1- i)))
        do (eval `(declaim (inline ,name)))
           (eval `(defun ,name (x) ,(funcall body prev)))))

(defun %u64sn-time (prefix depth body arg)
  (let ((t0 (get-internal-real-time)))
    (%u64sn-define prefix depth body)
    (let ((f (compile nil `(lambda (x) (,(intern (format nil "~a~d" prefix depth)) x)))))
      (list (funcall f arg)
            (< (/ (- (get-internal-real-time) t0) internal-time-units-per-second) 10)))))

(deftest-emitting-only u64-structured-nesting.operand
  (%u64sn-time "%U64SN-G" 20
               (lambda (prev) `(let ((y (logxor (,prev x) 1))) (if (consp y) (car y) y)))
               (list 5))
  (5 t))

(deftest-emitting-only u64-structured-nesting.whole-expression
  (%u64sn-time "%U64SN-F" 20
               (lambda (prev) `(let ((y (,prev x))) (car y)))
               (let ((v :end)) (dotimes (i 21 v) (setf v (list v)))))
  (:end t))
