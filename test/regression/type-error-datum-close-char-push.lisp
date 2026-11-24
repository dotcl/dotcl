;;; Expected values come from SBCL: (close 5) is a TYPE-ERROR (datum 5, expected STREAM),
;;; (setf (char "abc" 0) 5) has datum 5 and expected type CHARACTER, and VECTOR-PUSH on a
;;; vector without a fill pointer is a TYPE-ERROR whose datum is the vector.

(defun %tedc-info (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))))

(deftest type-error-datum-close-char-push.close
  (list (%tedc-info (lambda () (close 5)))
        (%tedc-info (lambda () (close nil))))
  ((5 stream) (nil stream)))

(deftest type-error-datum-close-char-push.setf-char
  (list (%tedc-info (lambda () (setf (char (make-string 3) 0) 5)))
        (%tedc-info (lambda () (setf (char 5 0) #\a)))
        (let ((s (make-string 3 :initial-element #\-))) (setf (char s 0) #\a) s))
  ((5 character) (5 string) "a--"))

(deftest type-error-datum-close-char-push.vector-push
  (let ((v (vector 1)))
    (let ((r (%tedc-info (lambda () (vector-push 2 v)))))
      (list (eq (first r) v) (equal (second r) '(and vector (satisfies array-has-fill-pointer-p))))))
  (t t))

(deftest type-error-datum-close-char-push.unchanged
  (let ((s (make-string-input-stream "a")) (v (make-array 3 :fill-pointer 0)))
    (list (close s) (vector-push 1 v)))
  (t 0))
