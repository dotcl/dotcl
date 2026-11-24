;;; regression: skip-under-interp
;;; A character pushed back with UNREAD-CHAR on an echo stream is not echoed a second
;;; time when READ-LINE or READ-SEQUENCE takes it again.

(defun %esu (reader)
  (let* ((out (make-string-output-stream))
         (e (make-echo-stream (make-string-input-stream (format nil "ab~%cd")) out)))
    (unread-char (read-char e) e)
    (let ((r (funcall reader e)))
      (list r (get-output-stream-string out)))))

(deftest echo-stream-unread-no-reecho.read-line
  (%esu (lambda (e) (read-line e)))
  ("ab" #.(format nil "ab~%")))

(deftest echo-stream-unread-no-reecho.read-sequence-string
  (%esu (lambda (e) (let ((v (make-string 2))) (list (read-sequence v e) v))))
  ((2 "ab") "ab"))

(deftest echo-stream-unread-no-reecho.read-sequence-vector
  (%esu (lambda (e) (let ((v (make-array 2))) (list (read-sequence v e) (coerce v 'list)))))
  ((2 (#\a #\b)) "ab"))

(deftest echo-stream-unread-no-reecho.read-sequence-list
  (%esu (lambda (e) (let ((v (list 0 0))) (list (read-sequence v e) v))))
  ((2 (#\a #\b)) "ab"))
