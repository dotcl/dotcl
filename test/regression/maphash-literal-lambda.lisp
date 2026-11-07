;;; (MAPHASH (LAMBDA (K V) ...) TABLE) with a literal lambda is compiled into a loop over the
;;; table's snapshot, whether or not the lambda leaves the walk by RETURN-FROM or GO. The meaning
;;; is MAPHASH's: every entry once, the table may change during the walk, an error in the body
;;; ends the walk and propagates, and a lambda of another shape is the ordinary call.

(defun %mh-table (n)
  (let ((h (make-hash-table :test 'eql)))
    (dotimes (i n h) (setf (gethash i h) (* i 3)))))

(defun %mh-sum (h) (let ((sum 0)) (maphash (lambda (k v) (declare (ignore k)) (incf sum v)) h) sum))
(defun %mh-keys (h) (let ((ks '())) (maphash (lambda (k v) (declare (ignore v)) (push k ks)) h) (sort ks #'<)))
(defun %mh-find (h value)
  (block found (maphash (lambda (k v) (when (= v value) (return-from found k))) h) nil))
(defun %mh-grow (h)
  ;; The table changes during the walk: the walk is of the entries as they were.
  (let ((n 0)) (maphash (lambda (k v) (declare (ignore v)) (incf n) (setf (gethash (+ k 1000) h) 1)) h) n))
(defun %mh-nested (a b)
  (let ((pairs 0)) (maphash (lambda (k1 v1) (declare (ignore k1 v1)) (maphash (lambda (k2 v2) (declare (ignore k2 v2)) (incf pairs)) b)) a) pairs))
(defun %mh-error (h)
  (let ((seen 0))
    (list (handler-case (maphash (lambda (k v) (declare (ignore v)) (incf seen) (when (= k 2) (error "stop"))) h)
            (error () :stopped))
          (<= seen 4)
          ;; the table is usable and a later walk sees everything
          (%mh-sum h))))
(defun %mh-optional (h)
  (let ((n 0)) (maphash (lambda (k v &optional extra) (declare (ignore k v extra)) (incf n)) h) n))
(defun %mh-function-object (h)
  (let* ((n 0) (f (lambda (k v) (declare (ignore k v)) (incf n)))) (maphash f h) n))
(defun %mh-result (h) (maphash (lambda (k v) (declare (ignore k v)) 1) h))

(deftest maphash-literal-lambda.same-meaning
  (let ((h (%mh-table 10)))
    (list (%mh-sum h) (%mh-keys h) (%mh-find h 12) (%mh-find h 1000)
          (%mh-grow (%mh-table 5)) (%mh-nested (%mh-table 3) (%mh-table 4))
          (%mh-optional h) (%mh-function-object h) (%mh-result h)
          (%mh-sum (make-hash-table)) (%mh-error h)))
  (135 (0 1 2 3 4 5 6 7 8 9) 4 nil 5 12 10 10 nil 0 (:stopped t 135)))
