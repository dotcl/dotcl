;;; MAPHASH does not visit an entry the walk itself removed.

(defun %mre-table (n) (let ((h (make-hash-table))) (dotimes (i n) (setf (gethash i h) (* i 10))) h))

(deftest maphash-removed-entries.remhash-later-key
  (let ((h (%mre-table 8)) (seen 0))
    (maphash (lambda (k v) (declare (ignore v)) (incf seen) (remhash (1+ k) h)) h)
    (list seen (hash-table-count h)))
  (4 4))

(deftest maphash-removed-entries.clrhash
  (let ((h (%mre-table 8)) (seen 0))
    (maphash (lambda (k v) (declare (ignore k v)) (incf seen) (clrhash h)) h)
    (list seen (hash-table-count h)))
  (1 0))

(deftest maphash-removed-entries.changed-value-is-current
  (let ((h (make-hash-table)) (vals nil))
    (setf (gethash 1 h) :a (gethash 2 h) :a (gethash 3 h) :a)
    (maphash (lambda (k v) (push v vals) (when (= k 1) (setf (gethash 2 h) :b) (setf (gethash 3 h) :b) (setf (gethash 9 h) :new))) h)
    (let ((r (reverse vals))) (list (length r) (first r) (count :b r))))
  (3 :a 2))

(deftest maphash-removed-entries.current-entry-and-plain-walks
  (let ((h (%mre-table 100)) (seen 0))
    (maphash (lambda (k v) (declare (ignore v)) (incf seen) (remhash k h)) h)
    (let ((g (%mre-table 50)) (n 0))
      (maphash (lambda (k v) (declare (ignore k v)) (incf n)) g)
      (list seen (hash-table-count h) n)))
  (100 0 50))

(deftest maphash-removed-entries.literal-lambda-walk
  (let ((h (%mre-table 8)) (seen 0))
    (block done
      (maphash (lambda (k v) (declare (ignore v)) (incf seen) (clrhash h) (when (> k 100) (return-from done nil))) h))
    (list seen (hash-table-count h)))
  (1 0))
