;;; An opened MAPHASH (a literal lambda) walks the table through single-valued runtime calls and
;;; leaves the multiple-values channel alone.

(defun %mwv-table (n)
  (let ((h (make-hash-table)))
    (dotimes (i n h) (setf (gethash i h) (* i i)))))

(defun %mwv-sum (h)
  (let ((s 0)) (maphash (lambda (k v) (incf s (+ k v))) h) s))

(defun %mwv-find (h x)
  (maphash (lambda (k v) (when (eql v x) (return-from %mwv-find k))) h)
  :none)

(defun %mwv-values (h)
  (maphash (lambda (k v) (declare (ignore k v)) (values 1 2 3)) h)
  (values :a :b))

(defun %mwv-grow (h)
  (let ((seen 0))
    (maphash (lambda (k v) (declare (ignore v)) (incf seen) (when (< k 3) (setf (gethash (+ k 100) h) 0))) h)
    (list seen (hash-table-count h))))

(defun %mwv-remove (h)
  (let ((seen '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k seen) (remhash (1+ k) h)) h)
    (length seen)))

(defun %mwv-nested (h)
  (let ((n 0))
    (maphash (lambda (k v) (declare (ignore k v)) (maphash (lambda (k2 v2) (declare (ignore k2 v2)) (incf n)) h)) h)
    n))

(deftest maphash-walk-values.sum
  (%mwv-sum (%mwv-table 100))
  333300)

(deftest maphash-walk-values.return-from
  (%mwv-find (%mwv-table 50) 144)
  12)

(deftest maphash-walk-values.channel-clean
  (multiple-value-list (%mwv-values (%mwv-table 5)))
  (:a :b))

(deftest maphash-walk-values.add-during-walk
  (let ((r (%mwv-grow (%mwv-table 5))))
    (list (>= (car r) 5) (cadr r)))
  (t 8))

(deftest maphash-walk-values.remove-during-walk
  (< (%mwv-remove (%mwv-table 10)) 10)
  t)

(deftest maphash-walk-values.nested
  (%mwv-nested (%mwv-table 20))
  400)

(deftest maphash-walk-values.empty
  (%mwv-sum (make-hash-table))
  0)
