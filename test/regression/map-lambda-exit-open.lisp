;;; MAPHASH and MAP NIL with a literal lambda that leaves the walk: the walk is
;;; opened into a loop and the exit is a jump. Values, cleanups, changes to the
;;; table during the walk and the error cases keep their meaning.

(defun %mle-table (n)
  (let ((h (make-hash-table)))
    (dotimes (i n h) (setf (gethash i h) (* i 10)))))

(defun %mle-find (h k)
  (block found
    (maphash (lambda (key v) (when (eql key k) (return-from found v))) h)
    :none))

(deftest map-lambda-exit-open.maphash-return-from
  (let ((h (%mle-table 8)))
    (list (%mle-find h 3) (%mle-find h 99)))
  (30 :none))

(defun %mle-mv (h)
  (block b
    (maphash (lambda (k v) (when (= k 2) (return-from b (values k v :x)))) h)))

(deftest map-lambda-exit-open.maphash-multiple-values
  (multiple-value-list (%mle-mv (%mle-table 4)))
  (2 20 :x))

(defun %mle-return-in-dolist (h)
  ;; (RETURN) in the lambda leaves the DOLIST around the MAPHASH.
  (let ((n 0))
    (dolist (x '(a b c) n)
      (maphash (lambda (k v) (declare (ignore v)) (when (= k 1) (incf n) (return x))) h))))

(deftest map-lambda-exit-open.maphash-return
  (%mle-return-in-dolist (%mle-table 3))
  a)

(defun %mle-go (h)
  (let ((seen 0))
    (tagbody
       (maphash (lambda (k v) (declare (ignore k v)) (incf seen) (when (= seen 2) (go out))) h)
       (return-from %mle-go :not-left)
     out)
    seen))

(deftest map-lambda-exit-open.maphash-go
  (%mle-go (%mle-table 5))
  2)

(defun %mle-modify (h)
  ;; Adding and removing entries during the walk sees the entries the walk began with.
  (let ((n 0))
    (block nil
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (incf n)
                 (remhash k h)
                 (setf (gethash (+ k 100) h) t)
                 (when (> n 50) (return)))
               h))
    (list n (hash-table-count h))))

(deftest map-lambda-exit-open.maphash-modify
  (%mle-modify (%mle-table 6))
  (6 6))

(defun %mle-nested (h)
  (let ((pairs 0))
    (block outer
      (maphash (lambda (a x)
                 (declare (ignore x))
                 (maphash (lambda (b y)
                            (declare (ignore y))
                            (incf pairs)
                            (when (and (= a 2) (= b 2)) (return-from outer)))
                          h))
               h))
    pairs))

(deftest map-lambda-exit-open.maphash-nested-and-after
  ;; After the exits the per-thread buffer is free again: a plain walk still sees
  ;; every entry.
  (let ((h (%mle-table 4)))
    (list (<= 3 (%mle-nested h) 16)
          (let ((n 0)) (maphash (lambda (k v) (declare (ignore k v)) (incf n)) h) n)
          (%mle-find h 3)))
  (t 4 30))

(defun %mle-cleanup (h log)
  (block b
    (unwind-protect
         (maphash (lambda (k v) (declare (ignore v)) (when (= k 1) (return-from b :left))) h)
      (push :cleanup (car log)))))

(deftest map-lambda-exit-open.maphash-cleanup
  (let ((log (list nil)))
    (list (%mle-cleanup (%mle-table 3) log) (car log)))
  (:left (:cleanup)))

(deftest map-lambda-exit-open.maphash-weak-and-synchronized
  (let ((w (make-hash-table :weakness :key))
        (s (make-hash-table :synchronized t)))
    (setf (gethash 'a w) 1 (gethash 'b w) 2 (gethash 'a s) 1 (gethash 'b s) 2)
    (list (%mle-find w 'b) (%mle-find s 'a) (%mle-find s 'zz)))
  (2 1 :none))

(deftest map-lambda-exit-open.maphash-not-a-table
  (handler-case (%mle-find (list 1 2) 1)
    (type-error () :type-error))
  :type-error)

(deftest map-lambda-exit-open.maphash-value-is-nil
  (let ((h (%mle-table 2)))
    (block b (maphash (lambda (k v) (declare (ignore v)) (when (= k 99) (return-from b 1))) h)))
  nil)

(defun %mle-map-find (seq k)
  (block found
    (map nil (lambda (x) (when (eql x k) (return-from found (list :at x)))) seq)
    :none))

(deftest map-lambda-exit-open.map-nil-sequences
  (let ((fp (make-array 5 :initial-contents '(1 2 3 4 5) :fill-pointer 2)))
    (list (%mle-map-find '(1 2 3) 2)
          (%mle-map-find (vector 1 2 3) 3)
          (%mle-map-find "abc" #\b)
          (%mle-map-find fp 1)
          (%mle-map-find fp 4)
          (%mle-map-find '() 1)))
  ((:at 2) (:at 3) (:at #\b) (:at 1) :none :none))

(deftest map-lambda-exit-open.map-quoted-nil-value
  (let ((n 0))
    (list (map 'nil (lambda (x) (incf n x)) '(1 2 3)) n))
  (nil 6))

(deftest map-lambda-exit-open.map-nil-not-a-sequence
  (handler-case (%mle-map-find 42 1)
    (type-error () :type-error))
  :type-error)
