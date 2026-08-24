;;; Three iteration/building paths stopped allocating per call:
;;;   MAPHASH      -- reuses a per-thread snapshot buffer and takes the Lisp
;;;                   function directly instead of closing over it
;;;   CONCATENATE  -- all-strings goes to one exact-size result
;;;   SEARCH       -- the 2-argument shape reaches the core without an args
;;;                   array, and the element test is a call, not a closure
;;; The invariants those rest on are what these pin.

;;; --- MAPHASH -------------------------------------------------------------
;;; The snapshot exists so the function may add or remove entries mid-walk;
;;; the buffer is per thread, so a nested walk must get its own.

(deftest maphash-buf.walks-all-entries
  (let ((h (make-hash-table)) (acc nil))
    (dotimes (i 5) (setf (gethash i h) (* i 10)))
    (maphash (lambda (k v) (push (cons k v) acc)) h)
    (sort acc #'< :key #'car))
  ((0 . 0) (1 . 10) (2 . 20) (3 . 30) (4 . 40)))

(deftest maphash-buf.empty-table
  (let ((acc :untouched))
    (maphash (lambda (k v) (declare (ignore k v)) (setq acc :touched)) (make-hash-table))
    acc)
  :untouched)

(deftest maphash-buf.nested-same-table
  (let ((h (make-hash-table)) (n 0))
    (dotimes (i 3) (setf (gethash i h) i))
    (maphash (lambda (k v) (declare (ignore k v))
               (maphash (lambda (k2 v2) (declare (ignore k2 v2)) (incf n)) h))
             h)
    n)
  9)

(deftest maphash-buf.nested-other-table
  (let ((h1 (make-hash-table)) (h2 (make-hash-table)) (n 0))
    (dotimes (i 3) (setf (gethash i h1) i))
    (setf (gethash :only h2) t)
    (maphash (lambda (k v) (declare (ignore k v))
               (maphash (lambda (k2 v2) (declare (ignore k2 v2)) (incf n)) h2))
             h1)
    n)
  3)

(deftest maphash-buf.insert-during-walk
  (let ((h (make-hash-table)) (seen nil))
    (setf (gethash 1 h) 1)
    (maphash (lambda (k v) (declare (ignore v))
               (push k seen)
               (when (= k 1) (setf (gethash 2 h) 2)))
             h)
    (list (sort seen #'<) (hash-table-count h)))
  ((1) 2))

(deftest maphash-buf.remove-during-walk
  (let ((h (make-hash-table)) (seen nil))
    (dotimes (i 3) (setf (gethash i h) i))
    (maphash (lambda (k v) (declare (ignore v)) (push k seen) (remhash k h)) h)
    (list (sort seen #'<) (hash-table-count h)))
  ((0 1 2) 0))

(deftest maphash-buf.equal-and-synchronized-tables
  (list (let ((h (make-hash-table :test 'equal)) (n 0))
          (setf (gethash "a" h) 1) (setf (gethash '(1) h) 2)
          (maphash (lambda (k v) (declare (ignore k v)) (incf n)) h) n)
        (let ((h (make-hash-table :synchronized t)) (n 0))
          (dotimes (i 4) (setf (gethash i h) i))
          (maphash (lambda (k v) (declare (ignore k v)) (incf n)) h) n))
  (2 4))

;;; --- CONCATENATE ---------------------------------------------------------

(deftest concat-fast.strings
  (list (concatenate 'string "ab" "cd")
        (concatenate 'string "a" "b" "c")
        (concatenate 'string "" "x")
        (concatenate 'string)
        (concatenate 'string "solo"))
  ("abcd" "abc" "x" "" "solo"))

(deftest concat-fast.mixed-sequences-take-the-general-path
  (list (concatenate 'string "ab" '(#\c #\d))
        (concatenate 'string #(#\a) "b")
        (concatenate 'string '(#\a) '(#\b)))
  ("abcd" "ab" "ab"))

(deftest concat-fast.other-result-types
  (list (concatenate 'list "ab" '(1))
        (coerce (concatenate 'vector '(1) #(2)) 'list))
  ((#\a #\b 1) (1 2)))

(deftest concat-fast.type-errors
  (list (handler-case (concatenate 'string '(1 2)) (type-error () :te))
        (handler-case (concatenate 'string 5) (type-error () :te) (error () :err)))
  (:te :te))

(deftest concat-fast.result-is-fresh
  (let* ((a "ab") (r (concatenate 'string a)))
    (list r (eq r a)))
  ("ab" nil))

;;; --- SEARCH --------------------------------------------------------------

(deftest search-core.two-arg
  (list (search "wor" "hello world")
        (search "xyz" "hello")
        (search "" "abc")
        (search "abc" "")
        (search "hello" "hello"))
  (6 nil 0 nil 0))

(deftest search-core.lists-and-vectors
  (list (search '(2 3) '(1 2 3 4)) (search #(2 3) #(1 2 3 4)) (search '(#\b) "abc"))
  (1 1 1))

(deftest search-core.keywords
  (list (search "WOR" "hello world" :test #'char-equal)
        (search "l" "hello world" :from-end t)
        (search "o" "hello world" :start2 5)
        (search "lo" "hello world" :end2 5)
        (search '(1) '(1 2) :test-not #'eql))
  (6 9 7 3 1))

(deftest search-core.bounds-are-checked
  (handler-case (search "a" "abc" :start2 9) (error () :error))
  :error)
