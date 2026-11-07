;;; HASH-TABLE-REHASH-SIZE and HASH-TABLE-REHASH-THRESHOLD return what
;;; MAKE-HASH-TABLE was given (CLHS), not a fixed 1.5 and 1.0.

(deftest hash-table-rehash-size.integer
  (hash-table-rehash-size (make-hash-table :rehash-size 2))
  2)

(deftest hash-table-rehash-size.float
  (list (hash-table-rehash-size (make-hash-table :rehash-size 1.25))
        (hash-table-rehash-size (make-hash-table :rehash-size 3.5d0 :test 'equal)))
  (1.25 3.5d0))

(deftest hash-table-rehash-size.default
  (let ((n (hash-table-rehash-size (make-hash-table))))
    (and (realp n) (> n 1)))
  t)

(deftest hash-table-rehash-threshold.given
  (list (hash-table-rehash-threshold (make-hash-table :rehash-threshold 0.5))
        (hash-table-rehash-threshold (make-hash-table :rehash-threshold 1/2))
        (hash-table-rehash-threshold (make-hash-table :rehash-threshold 0)))
  (0.5 1/2 0))

(deftest hash-table-rehash-threshold.default
  (let ((n (hash-table-rehash-threshold (make-hash-table))))
    (and (realp n) (<= 0 n 1)))
  t)

(deftest hash-table-rehash-args.first-keyword-wins
  (list (hash-table-rehash-size (make-hash-table :rehash-size 2 :rehash-size 3))
        (hash-table-rehash-threshold
         (make-hash-table :rehash-threshold 0.25 :rehash-threshold 0.75)))
  (2 0.25))

(deftest hash-table-rehash-args.hash-function
  (let ((h (make-hash-table :test 'eql :rehash-size 4 :rehash-threshold 0.9)))
    (list (hash-table-rehash-size h) (hash-table-rehash-threshold h)))
  (4 0.9))
