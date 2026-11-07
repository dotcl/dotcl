;;; HASH-TABLE-SIZE reports the :size the table was made with (SBCL: 100 for :size 100),
;;; and the entry count once the table has grown past it. It answered the entry count,
;;; at least 16.

(deftest hash-table-size-declared.made-with-size
  (list (hash-table-size (make-hash-table :size 100))
        (hash-table-size (make-hash-table :size 5))
        (hash-table-size (make-hash-table :size 100 :test 'equal)))
  (100 5 100))

(deftest hash-table-size-declared.grows-past-it
  (let ((h (make-hash-table :size 10)))
    (dotimes (i 50) (setf (gethash i h) i))
    (>= (hash-table-size h) 50))
  t)

(deftest hash-table-size-declared.default-is-a-positive-size
  (let ((n (hash-table-size (make-hash-table))))
    (and (integerp n) (plusp n)))
  t)

(deftest hash-table-size-declared.count-unchanged
  (let ((h (make-hash-table :size 100)))
    (setf (gethash 1 h) 1)
    (list (hash-table-count h) (gethash 1 h)))
  (1 1))
