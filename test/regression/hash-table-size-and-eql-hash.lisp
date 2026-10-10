;;; MAKE-HASH-TABLE reserves room for :SIZE entries, and the EQL / EQ hashes
;;; take a shorter path for keys that are not numbers or characters. These
;;; check that the tables still find what they hold.

(deftest hash-table-size.filled-past-size
  (let ((h (make-hash-table :test 'eq :size 10)))
    (dotimes (i 100) (setf (gethash (intern (format nil "HTS-~a" i) :keyword) h) i))
    (list (hash-table-count h)
          (gethash :hts-0 h) (gethash :hts-99 h)
          (>= (hash-table-size h) 100)))
  (100 0 99 t))

(deftest hash-table-size.odd-sizes
  (list (hash-table-count (make-hash-table :size 0))
        (hash-table-count (make-hash-table :size 1.5))
        (hash-table-count (make-hash-table :size (expt 2 40)))
        (let ((h (make-hash-table :test 'equal :size 1000000)))
          (setf (gethash "a" h) 1)
          (gethash "a" h)))
  (0 0 0 1))

(deftest hash-table-size.with-hash-function
  (let ((h (make-hash-table :test 'equal :size 50 :hash-function #'sxhash)))
    (setf (gethash '(1 2) h) :x)
    (values (gethash (list 1 2) h)))
  :x)

(deftest hash-table-eql-hash.mixed-keys
  (let ((h (make-hash-table :test 'eql :size 20))
        (keys (list 'sym :kw t nil 1 (expt 2 70) 1.5 1.5d0 1/3 #c(1 2) #\a "str" (list 1))))
    (dolist (k keys) (setf (gethash k h) k))
    (list (every (lambda (k) (eq (gethash k h) k)) keys)
          (gethash (expt 2 70) h)
          (gethash 1/3 h)
          (gethash #\a h)
          (nth-value 1 (gethash "str" h))
          (nth-value 1 (gethash 1.5d0 h))))
  (t 1180591620717411303424 1/3 #\a nil t))

(deftest hash-table-eq-hash.t-and-nil
  (let ((h (make-hash-table :test 'eq)))
    (setf (gethash t h) :t (gethash nil h) :nil)
    (list (gethash 't h) (gethash 'nil h) (gethash (not nil) h) (gethash (not t) h)))
  (:t :nil :t :nil))
