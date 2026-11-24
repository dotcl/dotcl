;;; (INCF (GETHASH K H D) DELTA) and DECF find the entry once and rewrite it in place. The
;;; value stored and returned, the order in which the forms are evaluated, the errors, and
;;; every kind of table are the ones the general (SETF GETHASH) expansion gave.

(defun %gi-ref-incf (k h d delta) (setf (gethash k h) (+ (gethash k h d) delta)))
(defun %gi-ref-decf (k h d delta) (setf (gethash k h) (- (gethash k h d) delta)))
(defun %gi-incf (k h d delta) (incf (gethash k h d) delta))
(defun %gi-decf (k h d delta) (decf (gethash k h d) delta))
(defun %gi-incf1 (k h) (incf (gethash k h 0)))
(defun %gi-incf-nodef (k h) (incf (gethash k h)))

(defun %gi-table (test &rest args) (apply #'make-hash-table :test test args))

(deftest gethash-incf.counts-and-returns-the-new-value
  (let ((h (%gi-table 'equal)))
    (list (%gi-incf1 "a" h) (%gi-incf1 "a" h) (%gi-incf1 (copy-seq "a") h) (%gi-incf1 "b" h)
          (%gi-incf "a" h 0 10) (%gi-decf "a" h 0 4) (%gi-decf "z" h 100 1)
          (gethash "a" h) (gethash "b" h) (gethash "z" h) (hash-table-count h)))
  (1 2 3 1 13 9 99 9 1 99 3))

(deftest gethash-incf.number-types
  (let ((h (%gi-table 'eql)))
    (setf (gethash :f h) 1.5d0 (gethash :r h) 1/3 (gethash :b h) (expt 10 20) (gethash :s h) 2.5f0)
    (list (%gi-incf :f h 0 0.25d0) (%gi-incf :r h 0 1/6) (%gi-incf :b h 0 1) (%gi-decf :b h 0 (expt 10 20))
          (%gi-incf :s h 0 1) (%gi-incf :new h 0.5d0 1) (%gi-incf :big h most-positive-fixnum 1)
          (%gi-decf :neg h most-negative-fixnum 1) (%gi-incf :c h #C(1 2) #C(3 4))))
  (1.75d0 1/2 100000000000000000001 1 3.5 1.5d0 #.(1+ most-positive-fixnum)
   #.(1- most-negative-fixnum) #C(4 6)))

(defun %gi-agree-count ()
  (let ((bad 0))
    (dolist (test '(eq eql equal equalp))
      (dolist (key (list :k 'sym 5 -3 1.5d0 #\x (expt 10 30) "str" '(1 2)))
        (dolist (d '(0 7 -2 1/2 2.5d0))
          (dolist (delta '(1 -1 0 3 1/3 0.5d0))
            (let ((a (%gi-table test)) (b (%gi-table test)))
              (dotimes (round 2)
                (let ((ra (%gi-incf key a d delta)) (rb (%gi-ref-incf key b d delta))
                      (sa (%gi-decf key a d delta)) (sb (%gi-ref-decf key b d delta)))
                  (unless (and (eql ra rb) (eql sa sb) (eql (gethash key a) (gethash key b))
                               (= (hash-table-count a) (hash-table-count b)))
                    (incf bad)))))))))
    bad))

(deftest gethash-incf.agrees-with-the-general-expansion
  (%gi-agree-count)
  0)

(deftest gethash-incf.evaluation-order-and-once
  (let ((log nil) (h (%gi-table 'equal)))
    (flet ((note (x) (push x log) x))
      (incf (gethash (note :key) (progn (note :table) h) (note 0)) (note 5))
      (incf (gethash (note "k2") h 0) 1))
    (list (reverse log) (gethash :key h)))
  ((:key :table 0 5 "k2") 5))

(deftest gethash-incf.errors-leave-the-entry-alone
  (let ((h (%gi-table 'equal)))
    (setf (gethash "s" h) :symbol (gethash "n" h) 1)
    (list (handler-case (%gi-incf "s" h 0 1) (type-error () :type-error))
          (gethash "s" h)
          (handler-case (%gi-incf "n" h 0 :x) (type-error () :type-error))
          (gethash "n" h)
          (handler-case (%gi-incf-nodef "absent" h) (type-error () :type-error))
          (nth-value 1 (gethash "absent" h))
          (handler-case (%gi-incf "n" 5 0 1) (type-error () :type-error))
          (handler-case (%gi-incf "n" nil 0 1) (type-error () :type-error))))
  (:type-error :symbol :type-error 1 :type-error nil :type-error :type-error))

(deftest gethash-incf.table-kinds
  (let ((tables (list (%gi-table 'equal :synchronized t) (%gi-table 'eql :weakness :key)
                      (%gi-table 'equal :weakness :value) (%gi-table 'eq :size 1000)
                      (%gi-table 'equalp) (%gi-table 'eql :test 'eql))))
    (loop for h in tables
          collect (list (%gi-incf1 :a h) (%gi-incf1 :a h) (%gi-incf 7 h 100 5) (%gi-incf 7 h 100 5)
                        (hash-table-count h))))
  #.(make-list 6 :initial-element '(1 2 105 110 2)))

(deftest gethash-incf.fixnum-keys-and-iteration
  (let ((h (%gi-table 'eql)) (seen nil))
    (dotimes (i 10) (dotimes (j (1+ i)) (%gi-incf1 i h)))
    (%gi-incf1 "s" h)
    (maphash (lambda (k v) (when (integerp k) (push (cons k v) seen))) h)
    (list (sort seen #'< :key #'car) (gethash 9 h) (hash-table-count h)))
  (((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6) (6 . 7) (7 . 8) (8 . 9) (9 . 10)) 10 11))

(deftest gethash-incf.mutated-string-key
  (let ((h (%gi-table 'equal)) (k (make-string 3 :initial-element #\a)))
    (setf (char k 1) #\b)
    (%gi-incf1 k h)
    (%gi-incf1 "aba" h)
    (let ((before (gethash k h)))
      (setf k (copy-seq k))
      (setf (char k 2) #\c)
      (list before (gethash "aba" h) (gethash k h) (%gi-incf1 k h) (gethash "abc" h))))
  (2 2 nil 1 1))

(deftest gethash-incf.local-function-shadows
  (flet ((gethash (k h &optional d) (declare (ignore h d)) (list :mine k)))
    (declare (notinline gethash))
    (handler-case (incf (gethash 1 nil 0)) (error () :error)))
  :error)
