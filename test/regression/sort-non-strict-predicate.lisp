;;; SORT and STABLE-SORT with a non-strict predicate (#'>=, #'<=) still sort.
;;; Such a predicate is true both ways for equal keys; the three-way comparator
;;; built from it contradicted itself, the underlying sort gave up, and the
;;; sequence came back partly or entirely unsorted. serapeum's SORT-NEW is
;;; called this way in its tests (LONG-SHORT-GENERATIVE).

(defun snsp-sorted-p (seq pred)
  (let ((v (coerce seq 'vector)))
    (loop for i from 1 below (length v)
          always (funcall pred (aref v (1- i)) (aref v i)))))

(defparameter *snsp-data* '(3 1 3 2 3 1 2 3 3 1 0 3 2 2 1 3 3 2 0 1 2 3 1 2))

(deftest sort-non-strict.vector->=
  (let ((v (sort (coerce *snsp-data* 'vector) #'>=)))
    (list (snsp-sorted-p v #'>=) (length v) (reduce #'+ v)))
  (t 24 47))

(deftest sort-non-strict.list-<=
  (let ((l (sort (copy-list *snsp-data*) #'<=)))
    (list (snsp-sorted-p l #'<=) (length l)))
  (t 24))

(deftest sort-non-strict.stable->=
  (snsp-sorted-p (stable-sort (coerce *snsp-data* 'vector) #'>=) #'>=)
  t)

(deftest sort-non-strict.key
  (let ((v (sort (map 'vector (lambda (n) (make-list n)) *snsp-data*) #'>= :key #'length)))
    (snsp-sorted-p (map 'vector #'length v) #'>=))
  t)

;;; STABLE-SORT with a strict predicate keeps equal keys in their original order.
(deftest sort-non-strict.stable-order-kept
  (stable-sort (list '(1 . a) '(0 . b) '(1 . c) '(0 . d) '(1 . e)) #'< :key #'car)
  ((0 . b) (0 . d) (1 . a) (1 . c) (1 . e)))

;;; A longer input than the insertion-sort cutoff, both ways.
(deftest sort-non-strict.long
  (let* ((data (loop for i below 500 collect (mod (* i 7919) 37)))
         (a (sort (copy-list data) #'<))
         (b (sort (coerce data 'vector) #'>=))
         (c (stable-sort (copy-list data) #'<=)))
    (list (snsp-sorted-p a #'<=) (snsp-sorted-p b #'>=) (snsp-sorted-p c #'<=)
          (equal a (reverse (coerce b 'list))) (equal a c)))
  (t t t t t))
