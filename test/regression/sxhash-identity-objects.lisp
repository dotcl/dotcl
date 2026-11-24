;;; SXHASH of an object EQUAL compares by identity (a structure instance, a CLOS instance, a
;;; condition, a hash table, a function) differs from one object to the next: it was the same
;;; value for all of them, which put every key of a table built on SXHASH in one bucket.

(defstruct sxi-pt x y)
(defclass sxi-c () ())
(define-condition sxi-cnd (error) ())

(defun %sxi-distinct (n make) (length (remove-duplicates (loop repeat n collect (sxhash (funcall make))))))

(deftest sxhash-identity.distinct
  (list (%sxi-distinct 200 #'make-sxi-pt)
        (%sxi-distinct 200 (lambda () (make-instance 'sxi-c)))
        (%sxi-distinct 50 (lambda () (make-condition 'sxi-cnd)))
        (%sxi-distinct 50 #'make-hash-table))
  (200 200 50 50))

(deftest sxhash-identity.stable-and-a-fixnum
  (let ((o (make-sxi-pt)))
    (let ((h (sxhash o)))
      (gc)
      (list (= h (sxhash o)) (typep h '(and fixnum (integer 0))))))
  (t t))

(deftest sxhash-identity.content-hashes-unchanged
  (list (= (sxhash "abc") (sxhash (copy-seq "abc"))) (= (sxhash '(1 2 3)) (sxhash (list 1 2 3)))
        (= (sxhash (expt 2 100)) (sxhash (* (expt 2 50) (expt 2 50)))))
  (t t t))
