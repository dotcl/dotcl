;;; MAKE-HASH-TABLE is open-coded for no arguments and for (:TEST x). The
;;; argument scan only looked at literal keywords, so a key that is computed
;;; ((make-hash-table k 'equalp) with K bound to :TEST, or (identity :test))
;;; was neither recognized nor evaluated: the call made an EQL table and
;;; dropped the arguments' side effects. Found by the random integer form test
;;; with its extra shapes (make test-random-forms RANDOM_EXTRA=1).

(defun %mhtck-var (k) (hash-table-test (make-hash-table k 'equalp)))
(defun %mhtck-call () (hash-table-test (make-hash-table (identity :test) (identity 'equal))))
(defun %mhtck-side-effect ()
  (let ((n 0))
    (make-hash-table (progn (incf n) :size) (progn (incf n 10) 20))
    n))

(deftest make-hash-table-computed-keyword
  (list (%mhtck-var :test) (%mhtck-call) (%mhtck-side-effect)
        (hash-table-test (make-hash-table :test 'equalp)))
  (equalp equal 11 equalp))
