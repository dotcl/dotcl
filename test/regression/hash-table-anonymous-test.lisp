;;; Regression: MAKE-HASH-TABLE with :TEST given as an anonymous function (not
;;; one of EQ / EQL / EQUAL / EQUALP) quietly made an EQL table, so keys the
;;; caller meant to compare with that function were never found. It now signals
;;; TYPE-ERROR, as a named unknown test already did. Function objects of the
;;; four standard tests still work.

(deftest hash-table-anonymous-test-signals
  (list (handler-case (progn (make-hash-table :test (lambda (a b) (equal a b))) :made)
          (type-error () :type-error))
        (handler-case (progn (make-hash-table :test 'no-such-hash-test) :made)
          (type-error () :type-error)))
  (:type-error :type-error))

(deftest hash-table-anonymous-test-standard-functions
  (mapcar (lambda (f) (hash-table-test (make-hash-table :test f)))
          (list #'eq #'eql #'equal #'equalp 'equal))
  (eq eql equal equalp equal))
