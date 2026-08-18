;;; MAKE-HASH-TABLE rejects an unusable :test / :weakness with a TYPE-ERROR.
;;;
;;; Bug: both were ArgumentException from the LispHashTable constructor, and the
;;; CLR-exception mapping turns that into PROGRAM-ERROR. (make-hash-table :test
;;; 'string=) is a common slip -- "your program is broken" is the wrong thing to
;;; say about a bad argument, and the condition carried no datum to inspect.

(defun %htt-outcome (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e)
      (list :type-error (type-error-datum e)
            (let ((et (type-error-expected-type e)))
              (and (consp et) (eq (first et) 'member)))))
    (error (e) (list :other (type-of e)))))

(deftest hash-table-test-arg.unknown-test
  (%htt-outcome (lambda () (make-hash-table :test 'string=)))
  (:type-error string= t))

(deftest hash-table-test-arg.unknown-weakness
  (%htt-outcome (lambda () (make-hash-table :weakness :sometimes)))
  (:type-error :sometimes t))

;;; The four standard tests keep working, as does :weakness :value.

(deftest hash-table-test-arg.standard-tests
  (mapcar (lambda (test)
            (let ((h (make-hash-table :test test)))
              (setf (gethash "k" h) 1)
              (hash-table-count h)))
          '(eq eql equal equalp))
  (1 1 1 1))

(deftest hash-table-test-arg.weak-value
  (let ((h (make-hash-table :weakness :value)))
    (setf (gethash :a h) 1)
    (gethash :a h))
  1 t)
