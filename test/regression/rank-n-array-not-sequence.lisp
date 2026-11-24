;;; An array of rank other than 1 is not a sequence: LENGTH and the sequence functions signal
;;; TYPE-ERROR; the :count of REMOVE and friends must be an integer or NIL, TYPE-ERROR otherwise.

(defun %rns-kind (thunk)
  (handler-case (progn (funcall thunk) :ok) (type-error () :type-error) (error () :other)))

(deftest rank-n-array-not-sequence.length
  (list (%rns-kind (lambda () (length (make-array '(2 2)))))
        (%rns-kind (lambda () (length (make-array nil))))
        (%rns-kind (lambda () (length (make-array '(2 2) :adjustable t))))
        (length (make-array 3)) (length (make-array 3 :fill-pointer 1)) (length "ab") (length '(1 2)))
  (:type-error :type-error :type-error 3 1 2 2))

(deftest rank-n-array-not-sequence.functions
  (let ((a (make-array '(2 2) :initial-element 1)))
    (list (%rns-kind (lambda () (position 1 a))) (%rns-kind (lambda () (find 1 a)))
          (%rns-kind (lambda () (count 1 a))) (%rns-kind (lambda () (remove 1 a)))
          (%rns-kind (lambda () (reduce #'+ a)))))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest rank-n-array-not-sequence.count-key
  (list (%rns-kind (lambda () (remove 1 '(1 2) :count 1.5)))
        (%rns-kind (lambda () (remove-if #'oddp '(1 2) :count 'a)))
        (%rns-kind (lambda () (substitute 1 2 '(2) :count 1.5)))
        (remove 1 '(1 2 1) :count 1) (remove 1 '(1 2 1) :count nil) (remove 1 '(1 2 1) :count -1))
  (:type-error :type-error :type-error (2 1) (2) (1 2 1)))
