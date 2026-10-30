;;; Argument checks found by the sequence and hash-table differential tests
;;; against SBCL.

(defun shac-type-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)))

(defun shac-type-error-detail (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list :type-error (type-error-datum e)
                          (type-error-expected-type e)))))

(defun shac-program-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (program-error () :program-error)))

(deftest shac-merge-checks-fixed-length
  (list (shac-type-error
         (lambda () (merge '(simple-array fixnum (3)) '(1 3) '(2 4) #'<)))
        (shac-type-error
         (lambda ()
           (merge '(simple-array bit (3))
                  (make-array 3 :element-type 'bit :initial-contents '(1 0 1))
                  (make-array 2 :element-type 'bit :initial-contents '(0 1))
                  #'<)))
        (shac-type-error
         (lambda () (merge '(simple-array * (3)) '(1 3) '(2 4) #'<)))
        (shac-type-error
         (lambda () (merge '(simple-array character (3)) "ab" "cd" #'char<))))
  (:type-error :type-error :type-error :type-error))

(deftest shac-merge-accepts-the-required-length
  (list (let ((v (merge '(simple-array fixnum (3)) '(1 3) '(2) #'<)))
          (list (coerce v 'list) (length v)))
        (let ((v (merge '(simple-array fixnum (*)) '(1 3) '(2) #'<)))
          (list (coerce v 'list) (length v))))
  (((1 2 3) 3) ((1 2 3) 3)))

(deftest shac-reverse-rejects-dotted-list
  (list (shac-type-error-detail (lambda () (reverse (cons 1 (cons 2 3)))))
        (shac-type-error-detail (lambda () (nreverse (cons 1 (cons 2 3))))))
  ((:type-error 3 list) (:type-error 3 list)))

(deftest shac-reverse-still-handles-proper-sequences
  (list (reverse '(1 2 3))
        (nreverse (list 1 2 3))
        (reverse "abc")
        (coerce (nreverse (vector 1 2 3)) 'list)
        (reverse #*101)
        (nreverse (make-array 3 :element-type 'bit :initial-contents '(1 0 1))))
  ((3 2 1) (3 2 1) "cba" (3 2 1) #*101 #*101))

(deftest shac-make-hash-table-keyword-errors
  (list (shac-program-error (lambda () (make-hash-table :bogus 1)))
        (shac-program-error (lambda () (make-hash-table :size)))
        (shac-program-error (lambda () (make-hash-table :test 'eq :size)))
        (shac-program-error
         (lambda () (make-hash-table :allow-other-keys nil :bogus 1))))
  (:program-error :program-error :program-error :program-error))

(deftest shac-make-hash-table-allows-other-keys
  (list (hash-table-p (make-hash-table :bogus 1 :allow-other-keys t))
        (hash-table-p (make-hash-table :allow-other-keys t :bogus 1)))
  (t t))

(deftest shac-make-hash-table-checks-argument-types
  (list (shac-type-error (lambda () (make-hash-table :test nil)))
        (shac-type-error (lambda () (make-hash-table :test 5)))
        (shac-type-error (lambda () (make-hash-table :test "equal")))
        (shac-type-error (lambda () (make-hash-table :size -1)))
        (shac-type-error (lambda () (make-hash-table :size 1.5)))
        (shac-type-error (lambda () (make-hash-table :rehash-size 1.0)))
        (shac-type-error (lambda () (make-hash-table :rehash-size 0.5)))
        (shac-type-error (lambda () (make-hash-table :rehash-threshold 2)))
        (shac-type-error (lambda () (make-hash-table :rehash-threshold -0.1))))
  (:type-error :type-error :type-error :type-error :type-error
   :type-error :type-error :type-error :type-error))

(deftest shac-make-hash-table-accepts-valid-arguments
  (list (hash-table-test
         (make-hash-table :test 'equal :size 0 :rehash-size 1
                          :rehash-threshold 0))
        (hash-table-test
         (make-hash-table :test #'eq :rehash-size 1.5 :rehash-threshold 1))
        (hash-table-test
         (make-hash-table :test #'eql :rehash-size 1.5d0
                          :rehash-threshold 1/2)))
  (equal eq eql))
