;;; MAKE-HASH-TABLE :HASH-FUNCTION (the SBCL extension, also in CCL, Allegro
;;; and LispWorks): any two-argument predicate as :TEST, given as a function or
;;; its name, hashed by the given function. With a standard test the hash
;;; function only replaces the hashing. Expected values are SBCL 2.6.8's.

(defun %hthf-labels-hash (labels) (sxhash labels))
(defun %hthf-labels-equal (a b) (every #'equal a b))

(deftest hash-table-hash-function.function-test
  (let ((h (make-hash-table :test #'%hthf-labels-equal :hash-function #'%hthf-labels-hash)))
    (setf (gethash (list "a" "b") h) 1)
    (setf (gethash (list "a" "b") h) 2)
    (setf (gethash (list "c") h) 3)
    (list (gethash (list "a" "b") h) (gethash (list "c") h)
          (multiple-value-list (gethash (list "z") h))
          (hash-table-count h) (hash-table-test h)))
  (2 3 (nil nil) 2 %hthf-labels-equal))

(deftest hash-table-hash-function.named-test
  (let ((h (make-hash-table :test '%hthf-labels-equal :hash-function '%hthf-labels-hash)))
    (setf (gethash (list 1 2) h) :x)
    (list (gethash (list 1 2) h) (remhash (list 1 2) h) (hash-table-count h)
          (hash-table-test h)))
  (:x t 0 %hthf-labels-equal))

(deftest hash-table-hash-function.anonymous
  (let ((h (make-hash-table :test (lambda (a b) (string-equal a b))
                            :hash-function (lambda (s) (sxhash (string-downcase s))))))
    (setf (gethash "Foo" h) 1)
    (list (gethash "FOO" h) (gethash "foo" h) (functionp (hash-table-test h))))
  (1 1 t))

;; A standard test keeps its comparison; the hash function only hashes, here
;; putting every key in one bucket.
(deftest hash-table-hash-function.standard-test
  (let ((h (make-hash-table :test #'equal :hash-function (lambda (x) (declare (ignore x)) 7))))
    (setf (gethash (list 1) h) :one (gethash (list 2) h) :two)
    (list (gethash (list 1) h) (gethash (list 2) h) (hash-table-count h) (hash-table-test h)))
  (:one :two 2 equal))

;; Without :HASH-FUNCTION a test other than the standard four is still an error.
(deftest hash-table-hash-function.required
  (handler-case (hash-table-count (make-hash-table :test #'%hthf-labels-equal))
    (error () :error))
  :error)

;; MAPHASH and a :SYNCHRONIZED table.
(deftest hash-table-hash-function.maphash-synchronized
  (let ((h (make-hash-table :test #'%hthf-labels-equal :hash-function #'%hthf-labels-hash
                            :synchronized t))
        (keys '()))
    (setf (gethash (list "a") h) 1 (gethash (list "b") h) 2)
    (maphash (lambda (k v) (declare (ignore v)) (push (car k) keys)) h)
    (sort keys #'string<))
  ("a" "b"))
