;;; EQUALP on arrays: same rank and dimensions, active elements EQUALP.
;;; Expected values are what SBCL returns.

(defun %eqpa-fp (contents len &optional (type t))
  (make-array (length contents) :element-type type :initial-contents contents
                                :fill-pointer len))

(defun %eqpa-cases ()
  (list
   (cons #2A((1 2) (3 4)) #(1 2 3 4))
   (cons #2A((1 2) (3 4)) (make-array '(2 2) :initial-contents '((1.0 2) (3 4))))
   (cons (make-array '(2 3) :initial-element 0) (make-array '(3 2) :initial-element 0))
   (cons (make-array '() :initial-element 1) (make-array '() :initial-element 1.0))
   (cons (make-array '() :initial-element 1) #(1))
   (cons (make-array '(1 2) :element-type 'character :initial-contents '((#\a #\b))) "AB")
   (cons (make-array '(1 2) :element-type 'character :initial-contents '((#\a #\b)))
         (make-array '(1 2) :initial-contents '((#\A #\b))))
   (cons "ab" (vector #\A #\b))
   (cons "ab" (vector #\a))
   (cons (%eqpa-fp '(1 2 3) 2) #(1 2))
   (cons (%eqpa-fp '(1 2 3) 2) #(1 2 3))
   (cons (%eqpa-fp "abc" 2 'character) "AB")
   (cons (%eqpa-fp "abc" 2 'character) (vector #\a #\B))
   (cons #*101 #(1 0 1))
   (cons #*101 #(1.0 0 1))
   (cons #*101 #(1 1 1))
   (cons (%eqpa-fp '(1 0 1) 2 'bit) #*10)
   (cons (make-array '(2 2) :element-type 'bit :initial-element 1) #*1111)
   (cons "ab" #(97 98))
   (cons (vector (vector 1 2) "x") (vector (vector 1.0 2) "X"))))

(deftest equalp-arrays.compare
  (mapcar (lambda (p) (list (equalp (car p) (cdr p)) (equalp (cdr p) (car p))
                            (funcall 'equalp (car p) (cdr p))))
          (%eqpa-cases))
  ((nil nil nil) (t t t) (nil nil nil) (t t t) (nil nil nil) (nil nil nil) (t t t)
   (t t t) (nil nil nil) (t t t) (nil nil nil) (t t t) (t t t) (t t t) (t t t)
   (nil nil nil) (t t t) (nil nil nil) (nil nil nil) (t t t)))

(deftest equalp-arrays.hash-table-keys
  (mapcan (lambda (p)
            (when (equalp (car p) (cdr p))
              (let ((h (make-hash-table :test 'equalp)))
                (setf (gethash (car p) h) :v)
                (list (gethash (cdr p) h)))))
          (%eqpa-cases))
  (:v :v :v :v :v :v :v :v :v :v :v))

(deftest equalp-arrays.hash-table-mixed-keys
  (let ((h (make-hash-table :test 'equalp)))
    (loop for k in (list 1 "ab" #\a (list 1 #\a "X") #(1 2) #2A((1 2) (3 4)))
          for v from 0
          do (setf (gethash k h) v))
    (list (hash-table-count h)
          (gethash 1.0 h) (gethash (vector #\A #\B) h) (gethash #\A h)
          (gethash (list 1.0 #\A (vector #\x)) h) (gethash (vector 1.0 2.0) h)
          (gethash #(1 2 3 4) h)
          (gethash (make-array '(2 2) :initial-contents '((1.0 2) (3 4))) h)))
  (6 0 1 2 3 4 nil 5))
