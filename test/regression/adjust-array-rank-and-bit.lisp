;;; ADJUST-ARRAY keeps the rank of the array (CLHS 15.2), and a bit array
;;; stays a bit array when it grows, with the new bits from :INITIAL-ELEMENT
;;; or 0. Both are checked on simple, adjustable and displaced arrays, with
;;; fill pointers, and on rank 0.

(defun %arab-error-p (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (error () :error)))

(defun %arab-list (a)
  (loop for i below (array-total-size a) collect (row-major-aref a i)))

(defun %arab-bit-p (a)
  (and (every (lambda (x) (typep x 'bit)) (%arab-list a)) t))

(deftest adjust-array-rank-and-bit.rank-change-simple
  (list (%arab-error-p (lambda () (adjust-array (make-array 3 :initial-element 0) '(2 3))))
        (%arab-error-p (lambda () (adjust-array (make-array '(2 3) :initial-element 0) 6)))
        (%arab-error-p (lambda () (adjust-array (make-array nil) '(1))))
        (%arab-error-p (lambda () (adjust-array (make-array 3) nil))))
  (:error :error :error :error))

(deftest adjust-array-rank-and-bit.rank-change-adjustable
  (let ((a (make-array 3 :adjustable t :initial-element 0))
        (b (make-array 3 :adjustable t :fill-pointer 2 :initial-element 0))
        (c (make-array 2 :displaced-to (make-array 4))))
    (list (%arab-error-p (lambda () (adjust-array a '(2 3))))
          (%arab-error-p (lambda () (adjust-array b '(2 3))))
          (%arab-error-p (lambda () (adjust-array c '(2 2))))
          (array-rank a) (array-dimensions a) (fill-pointer b)
          (and (array-displacement c) t)))
  (:error :error :error 1 (3) 2 t))

(deftest adjust-array-rank-and-bit.rank-change-bit
  (let ((a (make-array 3 :element-type 'bit :initial-element 0))
        (b (make-array 3 :element-type 'bit :adjustable t :initial-element 0))
        (c (make-array 2 :element-type 'bit
                       :displaced-to (make-array 6 :element-type 'bit
                                                 :initial-element 0))))
    (list (%arab-error-p (lambda () (adjust-array a '(2 3))))
          (%arab-error-p (lambda () (adjust-array b '(2 3))))
          (%arab-error-p (lambda () (adjust-array c '(2 2))))
          (array-rank a) (array-dimensions a) (array-element-type c)))
  (:error :error :error 1 (3) bit))

(deftest adjust-array-rank-and-bit.same-rank
  (let* ((a (make-array 3 :initial-contents '(1 2 3)))
         (b (adjust-array a '(4) :initial-element 0))
         (c (make-array '(2 2) :initial-contents '((1 2) (3 4))))
         (d (adjust-array c '(1 4) :initial-element 0))
         (e (make-array nil :initial-element 7))
         (f (adjust-array e nil)))
    (list (array-rank b) (array-dimensions b) (%arab-list b)
          (array-rank d) (array-dimensions d) (%arab-list d)
          (array-rank f) (array-dimensions f) (aref f)))
  (1 (4) (1 2 3 0) 2 (1 4) (1 2 0 0) 0 nil 7))

(deftest adjust-array-rank-and-bit.grow-simple
  (let* ((a (make-array 3 :element-type 'bit :initial-contents '(1 0 1)))
         (b (adjust-array a 5))
         (c (make-array 3 :element-type 'bit :initial-contents '(1 0 1)))
         (d (adjust-array c 5 :initial-element 1)))
    (list (array-element-type b) (array-dimensions b) (%arab-list b)
          (%arab-bit-p b) (typep (aref b 4) 'bit)
          (array-element-type d) (%arab-list d)
          (%arab-bit-p d) (typep (aref d 4) 'bit)))
  (bit (5) (1 0 1 0 0) t t
       bit (1 0 1 1 1) t t))

(deftest adjust-array-rank-and-bit.shrink-simple
  (let ((b (adjust-array
            (make-array 5 :element-type 'bit :initial-contents '(1 0 1 1 0))
            2)))
    (list (array-element-type b) (%arab-list b) (%arab-bit-p b)))
  (bit (1 0) t))

(deftest adjust-array-rank-and-bit.grow-adjustable-fill-pointer
  (let* ((a (make-array 3 :element-type 'bit :adjustable t :fill-pointer 2
                          :initial-contents '(1 0 1)))
         (b (adjust-array a 5 :initial-element 1))
         (c (make-array 3 :element-type 'bit :adjustable t
                          :initial-contents '(1 0 1)))
         (d (adjust-array c 6)))
    (list (eq a b) (array-element-type b) (fill-pointer b)
          (array-dimensions b)
          (progn (setf (fill-pointer b) 5) (%arab-list b))
          (%arab-list d) (%arab-bit-p d)))
  (t bit 2 (5) (1 0 1 1 1) (1 0 1 0 0 0) t))

(deftest adjust-array-rank-and-bit.grow-displaced
  (let* ((source (make-array 6 :element-type 'bit
                               :initial-contents '(1 0 1 1 0 1)))
         (a (make-array 2 :element-type 'bit :displaced-to source
                          :displaced-index-offset 1))
         (b (adjust-array a 4))
         (c (make-array 2 :element-type 'bit :displaced-to source
                          :displaced-index-offset 1))
         (d (adjust-array c 4 :initial-element 1)))
    (list (array-element-type b) (%arab-list b) (%arab-bit-p b)
          (array-displacement b)
          (array-element-type d) (%arab-list d) (%arab-bit-p d)))
  (bit (0 1 0 0) t nil
       bit (0 1 1 1) t))

(deftest adjust-array-rank-and-bit.grow-multidim
  (let* ((a (make-array '(2 2) :element-type 'bit
                          :initial-contents '((1 0) (1 1))))
         (b (adjust-array a '(3 2) :initial-element 0))
         (c (make-array '(2 2) :element-type 'bit :adjustable t
                          :initial-contents '((1 0) (1 1))))
         (d (adjust-array c '(2 3) :initial-element 1)))
    (list (array-rank b) (array-dimensions b) (%arab-list b) (%arab-bit-p b)
          (array-rank d) (array-dimensions d) (%arab-list d) (%arab-bit-p d)))
  (2 (3 2) (1 0 1 1 0 0) t
   2 (2 3) (1 0 1 1 1 1) t))

(deftest adjust-array-rank-and-bit.bad-initial-element
  (let* ((a (make-array 3 :element-type 'bit :adjustable t :initial-element 0))
         (b (make-array 3 :element-type 'bit :initial-element 0))
         (c (make-array 2 :element-type 'bit
                          :displaced-to (make-array 4 :element-type 'bit
                                                    :initial-element 0))))
    (list (%arab-error-p (lambda () (adjust-array a 5 :initial-element 2)))
          (%arab-error-p (lambda () (adjust-array a 5 :initial-element 'x)))
          (%arab-error-p (lambda () (adjust-array a 5 :initial-element nil)))
          (%arab-error-p (lambda () (adjust-array b 5 :initial-element 2)))
          (%arab-error-p (lambda () (adjust-array c 4 :initial-element 2)))
          (array-dimensions a) (%arab-list a)))
  (:error :error :error :error :error (3) (0 0 0)))
