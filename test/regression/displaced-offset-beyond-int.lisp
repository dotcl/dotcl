;;; A :DISPLACED-INDEX-OFFSET that does not fit in 32 bits is an error, not an offset
;;; wrapped into range.

(defun %dob-rejected (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest displaced-offset-beyond-int.make-array
  (let ((target (make-array 6 :initial-contents '(0 1 2 3 4 5))))
    (list (mapcar (lambda (off) (%dob-rejected (lambda () (make-array 2 :displaced-to target :displaced-index-offset off))))
                  (list 5 7 -1 (expt 2 31) (expt 2 32) (+ (expt 2 32) 3) (+ (expt 2 40) 1) (expt 2 62) (expt 2 64)))
          (let ((a (make-array 2 :displaced-to target :displaced-index-offset 3)))
            (list (aref a 0) (aref a 1)))))
  ((t t t t t t t t t) (3 4)))

(deftest displaced-offset-beyond-int.adjust-array
  (let* ((target (make-array 6 :initial-contents '(0 1 2 3 4 5)))
         (a (make-array 2 :adjustable t :initial-element 9)))
    (list (mapcar (lambda (off) (%dob-rejected (lambda () (adjust-array a 2 :displaced-to target :displaced-index-offset off))))
                  (list 5 -1 (expt 2 31) (expt 2 32) (+ (expt 2 32) 3) (+ (expt 2 40) 1) (expt 2 64)))
          (progn (adjust-array a 2 :displaced-to target :displaced-index-offset 2) (list (aref a 0) (aref a 1)))))
  ((t t t t t t t) (2 3)))
