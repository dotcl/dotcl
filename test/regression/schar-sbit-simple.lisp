;;; (SETF SBIT) takes a simple bit array (CLHS), as SBIT does: an adjustable one is a TYPE-ERROR,
;;; while BIT accepts it. (SCHAR is left as it is: dotcl reads and writes a string with a fill
;;; pointer or an adjustable one through SCHAR as it does through CHAR, which other tests rely on.)

(defun %ssb-type-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (type-error () t)))

(deftest schar-sbit-simple.sbit-setf-rejects-non-simple
  (let ((adj (make-array 3 :element-type 'bit :adjustable t :initial-element 0))
        (simple (make-array 3 :element-type 'bit :initial-element 0)))
    (setf (sbit simple 1) 1)
    (setf (bit adj 2) 1)
    (list (%ssb-type-error-p (lambda () (setf (sbit adj 1) 1)))
          (%ssb-type-error-p (lambda () (incf (sbit adj 1))))
          simple adj (incf (sbit simple 0))))
  (t t #*110 #*001 1))
