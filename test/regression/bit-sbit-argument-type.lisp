;;; BIT takes a bit array and SBIT a simple bit array (CLHS): a string, a general vector,
;;; or (for SBIT) an adjustable bit vector is a TYPE-ERROR, not an element read. The
;;; check is in the function entries; a call the compiler turns into an inline array
;;; access keeps its fast path unchecked. Valid arguments are unchanged.

(defun %bsat (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(deftest bit-sbit-argument-type.not-a-bit-array
  (list (%bsat (lambda () (bit "abc" 0)))
        (%bsat (lambda () (sbit #(1 0) 0)))
        (%bsat (lambda () (funcall 'bit (list 1 0) 0)))
        (%bsat (lambda () (funcall 'sbit "abc" 0))))
  (:type-error :type-error :type-error :type-error))

(deftest bit-sbit-argument-type.sbit-needs-a-simple-array
  (let ((adj (make-array 3 :element-type 'bit :adjustable t :initial-element 1)))
    (list (let ((f (symbol-function 'sbit))) (%bsat (lambda () (funcall f adj 1))))
          (%bsat (lambda () (funcall 'bit adj 1)))))
  (:type-error 1))

(deftest bit-sbit-argument-type.valid-arguments
  (let ((simple (make-array 3 :element-type 'bit :initial-element 1))
        (two (make-array '(2 2) :element-type 'bit :initial-element 1)))
    (list (bit simple 0) (sbit simple 1) (bit two 1 1) (sbit two 0 1)
          (progn (setf (sbit simple 2) 0) (bit simple 2))
          (funcall 'sbit simple 0)
          (bit #*101 2)))
  (1 1 1 1 0 1 1))
