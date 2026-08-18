;;; CHAR / SCHAR / ROW-MAJOR-AREF / (setf fill-pointer) validate their index.
;;;
;;; Bug: all four indexed the storage directly. (char "abc" 9) reached the
;;; char[] and raised IndexOutOfRangeException, which the CLR-exception mapping
;;; reports as PROGRAM-ERROR; a non-integer index to CHAR was quietly turned
;;; into -1 first. (setf (fill-pointer v) 9) stored the out-of-range value and
;;; left the vector to fail later, somewhere else.

(defun %sib-outcome (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list :type-error (type-error-datum e)))
    (error (e) (list :other (type-of e)))))

(deftest string-index-bounds.char-past-end
  (%sib-outcome (lambda () (char "abc" 9)))
  (:type-error 9))

(deftest string-index-bounds.char-negative
  (%sib-outcome (lambda () (char "abc" -1)))
  (:type-error -1))

(deftest string-index-bounds.char-non-integer
  (%sib-outcome (lambda () (char "abc" 'x)))
  (:type-error x))

(deftest string-index-bounds.schar-past-end
  (%sib-outcome (lambda () (schar "abc" 9)))
  (:type-error 9))

(deftest string-index-bounds.set-char-past-end
  (%sib-outcome (lambda () (setf (char (copy-seq "abc") 9) #\x)))
  (:type-error 9))

(deftest string-index-bounds.row-major-aref-past-end
  (%sib-outcome (lambda () (row-major-aref (make-array '(2 2) :initial-element 0) 9)))
  (:type-error 9))

(deftest string-index-bounds.set-row-major-aref-past-end
  (%sib-outcome (lambda ()
                  (setf (row-major-aref (make-array '(2 2) :initial-element 0) 9) 1)))
  (:type-error 9))

(deftest string-index-bounds.fill-pointer-past-size
  (%sib-outcome (lambda () (setf (fill-pointer (make-array 3 :fill-pointer 0)) 9)))
  (:type-error 9))

(deftest string-index-bounds.fill-pointer-negative
  (%sib-outcome (lambda () (setf (fill-pointer (make-array 3 :fill-pointer 0)) -1)))
  (:type-error -1))

;;; In-range uses are untouched, including the boundary cases: CHAR addresses
;;; the whole string past a fill pointer, and a fill pointer may equal the size.

(deftest string-index-bounds.char-in-range
  (list (char "abc" 0) (char "abc" 2) (schar "abc" 1))
  (#\a #\c #\b))

(deftest string-index-bounds.char-reaches-past-fill-pointer
  (let ((v (make-array 5 :element-type 'character :fill-pointer 2
                         :initial-element #\z)))
    (char v 4))
  #\z)

(deftest string-index-bounds.fill-pointer-may-equal-size
  (let ((v (make-array 3 :fill-pointer 0 :initial-element 0)))
    (setf (fill-pointer v) 3)
    (list (fill-pointer v) (length v)))
  (3 3))

(deftest string-index-bounds.row-major-aref-in-range
  (let ((a (make-array '(2 2) :initial-element 0)))
    (setf (row-major-aref a 3) 'x)
    (list (row-major-aref a 3) (aref a 1 1)))
  (x x))
