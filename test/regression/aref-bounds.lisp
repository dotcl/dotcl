;;; AREF bounds behavior, read and write.
;;;
;;; Bug: only the read side checked. (setf (aref string i)) went straight to the
;;; string's char[] and (setf (aref a i j)) stored at an unvalidated flat index,
;;; so an out-of-range write surfaced as a raw .NET exception (PROGRAM-ERROR)
;;; instead of the TYPE-ERROR the spec calls for. The multi-dimensional paths
;;; also validated only the flattened index, so a subscript past its own axis
;;; that still landed inside the storage silently hit a neighbouring element --
;;; on a 2x3 array, (aref a 0 5) returned the element at row 1.
;;;
;;; Fix: check each subscript against its own axis on both sides, check string
;;; writes, and signal TYPE-ERROR (datum = the index, expected type
;;; (INTEGER 0 (LIMIT))) where a bare SIMPLE-ERROR was used before.

(defun %ab-outcome (thunk)
  "Class of condition FUNCALLing THUNK signals, coarse enough to be stable."
  (handler-case (progn (funcall thunk) :no-error)
    (program-error () :program-error)
    (type-error () :type-error)
    (error (e) (list :other (type-of e)))))

(defun %ab-detail (thunk)
  "TYPE-ERROR slots: the datum must be the offending index and the expected
type the range it had to fall in."
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e)
      (list (type-error-datum e)
            (let ((et (type-error-expected-type e)))
              (and (consp et) (eq (first et) 'integer)))))
    (error (e) (list :other (type-of e)))))

;;; ---- 1D writes ----

(deftest aref-bounds.vector-write-past-end
  (%ab-outcome (lambda () (setf (aref (make-array 3 :initial-element 0) 5) 9)))
  :type-error)

(deftest aref-bounds.vector-write-negative
  (%ab-outcome (lambda () (setf (aref (make-array 3 :initial-element 0) -1) 9)))
  :type-error)

(deftest aref-bounds.vector-write-datum
  (%ab-detail (lambda () (setf (aref (make-array 3 :initial-element 0) 5) 9)))
  (5 t))

;; A string write is the case with no fast path at all: it used to index the
;; underlying char[] directly.
(deftest aref-bounds.string-write-past-end
  (%ab-outcome (lambda () (setf (aref (copy-seq "abc") 7) #\x)))
  :type-error)

(deftest aref-bounds.string-write-negative
  (%ab-outcome (lambda () (setf (aref (copy-seq "abc") -1) #\x)))
  :type-error)

(deftest aref-bounds.string-read-and-write-in-range
  (let ((s (copy-seq "abc")))
    (setf (aref s 2) #\z)
    (list s (aref s 0)))
  ("abz" #\a))

;;; ---- per-axis checking on multi-dimensional arrays ----

;; 2x3: subscript 5 on axis 1 flattens to 5, which is a valid storage offset.
(deftest aref-bounds.2d-read-past-axis
  (%ab-outcome (lambda () (aref (make-array '(2 3) :initial-element 0) 0 5)))
  :type-error)

(deftest aref-bounds.2d-write-past-axis
  (%ab-outcome (lambda () (setf (aref (make-array '(2 3) :initial-element 0) 0 5) 9)))
  :type-error)

;; ...and the neighbouring element must be untouched by the failed write.
(deftest aref-bounds.2d-write-past-axis-stores-nothing
  (let ((a (make-array '(2 3) :initial-element 0)))
    (ignore-errors (setf (aref a 0 5) 9))
    (list (aref a 1 2) (aref a 1 0)))
  (0 0))

(deftest aref-bounds.2d-write-past-end
  (%ab-outcome (lambda () (setf (aref (make-array '(2 3) :initial-element 0) 2 0) 9)))
  :type-error)

(deftest aref-bounds.3d-write-past-axis
  (%ab-outcome (lambda () (setf (aref (make-array '(2 2 2) :initial-element 0) 0 3 0) 9)))
  :type-error)

(deftest aref-bounds.3d-read-past-axis
  (%ab-outcome (lambda () (aref (make-array '(2 2 2) :initial-element 0) 0 3 0)))
  :type-error)

;; Element type with unboxed storage takes the same route.
(deftest aref-bounds.2d-fixnum-array-write-past-axis
  (%ab-outcome (lambda ()
                 (setf (aref (make-array '(2 3) :element-type '(unsigned-byte 8)
                                                :initial-element 0)
                             0 5)
                       9)))
  :type-error)

;;; ---- wrong number of subscripts ----

(deftest aref-bounds.rank-1-with-two-subscripts
  (%ab-outcome (lambda () (aref (make-array 3 :initial-element 0) 1 2)))
  :program-error)

(deftest aref-bounds.rank-1-write-with-two-subscripts
  (%ab-outcome (lambda () (setf (aref (make-array 3 :initial-element 0) 1 2) 9)))
  :program-error)

(deftest aref-bounds.rank-2-with-one-subscript
  (%ab-outcome (lambda () (aref (make-array '(2 3) :initial-element 0) 1)))
  :program-error)

;;; ---- non-integer subscripts and dimensions ----
;;; A bare C# cast used to raise InvalidCastException here, which the CLR
;;; exception mapping turns into PROGRAM-ERROR.

(deftest aref-bounds.non-integer-subscript-1d
  (%ab-outcome (lambda () (aref (make-array 3 :initial-element 0) 1.5)))
  :type-error)

(deftest aref-bounds.non-integer-subscript-2d
  (%ab-outcome (lambda () (aref (make-array '(2 3) :initial-element 0) 0 1.5)))
  :type-error)

(deftest aref-bounds.non-integer-subscript-string-write
  (%ab-outcome (lambda () (setf (aref (copy-seq "abc") 1.5) #\x)))
  :type-error)

(deftest aref-bounds.negative-dimension
  (%ab-outcome (lambda () (make-array -1)))
  :type-error)

(deftest aref-bounds.negative-dimension-in-list
  (%ab-outcome (lambda () (make-array '(2 -1))))
  :type-error)

(deftest aref-bounds.non-integer-dimension
  (%ab-outcome (lambda () (make-array '(2 x))))
  :type-error)

(deftest aref-bounds.adjust-array-negative-dimension
  (%ab-outcome (lambda ()
                 (adjust-array (make-array 3 :adjustable t :initial-element 0) -1)))
  :type-error)

;;; ---- in-range accesses keep working ----

(deftest aref-bounds.2d-in-range
  (let ((a (make-array '(2 3) :initial-element 0)))
    (setf (aref a 1 2) 'x)
    (setf (aref a 0 0) 'y)
    (list (aref a 1 2) (aref a 0 0) (aref a 0 1)))
  (x y 0))

(deftest aref-bounds.3d-in-range
  (let ((a (make-array '(2 2 2) :initial-element 0)))
    (setf (aref a 1 0 1) 7)
    (list (aref a 1 0 1) (aref a 0 1 0)))
  (7 0))

;; AREF addresses the whole array, fill pointer or not.
(deftest aref-bounds.fill-pointer-reaches-storage
  (let ((v (make-array 5 :fill-pointer 2 :initial-element 'a)))
    (setf (aref v 4) 'z)
    (list (aref v 4) (%ab-outcome (lambda () (setf (aref v 5) 'q)))))
  (z :type-error))

;; Displaced arrays index through to the base array, and their own dimensions
;; bound the check.
(deftest aref-bounds.displaced
  (let* ((base (make-array 10 :initial-element 0))
         (d (make-array 4 :displaced-to base :displaced-index-offset 2)))
    (setf (aref d 3) 'x)
    (list (aref base 5) (%ab-outcome (lambda () (setf (aref d 4) 'q)))))
  (x :type-error))
