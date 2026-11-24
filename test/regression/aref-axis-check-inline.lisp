;;; The per-axis subscript check of an AREF / (SETF AREF) on an array whose dimensions the
;;; declaration gives is inline IL, not a call of a helper the JIT may leave a call in a very
;;; large method. The values, the error for a subscript out of range (the message names the
;;; axis) and the arrays that do not take this path (displaced, adjustable, with a fill
;;; pointer) must be as before.

(setf dotcl:*save-sil* t)

(defun %aac-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %aac-ref2 (a i j)
  (declare (type (simple-array (unsigned-byte 32) (3 5)) a) (fixnum i j) (optimize (safety 1)))
  (aref a i j))
(defun %aac-set2 (a i j v)
  (declare (type (simple-array (unsigned-byte 32) (3 5)) a) (fixnum i j) (type (unsigned-byte 32) v)
           (optimize (safety 1)))
  (setf (aref a i j) v))
(defun %aac-ref3 (a i j k)
  (declare (type (simple-array t (2 3 4)) a) (fixnum i j k) (optimize (safety 1)))
  (aref a i j k))
(defun %aac-ref2-unsafe (a i j)
  (declare (type (simple-array (unsigned-byte 32) (3 5)) a) (fixnum i j) (optimize (safety 0)))
  (aref a i j))
(defun %aac-ref2-generic (a i j) (aref a i j))

(defun %aac-array2 ()
  (let ((a (make-array '(3 5) :element-type '(unsigned-byte 32))))
    (dotimes (i 3) (dotimes (j 5) (setf (aref a i j) (+ (* 10 i) j))))
    a))

(defun %aac-error (thunk)
  ;; the tree-walk evaluator names the store "(SETF AREF)"; the message after it is the same
  (handler-case (progn (funcall thunk) :no-error)
    (error (e) (let ((m (princ-to-string e)))
                 (if (eql 0 (search "(SETF AREF)" m)) (concatenate 'string "AREF" (subseq m 11)) m)))))

(deftest aref-axis-check-inline.values
  (let ((a (%aac-array2)))
    (list (%aac-ref2 a 0 0) (%aac-ref2 a 2 4) (%aac-ref2 a 1 3)
          (progn (%aac-set2 a 2 4 99) (%aac-ref2 a 2 4))
          (%aac-ref3 (make-array '(2 3 4) :initial-element :x) 1 2 3)
          (%aac-ref2-unsafe a 1 1)))
  (0 24 13 99 :x 11))

(deftest aref-axis-check-inline.out-of-range
  (let ((a (%aac-array2)))
    (list (%aac-error (lambda () (%aac-ref2 a 3 0)))
          (%aac-error (lambda () (%aac-ref2 a 0 5)))
          (%aac-error (lambda () (%aac-ref2 a -1 0)))
          (%aac-error (lambda () (%aac-ref2 a 0 -1)))
          (%aac-error (lambda () (%aac-set2 a 3 0 1)))
          (%aac-error (lambda () (%aac-ref3 (make-array '(2 3 4) :initial-element 0) 0 0 4)))))
  ("AREF: index 3 out of range for axis 0 of size 3"
   "AREF: index 5 out of range for axis 1 of size 5"
   "AREF: index -1 out of range for axis 0 of size 3"
   "AREF: index -1 out of range for axis 1 of size 5"
   "AREF: index 3 out of range for axis 0 of size 3"
   "AREF: index 4 out of range for axis 2 of size 4"))

;; arrays that are not the declared simple array take the generic path, as before
(deftest aref-axis-check-inline.other-arrays
  (let* ((base (make-array 15 :element-type '(unsigned-byte 32) :initial-contents (loop for i below 15 collect i)))
         (disp (make-array '(3 5) :element-type '(unsigned-byte 32) :displaced-to base))
         (adj (make-array '(3 5) :element-type '(unsigned-byte 32) :adjustable t :initial-element 7)))
    (list (%aac-ref2-generic disp 1 2) (%aac-ref2-generic adj 2 4)
          (handler-case (%aac-ref2-generic disp 3 0) (error () :error))))
  (7 7 :error))

(deftest-emitting-only aref-axis-check-inline.no-helper-call
  (list (%aac-count "Runtime.AxisIndexL" #'%aac-ref2)
        (plusp (%aac-count "Runtime.AxisIndexError" #'%aac-ref2))
        (%aac-count "Runtime.AxisIndexError" #'%aac-ref2-unsafe))
  (0 t 0))
