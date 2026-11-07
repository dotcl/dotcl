;;; A LET variable whose init is a global with a proclaimed SIMPLE-ARRAY type, or a
;;; structure slot declared with one, is read as that array type. The values read
;;; must be what the array holds, also when the declaration is not true of the value.

(defvar *lda-tbl* (let ((a (make-array 8 :element-type '(unsigned-byte 32))))
                    (dotimes (i 8 a) (setf (aref a i) (* i 1000003)))))
(declaim (type (simple-array (unsigned-byte 32) (8)) *lda-tbl*))
(defstruct lda-st (table *lda-tbl* :type (simple-array (unsigned-byte 32) (8))) (n 0 :type fixnum))

(defun %lda-sum-global (n)
  (declare (fixnum n))
  (let ((table *lda-tbl*) (s 0))
    (declare (type (unsigned-byte 62) s))
    (dotimes (i n s) (incf s (aref table (logand i 7))))))

(defun %lda-sum-slot (st n)
  (declare (type lda-st st) (fixnum n))
  (let ((s 0))
    (declare (type (unsigned-byte 62) s))
    (do ((i 0 (1+ i)) (table (lda-st-table st))) ((>= i n) s)
      (incf s (aref table (logand i 7))))))

(defun %lda-set (v) (setf *lda-tbl* v))

(deftest let-declared-array-init.values
  (list (%lda-sum-global 16) (%lda-sum-slot (make-lda-st) 16)
        (let ((st (make-lda-st)))
          (setf (aref (lda-st-table st) 3) 7)
          (list (%lda-sum-slot st 8) (aref *lda-tbl* 3))))
  (56000168 56000168 (25000082 7)))

(deftest let-declared-array-init.false-declaration
  ;; the proclamation is wrong for these values: reads still give the array's contents
  (let ((old *lda-tbl*))
    (unwind-protect
         (list (progn (%lda-set (make-array 8 :element-type '(unsigned-byte 8) :initial-element 3))
                      (%lda-sum-global 8))
               (progn (%lda-set (make-array 8 :initial-element 5)) (%lda-sum-global 8))
               (progn (%lda-set (make-array 16 :element-type '(unsigned-byte 32) :initial-element 2
                                               :fill-pointer 16 :adjustable t))
                      (%lda-sum-global 8)))
      (%lda-set old)))
  (24 40 16))

;;; A call of a global function with a declaimed FTYPE answering an array type.
(declaim (ftype (function (t) (simple-array (unsigned-byte 32) (8))) %lda-pick))
(defun %lda-pick (x) (if x *lda-tbl* (make-array 8 :element-type '(unsigned-byte 32) :initial-element 4)))

(defun %lda-sum-call (x n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (type (unsigned-byte 62) s))
    (do ((i 0 (1+ i)) (table (%lda-pick x))) ((>= i n) s)
      (incf s (aref table (logand i 7))))))

(deftest let-declared-array-init.ftype-call
  (list (= (%lda-sum-call t 8) (loop for i below 8 sum (aref *lda-tbl* i)))
        (%lda-sum-call nil 16))
  (t 64))
