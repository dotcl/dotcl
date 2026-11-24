;;; A parameter declared (simple-array (unsigned-byte 64) (*)) -- or 8 or 16 bits -- is fetched
;;; once, at entry, with the entry that returns the array or signals; an access is the bare
;;; element load with no test for a missing buffer. That holds at (safety 0) too. The results
;;; must be those of the generic (undeclared) code, a value that is not the declared array is a
;;; TYPE-ERROR at entry, and a subscript outside the array is an error, not another element.

(defun %abnn-u64-0 (x i)
  (declare (type (simple-array (unsigned-byte 64) (*)) x) (type fixnum i)
           (optimize (speed 3) (safety 0)))
  (aref x i))
(defun %abnn-u64-1 (x i)
  (declare (type (simple-array (unsigned-byte 64) (*)) x) (type fixnum i))
  (aref x i))
(defun %abnn-u8-0 (x i)
  (declare (type (simple-array (unsigned-byte 8) (*)) x) (type fixnum i)
           (optimize (speed 3) (safety 0)))
  (aref x i))
(defun %abnn-u16-0 (x i)
  (declare (type (simple-array (unsigned-byte 16) (*)) x) (type fixnum i)
           (optimize (speed 3) (safety 0)))
  (aref x i))
(defun %abnn-generic (x i) (aref x i))

;;; fixed index, fixed length, a store and a read after it
(defun %abnn-swap (x)
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (optimize (speed 3) (safety 0)))
  (let ((a (aref x 0)) (b (aref x 3)))
    (setf (aref x 0) b (aref x 3) a)
    (logxor (aref x 0) (aref x 3))))

(defun %abnn-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest array-backing-never-null.values-match-generic
  (let ((u64 (make-array 4 :element-type '(unsigned-byte 64)
                           :initial-contents '(0 1 #xFFFFFFFFFFFFFFFF #x8000000000000000)))
        (u8 (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 1 254 255)))
        (u16 (make-array 4 :element-type '(unsigned-byte 16) :initial-contents '(0 1 65534 65535))))
    (list (loop for i below 4 collect (list (= (%abnn-u64-0 u64 i) (%abnn-generic u64 i))
                                            (= (%abnn-u64-1 u64 i) (%abnn-generic u64 i))))
          (loop for i below 4 always (= (%abnn-u8-0 u8 i) (%abnn-generic u8 i)))
          (loop for i below 4 always (= (%abnn-u16-0 u16 i) (%abnn-generic u16 i)))))
  (((t t) (t t) (t t) (t t)) t t))

(deftest array-backing-never-null.subscript-bounds
  (let ((u64 (make-array 4 :element-type '(unsigned-byte 64) :initial-element 7)))
    (list (%abnn-error-p (lambda () (%abnn-u64-0 u64 4)))
          (%abnn-error-p (lambda () (%abnn-u64-0 u64 -1)))
          (%abnn-error-p (lambda () (%abnn-u64-0 u64 most-positive-fixnum)))
          (%abnn-error-p (lambda () (%abnn-u64-0 u64 most-negative-fixnum)))
          (%abnn-error-p (lambda () (%abnn-u64-1 u64 4)))
          (%abnn-error-p (lambda () (%abnn-u64-0 u64 3)))))
  (t t t t t nil))

(deftest-emitting-only array-backing-never-null.not-the-declared-array
  (let ((fp (make-array 4 :element-type '(unsigned-byte 64) :fill-pointer 4 :initial-element 1))
        (adj (make-array 4 :element-type '(unsigned-byte 64) :adjustable t :initial-element 1))
        (wrong (make-array 4 :element-type '(unsigned-byte 8) :initial-element 1)))
    (list (%abnn-error-p (lambda () (%abnn-u64-0 fp 0)))
          (%abnn-error-p (lambda () (%abnn-u64-0 adj 0)))
          (%abnn-error-p (lambda () (%abnn-u64-0 wrong 0)))
          (%abnn-error-p (lambda () (%abnn-u64-0 "abcd" 0)))
          (%abnn-error-p (lambda () (%abnn-u64-0 nil 0)))
          (%abnn-error-p (lambda () (%abnn-u64-1 fp 0)))
          (%abnn-error-p (lambda () (%abnn-u8-0 adj 0)))))
  (t t t t t t t))

(deftest array-backing-never-null.constant-index-store
  (let ((x (make-array 4 :element-type '(unsigned-byte 64) :initial-contents '(10 20 30 40))))
    (list (%abnn-swap x) (coerce x 'list)))
  (34 (40 20 30 10)))
