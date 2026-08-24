;;; Reading an element of a specialized array unboxed must not depend on the
;;; SUBSCRIPT being statically fixnum-typed: how the element is stored is a
;;; property of the array. It used to depend on it, so the ordinary undeclared
;;; loop counter -- (do ((i 0 (1+ i))) ...) -- put the whole access on the
;;; generic path and boxed every element the loop touched (72 B an iteration in
;;; the fft benchmark's inner loops).
;;;
;;; The subscript now lowers through Runtime.IndexL when its type is not known,
;;; so these tests pin what that has to keep: the same values, and the same
;;; error for a subscript that is not an integer.

(defvar *uai-single* (make-array 8 :element-type 'single-float :initial-element 0.0))
(defvar *uai-double* (make-array 8 :element-type 'double-float :initial-element 0.0d0))
(defvar *uai-fix* (make-array 8 :element-type 'fixnum :initial-element 0))
(defvar *uai-2d* (make-array '(3 3) :element-type 'double-float :initial-element 0.0d0))

;;; No declaration on I anywhere in these.
(defun uai-sum-single (a n)
  (let ((s 0.0))
    (declare (single-float s))
    (do ((i 0 (1+ i))) ((>= i n) s) (setq s (+ s (aref a i))))))

(defun uai-sum-double (a n)
  (let ((s 0.0d0))
    (declare (double-float s))
    (do ((i 0 (1+ i))) ((>= i n) s) (setq s (+ s (aref a i))))))

(defun uai-sum-fix (a n)
  (let ((s 0))
    (declare (fixnum s))
    (do ((i 0 (1+ i))) ((>= i n) s) (setq s (+ s (aref a i))))))

(defun uai-scale (a n)
  (do ((i 0 (1+ i))) ((>= i n) a)
    (setf (aref a i) (* 2.0 (aref a i)))))

(deftest unboxed-aref-any-index.single-float
  (progn (dotimes (i 8) (setf (aref *uai-single* i) (float (1+ i) 1.0)))
         (list (uai-sum-single *uai-single* 8)
               (coerce (uai-scale *uai-single* 8) 'list)))
  (36.0 (2.0 4.0 6.0 8.0 10.0 12.0 14.0 16.0)))

(deftest unboxed-aref-any-index.double-float
  (progn (dotimes (i 8) (setf (aref *uai-double* i) (float (1+ i) 1.0d0)))
         (uai-sum-double *uai-double* 8))
  36.0d0)

(deftest unboxed-aref-any-index.fixnum
  (progn (dotimes (i 8) (setf (aref *uai-fix* i) (* (1+ i) 3)))
         (uai-sum-fix *uai-fix* 8))
  108)

;;; Two dimensions, both subscripts undeclared.
(defun uai-2d-trace (a n)
  (let ((s 0.0d0))
    (declare (double-float s))
    (do ((i 0 (1+ i))) ((>= i n) s) (setq s (+ s (aref a i i))))))

(deftest unboxed-aref-any-index.two-dimensional
  (progn (dotimes (i 3) (dotimes (j 3) (setf (aref *uai-2d* i j) (float (+ (* i 3) j) 1.0d0))))
         (uai-2d-trace *uai-2d* 3))
  12.0d0)

;;; A subscript that is not an integer is still a TYPE-ERROR, not a raw .NET
;;; cast failure escaping as something else.
(defun uai-bad-read (a i) (aref a i))
(defun uai-bad-write (a i) (setf (aref a i) 1.0))

(deftest unboxed-aref-any-index.non-integer-subscript-signals-type-error
  (list (handler-case (progn (uai-bad-read *uai-single* 'x) :no-error)
          (type-error () :type-error)
          (error () :other-error))
        (handler-case (progn (uai-bad-write *uai-single* "s") :no-error)
          (type-error () :type-error)
          (error () :other-error)))
  (:type-error :type-error))

;;; An out-of-bounds subscript still signals, and the array is untouched.
(deftest unboxed-aref-any-index.out-of-bounds-signals
  (list (handler-case (progn (uai-bad-read *uai-single* 99) :no-error)
          (error () :error))
        (aref *uai-single* 0))
  (:error 2.0))
