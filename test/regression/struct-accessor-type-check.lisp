;;; A DEFSTRUCT accessor applied to a structure of an unrelated type signals
;;; TYPE-ERROR (at the default safety) instead of reading or writing the slot
;;; at the same position of the other structure. A structure that :INCLUDEs
;;; the accessor's type is accepted.

(defstruct satc-a x)
(defstruct satc-b y)
(defstruct (satc-c (:include satc-a)) z)
(defstruct satc-n (i 0 :type fixnum) (d 0d0 :type double-float))
(defstruct satc-m (j 0 :type fixnum) (e 0d0 :type double-float))

(defun %satc-read (o) (satc-a-x o))
(defun %satc-write (o v) (setf (satc-a-x o) v))
(defun %satc-read-i (o) (the fixnum (+ 1 (satc-n-i o))))
(defun %satc-read-d (o) (+ 1d0 (satc-n-d o)))
(defun %satc-write-i (o v) (declare (fixnum v)) (setf (satc-n-i o) v) nil)
(defun %satc-write-d (o v) (declare (double-float v)) (setf (satc-n-d o) v) nil)

(defmacro %satc-err (form)
  `(handler-case (progn ,form :no-error)
     (type-error (e) (list :type-error (type-error-expected-type e)))
     (error (e) (type-of e))))

(deftest struct-accessor-type-check.read
  (list (%satc-err (%satc-read (make-satc-b :y 1)))
        (%satc-err (satc-a-x (make-satc-b :y 2)))
        (%satc-err (funcall 'satc-a-x (make-satc-b :y 3)))
        (%satc-err (%satc-read (list 1 2))))
  ((:type-error satc-a) (:type-error satc-a) (:type-error satc-a)
   (:type-error satc-a)))

(deftest struct-accessor-type-check.write
  (let ((b (make-satc-b :y 1)))
    (list (%satc-err (%satc-write b 5))
          (%satc-err (funcall #'(setf satc-a-x) 6 b))
          (satc-b-y b)))
  ((:type-error satc-a) (:type-error satc-a) 1))

(deftest struct-accessor-type-check.raw-slots
  (let ((m (make-satc-m :j 4 :e 2d0)))
    (list (%satc-err (%satc-read-i m))
          (%satc-err (%satc-read-d m))
          (%satc-err (%satc-write-i m 9))
          (%satc-err (%satc-write-d m 9d0))
          (satc-m-j m) (satc-m-e m)))
  ((:type-error satc-n) (:type-error satc-n) (:type-error satc-n)
   (:type-error satc-n) 4 2d0))

(deftest struct-accessor-type-check.include
  (let ((c (make-satc-c :x 7 :z 8)))
    (list (%satc-read c) (satc-a-x c) (funcall 'satc-a-x c)
          (%satc-write c 9) (satc-c-x c)
          (%satc-read (make-satc-a :x 10))))
  (7 7 7 9 9 10))

(deftest struct-accessor-type-check.child-accessor-rejects-parent
  (%satc-err (satc-c-z (make-satc-a :x 1)))
  (:type-error satc-c))
