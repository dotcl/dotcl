;;; A constructor call whose values for the raw slots (DOUBLE-FLOAT,
;;; SINGLE-FLOAT, FIXNUM) are already raw builds the structure without boxing
;;; them, and without the slot type check the value cannot fail. The structure
;;; reads, writes, copies, compares and prints like one built the general way,
;;; and code compiled against an older layout of the structure still builds a
;;; correct instance after a redefinition.

(setf dotcl:*save-sil* t)

(declaim (inline %src-mix %src-v3))
(defstruct (%src-m (:constructor %src-mix (d n o s)))
  (d 0d0 :type double-float) (n 0 :type (integer 0 2000)) (o nil) (s 0f0 :type single-float))
(defstruct (%src-v (:constructor %src-v3 (x y z)))
  (x 0d0 :type double-float) (y 0d0 :type double-float) (z 0d0 :type double-float))

(defun %src-build-mix (a k f)
  (declare (double-float a) (fixnum k) (single-float f))
  (let ((d (* a 2d0)) (s (* f 3f0)))
    (if (< d 1d10) (%src-mix d (logand (+ k 1) 1023) (list a) s) nil)))

(defun %src-add (p q)
  (declare (double-float p q))
  (let ((x (+ p q)) (y (- p q)) (z (* p q)))
    (if (> x y) (%src-v3 x y z) (%src-v3 y x z))))

(deftest struct-raw-construct.values
  (let ((m (%src-build-mix 1.5d0 41 0.5f0))
        (v (%src-add 3d0 2d0))
        (w (%src-add 1d0 2d0)))
    (list (%src-m-d m) (%src-m-n m) (%src-m-o m) (%src-m-s m)
          (%src-v-x v) (%src-v-y v) (%src-v-z v)
          (%src-v-x w) (%src-v-y w)
          (equalp v (%src-v3 5d0 1d0 6d0))
          (equalp v w)))
  (3d0 42 (1.5d0) 1.5f0 5d0 1d0 6d0 3d0 -1d0 t nil))

(deftest struct-raw-construct.mutate-copy-print
  (let* ((v (%src-add 3d0 2d0))
         (c (copy-structure v))
         (m (%src-build-mix 1d0 1 1f0)))
    (setf (%src-v-y v) 10d0)
    (setf (%src-m-o m) :x (%src-m-n m) 7)
    (list (%src-v-y v) (%src-v-y c) (%src-v-x c)
          (%src-m-o m) (%src-m-n m) (%src-m-d m)
          (handler-case (progn (setf (%src-v-z v) 1) :stored) (type-error () :type-error))
          (let ((*print-readably* nil)) (prin1-to-string c))))
  (10d0 1d0 5d0 :x 7 2d0 :type-error "#S(%SRC-V :X 5.0d0 :Y 1.0d0 :Z 6.0d0)"))

(deftest-compiled-only struct-raw-construct.no-box
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "MakeStructRaw" #'%src-add) (has "CheckSlotType" #'%src-add)
          (has "Runtime.MakeStruct)" #'%src-add)
          (has "MakeStructRaw" #'%src-build-mix)))
  (t nil nil t))

;; Built by code compiled against the first definition, after the structure is
;; redefined with other slot types: the values are boxed into the new layout.
(defstruct (%src-r (:constructor %src-r2 (a b)))
  (a 0d0 :type double-float) (b 0 :type fixnum))
(declaim (inline %src-r2))
(defstruct (%src-r (:constructor %src-r2 (a b)))
  (a 0d0 :type double-float) (b 0 :type fixnum))
(defun %src-make-r (a b)
  (declare (double-float a) (fixnum b))
  (let ((x (+ a 1d0)) (y (+ b 1))) (if (< y 100) (%src-r2 x y) nil)))
(defparameter *%src-r-before* (%src-make-r 1d0 2))
(handler-bind ((warning #'muffle-warning))
  (defstruct (%src-r (:constructor %src-r2 (a b)))
    (a 0d0 :type t) (b 0 :type t)))

(deftest struct-raw-construct.redefined
  (let ((r (%src-make-r 2d0 5)))
    (list (%src-r-a r) (%src-r-b r)))
  (3d0 6))

(setf dotcl:*save-sil* nil)
