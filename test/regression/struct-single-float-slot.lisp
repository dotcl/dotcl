;;; A structure slot declared SINGLE-FLOAT is stored raw, as its IEEE bits in
;;; the low half of the structure's long[] (the array integer and double-float
;;; slots already share). It used to stay boxed: every read of it in declared
;;; single-float arithmetic went through the generic slot read and the generic
;;; +, and every store through the generic slot type check, one SingleFloat per
;;; store.
;;;
;;; What the tests pin: the shape (raw read and store, no generic arithmetic or
;;; check), no allocation in a loop over the slot, and that every other way to
;;; reach the slot -- the generic accessor, COPY-STRUCTURE, EQUALP, printing,
;;; a #S literal, a constructor given a value of the wrong type -- answers
;;; what it did when the slot was boxed.

(setf dotcl:*save-sil* t)

(defun %ssf-sil (fn) (%flat-text (dotcl:function-sil fn)))
(defun %ssf-has (needle fn) (and (search needle (%ssf-sil fn)) t))

(defstruct ssf (x 0.0 :type single-float) (y 0.0 :type single-float))
(defstruct ssf-mix
  (n 0 :type fixnum) (s 0.0 :type single-float) (d 0d0 :type double-float)
  (o nil) (z -1.5 :type single-float))

(defun %ssf-incf (s n)
  (declare (type ssf s) (fixnum n))
  (dotimes (i n) (setf (ssf-x s) (+ (ssf-x s) 1.0)))
  (ssf-x s))

(defun %ssf-hoisted (s n)
  (declare (type ssf s) (fixnum n))
  (let ((acc 0.0))
    (declare (single-float acc))
    (dotimes (i n acc)
      (setf (ssf-y s) (* (ssf-x s) 2.0))
      (setq acc (+ acc (ssf-y s))))))

(defun %ssf-generic-read (s) (ssf-x s))

(deftest-emitting-only struct-single-float-slot.shape
  (list (%ssf-has "StructSetF" #'%ssf-incf)
        (or (%ssf-has "StructRefF" #'%ssf-incf)
            (%ssf-has "RawBitsToSingle" #'%ssf-incf))
        (%ssf-has "Runtime.Add" #'%ssf-incf)
        (%ssf-has "CheckSlotType" #'%ssf-incf)
        (%ssf-has "StructSetI" #'%ssf-incf)
        (%ssf-has "Runtime.Multiply" #'%ssf-hoisted))
  (t t nil nil nil nil))

(deftest-emitting-only struct-single-float-slot.no-allocation
  (let ((s (make-ssf)))
    (%ssf-incf s 100)
    (%ssf-hoisted s 100)
    (let ((a0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
      (%ssf-incf s 100000)
      (%ssf-hoisted s 100000)
      (< (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") a0)
         10000)))
  t)

(deftest struct-single-float-slot.values
  (let ((s (make-ssf :x 0.5)))
    (list (%ssf-incf s 3)
          (type-of (%ssf-generic-read s))
          (%ssf-hoisted (make-ssf :x 1.5) 4)
          (ssf-y (make-ssf :y -0.0))
          (let ((n (make-ssf :x (/ 0.0 0.0))))
            (/= (ssf-x n) (ssf-x n)))
          (ssf-x (make-ssf :x most-positive-single-float))))
  (3.5 single-float 12.0 -0.0 t #.most-positive-single-float))

(deftest struct-single-float-slot.mixed-struct
  (let ((m (make-ssf-mix :n 7 :s 2.5 :d 3.25d0 :o "x")))
    (setf (ssf-mix-s m) (+ (ssf-mix-s m) 1.0)
          (ssf-mix-d m) (+ (ssf-mix-d m) 1d0)
          (ssf-mix-n m) (+ (ssf-mix-n m) 1))
    (list (ssf-mix-n m) (ssf-mix-s m) (ssf-mix-d m) (ssf-mix-o m) (ssf-mix-z m)
          (type-of (ssf-mix-s m)) (type-of (ssf-mix-d m))))
  (8 3.5 4.25d0 "x" -1.5 single-float double-float))

(deftest struct-single-float-slot.copy-equalp-print
  (let* ((m (make-ssf-mix :n 1 :s 0.25 :d 2d0))
         (c (copy-ssf-mix m)))
    (setf (ssf-mix-s c) 9.0)
    (list (ssf-mix-s m) (ssf-mix-s c)
          (equalp m (make-ssf-mix :n 1 :s 0.25 :d 2d0))
          (equalp m c)
          (let ((*print-readably* nil)) (prin1-to-string (make-ssf :x 1.5 :y 2.0)))
          (ssf-x (read-from-string "#S(SSF :X 4.5 :Y 0.0)"))))
  (0.25 9.0 t nil "#S(SSF :X 1.5 :Y 2.0)" 4.5))

(deftest struct-single-float-slot.type-violation-signals
  (list (handler-case (progn (setf (ssf-x (make-ssf)) 1d0) :stored)
          (type-error () :type-error))
        (handler-case (progn (make-ssf :x 1) :made)
          (type-error () :type-error)))
  (:type-error :type-error))
