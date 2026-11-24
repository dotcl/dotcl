;;; AREF / (SETF AREF) of a global variable whose array type is declaimed read the array into a
;;; typed temporary, so the element is read and written as for a typed local. The values are those
;;; of the same access through a typed local; a rebinding or an assignment of the global is seen,
;;; and a value of the wrong type still signals.

(setf dotcl:*save-sil* t)

(declaim (type (simple-array (unsigned-byte 32) (256)) *gal-u32*))
(defvar *gal-u32* (let ((a (make-array 256 :element-type '(unsigned-byte 32))))
                    (dotimes (i 256 a) (setf (aref a i) (logand #xFFFFFFFF (* i 16777259))))))
(declaim (type (simple-array (unsigned-byte 8) (*)) *gal-u8*))
(defvar *gal-u8* (let ((a (make-array 300 :element-type '(unsigned-byte 8))))
                   (dotimes (i 300 a) (setf (aref a i) (logand 255 (* i 7))))))
(declaim (type (simple-array double-float (4 5)) *gal-d2*))
(defvar *gal-d2* (let ((a (make-array '(4 5) :element-type 'double-float)))
                   (dotimes (i 4 a) (dotimes (j 5) (setf (aref a i j) (+ (* i 10d0) j 0.5d0))))))
(declaim (type (simple-array (unsigned-byte 32) (8 256)) *gal-c*))
(defvar *gal-c* (let ((a (make-array '(8 256) :element-type '(unsigned-byte 32))))
                  (dotimes (i 8 a) (dotimes (j 256) (setf (aref a i j) (logand #xFFFFFFFF (+ (* i 65599) (* j 31))))))))
(declaim (type (simple-array fixnum (64)) *gal-fx*))
(defvar *gal-fx* (make-array 64 :element-type 'fixnum :initial-element 3))
(declaim (type simple-vector *gal-sv*))
(defvar *gal-sv* (vector 1 2 3 'a "b"))

(defun gal-sum-u32 (n) (declare (type fixnum n))
  (let ((s 0)) (declare (type fixnum s))
    (dotimes (i n s) (setq s (logand #xFFFFFFFFFF (+ s (aref *gal-u32* (logand i 255))))))))
(defun gal-sum-u32-local (n) (declare (type fixnum n))
  (let ((s 0) (v *gal-u32*)) (declare (type fixnum s) (type (simple-array (unsigned-byte 32) (256)) v))
    (dotimes (i n s) (setq s (logand #xFFFFFFFFFF (+ s (aref v (logand i 255))))))))
(defun gal-xor-u8 (n) (declare (type fixnum n))
  (let ((s 0)) (declare (type fixnum s))
    (dotimes (i n s) (setq s (logxor s (aref *gal-u8* (mod i 300)))))))
(defun gal-d2 (i j) (declare (type fixnum i j)) (aref *gal-d2* i j))
(defun gal-c (i j) (declare (type fixnum i j)) (aref *gal-c* i j))
(defun gal-set-fx (i v) (declare (type fixnum i v)) (setf (aref *gal-fx* i) v))
(defun gal-fx (i) (declare (type fixnum i)) (aref *gal-fx* i))
(defun gal-sv (i) (declare (type fixnum i)) (aref *gal-sv* i))
(defun gal-set-u32 (i v) (declare (type fixnum i)) (setf (aref *gal-u32* i) v))

(setf dotcl:*save-sil* nil)

(deftest global-array-aref.one-dimensional-sums
  (list (= (gal-sum-u32 1000) (gal-sum-u32-local 1000))
        (= (gal-sum-u32 0) 0)
        (let ((ref 0) (a (copy-seq *gal-u8*)))
          (dotimes (i 700) (setq ref (logxor ref (aref a (mod i 300)))))
          (eql ref (gal-xor-u8 700)))
        (eql (gal-sum-u32 5000) (gal-sum-u32-local 5000)))
  (t t t t))

(deftest global-array-aref.two-dimensional
  (list (gal-d2 0 0) (gal-d2 3 4) (gal-d2 2 3) (gal-c 0 0) (gal-c 7 255) (gal-c 3 100))
  (0.5d0 34.5d0 23.5d0 0 #.(logand #xFFFFFFFF (+ (* 7 65599) (* 255 31)))
   #.(logand #xFFFFFFFF (+ (* 3 65599) (* 100 31)))))

(deftest global-array-aref.store-and-read-back
  (progn (gal-set-fx 5 123456789) (gal-set-fx 6 -42)
         (list (gal-fx 5) (gal-fx 6) (gal-fx 7)))
  (123456789 -42 3))

(deftest global-array-aref.simple-vector-of-anything
  (list (gal-sv 0) (gal-sv 3) (gal-sv 4))
  (1 a "b"))

(deftest global-array-aref.a-dynamic-rebinding-is-seen
  (let ((*gal-fx* (make-array 64 :element-type 'fixnum :initial-element 9)))
    (list (gal-fx 0) (progn (gal-set-fx 0 77) (gal-fx 0))))
  (9 77))
(deftest global-array-aref.the-global-is-untouched-after-the-rebinding
  (gal-fx 0)
  3)

(deftest global-array-aref.assigning-the-global-is-seen
  (let ((old *gal-fx*))
    (unwind-protect
         (progn (setq *gal-fx* (make-array 64 :element-type 'fixnum :initial-element 5))
                (gal-fx 2))
      (setq *gal-fx* old)))
  5)

(deftest global-array-aref.a-wrong-type-still-signals
  (let ((old *gal-fx*))
    (unwind-protect
         (progn (setq *gal-fx* (list 1 2 3))
                (handler-case (gal-fx 1) (type-error () :type-error) (error () :other-error)))
      (setq *gal-fx* old)))
  :type-error)

(deftest global-array-aref.index-errors-are-the-same
  (list (handler-case (gal-fx 64) (error () :error))
        (handler-case (gal-d2 4 0) (error () :error))
        (handler-case (gal-sv 5) (error () :error)))
  (:error :error :error))

(deftest-emitting-only global-array-aref.no-generic-aref-in-the-sum
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Runtime.ArefL" #'gal-sum-u32) (has "Runtime.Aref2DL" #'gal-c)
          (has "Runtime.Aref2DL" #'gal-d2) (has "Runtime.ArefL" #'gal-fx)))
  (nil nil nil nil))
