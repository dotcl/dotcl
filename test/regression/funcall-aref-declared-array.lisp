;;; (FUNCALL F (AREF A I)) and (SETF (AREF D I) (AREF A ...)) when A's integer
;;; element type is declared: the element goes to F's typed entry or into D
;;; unboxed, as it does when A's type is unknown. Declaring the type must not
;;; make the code slower, nor change a value or an error.

(setf dotcl:*save-sil* t)

(declaim (ftype (function ((unsigned-byte 32)) (unsigned-byte 8)) %fad-red))
(defun %fad-red (c) (ldb (byte 8 16) c))
(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 16)) %fad-u8))
(defun %fad-u8 (x) (* x 2))
(declaim (ftype (function (fixnum) double-float) %fad-half))
(defun %fad-half (x) (/ x 2d0))

(defun %fad-call (f a i)
  (declare (type (simple-array (unsigned-byte 32) (*)) a) (fixnum i))
  (list (funcall f (aref a i))))
(defun %fad-call-u8 (f a i)
  (declare (type (simple-array (unsigned-byte 32) (*)) a) (fixnum i))
  (1+ (the (unsigned-byte 8) (funcall f (aref a i)))))
(defun %fad-call-fix (f a i)
  (declare (type (simple-array fixnum (*)) a) (fixnum i))
  (1+ (the fixnum (funcall f (aref a i)))))
(defun %fad-sum (f a)
  (declare (type function f) (type (simple-array (unsigned-byte 32) (16)) a))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i 16) (incf s (the (unsigned-byte 8) (funcall f (aref a i)))))
    s))
(defun %fad-the-call (f a i)
  (declare (fixnum i))
  (list (funcall f (aref (the (simple-array (unsigned-byte 32) (*)) a) i))))
(defun %fad-store (d a i)
  (declare (type (simple-array (unsigned-byte 32) (*)) a) (fixnum i))
  (setf (aref d i) (aref a i))
  nil)
(defun %fad-store2 (d a i j)
  (declare (type (simple-array (unsigned-byte 32) (* *)) a) (fixnum i j))
  (setf (aref d i j) (aref a i j))
  nil)
(defun %fad-the-store (d a i j)
  (declare (fixnum i j))
  (setf (aref d i) (aref (the (simple-array (unsigned-byte 32) (* *)) a) i j))
  nil)

(defun %fad-u32 (&rest xs)
  (make-array (length xs) :element-type '(unsigned-byte 32) :initial-contents xs))
(defun %fad-fix (&rest xs)
  (make-array (length xs) :element-type 'fixnum :initial-contents xs))

(deftest funcall-aref-declared-array.values
  (list (%fad-call #'%fad-red (%fad-u32 #xff123456 7) 0)
        (%fad-call #'%fad-u8 (%fad-u32 1 200) 1)
        (%fad-call #'%fad-half (%fad-u32 3) 0)
        (%fad-call (lambda (x) (list x)) (%fad-u32 #xffffffff) 0)
        (%fad-call '%fad-red (%fad-u32 #x00ab0000) 0)
        (%fad-call-u8 #'%fad-red (%fad-u32 #x00ab0000) 0)
        (%fad-call-u8 (lambda (x) (ldb (byte 8 8) x)) (%fad-u32 #x0000cd00) 0)
        (%fad-call-fix #'%fad-u8 (%fad-fix 100) 0)
        (%fad-sum #'%fad-red (make-array 16 :element-type '(unsigned-byte 32)
                                            :initial-element #x00020000))
        (%fad-the-call #'%fad-red (%fad-u32 #x00110000) 0))
  ((#x12) (400) (1.5d0) ((#xffffffff)) (#xab) #xac #xce 201 32 (#x11)))

;;; An element outside F's declared argument type, a value that is not the
;;; declared one, a subscript out of range: the errors of the ordinary call.
(deftest funcall-aref-declared-array.errors
  (flet ((te (thunk) (handler-case (funcall thunk) (type-error () :type-error) (error () :error))))
    (list (te (lambda () (%fad-call #'%fad-u8 (%fad-u32 256) 0)))
          (te (lambda () (%fad-call-fix #'%fad-u8 (%fad-fix -1) 0)))
          (te (lambda () (%fad-call-u8 (lambda (x) (list x)) (%fad-u32 1) 0)))
          (handler-case (%fad-call #'%fad-red (%fad-u32 1) 1) (error () :error))))
  (:type-error :type-error :type-error :error))

;;; The store into an array of unknown element type: every kind of array that
;;; can hold the element takes it, the others signal as (SETF AREF) does.
(deftest funcall-aref-declared-array.store
  (flet ((st (d v) (handler-case (progn (%fad-store d (%fad-u32 v) 0) (aref d 0))
                     (type-error () :type-error) (error () :error))))
    (list (st (vector nil) 7)
          (st (make-array 1 :element-type '(unsigned-byte 32)) #xffffffff)
          (st (make-array 1 :element-type '(unsigned-byte 8)) 255)
          (st (make-array 1 :element-type '(unsigned-byte 8)) 256)
          (st (make-array 1 :element-type '(signed-byte 16)) 40000)
          (st (make-array 1 :element-type 'fixnum) 9)
          (st (make-array 1 :element-type 'double-float) 1)
          (st (make-array 1 :element-type 'bit) 1)
          (st (make-array 1 :element-type 'bit) 2)
          (st (make-string 1) 65)
          (st (make-array 2 :element-type '(unsigned-byte 32)) 1)
          (handler-case (progn (%fad-store (vector 0) (%fad-u32 5) 1) :no-error)
            (error () :error))
          (let ((d (make-array '(2 2) :initial-element 0)))
            (%fad-store2 d (make-array '(2 2) :element-type '(unsigned-byte 32)
                                              :initial-contents '((1 2) (3 4)))
                         1 0)
            (list (aref d 0 0) (aref d 0 1) (aref d 1 0) (aref d 1 1)))
          (let ((d (make-array 2 :initial-element 0)))
            (%fad-the-store d (make-array '(2 2) :element-type '(unsigned-byte 32)
                                                 :initial-contents '((1 2) (3 4)))
                            1 1)
            (list (aref d 0) (aref d 1)))))
  (7 #xffffffff 255 :type-error :type-error 9 :type-error 1 :type-error :type-error 1 :error
   (0 0 3 0) (0 4)))

;;; The shapes: the declared element goes to the runtime unboxed, not through
;;; a box and the generic call.
(defun %fad-sil-has (fn text) (and (search text (%flat-text (dotcl:function-sil fn))) t))

(deftest-compiled-only funcall-aref-declared-array.sil
  (list (%fad-sil-has #'%fad-call "FuncallLong1")
        (%fad-sil-has #'%fad-call-u8 "FuncallLong1L")
        (%fad-sil-has #'%fad-sum "FuncallLong1L")
        (%fad-sil-has #'%fad-sum "Invoke1")
        (%fad-sil-has #'%fad-the-call "FuncallAref1L")
        (%fad-sil-has #'%fad-store "ArefSetLong1L")
        (%fad-sil-has #'%fad-store2 "ArefSetLong2L")
        (%fad-sil-has #'%fad-the-store "ArefCopy12L")
        (%fad-sil-has #'%fad-the-store "Fixnum.Make"))
  (t t t nil t t t t nil))

(setf dotcl:*save-sil* nil)

;;; The other way round: the destination's integer element type is known and
;;; the source's is not. The element moves without a box when the source holds
;;; it the same way, and otherwise the store signals as (SETF AREF) does.
(setf dotcl:*save-sil* t)

(defun %fad-into (s y x)
  (declare (fixnum y x))
  (let ((d (make-array 2 :element-type '(unsigned-byte 8))))
    (dotimes (i 2) (setf (aref d i) (aref s y (+ x i))))
    (list (aref d 0) (aref d 1))))

(deftest funcall-aref-declared-array.into-declared
  (flet ((st (s) (handler-case (%fad-into s 0 0) (type-error () :type-error) (error () :error))))
    (list (st (make-array '(1 2) :element-type '(unsigned-byte 8) :initial-contents '((1 2))))
          (st (make-array '(1 2) :element-type '(unsigned-byte 32) :initial-contents '((3 255))))
          (st (make-array '(1 2) :initial-contents '((4 5))))
          (st (make-array '(1 2) :element-type '(unsigned-byte 32) :initial-contents '((3 256))))
          (st (make-array '(1 2) :initial-contents '((4 :x))))
          (st (make-array '(1 2) :element-type 'double-float :initial-element 1d0))
          (st (make-array '(1 1) :element-type '(unsigned-byte 8)))))
  ((1 2) (3 255) (4 5) :type-error :type-error :type-error :type-error))

(deftest-compiled-only funcall-aref-declared-array.into-declared-sil
  (list (%fad-sil-has #'%fad-into "ArefCopy12L")
        (%fad-sil-has #'%fad-into "Aref2DL"))
  (t nil))

(setf dotcl:*save-sil* nil)
