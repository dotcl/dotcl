;;; (FUNCALL F (AREF A ...)) with A's element type unknown at compile time: when
;;; F has a typed entry the element can be passed to it unboxed. The result,
;;; the argument check of F's declaration, the order of evaluation and the
;;; errors must be those of the ordinary call, for every kind of array and
;;; every kind of function.

(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 16)) %fat-u8))
(defun %fat-u8 (x) (* x 2))
(declaim (ftype (function ((unsigned-byte 32)) (unsigned-byte 8)) %fat-red))
(defun %fat-red (c) (ldb (byte 8 16) c))
(declaim (ftype (function (fixnum) double-float) %fat-half))
(defun %fat-half (x) (/ x 2d0))
(declaim (ftype (function (double-float) double-float) %fat-dbl))
(defun %fat-dbl (x) (* x 2))
(declaim (ftype (function (double-float) fixnum) %fat-trunc))
(defun %fat-trunc (x) (values (truncate x)))
(defun %fat-plain (x) (list x))

(defun %fat-call (f a i) (funcall f (aref a i)))
(defun %fat-call2 (f a i j) (funcall f (aref a i j)))
(defun %fat-call-u8 (f a i) (the (unsigned-byte 8) (funcall f (aref a i))))

(defun %fat-vec (type &rest xs)
  (make-array (length xs) :element-type type :initial-contents xs))

(deftest funcall-aref-typed-entry.kinds
  (list (%fat-call #'%fat-red (%fat-vec '(unsigned-byte 32) #xff123456 7) 0)
        (%fat-call #'%fat-u8 (%fat-vec '(unsigned-byte 8) 1 200) 1)
        (%fat-call #'%fat-u8 (%fat-vec '(unsigned-byte 16) 1 255) 1)
        (%fat-call #'%fat-u8 (%fat-vec 'fixnum 3) 0)
        (%fat-call #'%fat-u8 (%fat-vec '(signed-byte 32) 4) 0)
        (%fat-call #'%fat-u8 (vector 5 6) 1)
        (%fat-call #'%fat-half (%fat-vec '(signed-byte 64) -3) 0)
        (%fat-call #'%fat-dbl (%fat-vec 'double-float 1.5d0) 0)
        (%fat-call #'%fat-dbl (vector 2.5d0) 0)
        (%fat-call #'%fat-trunc (%fat-vec 'double-float -7.5d0) 0)
        (%fat-call #'%fat-plain (%fat-vec '(unsigned-byte 32) #xffffffff) 0)
        (%fat-call #'%fat-plain (%fat-vec 'single-float 1.5f0) 0)
        (%fat-call #'%fat-plain "abc" 2)
        (%fat-call '%fat-u8 (%fat-vec '(unsigned-byte 8) 9) 0)
        (%fat-call (lambda (x) (+ x 1)) (%fat-vec '(unsigned-byte 32) #xffffffff) 0)
        (%fat-call-u8 #'%fat-red (%fat-vec '(unsigned-byte 32) #x00ab0000) 0)
        (%fat-call2 #'%fat-red (make-array '(2 2) :element-type '(unsigned-byte 32)
                                                  :initial-contents '((1 2) (3 #x00cd0000)))
                    1 1)
        (let ((d (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
          (%fat-call #'%fat-u8 (make-array 2 :element-type '(unsigned-byte 8)
                                             :displaced-to d :displaced-index-offset 2)
                     1)))
  (#x12 400 510 6 8 12 -1.5d0 3.0d0 5.0d0 -7 (#xffffffff) (1.5f0) (#\c) 18
   #x100000000 #xab #xcd 8))

;;; An element outside the declared argument type is the TYPE-ERROR of the
;;; ordinary call, whatever the array stores.
(deftest funcall-aref-typed-entry.argument-check
  (flet ((te (thunk) (handler-case (funcall thunk) (type-error () :type-error))))
    (list (te (lambda () (%fat-call #'%fat-u8 (%fat-vec '(unsigned-byte 16) 256) 0)))
          (te (lambda () (%fat-call #'%fat-u8 (%fat-vec 'fixnum -1) 0)))
          (te (lambda () (%fat-call #'%fat-u8 (vector :x) 0)))
          (te (lambda () (%fat-call #'%fat-u8 (vector 1.0d0) 0)))
          (te (lambda () (%fat-call #'%fat-dbl (%fat-vec 'fixnum 1) 0)))
          (te (lambda () (%fat-call #'%fat-dbl (%fat-vec 'single-float 1f0) 0)))
          (te (lambda () (%fat-call #'%fat-red (%fat-vec '(signed-byte 64) (expt 2 40)) 0)))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))

;;; Integer vectors stored 64 bits wide: the subscript is checked against the
;;; vector, a 2-D array takes no single subscript, a fill pointer is ignored.
(deftest funcall-aref-typed-entry.long-storage
  (let ((v (%fat-vec '(unsigned-byte 32) #x00110000 #x00220000))
        (fp (make-array 4 :element-type 'fixnum :fill-pointer 1 :initial-contents '(1 2 3 4)))
        (m (make-array '(2 2) :element-type '(unsigned-byte 32) :initial-element 0)))
    (list (%fat-call #'%fat-red v 1)
          (handler-case (%fat-call #'%fat-red v 2) (error () :index-error))
          (handler-case (%fat-call #'%fat-red v -1) (error () :index-error))
          (handler-case (%fat-call #'%fat-red m 0) (error () :rank-error))
          (%fat-call #'%fat-u8 fp 3)
          (%fat-call #'%fat-u8 (%fat-vec 'fixnum 255) 0)
          (handler-case (%fat-call #'%fat-u8 (%fat-vec 'fixnum 256) 0)
            (type-error () :type-error))))
  (#x22 :index-error :index-error :rank-error 8 510 :type-error))

;;; The value wanted as an int64 (an integer declaration around the call): the
;;; same values and errors as the boxed call, for every kind of array and of
;;; function.
(defun %fat-sum-u8 (f a i j)
  (declare (fixnum i j))
  (+ (the (unsigned-byte 8) (funcall f (aref a i)))
     (the (unsigned-byte 8) (funcall f (aref a j)))))
(defun %fat-float-u8 (f a i) (declare (fixnum i)) (float (the (unsigned-byte 8) (funcall f (aref a i))) 1f0))
(defun %fat-sum2-u8 (f a i j) (declare (fixnum i j)) (1+ (the (unsigned-byte 8) (funcall f (aref a i j)))))
(defun %fat-fix (f a i)
  (declare (fixnum i))
  (logand (the fixnum (funcall f (aref a i))) #xffff))

(deftest funcall-aref-typed-entry.long-value
  (flet ((te (thunk) (handler-case (funcall thunk) (type-error () :type-error))))
    (let ((v (%fat-vec '(unsigned-byte 32) #x00110000 #x00220000)))
      (list (%fat-sum-u8 #'%fat-red v 0 1)
            (%fat-float-u8 #'%fat-red v 1)
            (%fat-sum-u8 #'%fat-u8 (%fat-vec '(unsigned-byte 8) 3 4) 0 1)
            (%fat-sum-u8 (lambda (x) (ldb (byte 8 16) x)) v 0 1)
            (%fat-sum-u8 '%fat-red (vector #x00010000 #x00020000) 0 1)
            (%fat-sum2-u8 #'%fat-red (make-array '(1 2) :element-type '(unsigned-byte 32)
                                                 :initial-contents '((0 #x00330000)))
                          0 1)
            (%fat-fix #'%fat-u8 (%fat-vec 'fixnum 100) 0)
            (te (lambda () (%fat-sum-u8 #'%fat-red (%fat-vec '(signed-byte 64) -1 0) 0 1)))
            (handler-case (%fat-sum-u8 #'%fat-red v 0 2) (error () :index-error))
            (handler-case (%fat-fix (lambda (x) (list x)) v 0) (error () :error)))))
  (#x33 34.0f0 14 #x33 3 #x34 200 :type-error :index-error :error))

;;; The function is looked up before the subscripts are evaluated, and an
;;; undefined function is reported before a bad subscript.
(defvar *fat-trace* nil)

(deftest funcall-aref-typed-entry.order
  (let ((a (%fat-vec '(unsigned-byte 8) 1 2))
        (*fat-trace* nil))
    (list (funcall (progn (push :f *fat-trace*) #'%fat-u8)
                   (aref (progn (push :a *fat-trace*) a) (progn (push :i *fat-trace*) 1)))
          (reverse *fat-trace*)
          (handler-case (%fat-call #'%fat-u8 a 10) (error () :index-error))))
  (4 (:f :a :i) :index-error))

;;; Compiled code resolves the function before evaluating the arguments; the
;;; emit-free evaluator evaluates the arguments first.
(deftest-emitting-only funcall-aref-typed-entry.function-resolved-first
  (let ((a (%fat-vec '(unsigned-byte 8) 1 2))
        (f '%fat-no-such-function))
    (handler-case (funcall f (aref a 10))
      (undefined-function () :undefined)))
  :undefined)

;;; The same through a fasl: the typed entry of a compiled file's function.
(defun %fat-compile (name forms)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-compiled-only funcall-aref-typed-entry.fasl
  (progn
    (load (%fat-compile "funcall-aref-typed-entry"
                        '((declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 16)) %fatf-u8))
                          (defun %fatf-u8 (x) (+ x 1000))
                          (defun %fatf-call (f a i) (funcall f (aref a i))))))
    (list (%fatf-call #'%fatf-u8 (%fat-vec '(unsigned-byte 32) 7) 0)
          (handler-case (%fatf-call #'%fatf-u8 (%fat-vec '(unsigned-byte 32) 300) 0)
            (type-error () :type-error))
          (%fatf-call #'%fat-plain (%fat-vec '(unsigned-byte 32) 300) 0)))
  (1007 :type-error (300)))
