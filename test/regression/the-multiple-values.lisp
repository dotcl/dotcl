;;; THE returns all the values of its form; a single value type constrains only
;;; the primary one (CLHS THE). The typed fixnum lane used to keep only the
;;; quotient of FLOOR and TRUNCATE.

(defun %tmv-eval (mode lambda &rest args)
  (let ((dotcl:*evaluator-mode* mode))
    (multiple-value-list (apply (eval lambda) args))))

(defun %tmv-both (lambda &rest args)
  (list (apply #'%tmv-eval :compile lambda args)
        (apply #'%tmv-eval :interpret lambda args)))

(defun %tmv-compiled (lambda &rest args)
  (multiple-value-list (apply (compile nil lambda) args)))

(deftest the-multiple-values.fixnum-division
  (list (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                      (the fixnum (floor a b))) -127 65536)
        (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                      (the fixnum (truncate a b))) -127 65536)
        (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                      (the fixnum (round a b))) -127 65536)
        (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                      (the fixnum (ceiling a b))) -127 65536))
  (((-1 65409) (-1 65409)) ((0 -127) (0 -127))
   ((0 -127) (0 -127)) ((0 -127) (0 -127))))

(deftest the-multiple-values.float-division
  (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                (the float (ffloor a b))) -127 65536)
  ((-1.0 65409) (-1.0 65409)))

(deftest the-multiple-values.ftruncate
  (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                (the float (ftruncate a b))) -127 65536)
  ((0.0 -127) (0.0 -127)))

(deftest the-multiple-values.gethash
  (let ((h (make-hash-table)))
    (setf (gethash 'x h) 5)
    (%tmv-both '(lambda (h) (the fixnum (gethash 'x h))) h))
  ((5 t) (5 t)))

(deftest the-multiple-values.parse-integer
  (%tmv-both '(lambda (s) (the fixnum (parse-integer s))) "42")
  ((42 2) (42 2)))

(deftest the-multiple-values.decode-float
  (%tmv-both '(lambda (x) (declare (type double-float x))
                (the double-float (decode-float x))) 8.0d0)
  ((0.5d0 4 1.0d0) (0.5d0 4 1.0d0)))

(deftest the-multiple-values.values-forms
  (list (%tmv-both '(lambda () (the fixnum (values 1 2))))
        (%tmv-both '(lambda () (the (values) (floor 7 2))))
        (%tmv-both '(lambda () (the (values fixnum fixnum) (floor 7 2)))))
  (((1 2) (1 2)) ((3 1) (3 1)) ((3 1) (3 1))))

(deftest the-multiple-values.signed-byte-64
  (%tmv-both '(lambda (a b) (declare (type (signed-byte 64) a b))
                (the (signed-byte 64) (floor a b))) -127 65536)
  ((-1 65409) (-1 65409)))

(deftest the-multiple-values.unsigned-byte-64
  (%tmv-both '(lambda (a b) (declare (type (unsigned-byte 64) a b))
                (the (unsigned-byte 64) (floor a b))) 127 65536)
  ((0 127) (0 127)))

(deftest the-multiple-values.double-float
  (%tmv-both '(lambda (x) (declare (type double-float x))
                (the double-float (sqrt x))) 8.0d0)
  ((2.8284271247461903d0) (2.8284271247461903d0)))

(deftest the-multiple-values.single-value-contexts
  (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                (list (let ((x (the fixnum (floor a b)))) x)
                      (progn (the fixnum (floor a b)))
                      (+ (the fixnum (floor a b)) 1)
                      (multiple-value-list (values (the fixnum (floor a b))))))
             -127 65536)
  (((-1 -1 0 (-1))) ((-1 -1 0 (-1)))))

(deftest the-multiple-values.consumers-keep-both
  (%tmv-both '(lambda (a b) (declare (type fixnum a b))
                (list (multiple-value-bind (q r) (the fixnum (floor a b))
                        (list q r))
                      (nth-value 1 (the fixnum (floor a b)))))
             -127 65536)
  ((((-1 65409) 65409)) (((-1 65409) 65409))))

(deftest-compiled-only the-multiple-values.compiled
  (list (%tmv-compiled '(lambda (a b) (declare (type fixnum a b))
                          (the fixnum (floor a b))) -127 65536)
        (%tmv-compiled '(lambda (a b) (declare (type fixnum a b))
                          (the fixnum (truncate a b))) -127 65536)
        (%tmv-compiled '(lambda (a b) (declare (type (signed-byte 64) a b))
                          (the (signed-byte 64) (floor a b))) -127 65536)
        (%tmv-compiled '(lambda (a b) (declare (type (unsigned-byte 64) a b))
                          (the (unsigned-byte 64) (floor a b))) 127 65536))
  ((-1 65409) (0 -127) (-1 65409) (0 127)))

(defvar *tmv-cf-dir*
  (concatenate 'string (regression-temp-dir) "/dotcl-tmv-test/"))

(defun %tmv-compile-and-load (forms name)
  (ensure-directories-exist *tmv-cf-dir*)
  (let ((lisp (concatenate 'string *tmv-cf-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (dolist (f forms) (write f :stream s :readably t) (terpri s)))
    (load (compile-file lisp))))

(deftest-compiled-only the-multiple-values.compile-file
  (progn
    (%tmv-compile-and-load
     '((in-package :cl-user)
       (defun %tmv-cf-floor (a b)
         (declare (type fixnum a b))
         (the fixnum (floor a b)))
       (defun %tmv-cf-truncate (a b)
         (declare (type fixnum a b))
         (the fixnum (truncate a b))))
     "tmv-cf")
    (list (multiple-value-list (funcall (intern "%TMV-CF-FLOOR") -127 65536))
          (multiple-value-list (funcall (intern "%TMV-CF-TRUNCATE") -127 65536))))
  ((-1 65409) (0 -127)))
