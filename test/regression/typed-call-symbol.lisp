;;; A typed call site calls the typed entry the callee's symbol keeps for its
;;; function. Every way of changing the function under the name between two
;;; calls (redefinition with the same or another FTYPE, (SETF FDEFINITION),
;;; FMAKUNBOUND) must be seen by the next call, in memory and from a fasl.

(declaim (notinline %tcs-f %tcs-same %tcs-dbl))
(declaim (ftype (function (fixnum fixnum) fixnum) %tcs-f %tcs-same))
(defun %tcs-f (a b) (+ a b))
(defun %tcs-same (a b) (- a b 1000))
(declaim (ftype (function (double-float double-float) double-float) %tcs-dbl))
(defun %tcs-dbl (x y) (+ x y))
(defun %tcs-call (a b) (list (%tcs-f a b)))

(defun %tcs-sequence (call define)
  (list (funcall call 1 2)
        (progn (funcall define '(lambda (a b) (* a b))) (funcall call 3 4))
        (progn (setf (fdefinition '%tcs-f) #'%tcs-same) (funcall call 3 4))
        (progn (setf (fdefinition '%tcs-f) (lambda (a b) (- a b))) (funcall call 10 4))
        (progn (fmakunbound '%tcs-f)
               (handler-case (funcall call 1 1) (undefined-function () :undefined)))
        (progn (funcall define '(lambda (a b) (+ a b 100))) (funcall call 1 2))
        (progn (setf (fdefinition '%tcs-f) #'%tcs-dbl)
               (handler-case (funcall call 1 2) (type-error () :type-error)))
        (progn (setf (fdefinition '%tcs-f) (lambda (a b) (declare (ignore a b)) 1.5))
               (handler-case (funcall call 1 2) (type-error () :type-error)))
        (progn (funcall define '(lambda (a b) (+ a b))) (funcall call 5 6))))

(defun %tcs-define-in-memory (lambda)
  (eval `(defun %tcs-f ,@(cdr lambda))))

(deftest-emitting-only typed-call-symbol.in-memory
  (%tcs-sequence #'%tcs-call #'%tcs-define-in-memory)
  ((3) (12) (-1001) (6) :undefined (103) :type-error :type-error (11)))

(defvar *tcs-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-tcs-test/")))
    (ensure-directories-exist dir)
    dir))

(defvar *tcs-fasl-count* 0)

(defun %tcs-compile-and-load (source)
  (let ((lisp (format nil "~atcs-~d.lisp" *tcs-dir* (incf *tcs-fasl-count*))))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string "(in-package :cl-user)" s)
      (write-string source s))
    (load (compile-file lisp))
    t))

(defun %tcs-define-from-fasl (lambda)
  (%tcs-compile-and-load (format nil "~s" `(defun %tcs-f ,@(cdr lambda)))))

(deftest-compiled-only typed-call-symbol.fasl
  (progn
    (%tcs-define-from-fasl '(lambda (a b) (+ a b)))
    (%tcs-compile-and-load "(defun %tcs-fasl-call (a b) (list (%tcs-f a b)))")
    (list (%tcs-sequence (intern "%TCS-FASL-CALL") #'%tcs-define-from-fasl)
          (%tcs-sequence (intern "%TCS-FASL-CALL") #'%tcs-define-in-memory)))
  (((3) (12) (-1001) (6) :undefined (103) :type-error :type-error (11))
   ((3) (12) (-1001) (6) :undefined (103) :type-error :type-error (11))))
