;;; A function with a declared FTYPE, compiled to a fasl with (debug 0), keeps no frame but is
;;; entered by address like one that does: the boxed entry makes the stack and interrupt check the
;;; call would make, so deep recursion through it ends in a STORAGE-CONDITION and not in a crash,
;;; and its answers are those of the same function at (debug 1).

(defparameter *td0-source*
  "(declaim (ftype (function (fixnum) fixnum) td0-down td1-down))
(defun td0-down (n) (declare (optimize (speed 3) (safety 1) (debug 0)))
  (if (<= n 0) 0 (1+ (the fixnum (funcall 'td0-down (1- n))))))
(defun td1-down (n) (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if (<= n 0) 0 (1+ (the fixnum (funcall 'td1-down (1- n))))))
(declaim (ftype (function (double-float double-float) double-float) td0-add td1-add))
(defun td0-add (a b) (declare (optimize (debug 0))) (+ a b))
(defun td1-add (a b) (declare (optimize (debug 1))) (+ a b))
(defun td0-sum (n) (declare (fixnum n)) (let ((x 0d0)) (declare (double-float x)) (dotimes (i n x) (setq x (td0-add x 1d0)))))
(defun td1-sum (n) (declare (fixnum n)) (let ((x 0d0)) (declare (double-float x)) (dotimes (i n x) (setq x (td1-add x 1d0)))))
")

(defun %td0-load ()
  (let* ((dir (format nil "~A/td0" (regression-temp-dir)))
         (src (format nil "~A/td0.lisp" dir))
         (out (format nil "~A/td0.fasl" dir)))
    (ensure-directories-exist src)
    (with-open-file (o src :direction :output :if-exists :supersede) (write-string *td0-source* o))
    (compile-file src :output-file out)
    (load out)))

(deftest-emitting-only typed-debug0-entry.same-answers-and-stack-check
  (progn
    (%td0-load)
    (flet ((overflow-p (name n)
             (handler-case (progn (funcall name n) :no-overflow)
               (storage-condition () :overflow))))
      (list (funcall 'td0-down 100) (funcall 'td1-down 100)
            (funcall 'td0-sum 1000) (funcall 'td1-sum 1000)
            (funcall 'td0-add 1.5d0 2.25d0)
            (call-on-small-stack (lambda () (overflow-p 'td0-down 100000000)))
            (call-on-small-stack (lambda () (overflow-p 'td1-down 100000000))))))
  (100 100 1000d0 1000d0 3.75d0 :overflow :overflow))
