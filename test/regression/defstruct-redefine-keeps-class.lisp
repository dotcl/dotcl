;;; Evaluating a DEFSTRUCT again with the same shape keeps the class object, so the
;;; methods specialised on it still apply. A file that was loaded and then given to
;;; COMPILE-FILE evaluates its DEFSTRUCT again at compile time; the new class object
;;; left every method of the loaded one (MAKE-LOAD-FORM among them) applicable to
;;; nothing.

(defstruct dskc-a x)
(defmethod make-load-form ((o dskc-a) &optional env)
  (declare (ignore env))
  `(make-dskc-a :x ',(dskc-a-x o)))

(deftest defstruct-redefine-keeps-class.same-object
  (let ((before (find-class 'dskc-a)))
    (eval '(defstruct dskc-a x))
    (list (eq before (find-class 'dskc-a))
          (make-load-form (make-dskc-a :x 1))))
  (t (make-dskc-a :x '1)))

(deftest defstruct-redefine-keeps-class.different-shape-is-new
  (progn
    (eval '(defstruct dskc-b x))
    (let ((before (find-class 'dskc-b)))
      (eval '(defstruct dskc-b x y))
      (not (eq before (find-class 'dskc-b)))))
  t)

(deftest-compiled-only defstruct-redefine-keeps-class.compile-file
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "dskc-c.lisp" dir))
         (fasl (merge-pathnames "dskc-c.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defstruct dskc-a x)" s)
      (write-line "(defmacro dskc-m (v) `(quote ,(make-load-form (make-dskc-a :x v))))" s)
      (write-line "(defun dskc-f () (dskc-m 5))" s))
    (multiple-value-bind (out warnings-p failure-p)
        (let ((*error-output* (make-broadcast-stream)))
          (compile-file src :output-file fasl))
      (declare (ignore out))
      (list warnings-p failure-p
            (make-load-form (make-dskc-a :x 2)))))
  (nil nil (make-dskc-a :x '2)))
