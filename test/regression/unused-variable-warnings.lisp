;;; COMPILE and COMPILE-FILE give SBCL's style warnings for a lexical variable
;;; that is never read, or that is declared IGNORE and used anyway. Expected
;;; values are SBCL 2.6.8's, except that a variable named by an uninterned
;;; symbol is never reported (SBCL reports a gensym as well).

(defun uvw (form)
  "The style warnings COMPILE gives for FORM, as strings."
  (let ((out '()))
    (handler-bind ((style-warning
                     (lambda (c)
                       (push (princ-to-string c) out)
                       (muffle-warning c))))
      (compile nil form))
    (nreverse out)))

(deftest-emitting-only unused-variable-warnings.let
  (uvw '(lambda () (let ((foo 0)) (warn "Foo"))))
  ("The variable FOO is defined but never used."))

(deftest-emitting-only unused-variable-warnings.parameters
  (list (uvw '(lambda (x) 1))
        (uvw '(lambda (x) x))
        (uvw '(lambda (x) (declare (ignore x)) 1))
        (uvw '(lambda (x) (declare (ignorable x)) 1))
        (uvw '(lambda (&optional (a 1 a-p)) a))
        (uvw '(lambda (&key (a 1 a-p)) a-p))
        (uvw '(lambda (&rest r) 2))
        (uvw '(lambda (&aux (a 1)) 2)))
  (("The variable X is defined but never used.")
   ()
   ()
   ()
   ("The variable A-P is defined but never used.")
   ("The variable A is defined but never used.")
   ("The variable R is defined but never used.")
   ("The variable A is defined but never used.")))

(deftest-emitting-only unused-variable-warnings.ignored-but-used
  (list (uvw '(lambda (x) (declare (ignore x)) x))
        (uvw '(lambda () (let ((a 1)) (declare (ignore a)) (setq a 2))))
        (uvw '(lambda () (let ((a 1)) (declare (ignore a)) (lambda () a)))))
  (("reading an ignored variable: X")
   ("A is being set even though it was declared to be ignored.")
   ("reading an ignored variable: A")))

(deftest-emitting-only unused-variable-warnings.assigned
  (list (uvw '(lambda () (let ((a 1)) (setq a 2) 3)))
        (uvw '(lambda () (let ((a 1)) (lambda () (setq a 2)))))
        (uvw '(lambda () (let ((a 1)) (incf a) 2)))
        (uvw '(lambda () (let ((a 1) (b 2)) (multiple-value-setq (a b) (values 3 4)) 5))))
  (("The variable A is assigned but never read.") () () ()))

(deftest-emitting-only unused-variable-warnings.binding-forms
  (list (uvw '(lambda () (multiple-value-bind (a b) (values 1 2) a)))
        (uvw '(lambda (l) (destructuring-bind (a b) l a)))
        (uvw '(lambda () (flet ((f (y) 1)) (f 2))))
        (uvw '(lambda () (macrolet ((m (x) 1)) (m 2))))
        (uvw '(lambda () (handler-case 1 (error (e) 2))))
        (uvw '(lambda () (restart-case 1 (r (x) 2))))
        (uvw '(lambda (l) (mapcar (lambda (x) 1) l))))
  (("The variable B is defined but never used.")
   ("The variable B is defined but never used.")
   ("The variable Y is defined but never used.")
   ("The variable X is defined but never used.")
   ("The variable E is defined but never used.")
   ("The variable X is defined but never used.")
   ("The variable X is defined but never used.")))

;;; Shadowing, declarations that are not uses, and specials.
(deftest-emitting-only unused-variable-warnings.scope
  (list (uvw '(lambda () (let ((a 1)) (let ((a 2)) a))))
        (uvw '(lambda () (let ((a 1)) (declare (fixnum a)) 2)))
        (uvw '(lambda () (let ((a 1)) (declare (special a)) 2)))
        (uvw '(lambda () (symbol-macrolet ((x 1)) (let ((x 2)) 3))))
        (uvw '(lambda () (symbol-macrolet ((s a)) (let ((a 1)) s))))
        (uvw '(lambda (x) (tagbody x (go x)))))
  (("The variable A is defined but never used.")
   ("The variable A is defined but never used.")
   ()
   ("The variable X is defined but never used.")
   ()
   ("The variable X is defined but never used.")))

;;; The standard macros whose own variables SBCL does not report.
(deftest-emitting-only unused-variable-warnings.iteration-macros
  (list (uvw '(lambda () (dotimes (i 3) (print 1))))
        (uvw '(lambda (l) (dolist (x l) (print 1))))
        (uvw '(lambda () (do ((i 0 (1+ i)) (j 0)) ((> i 3)) (print 1))))
        (uvw '(lambda () (do* ((a 1)) (t 2))))
        (uvw '(lambda (l) (loop for (a b) in l collect a)))
        (uvw '(lambda (h) (loop for k being the hash-keys of h using (hash-value v) collect k)))
        (uvw '(lambda () (loop with a = 1 repeat 2 collect 3)))
        (uvw '(lambda () (let ((#:g 1)) 2)))
        (uvw '(lambda (l) (dolist (x l) (let ((y 1)) (print x))))))
  (() () () () () () () ()
   ("The variable Y is defined but never used.")))

(defgeneric uvw-gf (a b))

(deftest-emitting-only unused-variable-warnings.defmethod
  (list (uvw '(lambda () (defmethod uvw-gf ((a integer) b) 1)))
        (uvw '(lambda () (defmethod uvw-gf (a b) (let ((c 1)) a)))))
  (() ("The variable C is defined but never used.")))

(deftest-emitting-only unused-variable-warnings.definers
  (list (uvw '(lambda () (defmacro uvw-m (a b) a)))
        (uvw '(lambda () (define-compiler-macro uvw-cm (fn form) (declare (ignore fn form)) nil)))
        (uvw '(lambda () (defsetf uvw-get (x &optional d) (v) (declare (ignore d)) `(list ,x ,v))))
        (uvw '(lambda () (define-setf-expander uvw-place (x &environment env)
                          (declare (ignore x))
                          (get-setf-expansion 'uvw-y env))))
        (uvw '(lambda () (deftype uvw-type (a) (declare (ignore a)) 'integer))))
  (("The variable B is defined but never used.") () () () ()))

;;; A style warning: warnings-p true, failure-p false.
(deftest-emitting-only unused-variable-warnings.compile-values
  (let ((*error-output* (make-broadcast-stream)))
    (multiple-value-bind (fn warnings-p failure-p) (compile nil '(lambda (x) 1))
      (list (funcall fn 0) (and warnings-p t) failure-p)))
  (1 t nil))

(deftest-emitting-only unused-variable-warnings.compile-file
  (let ((src (regression-temp-file "uvw-compile-file.lisp"))
        (warnings '()))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defun uvw-cf (x y) x)" s))
    (unwind-protect
         (multiple-value-bind (out warnings-p failure-p)
             (handler-bind ((style-warning (lambda (c) (push (princ-to-string c) warnings))))
               (let ((*error-output* (make-broadcast-stream)))
                 (compile-file src)))
           (when out (delete-file out))
           (list warnings (and warnings-p t) failure-p))
      (delete-file src)))
  (("The variable Y is defined but never used.") t nil))

;;; A type declared for an IGNORABLE parameter is still checked on entry.
(deftest-emitting-only unused-variable-warnings.ignorable-type-checked
  (handler-case
      (funcall (compile nil '(lambda (x) (declare (ignorable x) (type simple-vector x)) x)) 1)
    (type-error () :type-error))
  :type-error)
