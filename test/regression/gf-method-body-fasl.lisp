;;; Method bodies compiled to a .fasl, called from call sites in the same .fasl:
;;; the site follows changes to the methods, and deep recursion through the
;;; methods still signals STORAGE-CONDITION.

(defparameter *gmbf-source* "
(defgeneric %gmbf-fib (x))
(defmethod %gmbf-fib (x) (if (< x 2) 1 (+ (%gmbf-fib (- x 1)) (%gmbf-fib (- x 2)))))
(defmethod %gmbf-efib ((x (eql 0))) 1)
(defmethod %gmbf-efib ((x (eql 1))) 1)
(defmethod %gmbf-efib ((x integer)) (+ (%gmbf-efib (- x 1)) (%gmbf-efib (- x 2))))
(defmethod %gmbf-efib ((x symbol)) (list :symbol x))
(defgeneric %gmbf-kind (x))
(defmethod %gmbf-kind ((x integer)) :integer)
(defmethod %gmbf-kind ((x t)) :t)
(defun %gmbf-call-kind (x) (%gmbf-kind x))
(defgeneric %gmbf-down (n))
(defmethod %gmbf-down (n) (if (zerop n) 0 (+ 1 (%gmbf-down (1- n)))))
(defgeneric %gmbf-down2 (n acc))
(defmethod %gmbf-down2 (n acc) (if (zerop n) acc (+ 1 (%gmbf-down2 (1- n) acc))))
(defgeneric %gmbf-down3 (n))
(defmethod %gmbf-down3 (n) (if (zerop n) 0 (+ 1 (funcall (symbol-function '%gmbf-down3) (1- n)))))
")

(defun %gmbf-load ()
  (let ((src (regression-temp-file "gf-method-body-fasl-src.lisp"))
        (fasl (regression-temp-file "gf-method-body-fasl-src.fasl")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *gmbf-source* s))
    (load (compile-file src :output-file fasl))))

(when (emitting-mode-p) (%gmbf-load))

(defun %gmbf-warm (fn args) (dotimes (k 50) (dolist (a args) (funcall fn a))))

(deftest-emitting-only gf-method-body-fasl.recursion
  (list (%gmbf-fib 20) (%gmbf-efib 20) (%gmbf-efib 'a) (%gmbf-down 1000) (%gmbf-down2 1000 5)
        (%gmbf-down3 1000))
  (10946 10946 (:symbol a) 1000 1005 1000))

(deftest-emitting-only gf-method-body-fasl.methods-change
  (let ((out '()))
    (%gmbf-warm #'%gmbf-call-kind '(3 a))
    (push (list (%gmbf-call-kind 3) (%gmbf-call-kind 'a)) out)
    (eval '(defmethod %gmbf-kind ((x fixnum)) :fixnum))
    (push (list (%gmbf-call-kind 3) (%gmbf-call-kind 'a)) out)
    (eval '(defmethod %gmbf-kind ((x fixnum)) (list :fixnum (call-next-method))))
    (%gmbf-warm #'%gmbf-call-kind '(3))
    (push (%gmbf-call-kind 3) out)
    (eval '(defmethod %gmbf-kind :around ((x integer)) (list :around (call-next-method))))
    (push (%gmbf-call-kind 3) out)
    (eval '(defmethod %gmbf-kind ((x (eql 7))) (list :seven (next-method-p))))
    (push (list (%gmbf-call-kind 7) (%gmbf-call-kind 8)) out)
    (remove-method #'%gmbf-kind (find-method #'%gmbf-kind '(:around) (list (find-class 'integer))))
    (remove-method #'%gmbf-kind (find-method #'%gmbf-kind nil (list (find-class 'fixnum))))
    (push (list (%gmbf-call-kind 7) (%gmbf-call-kind 8)) out)
    (nreverse out))
  ((:integer :t) (:fixnum :t) (:fixnum :integer) (:around (:fixnum :integer))
   ((:around (:seven t)) (:around (:fixnum :integer)))
   ((:seven t) :integer)))

(deftest-emitting-only gf-method-body-fasl.stack-overflow
  (call-on-small-stack
   (lambda ()
     (list (handler-case (progn (%gmbf-down 10000000) :no-overflow)
             (storage-condition () :storage-condition)
             (error () :error))
           (handler-case (progn (%gmbf-down2 10000000 0) :no-overflow)
             (storage-condition () :storage-condition)
             (error () :error))
           (handler-case
               (progn (%gmbf-down3 10000000) :no-overflow)
             (storage-condition () :storage-condition)
             (error () :error)))))
  (:storage-condition :storage-condition :storage-condition))
