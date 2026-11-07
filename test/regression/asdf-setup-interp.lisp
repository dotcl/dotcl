;;; The ASDF glue the runtime installs after (require "asdf") is evaluated by the interpreter, not
;;; compiled. A broken glue form has to fail the same way under both: the same values or the same
;;; error type and message, nothing swallowed by the interpreter that the compiler reported.

(require "asdf")

(defun %asi-run (f form)
  (handler-case (cons :values (mapcar (lambda (v) (if (or (numberp v) (symbolp v)) v (type-of v)))
                                       (multiple-value-list (funcall f form))))
    (error (e) (list :error (type-of e) (princ-to-string e)))))

(defun %asi-eval (form) (eval form))
(defun %asi-interp (form) (dotcl.cil-compiler::%mini-eval form nil))

(deftest asdf-setup-interp.same-as-compiled
  (let ((bad nil))
    (dolist (form '((undefined-fn-xyz 1)
                    (let ((x 1)) (car x))
                    (setq %asi-undefined-var 3)
                    (defmethod %asi-no-gf :after ((a integer)) a)
                    (funcall (find-symbol "LOAD-SYSTEM" "NO-SUCH-PACKAGE-XYZ") 'q)
                    (let ((u 1)) (declare (ignore u)) u)
                    (error "boom")
                    (values 1 2 3)
                    (let ((p (find-symbol "NOPE" "COMMON-LISP-USER"))) (when p (set p 1)))))
      (let ((a (%asi-run #'%asi-eval form)) (b (%asi-run #'%asi-interp form)))
        (unless (equal a b)
          (push (list form a b) bad))))
    bad)
  nil)

(deftest asdf-setup-interp.glue-installed
  (and (member (find-symbol "MODULE-PROVIDE-ASDF" "ASDF") (symbol-value (find-symbol "*MODULE-PROVIDER-FUNCTIONS*" "CL")))
       (find-method #'asdf:perform '(:after) (list (find-class 'asdf:compile-op) (find-class 'asdf:cl-source-file)) nil)
       (find-method #'asdf:perform-with-restarts '(:around) (list (find-class 'asdf:load-op) (find-class 'asdf:cl-source-file)) nil)
       t)
  t)
