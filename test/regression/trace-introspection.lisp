;;; TRACE must not change what introspection says about the traced name.
;;;
;;; TRACE used to put a fresh wrapper function in the name's place, so while a
;;; function was traced (DOCUMENTATION name 'FUNCTION) answered NIL, the lambda
;;; list was unknown, and a traced generic function was no longer a
;;; GENERIC-FUNCTION. SBCL answers all three about the traced function, and a
;;; documentation tool (mgl-pax) relies on it. A traced generic function now keeps
;;; its identity: the trace is installed as its discriminating function.

(defun %ti-fn (x &rest args)
  "TI-FN doc."
  (declare (ignore args))
  x)

(defgeneric %ti-gf (x)
  (:documentation "TI-GF doc."))
(defmethod %ti-gf ((x integer)) (* x 10))
(defmethod %ti-gf ((x string)) (length x))

(defmacro %ti-traced ((&rest names) &body body)
  `(let ((*trace-output* (make-broadcast-stream)))
     (unwind-protect (progn (trace ,@names) ,@body)
       (untrace ,@names))))

(deftest trace-introspection.function-documentation
  (%ti-traced (%ti-fn)
    (documentation '%ti-fn 'function))
  "TI-FN doc.")

(deftest trace-introspection.function-lambda-list
  (%ti-traced (%ti-fn)
    (list (dotcl:function-lambda-list '%ti-fn)
          (dotcl:function-lambda-list #'%ti-fn)))
  ((x &rest args) (x &rest args)))

(deftest trace-introspection.function-still-runs
  (%ti-traced (%ti-fn)
    (%ti-fn 3 4 5))
  3)

(deftest trace-introspection.gf-identity
  (%ti-traced (%ti-gf)
    (list (typep #'%ti-gf 'generic-function)
          (documentation '%ti-gf 'function)
          (dotcl:function-lambda-list '%ti-gf)))
  (t "TI-GF doc." (x)))

(deftest trace-introspection.gf-dispatch-while-traced
  (%ti-traced (%ti-gf)
    (list (%ti-gf 2) (%ti-gf "abc") (funcall #'%ti-gf 4)))
  (20 3 40))

(deftest trace-introspection.gf-calls-are-traced
  (let ((out (make-string-output-stream)))
    (let ((*trace-output* out))
      (trace %ti-gf)
      (unwind-protect (%ti-gf 1)
        (untrace %ti-gf)))
    (let ((s (get-output-stream-string out)))
      (and (search "%TI-GF" s) (search "10" s) t)))
  t)

(deftest trace-introspection.gf-untrace-restores
  (let ((out (make-string-output-stream)))
    (let ((*trace-output* out))
      (trace %ti-gf)
      (untrace %ti-gf)
      (%ti-gf 1))
    (list (get-output-stream-string out) (trace)))
  ("" nil))

(deftest trace-introspection.gf-new-method-while-traced
  (%ti-traced (%ti-gf)
    (defmethod %ti-gf ((x symbol)) :sym)
    (prog1 (list (%ti-gf 'a) (%ti-gf 5))
      (remove-method #'%ti-gf (find-method #'%ti-gf '() (list (find-class 'symbol))))))
  (:sym 50))
