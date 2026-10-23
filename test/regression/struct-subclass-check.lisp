;;; An argument declared a structure type accepts instances of the structures
;;; that include it, at any depth, and rejects other structures and non-structures;
;;; TYPEP called at run time answers the same.

(defstruct %ssc-a)
(defstruct (%ssc-b (:include %ssc-a)))
(defstruct (%ssc-c (:include %ssc-b)))
(defstruct %ssc-other)

(defun %ssc-take (x) (declare (type %ssc-a x)) (type-of x))
(defun %ssc-take-b (x) (declare (type %ssc-b x)) (type-of x))
(defun %ssc-try (fn x)
  (handler-case (funcall fn x) (type-error (e) (list :type-error (type-error-expected-type e)))))

(deftest struct-subclass-check.args
  (list (%ssc-try #'%ssc-take (make-%ssc-a))
        (%ssc-try #'%ssc-take (make-%ssc-b))
        (%ssc-try #'%ssc-take (make-%ssc-c))
        (%ssc-try #'%ssc-take (make-%ssc-other))
        (%ssc-try #'%ssc-take 5)
        (%ssc-try #'%ssc-take-b (make-%ssc-c))
        (%ssc-try #'%ssc-take-b (make-%ssc-a)))
  (%ssc-a %ssc-b %ssc-c (:type-error %ssc-a) (:type-error %ssc-a) %ssc-c (:type-error %ssc-b)))

(deftest struct-subclass-check.typep
  (let ((ty '%ssc-a) (tb '%ssc-b))
    (list (typep (make-%ssc-c) ty) (typep (make-%ssc-b) ty) (typep (make-%ssc-other) ty)
          (typep (make-%ssc-a) tb) (typep (make-%ssc-c) tb) (typep (make-%ssc-c) 'structure-object)))
  (t t nil nil t t))
