;;; DEFMETHOD on a name whose global function definition is an existing
;;; generic function must add the method to that generic function, even when
;;; the generic function was created under another name and installed with
;;; (setf symbol-function) or (setf fdefinition).
;;;
;;; The generic function lookup used only the registry keyed by the name the
;;; generic function was created under, so DEFMETHOD on the alias made a new,
;;; unrelated generic function and replaced the alias's definition with it.
;;; The method was then invisible through the original name, and
;;; CALL-NEXT-METHOD in it found no next method. cl-marshal's
;;; CLASS-PERSISTANT-SLOTS (a deliberately misspelled alias of
;;; CLASS-PERSISTENT-SLOTS) lost the DINGHY method this way.

(defgeneric %gfa-slots (x))
(defmethod %gfa-slots ((x t)) '(base))
(setf (symbol-function '%gfa-slots-alias) #'%gfa-slots)
(defclass %gfa-d1 () ())
(defmethod %gfa-slots-alias ((x %gfa-d1)) (append (call-next-method) '(extra)))

(deftest defmethod-on-gf-alias-adds-to-same-gf
  (list (%gfa-slots (make-instance '%gfa-d1))
        (%gfa-slots-alias (make-instance '%gfa-d1))
        (eq #'%gfa-slots #'%gfa-slots-alias)
        (%gfa-slots 3))
  ((base extra) (base extra) t (base)))

(defgeneric %gfa-ref (x))
(defgeneric (setf %gfa-ref) (v x))
(defmethod (setf %gfa-ref) (v (x cons)) (setf (car x) v))
(setf (fdefinition '(setf %gfa-ref-alias)) (fdefinition '(setf %gfa-ref)))
(defmethod (setf %gfa-ref-alias) (v (x vector)) (setf (aref x 0) v))

(deftest defmethod-on-setf-gf-alias-adds-to-same-gf
  (let ((c (list 1 2)) (v (vector 1 2)))
    (setf (%gfa-ref c) :a)
    (setf (%gfa-ref v) :b)
    (list c (coerce v (quote list)) (eq (fdefinition '(setf %gfa-ref)) (fdefinition '(setf %gfa-ref-alias)))))
  ((:a 2) (:b 2) t))
