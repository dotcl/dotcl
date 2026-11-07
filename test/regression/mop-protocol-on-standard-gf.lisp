;;; A method on a protocol generic function (COMPUTE-APPLICABLE-METHODS-USING-CLASSES)
;;; specialised on STANDARD-GENERIC-FUNCTION applies to every generic function,
;;; the protocol ones included. Calling any generic function used to overflow
;;; the stack: dispatching COMPUTE-APPLICABLE-METHODS went through the protocol,
;;; which called it again.

(defvar *mpsg-log* nil)

(defgeneric %mpsg-f (x))
(defmethod %mpsg-f ((x integer)) (* x 2))

(deftest mop-protocol-on-standard-gf.no-recursion
  (let ((m (eval '(defmethod dotcl-mop:compute-applicable-methods-using-classes
                      :before ((gf standard-generic-function) classes)
                    (declare (ignore classes))
                    (push (dotcl-mop:generic-function-name gf) *mpsg-log*)))))
    (setf *mpsg-log* nil)
    (unwind-protect
         (progn (defgeneric %mpsg-g (x))
                (defmethod %mpsg-g ((x symbol)) :sym)
                (list (%mpsg-g 'a) (%mpsg-f 4)
                      (not (null (member '%mpsg-g *mpsg-log*)))))
      (remove-method #'dotcl-mop:compute-applicable-methods-using-classes m)))
  (:sym 8 t))

(deftest mop-protocol-on-standard-gf.removed-restores
  (%mpsg-f 5)
  10)
