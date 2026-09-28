;;; Calling a generic function with no applicable method calls
;;; NO-APPLICABLE-METHOD with the generic function and the arguments (CLHS
;;; 7.6.6), so a method on it decides the result. Dispatch used to signal an
;;; error itself and never call it. The default method signals an error of a
;;; class of its own (a proper subtype of ERROR), as SBCL does; serapeum's
;;; NO-APPLICABLE-METHOD-ERROR type is computed from it.

(defgeneric namp-none (x))
(defgeneric namp-eql (x) (:method ((x (eql 3))) :three))
(defgeneric namp-plain (x))

(defmethod no-applicable-method ((gf (eql #'namp-none)) &rest args)
  (list :handled args))
(defmethod no-applicable-method ((gf (eql #'namp-eql)) &rest args)
  (list :handled-eql args))

(deftest no-applicable-method-protocol.user-method
  (list (namp-none 1) (namp-none 2))
  ((:handled (1)) (:handled (2))))

(deftest no-applicable-method-protocol.eql-miss
  (list (namp-eql 3) (namp-eql 1) (namp-eql 3) (namp-eql 1))
  (:three (:handled-eql (1)) :three (:handled-eql (1))))

(deftest no-applicable-method-protocol.default-error-class
  (handler-case (namp-plain 1)
    (error (e)
      (let ((type (type-of e)))
        (list (typep e 'error)
              (and (subtypep type 'error) (not (subtypep 'error type)))
              (typep (handler-case (no-applicable-method #'namp-plain 1)
                       (error (e2) e2))
                     type)))))
  (t t t))
