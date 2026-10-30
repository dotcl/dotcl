;;; A local function whose body returns from its own block kept its
;;; declarations inside that block, where they declare nothing: the parameter's
;;; type was not checked. Expected values are SBCL's.

(defmacro %lfrd (name expected form &rest args)
  `(deftest-emitting-only ,(intern (format nil "LOCAL-FN-RETURN-FROM-KEEPS-DECLARATIONS.~a" name))
     (let ((*error-output* (make-broadcast-stream)))
       (handler-case (funcall (compile nil ',form) ,@args)
         (type-error () :type-error)))
     ,expected))

(%lfrd "FLET" :type-error
  (lambda (v) (flet ((g (p) (declare (type (integer 0 10) p)) (return-from g p))) (g v)))
  100)

(%lfrd "FLET-IN-RANGE" 7
  (lambda (v) (flet ((g (p) (declare (type (integer 0 10) p)) (return-from g p))) (g v)))
  7)

(%lfrd "FLET-NOTINLINE" :type-error
  (lambda (v)
    (flet ((g (p) (declare (type (unsigned-byte 8) p)) (return-from g (+ p 1))))
      (declare (notinline g))
      (g v)))
  300)

(%lfrd "FLET-WHEN" :type-error
  (lambda (v)
    (flet ((g (p) (declare (type (integer 0 10) p)) (when (> v 0) (return-from g p)) 7))
      (g v)))
  100)

(%lfrd "LABELS" :type-error
  (lambda (v) (labels ((g (p) (declare (type (integer 0 10) p)) (return-from g p))) (g v)))
  100)

(%lfrd "LABELS-RECURSIVE" :type-error
  (lambda (v)
    (labels ((g (p n) (declare (type (integer 0 10) p))
               (if (> n 0) (g p (1- n)) (return-from g p))))
      (g v 2)))
  100)

(%lfrd "LABELS-OPTIONAL-FUNCALL" :type-error
  (lambda (v)
    (labels ((g (p &optional (q 1)) (declare (type (integer -560 20150) p))
               (return-from g (+ p q))))
      (declare (notinline g))
      (funcall #'g v)))
  (expt 2 64))

(%lfrd "SPECIAL" 5
  (lambda (v)
    (flet ((h () (locally (declare (special lfrd-x)) lfrd-x)))
      (flet ((g (lfrd-x) (declare (special lfrd-x)) (return-from g (h))))
        (g v))))
  5)

(%lfrd "DOCSTRING" :type-error
  (lambda (v) (flet ((g (p) "doc" (declare (type (integer 0 10) p)) (return-from g p))) (g v)))
  100)
