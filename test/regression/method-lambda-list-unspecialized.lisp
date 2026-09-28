;;; METHOD-LAMBDA-LIST answers the unspecialized lambda list DEFMETHOD was
;;; written with (AMOP): the parameter names the user chose, specializers
;;; dropped, everything after the required parameters as written.
;;;
;;; Bug: only the arity was recorded, so every DEFMETHOD method answered a
;;; rebuilt placeholder such as (#:R0 #:R1). Documentation tools that print a
;;; method's arglist (mgl-pax through SWANK-MOP) showed R0 instead of X.

(defgeneric mllu-gf (x y &optional z &key))
(defmethod mllu-gf ((x number) (y (eql :k)) &optional (z 3) &key (w 1 w-p) ((:v vv) nil))
  (list x y z w w-p vv))

(defgeneric mllu-plain (a &rest more))
(defmethod mllu-plain (a &rest more) (list a more))

(defgeneric mllu-none ())
(defmethod mllu-none () :none)

(defgeneric (setf mllu-place) (new obj))
(defmethod (setf mllu-place) ((new string) obj) (list new obj))

(defun %mllu-ll (gf)
  (method-lambda-list (first (dotcl-mop:generic-function-methods gf))))

(deftest method-lambda-list-unspecialized.required-and-rest
  (%mllu-ll #'mllu-gf)
  (x y &optional (z 3) &key (w 1 w-p) ((:v vv) nil)))

(deftest method-lambda-list-unspecialized.unspecialized-method
  (%mllu-ll #'mllu-plain)
  (a &rest more))

(deftest method-lambda-list-unspecialized.no-parameters
  (%mllu-ll #'mllu-none)
  nil)

(deftest method-lambda-list-unspecialized.setf-method
  (%mllu-ll (fdefinition '(setf mllu-place)))
  (new obj))

(deftest method-lambda-list-unspecialized.mop-symbol-agrees
  (let ((m (first (dotcl-mop:generic-function-methods #'mllu-gf))))
    (eq (method-lambda-list m) (dotcl-mop:method-lambda-list m)))
  t)

;; The method still dispatches and binds as before.
(deftest method-lambda-list-unspecialized.method-still-runs
  (mllu-gf 1 :k)
  (1 :k 3 1 nil nil))
