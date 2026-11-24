;;; CLHS 5.1.1.1: the subforms of (SETF (DOCUMENTATION ...)),
;;; (SETF (MACRO-FUNCTION ...)) and (SETF (SYMBOL-PLIST ...)) are evaluated
;;; left to right before the new value form. They used to run after it.

(defvar *sdmop-log* nil)
(defun sdmop-tick (n value) (push n *sdmop-log*) value)

(defmacro sdmop-order (&body body)
  `(progn (setf *sdmop-log* nil) ,@body (nreverse *sdmop-log*)))

(deftest setf-documentation-place-order
  (sdmop-order
    (setf (documentation (sdmop-tick 1 'sdmop-doc)
                         (sdmop-tick 2 'function))
          (sdmop-tick 3 "order doc")))
  (1 2 3))

(deftest setf-macro-function-place-order
  (sdmop-order
    (setf (macro-function (sdmop-tick 1 'sdmop-macro))
          (sdmop-tick 2 (lambda (form env) (declare (ignore form env)) nil))))
  (1 2))

(deftest setf-macro-function-environment-order
  (sdmop-order
    (setf (macro-function (sdmop-tick 1 'sdmop-macro-env)
                          (sdmop-tick 2 nil))
          (sdmop-tick 3 (lambda (form env) (declare (ignore form env)) nil))))
  (1 2 3))

(deftest setf-symbol-plist-place-order
  (sdmop-order
    (setf (symbol-plist (sdmop-tick 1 'sdmop-plist))
          (sdmop-tick 2 nil)))
  (1 2))

(deftest setf-documentation-returns-the-new-value
  (let ((new "return doc"))
    (prog1 (eq (setf (documentation 'sdmop-doc-ret 'function) new) new)
      (setf (documentation 'sdmop-doc-ret 'function) nil)))
  t)

(deftest setf-documentation-value-evaluated-once
  (let ((n 0))
    (setf (documentation 'sdmop-doc-once 'function)
          (progn (incf n) "once"))
    (prog1 n (setf (documentation 'sdmop-doc-once 'function) nil)))
  1)

(deftest setf-documentation-doc-types
  (prog1
      (list (progn (setf (documentation 'sdmop-dt 'variable) "var")
                   (documentation 'sdmop-dt 'variable))
            (progn (setf (documentation 'sdmop-dt 'type) "type")
                   (documentation 'sdmop-dt 'type))
            (progn (setf (documentation 'sdmop-dt 'setf) "setf")
                   (documentation 'sdmop-dt 'setf))
            (progn (setf (documentation 'sdmop-dt 'compiler-macro) "cm")
                   (documentation 'sdmop-dt 'compiler-macro)))
    (setf (documentation 'sdmop-dt 'variable) nil)
    (setf (documentation 'sdmop-dt 'type) nil)
    (setf (documentation 'sdmop-dt 'setf) nil)
    (setf (documentation 'sdmop-dt 'compiler-macro) nil))
  ("var" "type" "setf" "cm"))

(deftest setf-macro-function-returns-the-new-value
  (let ((fn (lambda (form env) (declare (ignore form env)) nil)))
    (eq (setf (macro-function 'sdmop-macro-ret) fn) fn))
  t)

(deftest setf-symbol-plist-returns-the-new-value
  (let ((plist (list 'a 1)))
    (prog1 (and (eq (setf (symbol-plist 'sdmop-plist-ret) plist) plist)
                (eq (symbol-plist 'sdmop-plist-ret) plist))
      (setf (symbol-plist 'sdmop-plist-ret) nil)))
  t)
