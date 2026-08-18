;;; A string literal whose value is discarded must not be evaluated at run time,
;;; and one whose value is used must still be there.
;;;
;;; A documentation string compiles to a string literal in statement position,
;;; so before the peephole every call of a documented function constructed and
;;; threw away a LispString. The hazard in eliding it is the body that consists
;;; of nothing but the string: there the string is the return value, not
;;; documentation.

(defun %slsp-doc-only ()
  "I am the return value.")

(defun %slsp-doc-then-body (x)
  "I am documentation."
  (* x 2))

(defun %slsp-doc-then-string (x)
  "I am documentation."
  (declare (ignore x))
  "I am the return value.")

(defun %slsp-middle (x)
  "I am documentation."
  (let ((kept "kept"))
    "discarded"
    (list kept x)))

(deftest string-literal-statement-position.doc-only
  (%slsp-doc-only)
  "I am the return value.")

(deftest string-literal-statement-position.doc-then-body
  (%slsp-doc-then-body 21)
  42)

(deftest string-literal-statement-position.doc-then-string
  (%slsp-doc-then-string 1)
  "I am the return value.")

(deftest string-literal-statement-position.middle-string-discarded
  (%slsp-middle 3)
  ("kept" 3))

;;; Same shapes through LAMBDA rather than DEFUN.

(deftest string-literal-statement-position.lambda-doc-only
  (funcall (lambda () "value"))
  "value")

(deftest string-literal-statement-position.lambda-doc-then-body
  (funcall (lambda (x) "doc" (1+ x)) 1)
  2)

;;; A discarded string with side-effecting neighbours keeps evaluation order.

(deftest string-literal-statement-position.order-preserved
  (let ((log '()))
    (progn (push :a log)
           "discarded"
           (push :b log))
    (reverse log))
  (:a :b))
