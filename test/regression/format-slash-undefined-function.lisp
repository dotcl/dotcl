;;; ~/name/ with a name that is no function signals UNDEFINED-FUNCTION; it does not print
;;; nothing and carry on (which hides a misspelt name).

(defun fsu-good (stream arg &rest params)
  (declare (ignore params))
  (format stream "<~A>" arg))

(deftest format-slash-undefined-function.unqualified
  (handler-case (format nil "a~/fsu-no-such-function/b" 1)
    (undefined-function (e) (list :undefined (cell-error-name e))))
  (:undefined fsu-no-such-function))

(deftest format-slash-undefined-function.qualified
  (handler-case (format nil "a~/cl-user::fsu-nosuch/b" 1)
    (undefined-function (e) (list :undefined (cell-error-name e))))
  (:undefined fsu-nosuch))

(deftest format-slash-undefined-function.modifiers-and-nesting
  (list (handler-case (format nil "~:/fsu-nosuch-a/" 1) (error (e) (type-of e)))
        (handler-case (format nil "~@/fsu-nosuch-b/" 1) (error (e) (type-of e)))
        (handler-case (format nil "~{~/fsu-nosuch-c/~}" '(1)) (error (e) (type-of e))))
  (undefined-function undefined-function undefined-function))

(deftest-emitting-only format-slash-undefined-function.formatter
  (handler-case (funcall (formatter "a~/fsu-nosuch-d/b") nil 1)
    (error (e) (type-of e)))
  undefined-function)

(deftest format-slash-undefined-function.defined-function-still-works
  (list (format nil "a~/fsu-good/b" 1)
        (format nil "a~/cl-user::fsu-good/b" 2)
        (format nil "~{~/fsu-good/~^,~}" '(1 2)))
  ("a<1>b" "a<2>b" "<1>,<2>"))
