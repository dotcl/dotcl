;;; A negative digit count for ~$ does not leak a .NET exception message.

(deftest format-dollar-negative-digits
  (stringp (handler-case (format nil "~-3$" 5) (error () "error")))
  t)

(deftest format-dollar-negative-digits.v
  (stringp (handler-case (format nil "~v$" -5 5) (error () "error")))
  t)

(deftest format-dollar-negative-digits.not-dotnet
  (let ((m (handler-case (progn (format nil "~-3$" 5) "")
             (error (c) (princ-to-string c)))))
    (null (search "Parameter" m)))
  t)

(deftest format-dollar-ok
  (format nil "~2$" 3.14159)
  "3.14")
