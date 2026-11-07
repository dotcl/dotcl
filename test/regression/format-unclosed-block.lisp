;;; A FORMAT block directive with no closing directive is an error, not a silent
;;; drop of the rest of the control string.

(defmacro fub-error-p (form)
  `(handler-case (progn ,form nil) (error () t)))

(deftest format-unclosed.conditional
  (fub-error-p (format nil "~[x" 0))
  t)

(deftest format-unclosed.conditional.at
  (fub-error-p (format nil "~@[x" 5))
  t)

(deftest format-unclosed.case
  (fub-error-p (format nil "~(abc" 5))
  t)

(deftest format-unclosed.iteration
  (fub-error-p (format nil "~{~A" '(1 2)))
  t)

(deftest format-unclosed.justify
  (fub-error-p (format nil "~<abc" 5))
  t)

(deftest format-unclosed.message-names-directive
  (let ((m (handler-case (format nil "~[x" 0) (error (c) (princ-to-string c)))))
    (and (search "~]" m) t))
  t)

(deftest format-unclosed.closed-still-fine
  (list (format nil "~[a~;b~]" 1) (format nil "~(ABC~)" nil) (format nil "~{~A~}" '(1 2)) (format nil "~@[x~]" nil))
  ("b" "abc" "12" ""))
