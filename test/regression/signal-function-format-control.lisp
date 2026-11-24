;;; SIGNAL, WARN and ERROR accept a function format control, with or without arguments.

(deftest signal-function-format-control
  (handler-case (signal (lambda (s) (format s "hi")))
    (condition (c) (princ-to-string c)))
  "hi")

(deftest signal-function-format-control-arguments
  (let ((fn (lambda (s a b) (format s "~a-~a" a b))))
    (handler-case (signal fn "x" "y")
      (condition (c)
        (list (type-of c)
              (princ-to-string c)
              (simple-condition-format-arguments c)
              (eq (simple-condition-format-control c) fn)))))
  (simple-condition "x-y" ("x" "y") t))

(deftest signal-function-format-control-report
  (handler-case (signal (lambda (s) (format s "plain")))
    (condition (c)
      (list (princ-to-string c)
            (format nil "~a" c)
            (with-output-to-string (s) (princ c s)))))
  ("plain" "plain" "plain"))

(deftest signal-function-format-control-report-arguments
  (handler-case (signal (lambda (s a b) (format s "~a-~a" a b)) "x" "y")
    (condition (c) (list (princ-to-string c) (format nil "~a" c))))
  ("x-y" "x-y"))

(deftest signal-function-format-control-no-handler
  (signal (lambda (s) (format s "nobody")))
  nil)

(deftest warn-function-format-control
  (handler-case (warn (lambda (s) (format s "careful")))
    (warning (c) (princ-to-string c)))
  "careful")

(deftest warn-function-format-control-arguments
  (let ((fn (lambda (s a b) (format s "~a-~a" a b))))
    (handler-case (warn fn "x" "y")
      (warning (c)
        (list (type-of c)
              (princ-to-string c)
              (simple-condition-format-arguments c)
              (eq (simple-condition-format-control c) fn)))))
  (simple-warning "x-y" ("x" "y") t))

(deftest error-function-format-control
  (handler-case (error (lambda (s) (format s "boom")))
    (error (c) (princ-to-string c)))
  "boom")

(deftest error-function-format-control-arguments
  (let ((fn (lambda (s a b) (format s "~a-~a" a b))))
    (handler-case (error fn "x" "y")
      (error (c)
        (list (type-of c)
              (princ-to-string c)
              (simple-condition-format-arguments c)
              (eq (simple-condition-format-control c) fn)))))
  (simple-error "x-y" ("x" "y") t))

(deftest make-condition-function-format-control
  (let ((fn (lambda (s) (format s "made"))))
    (let ((c (make-condition 'simple-condition :format-control fn)))
      (list (princ-to-string c)
            (format nil "~a" c)
            (eq (simple-condition-format-control c) fn))))
  ("made" "made" t))

(deftest make-condition-function-format-control-arguments
  (let ((c (make-condition 'simple-condition
                           :format-control (lambda (s a b) (format s "~a-~a" a b))
                           :format-arguments (list "x" "y"))))
    (list (princ-to-string c) (simple-condition-format-arguments c)))
  ("x-y" ("x" "y")))

(deftest make-instance-function-format-control
  (let ((c (make-instance 'simple-condition
                          :format-control (lambda (s) (format s "instance")))))
    (princ-to-string c))
  "instance")

(deftest signal-non-designator-type-error
  (handler-case (signal 42)
    (type-error () :type-error)
    (condition () :other))
  :type-error)
