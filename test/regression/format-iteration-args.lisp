;;; An iteration (~{ ~} or ~@{ ~}) whose body has no ~* runs on the argument array in place instead of
;;; copying the rest of it for every pass (checked by counting copies, not timing): the copy made a
;;; long iteration quadratic. A body with ~* still gets its own view of the arguments.

(defun %fia-copies (thunk)
  (let ((before (dotnet:static "DotCL.Runtime" "FormatIterationCopies")))
    (funcall thunk)
    (- (dotnet:static "DotCL.Runtime" "FormatIterationCopies") before)))

(deftest format-iteration-args.no-copy-without-star
  (let ((list (loop for i below 2000 collect i)))
    (list (%fia-copies (lambda () (format nil "~{~A~^,~}" list)))
          (%fia-copies (lambda () (format nil "~@{~A~^,~}" 1 2 3 4)))
          (%fia-copies (lambda () (format nil "~{~A ~*~}" list)))))
  (0 0 1000))

(deftest format-iteration-args.same-output
  (list (format nil "~{~A~^,~}" '(1 2 3))
        (format nil "~@{~A~^,~}" 1 2 3)
        (format nil "~{~A~A~^|~}" '(1 2 3 4))
        (format nil "~@{~A~A~^|~}" 1 2 3 4)
        (format nil "~2{~A~}" '(1 2 3 4))
        (format nil "~{~A ~*~}" '(1 2 3 4))
        (format nil "~{~A~:*~A~^ ~}" '(1 2 3))
        (format nil "x~@{~A~}y" 7 8 9)
        (format nil "~{~}" "~A~A" '(1 2))
        (format nil "~{~#[~;~A~:;~A,~]~}" '(1 2 3)))
  ("1,2,3" "1,2,3" "12|34" "12|34" "12" "1 3 " "11 22 33" "x789y" "12" "1,2,3"))
