;;; An iteration (~{ ~}) whose body has no justification (~<...~:;...~>) costs time in proportion to
;;; its output (checked by counting, not timing): it used to look for the column the text so far ends at before every pass, scanning
;;; back to the last newline, which made a long one quadratic (a very long one still has another,
;;; smaller superlinear cost). A body that does justify still
;;; sees the column.

(defun %fil-scans (thunk)
  "How many times FORMAT looked for the current column while THUNK ran."
  (let ((before (dotnet:static "DotCL.Runtime" "FormatColumnScans")))
    (funcall thunk)
    (- (dotnet:static "DotCL.Runtime" "FormatColumnScans") before)))

(deftest format-iteration-linear.no-column-scan-per-pass
  ;; A count, not a time: a body that has no justification asks for the column never; one that
  ;; may (a ~< , or a ~? / ~/ /, which run a format string of their own) asks once per pass.
  (let ((list (loop for i below 3000 collect i)))
    (list (%fil-scans (lambda () (format nil "~{~A~^, ~}" list)))
          (%fil-scans (lambda () (format nil "~{~S ~A~^|~}" '(1 2 3 4))))
          (> (%fil-scans (lambda () (format nil "~{~<~A~>~}" list))) 2999)
          (> (%fil-scans (lambda () (format nil "~{~?~}" (loop for i below 3000 append (list "~A" (list i)))))) 2999)))
  (0 0 t t))

(deftest format-iteration-linear.same-output
  (list (format nil "~{~A~^, ~}" '(1 2 3))
        (format nil "abc~{~A~^,~}" '(1 2))
        (format nil "~{~?~^|~}" '("~A" (1) "~S" ("x")))
        (length (format nil "~{~A~^, ~}" (loop for i below 1000 collect i))))
  ("1, 2, 3" "abc1,2" "1|\"x\"" 4888))
