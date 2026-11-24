;;; A newline written literally in a FORMAT control string, inside a logical block with a per-line
;;; prefix, starts the next line with the prefix, as ~% and WRITE-STRING do.

(defun %fln-block (fn)
  (with-output-to-string (s)
    (pprint-logical-block (s nil :per-line-prefix ";; ")
      (funcall fn s))))

(deftest format-literal-newline-line-prefix.block
  (list (%fln-block (lambda (s) (format s "one
two")))
        (%fln-block (lambda (s) (format s "one~%two")))
        (%fln-block (lambda (s) (write-string (format nil "one~%two") s)))
        (%fln-block (lambda (s) (format s "a~A
b" 1))))
  (";; one
;; two" ";; one
;; two" ";; one
;; two" ";; a1
;; b"))

(deftest format-literal-newline-line-prefix.outside-a-block
  (list (format nil "one
two") (with-output-to-string (s) (format s "x
y")))
  ("one
two" "x
y"))
