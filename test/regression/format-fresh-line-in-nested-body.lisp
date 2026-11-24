;;; ~& at the start of a nested body (~{, ~[, ~(, ~?, ~<) asks about the text already
;;; written before the body, not about the body's own empty buffer. Expected values are SBCL's.

(defun fresh-nested-run (control &rest args)
  (handler-case (apply #'format nil control args)
    (error () :error)))

(deftest format-fresh-line-in-nested-body.control-strings
  (list
   (apply #'fresh-nested-run '("~{~&~A~}" (a b)))
   (apply #'fresh-nested-run '("x~{~&~A~}" (a b)))
   (apply #'fresh-nested-run '("x~a~{~&~a~}" 1 (2)))
   (apply #'fresh-nested-run '("Got:~%~{~&~A~}~%E" (a b c)))
   (apply #'fresh-nested-run '("x~@{~&~A~}" a b))
   (apply #'fresh-nested-run '("x~:{~&~A~}" ((a) (b))))
   (apply #'fresh-nested-run '("~{~&~A~}~&z" (a b)))
   (apply #'fresh-nested-run '("x~{~%~&~A~}" (a b)))
   (apply #'fresh-nested-run '("x~{~&~&~A~}" (a b)))
   (apply #'fresh-nested-run '("x~{~2&~A~}" (a)))
   (apply #'fresh-nested-run '("x~{~0&~A~}" (a b)))
   (apply #'fresh-nested-run '("x~[~&y~]" 0))
   (apply #'fresh-nested-run '("x~[a~;~&y~]" 1))
   (apply #'fresh-nested-run '("x~:[~&n~;~&y~]" nil))
   (apply #'fresh-nested-run '("x~@[~&y~]" 1))
   (apply #'fresh-nested-run '("x~(~&y~)"))
   (apply #'fresh-nested-run '("x~:@(~&y~)"))
   (apply #'fresh-nested-run '("x~?" "~&y" nil))
   (apply #'fresh-nested-run '("x~@?" "~&y"))
   (apply #'fresh-nested-run '("~[~&y~]" 0))
   (apply #'fresh-nested-run '("~{~A~&~}" (a b)))
   (apply #'fresh-nested-run '("a~%~{~&~A~}" (b)))
   (apply #'fresh-nested-run '("a~%~[~&y~]" 0))
   (apply #'fresh-nested-run '("x~<~&y~>"))
   (apply #'fresh-nested-run '("x~<~&y~;z~>"))
   (apply #'fresh-nested-run '("x~10<~&y~>"))
   (apply #'fresh-nested-run '("~<~&x~>~&y"))
   (apply #'fresh-nested-run '("x~<~{~&~A~}~:>" (a b)))
   (apply #'fresh-nested-run '("x~{~&~A~^,~}" (a b)))
   (apply #'fresh-nested-run '("x~{~&~{~&~A~}~}" ((a b) (c))))
   (apply #'fresh-nested-run '("x~:{~&~A~}" ((a) (b))))
   (apply #'fresh-nested-run '("x~1{~&~A~}" (a b)))
   (apply #'fresh-nested-run '("~A~{~&~A~}~A" 1 (2 3) 4))
   )
  ("A
B" "x
A
B" "x1
2" "Got:
A
B
C
E" "x
A
B" "x
A
B" "A
B
z" "x
A
B" "x
A
B" "x

A" "xAB" "x
y" "x
y" "x
n" "x
y" "x
y" "x
Y" "x
y" "x
y" "y" "A
B
" "a
B" "a
y" "xy" "xyz" "x         y" "x
y" :error "x
A,
B" "x
A
B
C" "x
A
B" "x
A" "1
2
34"))

(deftest format-fresh-line-in-nested-body.stream
  (list
   (with-output-to-string (s) (write-string "z" s) (format s "~{~&~A~}" '(a b)))
   (with-output-to-string (s) (format s "~{~&~A~}" '(a b)))
   (with-output-to-string (s) (format s "~A~{~&~A~}" 'q '(a b)))
   (with-output-to-string (s) (write-string "z" s) (terpri s) (format s "~{~&~A~}" '(a b)))
   (with-output-to-string (s) (format s "x~[~&y~]" 0))
   (with-output-to-string (s) (write-string "z" s) (format s "~(~&y~)")))
  ("z
A
B" "A
B" "Q
A
B" "z
A
B" "x
y" "z
y"))
