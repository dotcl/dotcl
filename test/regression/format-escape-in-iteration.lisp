;;; ~^ directly in the body of ~{ ~} or ~@{ ~} ends the iteration without an exception;
;;; a ~^ in a nested construct still leaves through one. The results below are those of the
;;; exception-only implementation, so the text, the consumed arguments and the errors are unchanged.

(defmacro %fei (control &rest args)
  `(handler-case (format nil ,control ,@args) (error (e) (list :error (type-of e)))))

(deftest format-escape.case-01
  (%fei "~{~a~^,~}" '(1 2 3))
  "1,2,3")

(deftest format-escape.case-02
  (%fei "~{~a~^,~}" '(1))
  "1")

(deftest format-escape.case-03
  (%fei "~{~a~^,~}" NIL)
  "")

(deftest format-escape.case-04
  (%fei "[~{~a~^, ~}]" '(A B C D))
  "[A, B, C, D]")

(deftest format-escape.case-05
  (%fei "~{~a~^,~} end" '(1 2))
  "1,2 end")

(deftest format-escape.case-06
  (%fei "~@{~a~^-~}" 1 2 3)
  "1-2-3")

(deftest format-escape.case-07
  (%fei "~@{~a~^-~}~a" 1 2 3)
  (:ERROR PROGRAM-ERROR))

(deftest format-escape.case-08
  (%fei "~{~a~^,~}|~{~a~^;~}" '(1 2) '(3 4 5))
  "1,2|3;4;5")

(deftest format-escape.case-09
  (%fei "~{~^~a~}" '(1 2 3))
  "123")

(deftest format-escape.case-10
  (%fei "~2{~a~^,~}" '(1 2 3 4))
  "1,2,")

(deftest format-escape.case-11
  (%fei "~3{~a~^,~}" '(1))
  "1")

(deftest format-escape.case-12
  (%fei "~{~a~#^,~}" '(1 2 3))
  "1,2,3")

(deftest format-escape.case-13
  (%fei "~{~a~0^,~}" '(1 2 3))
  "1")

(deftest format-escape.case-14
  (%fei "~{~a~1,1^,~}" '(1 2 3))
  "1")

(deftest format-escape.case-15
  (%fei "~{~a~1,2,3^,~}" '(1 2 3))
  "1")

(deftest format-escape.case-16
  (%fei "~{~{~a~^+~}~^;~}" '((1 2) (3) (4 5 6)))
  "1+2;3;4+5+6")

(deftest format-escape.case-17
  (%fei "~{[~{~a~^,~}]~^ ~}" '((1 2) NIL (3)))
  "[1,2] [] [3]")

(deftest format-escape.case-18
  (%fei "~{~a~^~{x~^y~}~}" '(1 2))
  (:ERROR TYPE-ERROR))

(deftest format-escape.case-19
  (%fei "~{~a~[~^!~:;?~]~}" '(1 0 2 1 3))
  (:ERROR PROGRAM-ERROR))

(deftest format-escape.case-20
  (%fei "~{~(~a~^,~)~}" '(A B C))
  "a,b,c")

(deftest format-escape.case-21
  (%fei "~{~a~:[~;~^;~]~}" '(1 NIL 2 T 3))
  (:ERROR PROGRAM-ERROR))

(deftest format-escape.case-22
  (%fei "~:{~a~:^,~}" '((1 2) (3 4)))
  "1,3")

(deftest format-escape.case-23
  (%fei "~:{~a~^,~a~:^;~}" '((1 2) (3 4)))
  "1,2;3,4")

(deftest format-escape.case-24
  (%fei "~@:{~a~:^,~}" '(1 2) '(3 4))
  "1,3")

(deftest format-escape.case-25
  (%fei "~{~a~^,~} ~a" '(1 2 3) 'Z)
  "1,2,3 Z")

(deftest format-escape.case-26
  (%fei "~@{~a~^,~}~a" 1 2)
  (:ERROR PROGRAM-ERROR))

(deftest format-escape.case-27
  (%fei "~{~a~^,~}~{~a~^;~}" '(1) '(2 3))
  "12;3")

(deftest format-escape.case-28
  (%fei "ab~^cd")
  "ab")

(deftest format-escape.case-29
  (%fei "ab~a~^cd" 1)
  "ab1")

(deftest format-escape.case-30
  (%fei "ab~a~^cd~a" 1 2)
  "ab1cd2")

(deftest format-escape.case-31
  (%fei "~@{~a~^,~}" 1 2 3)
  "1,2,3")

(deftest format-escape.case-32
  (%fei "~@{~a~^,~}~a" 1 2 3 4)
  (:ERROR PROGRAM-ERROR))

(defun %fei-fn (list) (format nil "~{~a~^, ~}" list))
(deftest format-escape.compiled-call
  (list (%fei-fn '(1 2 3)) (%fei-fn nil) (%fei-fn '("a")))
  ("1, 2, 3" "" "a"))

(deftest format-escape.stream-output
  (with-output-to-string (o)
    (format o "<~{~a~^|~}>" '(1 2 3))
    (format o "~@{~a~^+~}" 4 5))
  "<1|2|3>4+5")
