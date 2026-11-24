;;; ~i,j^ and ~i,j,k^ keep a missing parameter's place: ~,0^ is two parameters.

(deftest format-escape-positional-params.two
  (list (format nil "~,0^x")
        (format nil "~1,1^x")
        (format nil "~1,2^x")
        (format nil "~v,v^x" nil 0)
        (format nil "~v,v^x" 1 1)
        (format nil "~{~A~,0^;~}" '(1 2 3)))
  ("x" "" "x" "x" "" "1;2;3;"))

(deftest format-escape-positional-params.one-and-three
  (list (format nil "~0^x") (format nil "~1^x") (format nil "~v^x" nil)
        (format nil "~v^x" 0) (format nil "~1,2,3^x") (format nil "~3,2,1^x"))
  ("" "x" "" "" "" "x"))
