;;; ~@( capitalizes the first word: "3rd place" stays "3rd place" (the first word
;;; starts with a digit), "1 nil" stays "1 nil". It used to capitalize the first
;;; letter wherever it was, giving "3Rd place" and "1 Nil".

(deftest format-capitalize-first-word.digit-first
  (list (format nil "~@(~A~)" "3rd place")
        (format nil "~@(~A~)" "1 nil")
        (format nil "~@(~A~)" "42 apples")
        (format nil "~@(~A~)" "123"))
  ("3rd place" "1 nil" "42 apples" "123"))

(deftest format-capitalize-first-word.letter-first-unchanged
  (list (format nil "~@(~A~)" "hello WORLD")
        (format nil "~@(~A~)" "  hello world")
        (format nil "~@(~A~)" "-abc")
        (format nil "~@(~A~)" "a1 b2")
        (format nil "~@(~A~)" "")
        (format nil "~:(~A~)" "3rd place"))
  ("Hello world" "  Hello world" "-Abc" "A1 b2" "" "3rd Place"))
