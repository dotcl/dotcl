;;; FORMAT ~( ~): the body shares the argument list with the directives around it.
;;;
;;; The body used to be formatted against a fresh array that started at the
;;; current argument, so a ~:* at its start had nothing behind it to back up
;;; over, and "~D time~(~:*~P~)" -- which ASDF's own test system prints --
;;; signalled "no argument left for directive ~*".

(deftest format-case-conversion-arguments.back-up-at-the-start
  (format nil "~D time~(~:*~P~)" 2)
  "2 times")

(deftest format-case-conversion-arguments.back-up-at-the-start-singular
  (format nil "~D time~(~:*~P~)" 1)
  "1 time")

(deftest format-case-conversion-arguments.back-up-and-reuse
  (format nil "~A ~(~:*~A~)" "Foo")
  "Foo foo")

(deftest format-case-conversion-arguments.absolute-goto-inside
  (format nil "~A ~A ~:@(~0@*~A~)" "x" "y")
  "x y X")

(deftest format-case-conversion-arguments.consumed-inside-count-outside
  (format nil "~(~A ~A~) ~A" "A" "B" "C")
  "a b C")

(deftest format-case-conversion-arguments.back-up-after-the-segment
  (format nil "~A ~:@(~A~) ~:*~A" "x" "y")
  "x Y y")

;;; Cases checked against SBCL 2.6.8.
(deftest format-case-conversion-arguments.sbcl.after-block
  (format nil "~(~A~) ~A" "ABC" "DEF")
  "abc DEF")

(deftest format-case-conversion-arguments.sbcl.iteration-inside
  (format nil "~(~{~A~}~)~A" '("A" "B") "C")
  "abC")

(deftest format-case-conversion-arguments.sbcl.nested
  (format nil "~(~A ~(~A~) ~A~)~A" "A" "B" "C" "D")
  "a b cD")

(deftest format-case-conversion-arguments.sbcl.skip-inside
  (format nil "~A ~(~A ~*~A~) ~A" 1 "X" 2 "Y" 3)
  "1 x y 3")

(deftest format-case-conversion-arguments.sbcl.absolute-goto-across
  (format nil "~A ~A ~(~1@*~A~) ~A" "p" "Q" "r")
  "p Q q r")

(deftest format-case-conversion-arguments.sbcl.up-and-out-inside
  (format nil "~(~A~^ ~A~)~A" "X" "Y" "Z")
  "x yZ")

(deftest format-case-conversion-arguments.sbcl.conditional-inside
  (format nil "~(~A~[a~;b~]~)~A" "X" 1 "Z")
  "xbZ")

(deftest format-case-conversion-arguments.sbcl.recursive-processing
  (format nil "~(~?~)~A" "~A ~A" '("X" "Y") "Z")
  "x yZ")

(deftest format-case-conversion-arguments.sbcl.goto-back-over-block
  (format nil "~A~(~A~)~0@*~A" "A" "B")
  "AbA")

(deftest format-case-conversion-arguments.sbcl.iteration-outside
  (format nil "~{~(~A~)~A~}" '("A" "B" "C" "D"))
  "aBcD")
