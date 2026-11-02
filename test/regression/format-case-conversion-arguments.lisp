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
