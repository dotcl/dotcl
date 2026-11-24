;;; ~^ inside ~:< ... ~:> ends the block, which still prints its suffix:
;;; (format nil "~:<a~^b~:>" nil) is "(a)", not "(a".

(deftest format-logical-block-escape-suffix.suffix-printed
  (list (format nil "~:<a~^b~:>" nil)
        (format nil "~:<~^a~:>" nil)
        (format nil "~:<a~^b~:>" '(1))
        (format nil "x~:<a~^b~:>y" nil)
        (format nil "~:@<~A~^~A~:>" '(1))
        (format nil "~@<[~;~A~^~A~;]~:>" '(1)))
  ("(a)" "()" "(ab)" "x(a)y" "((1))" "[(1)]"))
