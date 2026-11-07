;;; ~nR prints a ratio in radix n; ~:W binds *print-pretty* to true and ~@W binds *print-level*
;;; and *print-length* to NIL (CLHS 22.3.7.1). Plain ~R / ~W and integers are unchanged.

(deftest format-radix-ratio-and-w.radix
  (list (format nil "~2R" 1/3) (format nil "~16R" 255/256) (format nil "~2R" 5)
        (format nil "~16R" 255) (format nil "~D" 1/3) (format nil "~B" 1/3)
        (format nil "~8,6R" 3/4))
  ("1/11" "FF/100" "101" "FF" "1/3" "1/11" "   3/4"))

(deftest format-radix-ratio-and-w.w-modifiers
  (list (let ((*print-pretty* nil)) (format nil "~:W" '(quote a)))
        (let ((*print-pretty* nil)) (format nil "~W" '(quote a)))
        (let ((*print-level* 1) (*print-length* 1)) (format nil "~@W" '((a b) (c d))))
        (let ((*print-level* 1) (*print-length* 1)) (format nil "~W" '((a b) (c d))))
        (let ((*print-pretty* nil)) (format nil "~:@W" '(quote a))))
  ("'A" "(QUOTE A)" "((A B) (C D))" "(# ...)" "'A"))

(deftest format-radix-ratio-and-w.bindings-restored
  (let ((*print-pretty* nil) (*print-level* 3) (*print-length* 4))
    (format nil "~:@W" '(a b))
    (list *print-pretty* *print-level* *print-length*))
  (nil 3 4))
