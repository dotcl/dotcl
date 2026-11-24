;;; A #+ / #- inside a form that a failing #+ / #- skips is evaluated like any other: a true test makes
;;; the skipped form that nested form, a false one reads on to the next object, which is skipped as
;;; well. "#+nosuch #+common-lisp x y" reads Y, "#+nosuch #+nosuch2 x y z" reads Z (the idiom
;;; "#+lispworks #+lispworks :element-type 'lw:simple-char" skips both). Expected values are SBCL's.

(defun rfns-read (s) (handler-case (read-from-string s) (end-of-file () :eof)))

(deftest reader-feature-nested-skip.true-inner
  (list (rfns-read "#+nosuch #+common-lisp x y")
        (rfns-read "#-common-lisp #+common-lisp x y")
        (rfns-read "#+nosuch #-nosuch2 x y")
        (rfns-read "#+nosuch #+(and common-lisp (not nosuch)) x y")
        (rfns-read "(a #+nosuch #+common-lisp x y)"))
  (y y y y (a y)))

(deftest reader-feature-nested-skip.false-inner
  (list (rfns-read "#+nosuch #+nosuch2 x y z")
        (rfns-read "#+nosuch #+nosuch2 x y")
        (rfns-read "#-common-lisp #-common-lisp x y z")
        (rfns-read "#+nosuch #-common-lisp x y z")
        (rfns-read "(a #+nosuch #+nosuch2 x y z)")
        (rfns-read "#+nosuch #+nosuch2 #+nosuch3 x y z w")
        (rfns-read "(1 #+nosuch #+nosuch2 2 3)")
        (coerce (rfns-read "#(1 #+nosuch #+nosuch2 2 3)") (quote list)))
  (z :eof z z (a z) w (1) (1)))

(deftest reader-feature-nested-skip.unchanged
  (list (rfns-read "#+nosuch x y")
        (rfns-read "#+nosuch (a b) c")
        (rfns-read "#+common-lisp #+nosuch2 x y")
        (rfns-read "(a #+nosuch b c)")
        (rfns-read "'(1 #+nosuch 2 #+nosuch 3)"))
  (y c y (a c) (quote (1))))
