;;; A logical block with a per-line prefix that starts after a fill newline broke the
;;; line keeps its later lines under its new start column, not the column it had before
;;; the break. The expected strings are SBCL's.

(deftest format-prefix-block-after-fill-break.prefix-moves-left
  (let ((*print-pretty* t))
    (list (format nil "~@<Foo.~:_Caused by:~@<> ~@;ab~@:_cd~:>~:>")
          (format nil "~@<Foo. ~:_Caused by:~@<> ~@;ab~@:_cd~:>~:>")
          (format nil "~@<Foo. ~:_Caused by:~@<xx~@;ab~@:_cd~:>~:>")
          (format nil "~@<Foo. ~_Caused by:~@<> ~@;ab~@:_cd~:>~:>")))
  ("Foo.
Caused by:> ab
          > cd"
   "Foo.
Caused by:> ab
          > cd"
   "Foo.
Caused by:xxab
          xxcd"
   "Foo.
Caused by:> ab
          > cd"))

(deftest format-prefix-block-after-fill-break.unchanged-neighbours
  (let ((*print-pretty* t))
    (list (format nil "~@<abc~@:_def~@<> ~@;ab~@:_cd~:>~:>")
          (format nil "~@<Foo.~:_Caused by:~@<ab~@:_cd~:>~:>")
          (format nil "~@<Foo.~@[ ~:_Caused by:~&~@<> ~@;~A~@:>~]~:>" "x")))
  ("abc
def> ab
   > cd"
   "Foo.
Caused by:ab
          cd"
   "Foo. Caused by:
> x"))
