;;; *FEATURES* names :ANSI-CL and :IEEE-FLOATING-POINT, as SBCL's does: libraries branch
;;; on #+ansi-cl and #+ieee-floating-point, and without them took the branch written for
;;; a pre-ANSI Lisp or one without IEEE floats. :COMMON-LISP stays.

(deftest features-ansi-cl.members
  (list (and (member :ansi-cl *features*) t)
        (and (member :ieee-floating-point *features*) t)
        (and (member :common-lisp *features*) t))
  (t t t))

(deftest features-ansi-cl.read-conditionals
  (list (read-from-string "#+ansi-cl :yes #-ansi-cl :no")
        (read-from-string "#+ieee-floating-point :yes #-ieee-floating-point :no")
        (read-from-string "#+(and ansi-cl common-lisp) :both"))
  (:yes :yes :both))
