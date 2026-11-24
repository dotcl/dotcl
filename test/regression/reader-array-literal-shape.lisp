;;; Values come from SBCL 2.6.8. #nA needs its rank and a nest of sequences (lists, vectors,
;;; strings) that deep and regular; anything else is a reader error. Strings are sequences too.

(defun %ral (s) (handler-case (let ((*print-pretty* nil)) (prin1-to-string (read-from-string s))) (reader-error () :reader-error)))

(deftest reader-array-literal-shape.errors
  (mapcar #'%ral '("#a(1)" "#a5" "#2a(1 2)" "#2a((1 2) 3)" "#2a((1 2) (3 4 5))" "#1a5" "#3a((1 2))"))
  (:reader-error :reader-error :reader-error :reader-error :reader-error :reader-error :reader-error))

(deftest reader-array-literal-shape.valid
  (mapcar #'%ral '("#2a((1 2) (3 4))" "#0a5" "#1a(1 2)" "#2a()" "#2a(() ())" "#3a(((1 2)) ((3 4)))"
                   "#2a(#(1 2) #(3 4))" "#1a#(1 2)" "#2a(\"ab\" \"cd\")" "#2a((1 2) \"ab\")"))
  ("#2A((1 2) (3 4))" "#0A5" "#(1 2)" "#2A()" "#2A(() ())" "#3A(((1 2)) ((3 4)))"
   "#2A((1 2) (3 4))" "#(1 2)" "#2A((#\\a #\\b) (#\\c #\\d))" "#2A((1 2) (#\\a #\\b))"))
