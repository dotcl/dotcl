;;; ADJUST-ARRAY of a simple string answers a new string (a simple string is not
;;; adjustable); the original is left as it was.

(defun %aas-codes (s) (map 'list #'char-code s))

(deftest adjust-array-simple-string.grow
  (let* ((s (copy-seq "XYZ")) (r (adjust-array s 5)))
    (list (%aas-codes r) (stringp r) (simple-string-p r) (length r) (%aas-codes s)))
  ((88 89 90 0 0) t t 5 (88 89 90)))

(deftest adjust-array-simple-string.shrink
  (%aas-codes (adjust-array (copy-seq "XYZ") 2))
  (88 89))

(deftest adjust-array-simple-string.same-size
  (%aas-codes (adjust-array (copy-seq "XYZ") 3))
  (88 89 90))

(deftest adjust-array-simple-string.literal
  (adjust-array "abc" 4 :initial-element #\z)
  "abcz")

(deftest adjust-array-simple-string.initial-contents
  (adjust-array (copy-seq "XYZ") 4 :initial-contents "wxyz")
  "wxyz")

(deftest adjust-array-simple-string.list-dimension
  (adjust-array (copy-seq "XYZ") '(4) :initial-element #\q)
  "XYZq")

(deftest adjust-array-simple-string.empty
  (list (adjust-array (copy-seq "") 2 :initial-element #\a)
        (length (adjust-array (copy-seq "abc") 0)))
  ("aa" 0))

(deftest adjust-array-simple-string.errors
  ;; A simple string has no fill pointer; the rank stays 1.
  (list (handler-case (progn (adjust-array (copy-seq "XYZ") 5 :fill-pointer 3) :no-error)
          (error () :error))
        (handler-case (progn (adjust-array (copy-seq "XYZ") '(2 2)) :no-error)
          (error () :error)))
  (:error :error))

(deftest adjust-array-simple-string.character-vector-still-works
  (let* ((v (make-array 3 :element-type 'character :initial-element #\a))
         (r (adjust-array v 5 :initial-element #\b)))
    (list (subseq r 0 3) (char r 4)))
  ("aaa" #\b))
