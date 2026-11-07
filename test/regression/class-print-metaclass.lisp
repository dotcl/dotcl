;;; A class object prints with its own metaclass: built-in, structure and
;;; standard classes are not all #<STANDARD-CLASS ...>.

(defstruct cpm-struct a)
(defclass cpm-class () ())

(defun cpm-head (class)
  (let ((s (prin1-to-string class)))
    (subseq s 0 (position #\Space s))))

(deftest class-print-metaclass.built-in
  (cpm-head (find-class 'integer))
  "#<BUILT-IN-CLASS")

(deftest class-print-metaclass.structure
  (cpm-head (find-class 'cpm-struct))
  "#<STRUCTURE-CLASS")

(deftest class-print-metaclass.standard
  (cpm-head (find-class 'cpm-class))
  "#<STANDARD-CLASS")
