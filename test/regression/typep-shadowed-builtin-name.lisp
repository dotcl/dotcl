;;; A class named by a symbol that shadows a built-in type name is that class
;;; and nothing else. magicl shadows VECTOR, defines MAGICL:VECTOR as a class
;;; and does (set-pprint-dispatch 'vector 'pprint-vector). TYPEP matched the
;;; symbol by name, so every string satisfied it and printing any string with
;;; *PRINT-PRETTY* on called magicl's printer, including inside the compiler.

(defpackage :typep-shadow (:use :cl) (:shadow #:vector #:array))
(in-package :typep-shadow)
(defclass vector () ())
(defstruct (array (:constructor make-shadow-array)) cells)
(in-package :cl-user)

(deftest typep-shadowed-builtin-name.class
  (list (typep "abc" 'typep-shadow::vector)
        (typep #(1 2) 'typep-shadow::vector)
        (typep (make-instance 'typep-shadow::vector) 'typep-shadow::vector)
        (typep "abc" 'cl:vector)
        (typep (make-instance 'typep-shadow::vector) 'cl:vector))
  (nil nil t t nil))

(deftest typep-shadowed-builtin-name.struct
  (list (typep #(1) 'typep-shadow::array)
        (typep (typep-shadow::make-shadow-array) 'typep-shadow::array)
        (typep (typep-shadow::make-shadow-array) 'cl:array))
  (nil t nil))

(deftest typep-shadowed-builtin-name.pprint-dispatch
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'typep-shadow::vector
                         (lambda (s o) (declare (ignore o)) (write-string "SHADOW-VECTOR" s)))
    (let ((*print-pretty* t))
      (list (prin1-to-string "abc")
            (prin1-to-string (make-instance 'typep-shadow::vector)))))
  ("\"abc\"" "SHADOW-VECTOR"))
