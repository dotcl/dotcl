;;; Classes a program defines under symbols of its own whose names are those of CL condition
;;; types are not the CL types: SUBTYPEP follows the classes.

(defpackage :stsn-p (:use :cl) (:shadow #:warning #:error))
(defclass stsn-p::warning () ())
(defclass stsn-p::error (stsn-p::warning) ())

(defun stsn (a b) (multiple-value-list (subtypep a b)))

(deftest subtypep-shadowed-names.user-hierarchy
  (list (stsn 'stsn-p::error 'stsn-p::warning)
        (stsn 'stsn-p::warning 'stsn-p::error))
  ((t t) (nil t)))

(deftest subtypep-shadowed-names.not-the-cl-types
  (list (stsn 'stsn-p::warning 'cl:warning)
        (stsn 'stsn-p::error 'cl:error)
        (stsn 'cl:error 'stsn-p::warning)
        (stsn 'cl:simple-error 'stsn-p::warning))
  ((nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-shadowed-names.class-objects
  (list (stsn (find-class 'stsn-p::error) (find-class 'stsn-p::warning))
        (stsn (find-class 'stsn-p::warning) (find-class 'cl:warning)))
  ((t t) (nil t)))

(deftest subtypep-shadowed-names.typep
  (list (typep (make-instance 'stsn-p::error) 'stsn-p::warning)
        (typep (make-instance 'stsn-p::error) 'cl:warning)
        (typep (make-condition 'cl:simple-error) 'stsn-p::warning))
  (t nil nil))

(deftest subtypep-shadowed-names.cl-conditions-unchanged
  (list (stsn 'cl:simple-error 'cl:error)
        (stsn 'cl:warning 'cl:condition)
        (stsn 'cl:error 'cl:warning)
        (stsn (find-class 'cl:simple-error) (find-class 'cl:error)))
  ((t t) (t t) (nil t) (t t)))
