;;; #S prints the structure's type name as a symbol is printed: with its
;;; package prefix when the symbol is not accessible in *PACKAGE*. It printed
;;; the bare name, so the #S form of a structure from another package did not
;;; read back ("#S: FUNCTION-TY is not a known structure type"); Coalton prints
;;; its type structures into generated code and reads them back.

(defpackage :pstnp (:use :cl))
(defstruct (pstnp::pt (:constructor pstnp::make-pt)) x y)

(deftest print-struct-type-name-package.prefix
  (let ((*package* (find-package :cl-user)))
    (prin1-to-string (pstnp::make-pt :x 1 :y 2)))
  "#S(PSTNP::PT :X 1 :Y 2)")

(deftest print-struct-type-name-package.reads-back
  (let* ((*package* (find-package :cl-user))
         (back (read-from-string (prin1-to-string (pstnp::make-pt :x 1 :y "a")))))
    (list (type-of back) (equalp back (pstnp::make-pt :x 1 :y "a"))))
  (pstnp::pt t))

(deftest print-struct-type-name-package.accessible-unprefixed
  (let ((*package* (find-package :pstnp)))
    (prin1-to-string (pstnp::make-pt :x 1 :y 2)))
  "#S(PT :X 1 :Y 2)")
