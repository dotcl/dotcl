;;; Structures of different types whose names are the same string are not
;;; EQUALP and stay distinct keys in an EQUALP hash table.

(defpackage :eqssn-a (:use :cl))
(defpackage :eqssn-b (:use :cl))

(defstruct (eqssn-a::node (:constructor eqssn-a::make-node)
                          (:conc-name eqssn-a::node-))
  x)
(defstruct (eqssn-b::node (:constructor eqssn-b::make-node)
                          (:conc-name eqssn-b::node-))
  x)

(deftest equalp-struct-same-name.different-types
  (let ((a (eqssn-a::make-node :x 1))
        (b (eqssn-b::make-node :x 1)))
    (list (equalp a b) (equalp b a)))
  (nil nil))

(deftest equalp-struct-same-name.same-type
  (list (equalp (eqssn-a::make-node :x 1) (eqssn-a::make-node :x 1))
        (equalp (eqssn-a::make-node :x 1) (eqssn-a::make-node :x 2)))
  (t nil))

(deftest equalp-struct-same-name.hash-table
  (let ((h (make-hash-table :test 'equalp))
        (a (eqssn-a::make-node :x 1))
        (b (eqssn-b::make-node :x 1)))
    (setf (gethash a h) :a)
    (let ((before (list (hash-table-count h) (gethash b h)
                        (gethash (eqssn-a::make-node :x 1) h))))
      (setf (gethash b h) :b)
      (list before (hash-table-count h) (gethash a h) (gethash b h))))
  ((1 nil :a) 2 :a :b))
