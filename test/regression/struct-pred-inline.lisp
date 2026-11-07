;;; A call of a standard structure's predicate is compiled as TYPEP of the structure name,
;;; and an accessor of an included structure applied to a subtype takes the parent chain
;;; directly. Results for subtypes, unrelated structures and other objects, local functions
;;; of the same name, NOTINLINE, FUNCALL and the type errors are as before; the expected
;;; values are those of the build before the change.

(defstruct sp-a x y)
(defstruct (sp-b (:include sp-a)) z)
(defstruct (sp-c (:include sp-b)) w)
(defstruct sp-other q)
(defstruct (sp-pred (:predicate sp-is)) v)
(defstruct (sp-nopred (:predicate nil)) v)
(defstruct (sp-vec (:type vector) :named) a)
(defclass sp-cls () ())
(defun sp-redef-a (o) (sp-a-p o))

(deftest struct-pred-inline.1
  (handler-case (multiple-value-list (MAPCAR (FUNCTION SP-A-P) (LIST (MAKE-SP-A) (MAKE-SP-B) (MAKE-SP-C) (MAKE-SP-OTHER) 5 NIL "s" (QUOTE (1)) (MAKE-INSTANCE (QUOTE SP-CLS)) (VECTOR 1)))) (error () (list :error)))
  ((T T T NIL NIL NIL NIL NIL NIL NIL)))

(deftest struct-pred-inline.2
  (handler-case (multiple-value-list (LIST (SP-A-P (MAKE-SP-A)) (SP-A-P (MAKE-SP-B)) (SP-A-P (MAKE-SP-C)) (SP-A-P (MAKE-SP-OTHER)) (SP-A-P 5) (SP-A-P NIL) (SP-A-P "s") (SP-A-P (MAKE-INSTANCE (QUOTE SP-CLS))))) (error () (list :error)))
  ((T T T NIL NIL NIL NIL NIL)))

(deftest struct-pred-inline.3
  (handler-case (multiple-value-list (LIST (SP-B-P (MAKE-SP-A)) (SP-B-P (MAKE-SP-B)) (SP-B-P (MAKE-SP-C)) (SP-C-P (MAKE-SP-B)) (SP-C-P (MAKE-SP-C)))) (error () (list :error)))
  ((NIL T T NIL T)))

(deftest struct-pred-inline.4
  (handler-case (multiple-value-list (LIST (SP-IS (MAKE-SP-PRED)) (SP-IS 5) (SP-IS (MAKE-SP-A)))) (error () (list :error)))
  ((T NIL NIL)))

(deftest struct-pred-inline.5
  (handler-case (multiple-value-list (LIST (SP-OTHER-P (MAKE-SP-OTHER)) (SP-OTHER-P (MAKE-SP-A)))) (error () (list :error)))
  ((T NIL)))

(deftest struct-pred-inline.6
  (handler-case (multiple-value-list (LIST (SP-VEC-P (MAKE-SP-VEC :A 1)) (SP-VEC-P (VECTOR 1 2)) (SP-VEC-P 5))) (error () (list :error)))
  ((T NIL NIL)))

(deftest struct-pred-inline.7
  (handler-case (multiple-value-list (HANDLER-CASE (SP-A-P) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest struct-pred-inline.8
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (SP-A-P 1 2))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest struct-pred-inline.9
  (handler-case (multiple-value-list (FLET ((SP-A-P (X) (LIST :LOCAL X))) (SP-A-P 1))) (error () (list :error)))
  ((:LOCAL 1)))

(deftest struct-pred-inline.10
  (handler-case (multiple-value-list (LABELS ((SP-A-P (X) (LIST :LABELS X))) (SP-A-P 1))) (error () (list :error)))
  ((:LABELS 1)))

(deftest struct-pred-inline.11
  (handler-case (multiple-value-list (LOCALLY (DECLARE (NOTINLINE SP-A-P)) (SP-A-P (MAKE-SP-B)))) (error () (list :error)))
  (T))

(deftest struct-pred-inline.12
  (handler-case (multiple-value-list (FUNCALL (FUNCTION SP-A-P) (MAKE-SP-B))) (error () (list :error)))
  (T))

(deftest struct-pred-inline.13
  (handler-case (multiple-value-list (FUNCALL (QUOTE SP-A-P) (MAKE-SP-OTHER))) (error () (list :error)))
  (NIL))

(deftest struct-pred-inline.14
  (handler-case (multiple-value-list (LET ((R (SP-REDEF-A (MAKE-SP-B)))) R)) (error () (list :error)))
  (T))

(deftest struct-pred-inline.15
  (handler-case (multiple-value-list (MULTIPLE-VALUE-LIST (SP-A-P (MAKE-SP-A)))) (error () (list :error)))
  ((T)))

(deftest struct-pred-inline.16
  (handler-case (multiple-value-list (LET ((N 0)) (IF (SP-A-P (PROGN (INCF N) (MAKE-SP-A))) (LIST N) :NO))) (error () (list :error)))
  ((1)))

(deftest struct-pred-inline.17
  (handler-case (multiple-value-list (SP-A-X (MAKE-SP-B :X 7))) (error () (list :error)))
  (7))

(deftest struct-pred-inline.18
  (handler-case (multiple-value-list (SP-A-Y (MAKE-SP-C :Y 8))) (error () (list :error)))
  (8))

(deftest struct-pred-inline.19
  (handler-case (multiple-value-list (SP-B-Z (MAKE-SP-C :Z 9))) (error () (list :error)))
  (9))

(deftest struct-pred-inline.20
  (handler-case (multiple-value-list (LET ((C (MAKE-SP-C :X 1 :Y 2 :Z 3 :W 4))) (LIST (SP-A-X C) (SP-A-Y C) (SP-B-Z C) (SP-C-W C)))) (error () (list :error)))
  ((1 2 3 4)))

(deftest struct-pred-inline.21
  (handler-case (multiple-value-list (LET ((C (MAKE-SP-C))) (SETF (SP-A-X C) 10 (SP-B-Z C) 11) (LIST (SP-A-X C) (SP-B-Z C)))) (error () (list :error)))
  ((10 11)))

(deftest struct-pred-inline.22
  (handler-case (multiple-value-list (LET ((B (MAKE-SP-B))) (SETF (SP-A-Y B) 5) (SP-A-Y B))) (error () (list :error)))
  (5))

(deftest struct-pred-inline.23
  (handler-case (multiple-value-list (HANDLER-CASE (SP-B-Z (MAKE-SP-A)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest struct-pred-inline.24
  (handler-case (multiple-value-list (HANDLER-CASE (SP-A-X (MAKE-SP-OTHER)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest struct-pred-inline.25
  (handler-case (multiple-value-list (HANDLER-CASE (SP-A-X 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest struct-pred-inline.26
  (handler-case (multiple-value-list (HANDLER-CASE (SETF (SP-A-X (MAKE-SP-OTHER)) 1) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest struct-pred-inline.27
  (handler-case (multiple-value-list (HANDLER-CASE (SP-C-W (MAKE-SP-B)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest struct-pred-inline.28
  (handler-case (multiple-value-list (MAPCAR (FUNCTION SP-A-X) (LIST (MAKE-SP-A :X 1) (MAKE-SP-B :X 2) (MAKE-SP-C :X 3)))) (error () (list :error)))
  ((1 2 3)))

(deftest struct-pred-inline.29
  (handler-case (multiple-value-list (LET ((L (LIST (MAKE-SP-A) (MAKE-SP-B) 5 NIL))) (COUNT-IF (FUNCTION SP-A-P) L))) (error () (list :error)))
  (2))

(deftest struct-pred-inline.30
  (handler-case (multiple-value-list (TYPE-OF (MAKE-SP-C))) (error () (list :error)))
  (SP-C))

(deftest struct-pred-inline.31
  (handler-case (multiple-value-list (TYPEP (MAKE-SP-C) (QUOTE SP-A))) (error () (list :error)))
  (T))

