;;; DOTCL:TYPEXPAND-1 and DOTCL:TYPEXPAND expand DEFTYPE forms the way
;;; MACROEXPAND-1 and MACROEXPAND expand macro forms, returning
;;; (values expansion expandedp). introspect-environment reaches them for its
;;; TYPEXPAND, which serapeum's EXPLODE-TYPE uses to see through a DEFTYPE.

(deftype tx-foo (&optional (dims '*)) `(array integer ,dims))
(deftype tx-bar () '(tx-foo (7)))
(deftype tx-member () '(member :x :y :z))
;; A DEFTYPE whose body returns an EQUAL form still ran an expander.
(deftype tx-self (x) `(tx-self ,x))

(deftest typexpand.one-step
  (list (multiple-value-list (dotcl:typexpand-1 '(tx-foo)))
        (multiple-value-list (dotcl:typexpand-1 '(tx-foo (4 * 7))))
        (multiple-value-list (dotcl:typexpand-1 '(tx-bar)))
        (multiple-value-list (dotcl:typexpand-1 'tx-bar))
        (multiple-value-list (dotcl:typexpand-1 '(tx-self 1))))
  (((array integer *) t)
   ((array integer (4 * 7)) t)
   ((tx-foo (7)) t)
   ((tx-foo (7)) t)
   ((tx-self 1) t)))

(deftest typexpand.to-fixpoint
  (list (multiple-value-list (dotcl:typexpand '(tx-bar)))
        (multiple-value-list (dotcl:typexpand 'tx-member nil)))
  (((array integer (7)) t)
   ((member :x :y :z) t)))

;; Not a DEFTYPE form: returned unchanged with NIL. Only the top level is
;; expanded, as with MACROEXPAND.
(deftest typexpand.not-a-deftype
  (list (multiple-value-list (dotcl:typexpand-1 'integer))
        (multiple-value-list (dotcl:typexpand '(or tx-bar integer)))
        (multiple-value-list (dotcl:typexpand 'tx-no-such-type))
        (let ((c (find-class 'integer)))
          (multiple-value-bind (x e) (dotcl:typexpand c) (list (eq x c) e))))
  ((integer nil)
   ((or tx-bar integer) nil)
   (tx-no-such-type nil)
   (t nil)))

(deftest typexpand.exported-from-dotcl
  (list (nth-value 1 (find-symbol "TYPEXPAND-1" "DOTCL"))
        (nth-value 1 (find-symbol "TYPEXPAND" "DOTCL")))
  (:external :external))
