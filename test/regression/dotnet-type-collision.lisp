;;; Regression tests for CLR type ↔ Lisp class identity (same-simple-name collision).
;;; Two distinct .NET types that share a simple name (different namespaces) must map
;;; to DISTINCT Lisp classes; class-of / typep must not conflate them. Before the
;;; fix, EnsureDotNetTypeClass adopted the first same-simple-name class for the
;;; second type, so class-of returned the same class for both.

;; Same simple name "Gadget" in two namespaces -> distinct classes.
(deftest d280-simple-name-collision-distinct-classes
  (progn
    (dotnet:%define-class "Collide.AlphaNs.Gadget")
    (dotnet:%define-class "Collide.BetaNs.Gadget")
    (let ((a (dotnet:new "Collide.AlphaNs.Gadget"))
          (b (dotnet:new "Collide.BetaNs.Gadget")))
      (eq (class-of a) (class-of b))))
  nil)

;; typep must distinguish the two same-simple-name types.
(deftest d280-simple-name-collision-typep
  (progn
    (dotnet:%define-class "Collide.AlphaNs.Sprocket")
    (dotnet:%define-class "Collide.BetaNs.Sprocket")
    (let ((a (dotnet:new "Collide.AlphaNs.Sprocket"))
          (b (dotnet:new "Collide.BetaNs.Sprocket")))
      (list (typep a (class-of a))
            (typep a (class-of b))
            (typep b (class-of b)))))
  (t nil t))

;; We must not over-split: two instances of the SAME type share one class.
(deftest d280-same-type-same-class
  (progn
    (dotnet:%define-class "Collide.AlphaNs.Cog")
    (let ((a1 (dotnet:new "Collide.AlphaNs.Cog"))
          (a2 (dotnet:new "Collide.AlphaNs.Cog")))
      (eq (class-of a1) (class-of a2))))
  t)

;; The first claimant of a simple name keeps it; a BCL type with a unique simple
;; name still resolves by that name (backward-compat for unquoted symbols).
(deftest d280-unique-simple-name-still-friendly
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (string= (string (class-name (class-of sb))) "StringBuilder"))
  t)

;;; (part2): expose the boxed hint type and the actual instance type.
;;; dotnet:box keeps a user-supplied static hint type for overload resolution,
;;; while the wrapped value's real type may differ. dotnet:hint-type returns the
;;; hint (NIL if none); dotnet:object-type returns the actual runtime type.

;; Boxed value: hint and actual differ and are both retrievable as System.Type.
(deftest d286-boxed-hint-vs-object-type
  (let ((b (dotnet:box "hi" "System.Object")))
    (list (dotnet:invoke (dotnet:hint-type b) "get_FullName")
          (dotnet:invoke (dotnet:object-type b) "get_FullName")))
  ("System.Object" "System.String"))

;; A plain .NET object carries no hint -> hint-type is NIL; object-type is actual.
(deftest d286-plain-object-no-hint
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (list (dotnet:hint-type sb)
          (dotnet:invoke (dotnet:object-type sb) "get_FullName")))
  (nil "System.Text.StringBuilder"))

;; Non-.NET Lisp values: both accessors return NIL rather than erroring.
(deftest d286-non-dotnet-values
  (list (dotnet:hint-type 42) (dotnet:object-type "lisp-string"))
  (nil nil))

;; The hint type is a usable System.Type (its members are callable downstream).
(deftest d286-hint-type-is-a-type
  (let ((b (dotnet:box "hi" "System.Object")))
    (list (dotnet:invoke (dotnet:hint-type b) "get_Name")
          (dotnet:invoke (dotnet:hint-type b) "get_IsInterface")))
  ("Object" nil))

;;; dotnet:box of a primitive to an interface/base type it implements.
;;; Previously "Cannot convert Fixnum to IComparable" — LispToDotNet only handled
;;; concrete primitive targets. Now a primitive boxes at its natural .NET type when
;;; the target is assignable from it.

;; int -> IComparable (the original failing case): boxes, hint kept, actual is Int64.
(deftest d313-box-int-to-interface
  (let ((b (dotnet:box 7 "System.IComparable")))
    (list (dotnet:invoke (dotnet:hint-type b) "get_Name")
          (dotnet:invoke (dotnet:object-type b) "get_FullName")))
  ("IComparable" "System.Int64"))

;; double / string to a shared interface also marshal.
(deftest d313-box-double-to-interface
  (dotnet:invoke (dotnet:object-type (dotnet:box 3.5d0 "System.IComparable")) "get_FullName")
  "System.Double")

(deftest d313-box-string-to-interface
  (dotnet:invoke (dotnet:object-type (dotnet:box "hi" "System.IConvertible")) "get_FullName")
  "System.String")

;; Concrete primitive targets are unaffected.
(deftest d313-box-concrete-unaffected
  (dotnet:invoke (dotnet:object-type (dotnet:box 9 "System.Int32")) "get_FullName")
  "System.Int32")
