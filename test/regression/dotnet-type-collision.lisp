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
