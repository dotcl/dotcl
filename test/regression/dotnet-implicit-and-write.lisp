;;; Implicit conversion operators, and the write side of interface members.
;;;
;;; C# applies a user-defined implicit operator silently and .NET APIs are built
;;; around that -- ASP.NET takes StringValues, PathString and HostString wherever
;;; a caller writes a string. Reflection offers no such conversion, so those calls
;;; came back as "method not found" or "Cannot convert". Only implicit operators
;;; are applied: an explicit one is a cast the C# caller had to write.
;;;
;;; The write path (dotnet:%set-invoke, i.e. setf of dotnet:invoke) had neither
;;; that conversion nor the interface search the read path gained, so a header
;;; indexer could be read but not assigned.

;;; --- implicit conversions, on BCL types -------------------------------------

;;; DateTime -> DateTimeOffset. A wrapped .NET value used to go straight to
;;; Convert.ChangeType, which knows only IConvertible.
(deftest diw-wrapped-value-converts
  (let ((moment (dotnet:new "System.DateTime" 2026 8 25 0 0 0)))
    (dotnet:static "System.DateTimeOffset" "Compare" moment moment))
  0)

;;; Lisp integer -> BigInteger.
(deftest diw-integer-converts
  (dotnet:invoke (dotnet:static "System.Numerics.BigInteger" "Pow" 2 10) "ToString")
  "1024")

;;; Lisp double -> Complex.
(deftest diw-double-converts
  (dotnet:static "System.Numerics.Complex" "Abs" 3d0)
  3.0d0)

;;; A setter whose parameter is reachable only through the operator.
(deftest diw-setter-applies-the-operator
  (let ((box (dotnet:new "DotCL.TestSupport.TagBox")))
    (setf (dotnet:invoke box "Slot") "hello")
    (dotnet:invoke box "Read"))
  "hello")

;;; An explicit operator is not applied: string -> int has none either way, so
;;; this stays an error rather than silently parsing.
(deftest diw-no-operator-still-errors
  (handler-case (progn (dotnet:static "System.Math" "Abs" "not a number") :no-error)
    (error () :error))
  :error)

;;; --- the write path ---------------------------------------------------------

;;; List<T> implements IList's indexer explicitly; assigning through it needs the
;;; interface search on the write side.
(deftest diw-write-through-explicit-indexer
  (let ((list (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                    (list "System.Int32")))))
    (dotnet:invoke list "Add" 1)
    (setf (dotnet:invoke (dotnet:cast list "System.Collections.IList") "Item" 0) 42)
    (dotnet:invoke list "get_Item" 0))
  42)

;;; --- inherited interface members --------------------------------------------

;;; A cast names one interface; a member it declares itself answers.
(deftest diw-cast-reaches-own-member
  (dotnet:invoke (dotnet:cast (dotnet:new "DotCL.TestSupport.Layered")
                              "DotCL.TestSupport.IDerived")
                 "OwnValue")
  2)

;;; And one it inherits is still reachable through that cast.
(deftest diw-cast-reaches-inherited-member
  (dotnet:invoke (dotnet:cast (dotnet:new "DotCL.TestSupport.Layered")
                              "DotCL.TestSupport.IDerived")
                 "BaseValue")
  1)
