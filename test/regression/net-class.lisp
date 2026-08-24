;;; Regression tests for runtime emission of named .NET classes
;;; via DOTNET:%DEFINE-CLASS.

;;; Common helper: build a method-specs list containing a single Greet() that
;;; returns the given string (closed-over value). Used by the tests that verify
;;; a round-trip through the Lisp-dispatch path.
(defun %greet-spec (retval)
  (list (list "Greet" "System.String" nil
              (lambda (self) (declare (ignore self)) retval))))

;;; -------------------------------------------------------------------------
;;; Step 1: named class, default ctor, dynamic assembly visibility

(deftest d771-define-class-returns-fullname
  (dotnet:%define-class "DotclTest.NetClassA")
  "DotclTest.NetClassA")

;;; After %define-class, DOTNET:NEW resolves the name through
;;; AppDomain.CurrentDomain.GetAssemblies() and produces an instance.
(deftest d771-new-on-defined-class
  (progn
    (dotnet:%define-class "DotclTest.NetClassB")
    (not (null (dotnet:new "DotclTest.NetClassB"))))
  t)

;;; A user-supplied instance method (via method-specs) is invokable and its
;;; Lisp body runs, returning the Lisp value converted to the declared
;;; .NET return type.
(deftest d771-invoke-greet
  (progn
    (dotnet:%define-class "DotclTest.NetClassC" nil nil nil
      (%greet-spec "DotclTest.NetClassC"))
    (let ((obj (dotnet:new "DotclTest.NetClassC")))
      (dotnet:invoke obj "Greet")))
  "DotclTest.NetClassC")

;;; Re-defining the same name creates a fresh type in a new dynamic assembly;
;;; subsequent NEW / INVOKE still succeed.
(deftest d771-redefine-still-usable
  (progn
    (dotnet:%define-class "DotclTest.NetClassD" nil nil nil
      (%greet-spec "DotclTest.NetClassD"))
    (dotnet:%define-class "DotclTest.NetClassD" nil nil nil
      (%greet-spec "DotclTest.NetClassD"))
    (let ((obj (dotnet:new "DotclTest.NetClassD")))
      (dotnet:invoke obj "Greet")))
  "DotclTest.NetClassD")

;;; -------------------------------------------------------------------------
;;; Step 2: base type

;;; Without a base arg, the default is System.Object.
(deftest d772-default-base-is-object
  (progn
    (dotnet:%define-class "DotclTest.NetClassE")
    (let* ((obj (dotnet:new "DotclTest.NetClassE"))
           (type (dotnet:invoke obj "GetType"))
           (base (dotnet:invoke type "get_BaseType")))
      (dotnet:invoke base "get_FullName")))
  "System.Object")

;;; Nil base arg behaves the same as the 1-arg form — NEW is instantiable.
(deftest d772-nil-base-behaves-default
  (progn
    (dotnet:%define-class "DotclTest.NetClassF" nil)
    (not (null (dotnet:new "DotclTest.NetClassF"))))
  t)

;;; Inheriting from System.Exception. SetParent + base ctor chaining work
;;; end-to-end: the emitted class's BaseType is System.Exception.
(deftest d772-inherit-from-exception
  (progn
    (dotnet:%define-class "DotclTest.MyException" "System.Exception")
    (let* ((obj (dotnet:new "DotclTest.MyException"))
           (type (dotnet:invoke obj "GetType"))
           (base (dotnet:invoke type "get_BaseType")))
      (dotnet:invoke base "get_FullName")))
  "System.Exception")

;;; (Original d772-basetype-name-is-exception merged into the above.)

;;; Sealed base type must be rejected.
(deftest d772-sealed-base-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadClass1" "System.String")
    error)
  t)

;;; -------------------------------------------------------------------------
;;; Step 3: public instance fields

;;; Single field (int) set/get roundtrip
(deftest d773-int-field-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.FieldClassA" nil
      '(("Count" "System.Int32")))
    (let ((obj (dotnet:new "DotclTest.FieldClassA")))
      (dotnet:%set-invoke obj "Count" 42)
      (dotnet:invoke obj "Count")))
  42)

;;; String field
(deftest d773-string-field-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.FieldClassB" nil
      '(("Label" "System.String")))
    (let ((obj (dotnet:new "DotclTest.FieldClassB")))
      (dotnet:%set-invoke obj "Label" "hello")
      (dotnet:invoke obj "Label")))
  "hello")

;;; Multiple fields — each stored independently
(deftest d773-multiple-fields
  (progn
    (dotnet:%define-class "DotclTest.FieldClassC" nil
      '(("X" "System.Int32")
        ("Y" "System.Int32")
        ("Tag" "System.String")))
    (let ((obj (dotnet:new "DotclTest.FieldClassC")))
      (dotnet:%set-invoke obj "X" 10)
      (dotnet:%set-invoke obj "Y" 20)
      (dotnet:%set-invoke obj "Tag" "origin")
      (list (dotnet:invoke obj "X")
            (dotnet:invoke obj "Y")
            (dotnet:invoke obj "Tag"))))
  (10 20 "origin"))

;;; Duplicate field name is rejected
(deftest d773-duplicate-field-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadFieldClass" nil
      '(("Dup" "System.Int32") ("Dup" "System.String")))
    error)
  t)

;;; Nil field-specs behaves like the 2-arg form (NEW is possible)
(deftest d773-nil-fields-behaves-like-2arg
  (progn
    (dotnet:%define-class "DotclTest.FieldClassD" nil nil)
    (not (null (dotnet:new "DotclTest.FieldClassD"))))
  t)

;;; -------------------------------------------------------------------------
;;; Step 4: type-level custom attributes

;;; Applying an attribute with no arguments
(deftest d774-attribute-applied
  (progn
    (dotnet:%define-class "DotclTest.AttrClassA" nil nil
      '(("System.ObsoleteAttribute")))
    (let* ((obj (dotnet:new "DotclTest.AttrClassA"))
           (type (dotnet:invoke obj "GetType"))
           (attrs (dotnet:invoke type "GetCustomAttributes" t)))
      (dotnet:invoke attrs "get_Length")))
  1)

;;; No attributes means empty
(deftest d774-attribute-absent
  (progn
    (dotnet:%define-class "DotclTest.AttrClassB")
    (let* ((obj (dotnet:new "DotclTest.AttrClassB"))
           (type (dotnet:invoke obj "GetType"))
           (attrs (dotnet:invoke type "GetCustomAttributes" t)))
      (dotnet:invoke attrs "get_Length")))
  0)

;;; String constructor argument
(deftest d774-attribute-ctor-string-arg
  (progn
    (dotnet:%define-class "DotclTest.AttrClassC" nil nil
      '(("System.ObsoleteAttribute" "do not use this")))
    (let* ((obj (dotnet:new "DotclTest.AttrClassC"))
           (type (dotnet:invoke obj "GetType"))
           (attrs (dotnet:invoke type "GetCustomAttributes" t))
           (first (dotnet:invoke attrs "GetValue" 0)))
      (dotnet:invoke first "get_Message")))
  "do not use this")

;;; Multiple attributes
(deftest d774-multiple-attributes
  (progn
    (dotnet:%define-class "DotclTest.AttrClassD" nil nil
      '(("System.ObsoleteAttribute")
        ("System.ComponentModel.DescriptionAttribute" "a test class")))
    (let* ((obj (dotnet:new "DotclTest.AttrClassD"))
           (type (dotnet:invoke obj "GetType"))
           (attrs (dotnet:invoke type "GetCustomAttributes" t)))
      (dotnet:invoke attrs "get_Length")))
  2)

;;; Unknown attribute type name is an error
(deftest d774-unknown-attribute-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadAttrClass" nil nil
      '(("NoSuch.AttributeType")))
    error)
  t)

;;; -------------------------------------------------------------------------
;;; Step 5a: user-defined instance methods whose bodies dispatch to a
;;; Lisp lambda through DispatchLispMethod. self is passed as the first
;;; Lisp arg.

;;; String returning method, no params — Greet re-cast as a proper method spec
;;; test. Parallels d771-invoke-greet but explicitly exercises 5-arg form.
(deftest d776-string-return-no-params
  (progn
    (dotnet:%define-class "DotclTest.MethodClassA" nil nil nil
      (list (list "Label" "System.String" nil
                  (lambda (self) (declare (ignore self)) "constant"))))
    (let ((obj (dotnet:new "DotclTest.MethodClassA")))
      (dotnet:invoke obj "Label")))
  "constant")

;;; Int param + int return — ldarg/box/stelem/unbox.any path for value types.
(deftest d776-int-param-int-return
  (progn
    (dotnet:%define-class "DotclTest.MethodClassB" nil nil nil
      (list (list "Double" "System.Int32" '("System.Int32")
                  (lambda (self x) (declare (ignore self)) (* x 2)))))
    (let ((obj (dotnet:new "DotclTest.MethodClassB")))
      (dotnet:invoke obj "Double" 21)))
  42)

;;; Two-parameter method
(deftest d776-two-params
  (progn
    (dotnet:%define-class "DotclTest.MethodClassC" nil nil nil
      (list (list "Add" "System.Int32" '("System.Int32" "System.Int32")
                  (lambda (self a b) (declare (ignore self)) (+ a b)))))
    (let ((obj (dotnet:new "DotclTest.MethodClassC")))
      (dotnet:invoke obj "Add" 10 32)))
  42)

;;; String param + string return — castclass path for reference types
(deftest d776-string-param-string-return
  (progn
    (dotnet:%define-class "DotclTest.MethodClassD" nil nil nil
      (list (list "Echo" "System.String" '("System.String")
                  (lambda (self s) (declare (ignore self)) s))))
    (let ((obj (dotnet:new "DotclTest.MethodClassD")))
      (dotnet:invoke obj "Echo" "hello world")))
  "hello world")

;;; Can access self to read own fields. Integration of Fields + Methods.
(deftest d776-self-accesses-fields
  (progn
    (dotnet:%define-class "DotclTest.MethodClassE" nil
      '(("X" "System.Int32"))
      nil
      (list (list "GetX" "System.Int32" nil
                  (lambda (self) (dotnet:invoke self "X")))))
    (let ((obj (dotnet:new "DotclTest.MethodClassE")))
      (dotnet:%set-invoke obj "X" 7)
      (dotnet:invoke obj "GetX")))
  7)

;;; Multiple method definitions
(deftest d776-multiple-methods
  (progn
    (dotnet:%define-class "DotclTest.MethodClassF" nil nil nil
      (list (list "Plus1" "System.Int32" '("System.Int32")
                  (lambda (self n) (declare (ignore self)) (1+ n)))
            (list "Plus2" "System.Int32" '("System.Int32")
                  (lambda (self n) (declare (ignore self)) (+ n 2)))))
    (let ((obj (dotnet:new "DotclTest.MethodClassF")))
      (list (dotnet:invoke obj "Plus1" 10)
            (dotnet:invoke obj "Plus2" 10))))
  (11 12))

;;; Void return value
(deftest d776-void-return
  (progn
    (dotnet:%define-class "DotclTest.MethodClassG" nil
      '(("Counter" "System.Int32"))
      nil
      (list (list "Bump" "System.Void" nil
                  (lambda (self)
                    (let ((cur (dotnet:invoke self "Counter")))
                      (dotnet:%set-invoke self "Counter" (1+ cur)))))))
    (let ((obj (dotnet:new "DotclTest.MethodClassG")))
      (dotnet:%set-invoke obj "Counter" 0)
      (dotnet:invoke obj "Bump")
      (dotnet:invoke obj "Bump")
      (dotnet:invoke obj "Bump")
      (dotnet:invoke obj "Counter")))
  3)

;;; Duplicate method name is an error
(deftest d776-duplicate-method-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadMethodClass" nil nil nil
      (list (list "Dup" "System.Int32" nil (lambda (self) 1))
            (list "Dup" "System.Int32" nil (lambda (self) 2))))
    error)
  t)

;;; Non-function in the lambda part of a method spec is an error
(deftest d776-non-function-body-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadMethodClass2" nil nil nil
      (list (list "M" "System.Int32" nil "not a function")))
    error)
  t)

;;; -------------------------------------------------------------------------
;;; Step 5b: dotnet:define-class macro (syntactic sugar over %define-class)

;;; The macro lives in contrib. Load it via require.
(require :dotnet-class)

;;; Minimal form: name and superclass only. fields/attrs/methods omitted.
(deftest d777-macro-minimal
  (progn
    (dotnet:define-class "DotclTest.MacroClassA" ("System.Object"))
    (not (null (dotnet:new "DotclTest.MacroClassA"))))
  t)

;;; Via fields + methods. Classic use case of reading own fields via self.
;;; Param names are symbols (lexical vars), types are strings.
(deftest d777-macro-fields-and-methods
  (progn
    (dotnet:define-class "DotclTest.MacroClassB" ("System.Object")
      (:fields
        ("N" "System.Int32"))
      (:methods
        ("Get" () :returns "System.Int32"
          (dotnet:invoke self "N"))
        ("Add" ((x "System.Int32")) :returns "System.Int32"
          (+ (dotnet:invoke self "N") x))))
    (let ((obj (dotnet:new "DotclTest.MacroClassB")))
      (dotnet:%set-invoke obj "N" 5)
      (list (dotnet:invoke obj "Get")
            (dotnet:invoke obj "Add" 10))))
  (5 15))

;;; Via attributes — 3rd option of the macro
(deftest d777-macro-attributes
  (progn
    (dotnet:define-class "DotclTest.MacroClassC" ("System.Object")
      (:attributes
        ("System.ObsoleteAttribute" "macro-attached")))
    (let* ((obj (dotnet:new "DotclTest.MacroClassC"))
           (type (dotnet:invoke obj "GetType"))
           (attrs (dotnet:invoke type "GetCustomAttributes" t))
           (first (dotnet:invoke attrs "GetValue" 0)))
      (dotnet:invoke first "get_Message")))
  "macro-attached")

;;; Base class inheritance also works via macro — 1st element of 2nd arg is the base
(deftest d777-macro-inheritance
  (progn
    (dotnet:define-class "DotclTest.MacroException" ("System.Exception"))
    (let* ((obj (dotnet:new "DotclTest.MacroException"))
           (type (dotnet:invoke obj "GetType"))
           (base (dotnet:invoke type "get_BaseType")))
      (dotnet:invoke base "get_FullName")))
  "System.Exception")

;;; Void return + side effects — method body can call other methods
(deftest d777-macro-void-method
  (progn
    (dotnet:define-class "DotclTest.MacroClassD" ("System.Object")
      (:fields
        ("Count" "System.Int32"))
      (:methods
        ("Bump" () :returns "System.Void"
          (dotnet:%set-invoke self "Count"
                              (1+ (dotnet:invoke self "Count"))))))
    (let ((obj (dotnet:new "DotclTest.MacroClassD")))
      (dotnet:%set-invoke obj "Count" 0)
      (dotnet:invoke obj "Bump")
      (dotnet:invoke obj "Bump")
      (dotnet:invoke obj "Count")))
  2)

;;; -------------------------------------------------------------------------
;;; Step 5c: type short-name resolution + require integration

;;; BCL type symbol short-names are valid (Int32 / String / Void / Object)
(deftest d778-shortnames-primitive
  (progn
    (dotnet:define-class "DotclTest.ShortClassA" (Object)
      (:fields
        ("N" Int32)
        ("S" String))
      (:methods
        ("Echo" ((s String)) :returns String
          s)
        ("Add" ((a Int32) (b Int32)) :returns Int32
          (+ a b))))
    (let ((obj (dotnet:new "DotclTest.ShortClassA")))
      (dotnet:%set-invoke obj "N" 100)
      (dotnet:%set-invoke obj "S" "stored")
      (list (dotnet:invoke obj "N")
            (dotnet:invoke obj "S")
            (dotnet:invoke obj "Echo" "hello")
            (dotnet:invoke obj "Add" 3 4))))
  (100 "stored" "hello" 7))

;;; Symbols and strings can be mixed
(deftest d778-shortnames-mixed
  (progn
    (dotnet:define-class "DotclTest.ShortClassB" ("System.Object")
      (:fields
        ("X" Int32)
        ("Y" "System.Int32")))
    (let ((obj (dotnet:new "DotclTest.ShortClassB")))
      (dotnet:%set-invoke obj "X" 1)
      (dotnet:%set-invoke obj "Y" 2)
      (+ (dotnet:invoke obj "X") (dotnet:invoke obj "Y"))))
  3)

;;; Specifying BCL BaseType with a symbol
(deftest d778-shortname-base
  (progn
    (dotnet:define-class "DotclTest.ShortException" (Exception))
    (let* ((obj (dotnet:new "DotclTest.ShortException"))
           (type (dotnet:invoke obj "GetType"))
           (base (dotnet:invoke type "get_BaseType")))
      (dotnet:invoke base "get_FullName")))
  "System.Exception")

;;; Unknown short-name symbol is a macro-expansion error
(deftest d778-unknown-shortname-rejected
  (signals-error
    (macroexpand-1 '(dotnet:define-class "DotclTest.Bad" (NoSuchType)))
    error)
  t)

;;; User extension: adding to the table lets you use your own alias.
;;; setf must be evaluated before macro expansion, so it goes at top level.
(setf (gethash "MYHANDLER" dotnet::*type-aliases*) "DotclTest.UserAliasBase")
(dotnet:define-class "DotclTest.UserAliasBase" (Object))

(deftest d778-user-extended-alias
  (progn
    (dotnet:define-class "DotclTest.UsesAlias" (MyHandler))
    (let* ((obj (dotnet:new "DotclTest.UsesAlias"))
           (type (dotnet:invoke obj "GetType"))
           (base (dotnet:invoke type "get_BaseType")))
      (dotnet:invoke base "get_FullName")))
  "DotclTest.UserAliasBase")

;;; -------------------------------------------------------------------------
;;; Step 7a: ctor body (Lisp lambda invoked after base.ctor)

;;; The ctor body is called and can initialize own fields via self
(deftest d783-ctor-body-initializes-field
  (progn
    (dotnet:%define-class "DotclTest.CtorClassA" nil
      '(("Count" "System.Int32"))
      nil
      nil
      (lambda (self)
        (dotnet:%set-invoke self "Count" 99)))
    (let ((obj (dotnet:new "DotclTest.CtorClassA")))
      (dotnet:invoke obj "Count")))
  99)

;;; ctor body and method coexist; the method can read the value initialized in ctor
(deftest d783-ctor-then-method
  (progn
    (dotnet:%define-class "DotclTest.CtorClassB" nil
      '(("Seed" "System.Int32"))
      nil
      (list (list "Get" "System.Int32" nil
                  (lambda (self) (dotnet:invoke self "Seed"))))
      (lambda (self)
        (dotnet:%set-invoke self "Seed" 7)))
    (let ((obj (dotnet:new "DotclTest.CtorClassB")))
      (dotnet:invoke obj "Get")))
  7)

;;; Each call to new runs the ctor body independently
(deftest d783-ctor-runs-per-new
  (progn
    (dotnet:%define-class "DotclTest.CtorClassC" nil
      '(("Tag" "System.String"))
      nil
      nil
      (lambda (self)
        (dotnet:%set-invoke self "Tag" "initialized")))
    (let ((a (dotnet:new "DotclTest.CtorClassC"))
          (b (dotnet:new "DotclTest.CtorClassC")))
      (list (dotnet:invoke a "Tag")
            (dotnet:invoke b "Tag"))))
  ("initialized" "initialized"))

;;; nil ctor-body is the same as omitting it (default empty ctor)
(deftest d783-nil-ctor-body
  (progn
    (dotnet:%define-class "DotclTest.CtorClassD" nil nil nil nil nil)
    (not (null (dotnet:new "DotclTest.CtorClassD"))))
  t)

;;; Non-function ctor body is an error
(deftest d783-non-function-ctor-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadCtorClass" nil nil nil nil
                          "not a function")
    error)
  t)

;;; The same can be done via macro (:ctor ...)
(deftest d783-macro-ctor-option
  (progn
    (dotnet:define-class "DotclTest.MacroCtorA" (Object)
      (:fields
        ("N" Int32))
      (:ctor ()
        (dotnet:%set-invoke self "N" 123))
      (:methods
        ("Get" () :returns Int32
          (dotnet:invoke self "N"))))
    (let ((obj (dotnet:new "DotclTest.MacroCtorA")))
      (dotnet:invoke obj "Get")))
  123)

;;; -------------------------------------------------------------------------
;;; Step 7b: auto-properties (private backing field + public get/set)

;;; Int property set/get roundtrip (DOTNET:INVOKE finds get_X/set_X
;;; and calls them via InvokeMember)
(deftest d785-int-property-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.PropClassA" nil nil nil nil nil
      '(("Count" "System.Int32")))
    (let ((obj (dotnet:new "DotclTest.PropClassA")))
      (dotnet:%set-invoke obj "Count" 42)
      (dotnet:invoke obj "Count")))
  42)

;;; String property
(deftest d785-string-property-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.PropClassB" nil nil nil nil nil
      '(("Message" "System.String")))
    (let ((obj (dotnet:new "DotclTest.PropClassB")))
      (dotnet:%set-invoke obj "Message" "hello properties")
      (dotnet:invoke obj "Message")))
  "hello properties")

;;; Property type is visible as a public property via reflection
(deftest d785-property-visible-via-reflection
  (progn
    (dotnet:%define-class "DotclTest.PropClassC" nil nil nil nil nil
      '(("Tag" "System.String")))
    (let* ((obj (dotnet:new "DotclTest.PropClassC"))
           (type (dotnet:invoke obj "GetType"))
           (props (dotnet:invoke type "GetProperties")))
      ;; GetProperties returns at least 1
      (< 0 (dotnet:invoke props "get_Length"))))
  t)

;;; Duplicate property name is rejected
(deftest d785-duplicate-property-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadPropClass" nil nil nil nil nil
      '(("Dup" "System.Int32") ("Dup" "System.String")))
    error)
  t)

;;; Via macro (:properties ...)
(deftest d785-macro-properties
  (progn
    (dotnet:define-class "DotclTest.MacroProp" (Object)
      (:properties
        ("X" Int32)
        ("Label" String)))
    (let ((obj (dotnet:new "DotclTest.MacroProp")))
      (dotnet:%set-invoke obj "X" 5)
      (dotnet:%set-invoke obj "Label" "foo")
      (list (dotnet:invoke obj "X") (dotnet:invoke obj "Label"))))
  (5 "foo"))

;;; -------------------------------------------------------------------------
;;; Step 7c: virtual method override via DefineMethodOverride

;;; Basic: overriding System.Object.ToString(). Virtual dispatch selects the override.
(deftest d786-override-tostring
  (progn
    (dotnet:%define-class "DotclTest.OverrideA" nil nil nil
      (list (list "ToString" "System.String" nil
                  (lambda (self) (declare (ignore self)) "custom-tostring")
                  t)))
    (let ((obj (dotnet:new "DotclTest.OverrideA")))
      (dotnet:invoke obj "ToString")))
  "custom-tostring")

;;; A method without :override t is a new shadow, so calling base through the type
;;; would normally return the base — but here we do not verify the behavioral split
;;; between override and non-override versions (dotnet:invoke is name-resolution based).
;;; Instead we verify via reflection that the Virtual bit is set when overriding.
(deftest d786-override-method-is-virtual
  (progn
    (dotnet:%define-class "DotclTest.OverrideB" nil nil nil
      (list (list "ToString" "System.String" nil
                  (lambda (self) (declare (ignore self)) "b")
                  t)))
    (let* ((obj (dotnet:new "DotclTest.OverrideB"))
           (type (dotnet:invoke obj "GetType"))
           (mi (dotnet:invoke type "GetMethod" "ToString")))
      (dotnet:invoke mi "get_IsVirtual")))
  t)

;;; Override with an argument — System.Object.Equals(Object)
(deftest d786-override-equals
  (progn
    (dotnet:%define-class "DotclTest.OverrideC" nil nil nil
      (list (list "Equals" "System.Boolean" '("System.Object")
                  (lambda (self other)
                    (declare (ignore self other))
                    t)
                  t)))
    (let ((obj (dotnet:new "DotclTest.OverrideC")))
      (dotnet:invoke obj "Equals" obj)))
  t)

;;; Attempting to override a non-virtual method (Object.GetType) is an error
(deftest d786-override-non-virtual-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadOverrideA" nil nil nil
      (list (list "GetType" "System.Type" nil
                  (lambda (self) (declare (ignore self)) nil)
                  t)))
    error)
  t)

;;; Attempting to override a non-existent method name is an error
(deftest d786-override-missing-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadOverrideB" nil nil nil
      (list (list "NoSuchMethod" "System.String" nil
                  (lambda (self) (declare (ignore self)) "x")
                  t)))
    error)
  t)

;;; Return type mismatch with base is an error
(deftest d786-override-return-type-mismatch-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadOverrideC" nil nil nil
      (list (list "ToString" "System.Int32" nil
                  (lambda (self) (declare (ignore self)) 0)
                  t)))
    error)
  t)

;;; Can override a grandparent virtual (Object.ToString via Exception)
(deftest d786-override-from-grandparent
  (progn
    (dotnet:%define-class "DotclTest.OverrideExc" "System.Exception" nil nil
      (list (list "ToString" "System.String" nil
                  (lambda (self) (declare (ignore self)) "exc-override")
                  t)))
    (let ((obj (dotnet:new "DotclTest.OverrideExc")))
      (dotnet:invoke obj "ToString")))
  "exc-override")

;;; Via macro (:override t) path
(deftest d786-macro-override
  (progn
    (dotnet:define-class "DotclTest.MacroOverride" (Object)
      (:methods
        ("ToString" () :returns String :override t
          "macro-override")))
    (let ((obj (dotnet:new "DotclTest.MacroOverride")))
      (dotnet:invoke obj "ToString")))
  "macro-override")

;;; No :override in macro means a normal method (confirms compatibility with plain methods)
(deftest d786-macro-no-override-still-works
  (progn
    (dotnet:define-class "DotclTest.MacroNoOverride" (Object)
      (:methods
        ("Plain" () :returns String
          "plain")))
    (let ((obj (dotnet:new "DotclTest.MacroNoOverride")))
      (dotnet:invoke obj "Plain")))
  "plain")

;;; -------------------------------------------------------------------------
;;; Step 7d: interface implementations

;;; Implement IDisposable. The type is visible as an is-a IDisposable.
(deftest d787-implement-idisposable
  (progn
    (dotnet:%define-class "DotclTest.DisposableA" nil nil nil
      (list (list "Dispose" "System.Void" nil
                  (lambda (self) (declare (ignore self)) nil)))
      nil nil
      '("System.IDisposable"))
    (let* ((obj (dotnet:new "DotclTest.DisposableA"))
           (type (dotnet:invoke obj "GetType"))
           (iface (dotnet:static "System.Type" "GetType" "System.IDisposable")))
      (dotnet:invoke iface "IsAssignableFrom" type)))
  t)

;;; The Lisp body of the implemented method is called via interface dispatch
(deftest d787-interface-method-dispatches
  (progn
    (dotnet:%define-class "DotclTest.CloneableA" nil
      '(("Tag" "System.String"))
      nil
      (list (list "Clone" "System.Object" nil
                  (lambda (self) (dotnet:invoke self "Tag"))))
      nil nil
      '("System.ICloneable"))
    (let ((obj (dotnet:new "DotclTest.CloneableA")))
      (dotnet:%set-invoke obj "Tag" "cloned-via-iface")
      ;; Clone is the implementation of ICloneable.Clone. Can be called directly.
      (dotnet:invoke obj "Clone")))
  "cloned-via-iface")

;;; Implemented method is emitted as Virtual|Final (sealed override)
(deftest d787-impl-method-is-virtual-and-final
  (progn
    (dotnet:%define-class "DotclTest.DisposableB" nil nil nil
      (list (list "Dispose" "System.Void" nil
                  (lambda (self) (declare (ignore self)) nil)))
      nil nil
      '("System.IDisposable"))
    (let* ((obj (dotnet:new "DotclTest.DisposableB"))
           (type (dotnet:invoke obj "GetType"))
           (mi (dotnet:invoke type "GetMethod" "Dispose")))
      (list (dotnet:invoke mi "get_IsVirtual")
            (dotnet:invoke mi "get_IsFinal"))))
  (t t))

;;; Implementing multiple interfaces simultaneously
(deftest d787-multiple-interfaces
  (progn
    (dotnet:%define-class "DotclTest.DualIface" nil nil nil
      (list (list "Dispose" "System.Void" nil
                  (lambda (self) (declare (ignore self)) nil))
            (list "Clone" "System.Object" nil
                  (lambda (self) (declare (ignore self)) "cloned")))
      nil nil
      '("System.IDisposable" "System.ICloneable"))
    (let* ((obj (dotnet:new "DotclTest.DualIface"))
           (type (dotnet:invoke obj "GetType"))
           (ifaces (dotnet:invoke type "GetInterfaces")))
      (dotnet:invoke ifaces "get_Length")))
  2)

;;; Passing a non-interface to :implements is an error (System.Object is a class)
(deftest d787-non-interface-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadIface1" nil nil nil nil nil nil
      '("System.Object"))
    error)
  t)

;;; Duplicate interface is an error
(deftest d787-duplicate-interface-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadIface2" nil nil nil nil nil nil
      '("System.IDisposable" "System.IDisposable"))
    error)
  t)

;;; A method that does not match any interface method remains a plain public method
;;; (Foo is not in any interface, so it does not become virtual)
(deftest d787-nonmatching-method-stays-plain
  (progn
    (dotnet:%define-class "DotclTest.MixedIface" nil nil nil
      (list (list "Dispose" "System.Void" nil
                  (lambda (self) (declare (ignore self)) nil))
            (list "Extra" "System.String" nil
                  (lambda (self) (declare (ignore self)) "extra-value")))
      nil nil
      '("System.IDisposable"))
    (let ((obj (dotnet:new "DotclTest.MixedIface")))
      (dotnet:invoke obj "Extra")))
  "extra-value")

;;; Via macro (:implements ...) path — symbol short-names are also OK
(deftest d787-macro-implements
  (progn
    (dotnet:define-class "DotclTest.MacroDisposable" (Object)
      (:implements IDisposable)
      (:methods
        ("Dispose" () :returns Void
          nil)))
    (let* ((obj (dotnet:new "DotclTest.MacroDisposable"))
           (type (dotnet:invoke obj "GetType"))
           (iface (dotnet:static "System.Type" "GetType" "System.IDisposable")))
      (dotnet:invoke iface "IsAssignableFrom" type)))
  t)

;;; Macro integrating multiple interfaces + properties + methods
(deftest d787-macro-mvvm-scaffold
  (progn
    (dotnet:define-class "DotclTest.VMScaffold" (Object)
      (:implements IDisposable ICloneable)
      (:properties
        ("Title" String))
      (:ctor ()
        (dotnet:%set-invoke self "Title" "vm"))
      (:methods
        ("Dispose" () :returns Void
          nil)
        ("Clone" () :returns Object
          (dotnet:invoke self "Title"))))
    (let* ((obj (dotnet:new "DotclTest.VMScaffold"))
           (type (dotnet:invoke obj "GetType"))
           (ifaces (dotnet:invoke type "GetInterfaces")))
      (list (dotnet:invoke obj "Title")
            (dotnet:invoke obj "Clone")
            (dotnet:invoke ifaces "get_Length"))))
  ("vm" "vm" 2))

;;; -------------------------------------------------------------------------
;;; Step 7e: events (delegate field + add_/remove_ accessors + EventBuilder)

;;; Basic: event is visible via reflection
(deftest d788-event-visible-via-reflection
  (progn
    (dotnet:%define-class "DotclTest.EventA" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.EventA"))
           (type (dotnet:invoke obj "GetType"))
           (events (dotnet:invoke type "GetEvents")))
      (dotnet:invoke events "get_Length")))
  1)

;;; add_Name / remove_Name accessors are emitted
(deftest d788-event-add-remove-accessors-exist
  (progn
    (dotnet:%define-class "DotclTest.EventB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.EventB"))
           (type (dotnet:invoke obj "GetType"))
           (am (dotnet:invoke type "GetMethod" "add_Clicked"))
           (rm (dotnet:invoke type "GetMethod" "remove_Clicked")))
      (list (not (null am)) (not (null rm)))))
  (t t))

;;; Non-delegate type is rejected
(deftest d788-event-non-delegate-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadEventA" nil nil nil nil nil nil nil
      '(("Foo" "System.String")))
    error)
  t)

;;; Duplicate event name is rejected
(deftest d788-event-duplicate-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadEventB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")
        ("Clicked" "System.EventHandler")))
    error)
  t)

;;; Rejected when add_Name collides in name with an existing method
(deftest d788-event-method-collision-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadEventC" nil nil nil
      (list (list "add_Clicked" "System.Void" '("System.EventHandler")
                  (lambda (self h) (declare (ignore self h)) nil)))
      nil nil nil
      '(("Clicked" "System.EventHandler")))
    error)
  t)

;;; Implement INotifyPropertyChanged and attach a PropertyChanged event
;;; → add_/remove_PropertyChanged satisfies the interface slot
;;; (Type.GetType cannot resolve System.ObjectModel's INotifyPropertyChanged
;;;  without an assembly-qualified name, so here we use GetInterfaces via
;;;  reflection and match by FullName)
(deftest d788-inotifypropertychanged-implemented
  (progn
    (dotnet:%define-class "DotclTest.NotifyA" nil nil nil nil nil nil
      '("System.ComponentModel.INotifyPropertyChanged")
      '(("PropertyChanged" "System.ComponentModel.PropertyChangedEventHandler")))
    (let* ((obj (dotnet:new "DotclTest.NotifyA"))
           (type (dotnet:invoke obj "GetType"))
           (ifaces (dotnet:invoke type "GetInterfaces"))
           (first (dotnet:invoke ifaces "GetValue" 0)))
      (dotnet:invoke first "get_FullName")))
  "System.ComponentModel.INotifyPropertyChanged")

;;; When fitting an interface slot, add_/remove_ become Virtual|Final
(deftest d788-iface-event-accessors-are-virtual-and-final
  (progn
    (dotnet:%define-class "DotclTest.NotifyB" nil nil nil nil nil nil
      '("System.ComponentModel.INotifyPropertyChanged")
      '(("PropertyChanged" "System.ComponentModel.PropertyChangedEventHandler")))
    (let* ((obj (dotnet:new "DotclTest.NotifyB"))
           (type (dotnet:invoke obj "GetType"))
           (am (dotnet:invoke type "GetMethod" "add_PropertyChanged")))
      (list (dotnet:invoke am "get_IsVirtual")
            (dotnet:invoke am "get_IsFinal"))))
  (t t))

;;; dotnet:add-event / remove-event complete without crashing
;;; (whether the handler is actually called is verified later once the raiser exists)
(deftest d788-add-remove-event-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.EventC" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.EventC"))
           (handler (lambda (sender args)
                      (declare (ignore sender args)) nil)))
      (dotnet:add-event obj "Clicked" handler)
      (dotnet:remove-event obj "Clicked" handler)
      t))
  t)

;;; Via macro (:events ...) path — symbol short-names are also OK
(deftest d788-macro-events
  (progn
    (dotnet:define-class "DotclTest.EventMacro" (Object)
      (:events
        ("Clicked" EventHandler)))
    (let* ((obj (dotnet:new "DotclTest.EventMacro"))
           (type (dotnet:invoke obj "GetType"))
           (events (dotnet:invoke type "GetEvents")))
      (dotnet:invoke events "get_Length")))
  1)

;;; Full INotifyPropertyChanged integration via macro (interface + event + property)
(deftest d788-macro-inpc-scaffold
  (progn
    (dotnet:define-class "DotclTest.NotifyScaffold" (Object)
      (:implements INotifyPropertyChanged)
      (:events
        ("PropertyChanged" PropertyChangedEventHandler))
      (:properties
        ("Title" String)))
    (let* ((obj (dotnet:new "DotclTest.NotifyScaffold"))
           (type (dotnet:invoke obj "GetType"))
           (ifaces (dotnet:invoke type "GetInterfaces"))
           (events (dotnet:invoke type "GetEvents"))
           (props (dotnet:invoke type "GetProperties")))
      (list (dotnet:invoke ifaces "get_Length")
            (dotnet:invoke events "get_Length")
            (dotnet:invoke props "get_Length"))))
  (1 1 1))

;;; -------------------------------------------------------------------------
;;; Step 7f: event raiser auto-generation (OnName)

;;; OnName is emitted as a public method (sender-pattern)
(deftest d789-raiser-method-exists
  (progn
    (dotnet:%define-class "DotclTest.RaiserA" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.RaiserA"))
           (type (dotnet:invoke obj "GetType"))
           (mi (dotnet:invoke type "GetMethod" "OnClicked")))
      (not (null mi))))
  t)

;;; OnName is virtual
(deftest d789-raiser-is-virtual
  (progn
    (dotnet:%define-class "DotclTest.RaiserB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.RaiserB"))
           (type (dotnet:invoke obj "GetType"))
           (mi (dotnet:invoke type "GetMethod" "OnClicked")))
      (dotnet:invoke mi "get_IsVirtual")))
  t)

;;; sender-pattern: parameter count of OnName(args) is delegate Invoke params - 1
;;; (System.EventHandler.Invoke(object,EventArgs) → OnClicked(EventArgs))
(deftest d789-sender-pattern-strips-first
  (progn
    (dotnet:%define-class "DotclTest.RaiserC" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.RaiserC"))
           (type (dotnet:invoke obj "GetType"))
           (mi (dotnet:invoke type "GetMethod" "OnClicked"))
           (ps (dotnet:invoke mi "GetParameters")))
      (dotnet:invoke ps "get_Length")))
  1)

;;; When handler is non-null, the raiser fires and the side-effect is observable
;;; (add handler via dotnet:add-event, call OnClicked, closure counter increments)
(deftest d789-raiser-fires-handler
  (progn
    (dotnet:%define-class "DotclTest.FireA" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.FireA"))
           (counter 0)
           (handler (lambda (sender args)
                      (declare (ignore sender args))
                      (incf counter))))
      (dotnet:add-event obj "Clicked" handler)
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      counter))
  2)

;;; When handler is null, calling OnName does not crash
(deftest d789-raiser-null-handler-is-noop
  (progn
    (dotnet:%define-class "DotclTest.FireB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let ((obj (dotnet:new "DotclTest.FireB")))
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      t))
  t)

;;; remove-event can actually detach the delegate even when
;;; a bare Lisp lambda is passed. Handler identity is resolved via cache.
(deftest d794-remove-event-bare-lambda
  (progn
    (dotnet:%define-class "DotclTest.RemoveA" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.RemoveA"))
           (counter 0)
           (handler (lambda (s a) (declare (ignore s a)) (incf counter))))
      (dotnet:add-event obj "Clicked" handler)
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      (dotnet:remove-event obj "Clicked" handler)
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      counter))
  1)

;;; Can remove one specific handler out of multiple handlers
(deftest d794-remove-specific-handler-among-many
  (progn
    (dotnet:%define-class "DotclTest.RemoveB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.RemoveB"))
           (a-counter 0)
           (b-counter 0)
           (h-a (lambda (s a) (declare (ignore s a)) (incf a-counter)))
           (h-b (lambda (s a) (declare (ignore s a)) (incf b-counter))))
      (dotnet:add-event obj "Clicked" h-a)
      (dotnet:add-event obj "Clicked" h-b)
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      (dotnet:remove-event obj "Clicked" h-a)
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      (list a-counter b-counter)))
  (1 2))

;;; Removing an unregistered handler is a noop (no error)
(deftest d794-remove-unregistered-noop
  (progn
    (dotnet:%define-class "DotclTest.RemoveC" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let ((obj (dotnet:new "DotclTest.RemoveC"))
          (never-added (lambda (s a) (declare (ignore s a)) nil)))
      (dotnet:remove-event obj "Clicked" never-added)
      t))
  t)

;;; Rejected when the raiser name collides with a method (OnClicked is reserved)
(deftest d789-raiser-method-collision-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadRaiser" nil nil nil
      (list (list "OnClicked" "System.Void" '("System.EventArgs")
                  (lambda (self e) (declare (ignore self e)) nil)))
      nil nil nil
      '(("Clicked" "System.EventHandler")))
    error)
  t)

;;; Multiple handlers: all are called via Combine
(deftest d789-multiple-handlers
  (progn
    (dotnet:%define-class "DotclTest.FireD" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.FireD"))
           (a-counter 0)
           (b-counter 0))
      (dotnet:add-event obj "Clicked"
                        (lambda (s a) (declare (ignore s a)) (incf a-counter)))
      (dotnet:add-event obj "Clicked"
                        (lambda (s a) (declare (ignore s a)) (incf b-counter)))
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      (list a-counter b-counter)))
  (1 1))

;;; True INotifyPropertyChanged: end-to-end test that calls OnPropertyChanged
;;; in property-setter fashion and verifies the handler fires
(deftest d789-inpc-end-to-end
  (progn
    (dotnet:define-class "DotclTest.NotifyVM" (Object)
      (:implements INotifyPropertyChanged)
      (:events
        ("PropertyChanged" PropertyChangedEventHandler))
      (:properties
        ("Title" String))
      (:methods
        ("SetTitle" ((v String)) :returns Void
          (dotnet:%set-invoke self "Title" v)
          (dotnet:invoke self "OnPropertyChanged"
                         (dotnet:new "System.ComponentModel.PropertyChangedEventArgs"
                                     "Title")))))
    (let* ((obj (dotnet:new "DotclTest.NotifyVM"))
           (last-prop nil)
           (handler (lambda (sender args)
                      (declare (ignore sender))
                      (setf last-prop (dotnet:invoke args "get_PropertyName")))))
      (dotnet:add-event obj "PropertyChanged" handler)
      (dotnet:invoke obj "SetTitle" "new-title")
      (list (dotnet:invoke obj "Title") last-prop)))
  ("new-title" "Title"))

;;; -------------------------------------------------------------------------
;;; Step 7g: :notify t makes setter auto-call OnPropertyChanged

;;; With :notify t specified, PropertyChanged fires on property set
(deftest d790-notify-fires-property-changed
  (progn
    (dotnet:define-class "DotclTest.NotifyProp1" (Object)
      (:implements INotifyPropertyChanged)
      (:events ("PropertyChanged" PropertyChangedEventHandler))
      (:properties
        ("Title" String :notify t)))
    (let* ((obj (dotnet:new "DotclTest.NotifyProp1"))
           (last-name nil))
      (dotnet:add-event obj "PropertyChanged"
                        (lambda (sender args)
                          (declare (ignore sender))
                          (setf last-name (dotnet:invoke args "get_PropertyName"))))
      (dotnet:%set-invoke obj "Title" "new")
      last-name))
  "Title")

;;; Normal get/set still works correctly when :notify t is specified
(deftest d790-notify-get-set-roundtrip
  (progn
    (dotnet:define-class "DotclTest.NotifyProp2" (Object)
      (:implements INotifyPropertyChanged)
      (:events ("PropertyChanged" PropertyChangedEventHandler))
      (:properties
        ("Count" Int32 :notify t)))
    (let ((obj (dotnet:new "DotclTest.NotifyProp2")))
      (dotnet:%set-invoke obj "Count" 42)
      (dotnet:invoke obj "Count")))
  42)

;;; Properties with :notify and without :notify can coexist
(deftest d790-mixed-notify-and-plain
  (progn
    (dotnet:define-class "DotclTest.NotifyProp3" (Object)
      (:implements INotifyPropertyChanged)
      (:events ("PropertyChanged" PropertyChangedEventHandler))
      (:properties
        ("Title" String :notify t)
        ("Internal" Int32)))
    (let* ((obj (dotnet:new "DotclTest.NotifyProp3"))
           (fired 0))
      (dotnet:add-event obj "PropertyChanged"
                        (lambda (s a) (declare (ignore s a)) (incf fired)))
      (dotnet:%set-invoke obj "Internal" 10)  ; no notify → does not fire
      (dotnet:%set-invoke obj "Title" "x")    ; notify → fires
      fired))
  1)

;;; Using :notify t without declaring a PropertyChanged event is an error
(deftest d790-notify-without-event-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadNotify" nil nil nil nil nil
      '(("Title" "System.String" t))
      nil nil)
    error)
  t)

;;; Multiple :notify properties — correct name is notified on each set
(deftest d790-multiple-notify-properties
  (progn
    (dotnet:define-class "DotclTest.NotifyProp4" (Object)
      (:implements INotifyPropertyChanged)
      (:events ("PropertyChanged" PropertyChangedEventHandler))
      (:properties
        ("A" String :notify t)
        ("B" Int32 :notify t)))
    (let* ((obj (dotnet:new "DotclTest.NotifyProp4"))
           (names '()))
      (dotnet:add-event obj "PropertyChanged"
                        (lambda (sender args)
                          (declare (ignore sender))
                          (push (dotnet:invoke args "get_PropertyName") names)))
      (dotnet:%set-invoke obj "A" "hello")
      (dotnet:%set-invoke obj "B" 7)
      (dotnet:%set-invoke obj "A" "world")
      (reverse names)))
  ("A" "B" "A"))

;;; Full MVVM scaffold — boilerplate eliminated without SetX wrappers
(deftest d790-mvvm-no-boilerplate
  (progn
    (dotnet:define-class "DotclTest.CleanVM" (Object)
      (:implements INotifyPropertyChanged)
      (:events ("PropertyChanged" PropertyChangedEventHandler))
      (:properties
        ("Title" String :notify t)
        ("Count" Int32 :notify t)))
    (let* ((vm (dotnet:new "DotclTest.CleanVM"))
           (log '()))
      (dotnet:add-event vm "PropertyChanged"
                        (lambda (s a)
                          (declare (ignore s))
                          (push (list (dotnet:invoke a "get_PropertyName")) log)))
      (dotnet:%set-invoke vm "Title" "hello")
      (dotnet:%set-invoke vm "Count" 3)
      (list (dotnet:invoke vm "Title")
            (dotnet:invoke vm "Count")
            (reverse log))))
  ("hello" 3 (("Title") ("Count"))))

;;; -------------------------------------------------------------------------
;;; Auto-property integration

;;; Integration of properties + ctor + methods — ViewModel equivalent pattern
(deftest d785-integration-viewmodel
  (progn
    (dotnet:define-class "DotclTest.ViewModel" (Object)
      (:properties
        ("Title" String)
        ("Count" Int32))
      (:ctor ()
        (dotnet:%set-invoke self "Title" "initial")
        (dotnet:%set-invoke self "Count" 0))
      (:methods
        ("Inc" () :returns Void
          (dotnet:%set-invoke self "Count"
                              (1+ (dotnet:invoke self "Count"))))))
    (let ((vm (dotnet:new "DotclTest.ViewModel")))
      (dotnet:invoke vm "Inc")
      (dotnet:invoke vm "Inc")
      (dotnet:invoke vm "Inc")
      (list (dotnet:invoke vm "Title")
            (dotnet:invoke vm "Count"))))
  ("initial" 3))

;;; -------------------------------------------------------------------------
;;; dotnet:ref: indexer sugar

;;; List<int> get via dotnet:ref
(deftest d892-ref-list-get
  (let ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]")))
    (dotnet:invoke lst "Add" 10)
    (dotnet:invoke lst "Add" 20)
    (dotnet:invoke lst "Add" 30)
    (dotnet:ref lst 1))
  20)

;;; setf via dotnet:ref
(deftest d892-ref-list-setf
  (let ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]")))
    (dotnet:invoke lst "Add" 10)
    (setf (dotnet:ref lst 0) 99)
    (dotnet:ref lst 0))
  99)

;;; Dictionary<string,int>
(deftest d892-ref-dict-get-set
  (let ((d (dotnet:new "System.Collections.Generic.Dictionary`2[System.String,System.Int32]")))
    (setf (dotnet:ref d "key") 42)
    (dotnet:ref d "key"))
  42)

;;; -------------------------------------------------------------------------
;;; dotnet:using: IDisposable resource cleanup macro

;;; Body value is returned
(deftest d893-using-returns-body
  (dotnet:using ((sw (dotnet:new "System.IO.StringWriter")))
    (dotnet:invoke sw "Write" "hello")
    (dotnet:invoke sw "ToString"))
  "hello")

;;; Multiple bindings — each resource is independently bound
(deftest d893-using-multiple-bindings
  (let ((r '()))
    (dotnet:using ((a (dotnet:new "System.IO.StringWriter"))
                   (b (dotnet:new "System.IO.StringWriter")))
      (dotnet:invoke a "Write" "first")
      (dotnet:invoke b "Write" "second")
      (push (dotnet:invoke a "ToString") r)
      (push (dotnet:invoke b "ToString") r))
    (reverse r))
  ("first" "second"))

;;; Empty bindings — plain progn
(deftest d893-using-no-bindings
  (dotnet:using ()
    42)
  42)

;;; -------------------------------------------------------------------------
;;; parameterized constructors via (:ctor (params...) body...)

;;; Single Int32 param: value passed to new is forwarded to ctor body
(deftest d1081-ctor-single-int-param
  (progn
    (dotnet:define-class "DotclTest.ParamCtorA" (Object)
      (:properties ("Value" Int32))
      (:ctor ((val Int32))
        (dotnet:invoke self "set_Value" val)))
    (dotnet:invoke (dotnet:new "DotclTest.ParamCtorA" 42) "get_Value"))
  42)

;;; Two params: both forwarded correctly
(deftest d1081-ctor-two-params
  (progn
    (dotnet:define-class "DotclTest.ParamCtorB" (Object)
      (:properties ("X" Int32) ("Y" Int32))
      (:ctor ((x Int32) (y Int32))
        (dotnet:invoke self "set_X" x)
        (dotnet:invoke self "set_Y" y)))
    (let ((obj (dotnet:new "DotclTest.ParamCtorB" 3 7)))
      (list (dotnet:invoke obj "get_X")
            (dotnet:invoke obj "get_Y"))))
  (3 7))

;;; String param
(deftest d1081-ctor-string-param
  (progn
    (dotnet:define-class "DotclTest.ParamCtorC" (Object)
      (:properties ("Label" String))
      (:ctor ((s String))
        (dotnet:invoke self "set_Label" s)))
    (dotnet:invoke (dotnet:new "DotclTest.ParamCtorC" "hello") "get_Label"))
  "hello")

;;; Zero-param ctor still works (no regression)
(deftest d1081-ctor-zero-params-unchanged
  (progn
    (dotnet:define-class "DotclTest.ParamCtorD" (Object)
      (:properties ("N" Int32))
      (:ctor ()
        (dotnet:invoke self "set_N" 7)))
    (dotnet:invoke (dotnet:new "DotclTest.ParamCtorD") "get_N"))
  7)

;;; -------------------------------------------------------------------------
;;; CLOS dispatch on dotnet:define-class instances

;;; Top-level class definitions so macro expansion of subclasses sees the parent.
(dotnet:define-class "DotclTest.ClsBase1" (Object))
(dotnet:define-class "DotclTest.ClsBase2" (Object))
(dotnet:define-class "DotclTest.ClsBase3" (Object))
(dotnet:define-class "DotclTest.ClsAnimal" (Object))
(dotnet:define-class "DotclTest.ClsDog" (ClsAnimal))
(dotnet:define-class "DotclTest.ClsCat" (ClsAnimal))
(dotnet:define-class "DotclTest.ClsBird" (ClsAnimal))

(defgeneric d1101-speak (x))
(defmethod d1101-speak ((x clsanimal)) :animal)
(defmethod d1101-speak ((x clsdog)) :dog)

;;; type-of returns a symbol whose name matches the C# simple type name
(deftest d1101-type-of-returns-class-name
  (string= (symbol-name (type-of (dotnet:new "DotclTest.ClsBase1")))
           "ClsBase1")
  t)

;;; class-of returns a CLOS class object (built-in-class for dotnet:define-class types)
(deftest d1101-class-of-returns-class
  (typep (class-of (dotnet:new "DotclTest.ClsBase2")) 'built-in-class)
  t)

;;; find-class works via the uppercase Lisp symbol (reader upcases ClsBase3 → CLSBASE3)
(deftest d1101-find-class-works
  (string= (symbol-name (class-name (find-class 'clsbase3))) "ClsBase3")
  t)

;;; defmethod dispatch selects the most specific method
(deftest d1101-defmethod-dispatch
  (list (d1101-speak (dotnet:new "DotclTest.ClsAnimal"))
        (d1101-speak (dotnet:new "DotclTest.ClsDog")))
  (:animal :dog))

;;; typep with a class object checks CPL correctly
(deftest d1101-typep-cpl
  (let ((cat (dotnet:new "DotclTest.ClsCat")))
    (list (typep cat (find-class 'clscat))
          (typep cat (find-class 'clsanimal))
          (typep cat (find-class 'clsdog))))
  (t t nil))

;;; Inherited method fires when no specific method for subclass
(deftest d1101-inherited-method-fires
  (d1101-speak (dotnet:new "DotclTest.ClsBird"))
  :animal)

;;; -------------------------------------------------------------------------
;;; method overloading and constructor overloading

;;; Same method name, different arity → each dispatches correctly
(deftest d1106-method-overload-by-arity
  (progn
    (dotnet:%define-class "DotclTest.OverloadA" nil nil nil
      (list (list "Add" "System.Int32" '("System.Int32")
                  (lambda (self x) (declare (ignore self)) x))
            (list "Add" "System.Int32" '("System.Int32" "System.Int32")
                  (lambda (self x y) (declare (ignore self)) (+ x y)))))
    (let ((obj (dotnet:new "DotclTest.OverloadA")))
      (list (dotnet:invoke obj "Add" 10)
            (dotnet:invoke obj "Add" 3 4))))
  (10 7))

;;; Same method name, different param types → different body
(deftest d1106-method-overload-by-type
  (progn
    (dotnet:%define-class "DotclTest.OverloadB" nil nil nil
      (list (list "Describe" "System.String" '("System.String")
                  (lambda (self s) (declare (ignore self)) (concatenate 'string "str:" s)))
            (list "Describe" "System.String" '("System.Int32")
                  (lambda (self n) (declare (ignore self)) (format nil "int:~A" n)))))
    (let ((obj (dotnet:new "DotclTest.OverloadB")))
      (list (dotnet:invoke obj "Describe" "hello")
            (dotnet:invoke obj "Describe" 42))))
  ("str:hello" "int:42"))

;;; Method overloading via macro
(deftest d1106-macro-method-overload
  (progn
    (dotnet:define-class "DotclTest.OverloadC" (Object)
      (:methods
        ("Greet" () :returns String
          "hello")
        ("Greet" ((name String)) :returns String
          (concatenate 'string "hello " name))))
    (let ((obj (dotnet:new "DotclTest.OverloadC")))
      (list (dotnet:invoke obj "Greet")
            (dotnet:invoke obj "Greet" "world"))))
  ("hello" "hello world"))

;;; Constructor overloading via macro: no-arg and one-arg ctor
(deftest d1106-ctor-overload
  (progn
    (dotnet:define-class "DotclTest.OverloadCtorA" (Object)
      (:properties ("N" Int32))
      (:ctor ()
        (dotnet:invoke self "set_N" 0))
      (:ctor ((n Int32))
        (dotnet:invoke self "set_N" n)))
    (list (dotnet:invoke (dotnet:new "DotclTest.OverloadCtorA") "get_N")
          (dotnet:invoke (dotnet:new "DotclTest.OverloadCtorA" 42) "get_N")))
  (0 42))

;;; char-backed LispVector strings (BASE-STRING) marshal to System.String.
;;; CL strings have two runtime reprs (LispString and fill-pointered/adjustable
;;; char LispVector); both report type-of SIMPLE-BASE-STRING / BASE-STRING. Only
;;; LispString marshaled to System.String, so a char-vector string passed to a
;;; .NET method (e.g. Graphics.DrawString, here StringBuilder.Append) failed
;;; overload resolution with "Method not found". Repro'd via McCLIM
;;; replay-output-record, where text records store the string as a char vector.

;;; A fill-pointered char vector is NOT a LispString.
(deftest d1125-char-vector-is-base-string
  (let ((cv (make-array 5 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for c across "Hello" do (vector-push-extend c cv))
    (type-of cv))
  base-string)

;;; Passing that char-vector string to a .NET method binds the (string,...)
;;; overload and round-trips its characters.
(deftest d1125-char-vector-marshals-to-dotnet-string
  (let ((cv (make-array 5 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for c across "Hello" do (vector-push-extend c cv))
    (let ((sb (dotnet:new "System.Text.StringBuilder")))
      (dotnet:invoke sb "Append" cv)
      (dotnet:invoke sb "ToString")))
  "Hello")

;;; Nullable<T> marshalling. bool? mirrors plain bool (t->true, nil->false);
;;; (dotnet:null) is an explicit .NET null distinct from Lisp NIL. Before the fix,
;;; nil->bool? became null (so false was unreachable) and t->bool? errored.
(deftest issue305-dotnet-null-distinct
  (list (eq (dotnet:null) nil) (eq (dotnet:null) t))
  (nil nil))

(deftest issue305-bool-nullable-t
  ;; t into a bool? property round-trips as t (errored before the fix).
  (progn
    (dotnet:%define-class "Probe305a.H" "System.Object"
      nil nil nil nil '(("B" "System.Nullable`1[System.Boolean]")))
    (let ((h (dotnet:new "Probe305a.H")))
      (dotnet:invoke h "set_B" t)
      (dotnet:invoke h "get_B")))
  t)

(deftest issue305-bool-nullable-nil-and-null-no-error
  ;; nil (=> false) and (dotnet:null) (=> null) both marshal without error;
  ;; both read back as NIL (false and null collapse on the .NET->Lisp side).
  (progn
    (dotnet:%define-class "Probe305b.H" "System.Object"
      nil nil nil nil '(("B" "System.Nullable`1[System.Boolean]")))
    (let ((h (dotnet:new "Probe305b.H")))
      (dotnet:invoke h "set_B" nil)
      (let ((a (dotnet:invoke h "get_B")))
        (dotnet:invoke h "set_B" (dotnet:null))
        (list a (dotnet:invoke h "get_B")))))
  (nil nil))

;;; int? accepts a value and (dotnet:null); the value round-trips.
(deftest issue305-int-nullable-value
  (progn
    (dotnet:%define-class "Probe305c.H" "System.Object"
      nil nil nil nil '(("N" "System.Nullable`1[System.Int32]")))
    (let ((h (dotnet:new "Probe305c.H")))
      (dotnet:invoke h "set_N" 42)
      (dotnet:invoke h "get_N")))
  42)

;;; -------------------------------------------------------------------------
;;; Step 6: save-library — aggregate MANY types into one C#-referenceable .dll.
;;; The saved DLL is a facade (persisted, unloadable in this process), so these
;;; tests assert emission succeeds, the primitive returns the save-path, and a
;;; non-empty .dll lands on disk. C#-consumability is covered end-to-end by
;;; test/save-class-lib/check.sh (make test-save-class-lib).

(defun %savelib-temp-path (name)
  "A unique temp .dll path for a save-library test (overwritten each run)."
  (concatenate 'string
               (dotnet:static "System.IO.Path" "GetTempPath")
               name ".dll"))

(defun %file-nonempty-p (path)
  (and (probe-file path)
       (with-open-file (s path :element-type '(unsigned-byte 8))
         (> (file-length s) 0))))

;;; dotnet:%save-library primitive: tagged member-spec-list — two :class instance
;;; types + a static function (7th method-spec element = static-flag). One DLL.
(deftest savelib-primitive-multi-type
  (let ((path (%savelib-temp-path "dotcl-savelib-prim")))
    (list
     (string= path
              (dotnet:%save-library
               path "SaveLibPrim" "1.0.0.0"
               (list
                ;; tagged member = (:class DOC . 12-slots); DOC nil here.
                (list :class nil "SaveLibPrim.Calc" nil nil nil
                      (list (list "Add" "System.Int32"
                                  (list "System.Int32" "System.Int32")
                                  (lambda (self a b) (declare (ignore self)) (+ a b)))))
                (list :class nil "SaveLibPrim.Greeter" nil nil nil
                      (list (list "Hi" "System.String" (list "System.String")
                                  (lambda (self who) (declare (ignore self))
                                    (concatenate 'string "Hi " who)))))
                ;; static function: 7th method-spec element non-nil, lambda w/o self
                (list :class nil "SaveLibPrim.MathOps" nil nil nil
                      (list (list "Square" "System.Int32" (list "System.Int32")
                                  (lambda (x) (* x x))
                                  nil nil t))))))
     (%file-nonempty-p path)))
  (t t))

;;; %save-library requires exactly 4 args.
(deftest savelib-arity-error
  (handler-case
      (progn (dotnet:%save-library "x.dll" "X") nil)
    (error () :signaled))
  :signaled)

;;; An empty member-spec-list is an error (nothing to aggregate).
(deftest savelib-empty-list-error
  (handler-case
      (progn (dotnet:%save-library
              (%savelib-temp-path "dotcl-savelib-empty") "Empty" nil nil)
             nil)
    (error () :signaled))
  :signaled)

;;; An unknown member kind is an error.
(deftest savelib-unknown-kind-error
  (handler-case
      (progn (dotnet:%save-library
              (%savelib-temp-path "dotcl-savelib-badkind") "Bad" nil
              (list (list :bogus "Bad.T")))
             nil)
    (error () :signaled))
  :signaled)

;;; dotnet:library macro: :class instance types + :module static-function holder,
;;; emitted from the same surface a user writes. Assert a non-empty DLL lands.
(deftest library-macro-emits-dll
  (let ((path (%savelib-temp-path "dotcl-library-macro")))
    (dotnet:library ("LibMacro" :version "2.1.0.0" :path path)
      (:class "LibMacro.Calculator" ()
        (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
      (:class "LibMacro.Greeter" ()
        (:methods ("Hello" ((who String)) :returns String
          (concatenate 'string "Hi " who))))
      (:module "LibMacro.MathOps"
        (:functions ("Square" ((x Int32)) :returns Int32 (* x x)))))
    (%file-nonempty-p path))
  t)

;;; %save-library tagged :enum members: an enum-only library (no classes) is
;;; valid — enums are standalone metadata. Underlying nil defaults to Int32.
(deftest savelib-enum-only
  (let ((path (%savelib-temp-path "dotcl-savelib-enum")))
    (list
     (string= path
              (dotnet:%save-library
               path "SaveLibEnum" nil
               (list
                (list :enum nil "SaveLibEnum.Color" "System.Int32"
                      (list "Red" 0) (list "Green" 1) (list "Blue" 2))
                (list :enum nil "SaveLibEnum.Priority" nil
                      (list "Low" 10) (list "High" 20)))))
     (%file-nonempty-p path)))
  (t t))

;;; dotnet:library :enum forms: auto-increment ("Blue" => 2) and explicit values
;;; mixed with a class in one DLL. Assert a non-empty DLL lands.
(deftest library-macro-enum
  (let ((path (%savelib-temp-path "dotcl-library-enum")))
    (dotnet:library ("LibEnum" :version "1.0.0.0" :path path)
      (:class "LibEnum.Calc" ()
        (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
      (:enum "LibEnum.Color" "Red" "Green" "Blue")
      (:enum "LibEnum.Flags" :underlying Int32 ("A" 1) ("B" 2) ("C" 4) "D"))
    (%file-nonempty-p path))
  t)

;;; %save-library tagged :constants member: a const-holder of int/string/double
;;; literals. Like enums, standalone metadata; a const-only library is valid.
(deftest savelib-constants-only
  (let ((path (%savelib-temp-path "dotcl-savelib-const")))
    (list
     (string= path
              (dotnet:%save-library
               path "SaveLibConst" nil
               (list
                (list :constants nil "SaveLibConst.Config"
                      (list "MaxRetries" "System.Int32" 5)
                      (list "ApiUrl" "System.String" "https://example.com")
                      (list "Pi" "System.Double" 3.14159d0)))))
     (%file-nonempty-p path)))
  (t t))

;;; dotnet:library :constants form mixed with a class + enum in one DLL.
(deftest library-macro-constants
  (let ((path (%savelib-temp-path "dotcl-library-const")))
    (dotnet:library ("LibConst" :version "1.0.0.0" :path path)
      (:class "LibConst.Calc" ()
        (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
      (:enum "LibConst.Color" "Red" "Green" "Blue")
      (:constants "LibConst.Config"
        ("MaxRetries" Int32 5)
        ("ApiUrl" String "https://example.com")
        ("Pi" Double 3.14159d0)))
    (%file-nonempty-p path))
  t)

;;; Exception type: a class deriving System.Exception whose ctor forwards its
;;; message to base with NO body. Such a base-forwarding-only ctor emits no Lisp
;;; dispatch, so the type is standalone (a C# consumer throws/catches it with no
;;; DotCL.Runtime — asserted end-to-end in test/save-class-lib/check.sh).
(deftest library-macro-exception-type
  (let ((path (%savelib-temp-path "dotcl-library-exc")))
    (dotnet:library ("LibExc" :version "1.0.0.0" :path path)
      (:class "LibExc.MyError" ("System.Exception")
        (:ctor ((msg String)) (:base msg))))
    (%file-nonempty-p path))
  t)

;;; In-process: a base-forwarding-only ctor (no body) still constructs correctly
;;; via dotnet:new — the null-body ctor calls base and returns.
(deftest define-class-base-forwarding-ctor
  (progn
    (dotnet:define-class "DcExcTest.AppError" ("System.Exception")
      (:ctor ((msg String)) (:base msg)))
    (let ((e (dotnet:new "DcExcTest.AppError" "kaboom")))
      (dotnet:invoke e "get_Message")))
  "kaboom")

;;; %save-library tagged :struct member: a value type with public fields.
;;; Standalone data (no dispatch); a struct-only library is valid.
(deftest savelib-struct-only
  (let ((path (%savelib-temp-path "dotcl-savelib-struct")))
    (list
     (string= path
              (dotnet:%save-library
               path "SaveLibStruct" nil
               (list
                (list :struct nil "SaveLibStruct.Point"
                      (list "X" "System.Int32")
                      (list "Y" "System.Int32")))))
     (%file-nonempty-p path)))
  (t t))

;;; dotnet:library :struct form mixed with a class + enum + const in one DLL.
(deftest library-macro-struct
  (let ((path (%savelib-temp-path "dotcl-library-struct")))
    (dotnet:library ("LibStruct" :version "1.0.0.0" :path path)
      (:class "LibStruct.Calc" ()
        (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
      (:enum "LibStruct.Color" "Red" "Green" "Blue")
      (:constants "LibStruct.Config" ("Max" Int32 9))
      (:struct "LibStruct.Point" ("X" Int32) ("Y" Int32)))
    (%file-nonempty-p path))
  t)

;;; dotnet:library :interface — a standalone abstract-method contract. Assert a
;;; non-empty DLL; C#-implementability is asserted end-to-end in check.sh.
(deftest library-macro-interface
  (let ((path (%savelib-temp-path "dotcl-library-iface")))
    (dotnet:library ("LibIface" :version "1.0.0.0" :path path)
      (:interface "LibIface.IShape"
        ("Area" () :returns Double)
        ("Scale" ((factor Double)) :returns Void)))
    (%file-nonempty-p path))
  t)

;;; dotnet:library :delegate — a standalone callback type. Assert a non-empty
;;; DLL; C#-usability is asserted end-to-end in check.sh.
(deftest library-macro-delegate
  (let ((path (%savelib-temp-path "dotcl-library-del")))
    (dotnet:library ("LibDel" :version "1.0.0.0" :path path)
      (:delegate "LibDel.BinaryOp" ((a Int32) (b Int32)) :returns Int32))
    (%file-nonempty-p path))
  t)

;;; :doc on a member writes a sidecar <name>.xml with a <member name="T:Full">
;;; <summary>. A C# consumer's IntelliSense reads it. Assert the .xml lands with
;;; the member id + summary text.
(deftest library-macro-xmldoc
  (let* ((path (%savelib-temp-path "dotcl-library-xmldoc"))
         (xml (concatenate 'string (subseq path 0 (- (length path) 4)) ".xml")))
    (dotnet:library ("LibDoc" :version "1.0.0.0" :path path)
      (:enum "LibDoc.Color" :doc "A set of colors." "Red" "Green")
      (:struct "LibDoc.Point" :doc "A 2D point." ("X" Int32) ("Y" Int32)))
    (and (not (null (probe-file xml)))
         (with-open-file (s xml)
           (let ((content (make-string (file-length s))))
             (read-sequence content s)
             (and (not (null (search "T:LibDoc.Color" content)))
                  (not (null (search "A set of colors." content)))
                  (not (null (search "T:LibDoc.Point" content)))
                  t)))))
  t)

;;; Constructor parameters. The AspNetLispDemo README claimed :ctor was zero-arg
;;; only; it is not, and the difference matters because DI containers construct
;;; objects through their ctor. Reflection and Activator are checked here (both
;;; live in corelib); the ActivatorUtilities / IServiceProvider path needs the
;;; ASP.NET shared framework, so that verification lives in the issue instead.

(require "dotnet-class")

(dotnet:define-class "DotclTest.CtorParam" ()
  (:fields ("Name" String))
  (:ctor ((name String))
    (dotnet:%set-invoke self "Name" name))
  (:methods ("Greet" () :returns String
              (concatenate 'string "hello " (dotnet:invoke self "Name")))))

(deftest net-class-ctor-with-parameter
  (dotnet:invoke (dotnet:new "DotclTest.CtorParam" "world") "Greet")
  "hello world")

(deftest net-class-ctor-signature-visible-to-reflection
  (let* ((ty (dotnet:resolve-type "DotclTest.CtorParam"))
         (c (dotnet:invoke (dotnet:invoke ty "GetConstructors") "GetValue" 0))
         (ps (dotnet:invoke c "GetParameters")))
    (list (dotnet:invoke ps "Length")
          (dotnet:invoke (dotnet:invoke (dotnet:invoke ps "GetValue" 0) "ParameterType") "Name")))
  (1 "String"))

(deftest net-class-ctor-through-activator
  (let ((ty (dotnet:resolve-type "DotclTest.CtorParam"))
        (args (dotnet:make-array "System.Object" 1)))
    (dotnet:invoke args "SetValue" "activator" 0)
    (dotnet:invoke (dotnet:static "System.Activator" "CreateInstance" ty args) "Greet"))
  "hello activator")

;;; Parameter names. A MethodBuilder parameter is nameless unless DefineParameter
;;; says otherwise, so reflection reported "" for every method emitted here. A
;;; caller that binds arguments BY NAME then has nothing to bind to: Harmony
;;; picks its __instance / __result / __state injections that way, and DI
;;; containers do the same. The names were in the Lisp spec all along and were
;;; dropped on the way down.
;;;
;;; Case: a symbol read the ordinary way arrives upcased and is emitted
;;; lowercase (N -> "n"), while one written with its case preserved is taken
;;; verbatim -- the only way to spell a name like __originalMethod.

(dotnet:define-class "DotclTest.ParamNames" ()
  (:methods ("Instance" ((count "System.Int32")) :returns "System.Int32"
              (declare (ignore self))
              count)
            ("Static" ((|__originalMethod| "System.Object") (plain "System.Object"))
              :returns "System.Void" :static t
              (declare (ignore |__originalMethod| plain))
              nil)))

(defun %param-names (method-name)
  (let* ((ty (dotnet:resolve-type "DotclTest.ParamNames"))
         (mi (dotnet:invoke ty "GetMethod" method-name))
         (ps (dotnet:invoke mi "GetParameters")))
    (loop for i from 0 below (dotnet:invoke ps "Length")
          collect (dotnet:invoke (dotnet:invoke ps "GetValue" i) "Name"))))

(deftest net-class-instance-method-parameter-name
  (%param-names "Instance")
  ("count"))

(deftest net-class-static-method-parameter-names-keep-case
  (%param-names "Static")
  ("__originalMethod" "plain"))

;;; Byref parameters. A Lisp function is called with values, so a `ref' / `out'
;;; parameter had no way to exist: the emitted body marshalled every argument by
;;; value and nothing was ever written back. Harmony's __state is exactly this
;;; shape (a prefix stores, a postfix reads), so advice could not be expressed
;;; in Lisp at all.
;;;
;;; A byref parameter now arrives as a cell. What the body leaves in it is
;;; copied back into the caller's location; a body that does not write leaves
;;; the caller's value alone. Value types go through box on the way in and
;;; unbox on the way out, so "System.Int32&" works as well as "System.Object&".

(dotnet:define-class "DotclTest.ByRefParams" ()
  (:methods ("Stamp" ((|__state| "System.Object&")) :returns "System.Void" :static t
              (setf (dotnet:deref |__state|) "written")
              nil)
            ("Bump" ((n "System.Int32&")) :returns "System.Int32" :static t
              (setf (dotnet:deref n) (+ 1 (dotnet:deref n)))
              99)
            ("Untouched" ((x "System.Object&")) :returns "System.Void" :static t
              (declare (ignore x))
              nil)))

(defun %call-byref (method-name initial)
  "Invoke METHOD-NAME through reflection with one byref arg, returning
   (values written-back return-value)."
  (let* ((ty (dotnet:resolve-type "DotclTest.ByRefParams"))
         (mi (dotnet:invoke ty "GetMethod" method-name))
         (args (dotnet:make-array "System.Object" 1)))
    (dotnet:invoke args "SetValue" initial 0)
    (let ((ret (dotnet:invoke mi "Invoke" nil args)))
      (values (dotnet:invoke args "GetValue" 0) ret))))

(deftest net-class-byref-object-written-back
  (nth-value 0 (%call-byref "Stamp" "before"))
  "written")

(deftest net-class-byref-value-type-round-trips
  (multiple-value-list (%call-byref "Bump" 41))
  (42 99))

(deftest net-class-byref-untouched-keeps-callers-value
  (nth-value 0 (%call-byref "Untouched" "kept"))
  "kept")

(deftest net-class-byref-parameter-is-byref-in-metadata
  (let* ((ty (dotnet:resolve-type "DotclTest.ByRefParams"))
         (p (dotnet:invoke (dotnet:invoke (dotnet:invoke ty "GetMethod" "Stamp")
                                          "GetParameters")
                           "GetValue" 0)))
    (list (dotnet:invoke p "Name")
          (dotnet:invoke (dotnet:invoke p "ParameterType") "IsByRef")))
  ("__state" t))
