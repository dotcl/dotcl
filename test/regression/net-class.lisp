;;; Regression tests for D771-D776 — runtime emission of named .NET classes
;;; via DOTNET:%DEFINE-CLASS. See project memory
;;; `project_net_class_emission.md` for the multi-step roadmap.

;;; Common helper: build a method-specs list containing a single Greet() that
;;; returns the given string (closed-over value). Used by D771/D772/D773
;;; tests that verify a round-trip through the Lisp-dispatch path.
(defun %greet-spec (retval)
  (list (list "Greet" "System.String" nil
              (lambda (self) (declare (ignore self)) retval))))

;;; -------------------------------------------------------------------------
;;; D771 — Step 1: named class, default ctor, dynamic assembly visibility

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
;;; D772 — Step 2: base type

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
;;; D773 — Step 3: public instance fields

;;; 単一フィールド (int) の set/get roundtrip
(deftest d773-int-field-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.FieldClassA" nil
      '(("Count" "System.Int32")))
    (let ((obj (dotnet:new "DotclTest.FieldClassA")))
      (dotnet:%set-invoke obj "Count" 42)
      (dotnet:invoke obj "Count")))
  42)

;;; 文字列フィールド
(deftest d773-string-field-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.FieldClassB" nil
      '(("Label" "System.String")))
    (let ((obj (dotnet:new "DotclTest.FieldClassB")))
      (dotnet:%set-invoke obj "Label" "hello")
      (dotnet:invoke obj "Label")))
  "hello")

;;; 複数フィールド — 独立に保持される
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

;;; 重複 field 名は拒否される
(deftest d773-duplicate-field-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadFieldClass" nil
      '(("Dup" "System.Int32") ("Dup" "System.String")))
    error)
  t)

;;; Nil field-specs は 2 引数形と同等 (NEW 可能)
(deftest d773-nil-fields-behaves-like-2arg
  (progn
    (dotnet:%define-class "DotclTest.FieldClassD" nil nil)
    (not (null (dotnet:new "DotclTest.FieldClassD"))))
  t)

;;; -------------------------------------------------------------------------
;;; D774 — Step 4: type-level custom attributes

;;; 引数なし attribute の付与
(deftest d774-attribute-applied
  (progn
    (dotnet:%define-class "DotclTest.AttrClassA" nil nil
      '(("System.ObsoleteAttribute")))
    (let* ((obj (dotnet:new "DotclTest.AttrClassA"))
           (type (dotnet:invoke obj "GetType"))
           (attrs (dotnet:invoke type "GetCustomAttributes" t)))
      (dotnet:invoke attrs "get_Length")))
  1)

;;; 属性なしなら空
(deftest d774-attribute-absent
  (progn
    (dotnet:%define-class "DotclTest.AttrClassB")
    (let* ((obj (dotnet:new "DotclTest.AttrClassB"))
           (type (dotnet:invoke obj "GetType"))
           (attrs (dotnet:invoke type "GetCustomAttributes" t)))
      (dotnet:invoke attrs "get_Length")))
  0)

;;; string ctor 引数
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

;;; 複数 attribute
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

;;; 存在しない attribute 型名はエラー
(deftest d774-unknown-attribute-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadAttrClass" nil nil
      '(("NoSuch.AttributeType")))
    error)
  t)

;;; -------------------------------------------------------------------------
;;; D776 — Step 5a: user-defined instance methods whose bodies dispatch to a
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

;;; 2 引数メソッド
(deftest d776-two-params
  (progn
    (dotnet:%define-class "DotclTest.MethodClassC" nil nil nil
      (list (list "Add" "System.Int32" '("System.Int32" "System.Int32")
                  (lambda (self a b) (declare (ignore self)) (+ a b)))))
    (let ((obj (dotnet:new "DotclTest.MethodClassC")))
      (dotnet:invoke obj "Add" 10 32)))
  42)

;;; String param + string return — 参照型の castclass 経路
(deftest d776-string-param-string-return
  (progn
    (dotnet:%define-class "DotclTest.MethodClassD" nil nil nil
      (list (list "Echo" "System.String" '("System.String")
                  (lambda (self s) (declare (ignore self)) s))))
    (let ((obj (dotnet:new "DotclTest.MethodClassD")))
      (dotnet:invoke obj "Echo" "hello world")))
  "hello world")

;;; self にアクセスして自分のフィールドを読める。Fields + Methods の統合。
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

;;; 複数メソッド定義
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

;;; void 戻り値
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

;;; 重複 method 名はエラー
(deftest d776-duplicate-method-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadMethodClass" nil nil nil
      (list (list "Dup" "System.Int32" nil (lambda (self) 1))
            (list "Dup" "System.Int32" nil (lambda (self) 2))))
    error)
  t)

;;; method spec の lambda 部が non-function ならエラー
(deftest d776-non-function-body-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadMethodClass2" nil nil nil
      (list (list "M" "System.Int32" nil "not a function")))
    error)
  t)

;;; -------------------------------------------------------------------------
;;; D777 — Step 5b: dotnet:define-class macro (syntactic sugar over %define-class)

;;; Macro は contrib にある。require 経由でロード (D778)。
(require :dotnet-class)

;;; Minimal form: 名前と superclass だけ。fields/attrs/methods 省略。
(deftest d777-macro-minimal
  (progn
    (dotnet:define-class "DotclTest.MacroClassA" ("System.Object"))
    (not (null (dotnet:new "DotclTest.MacroClassA"))))
  t)

;;; Fields + methods 経由。self で自フィールドを読む古典的ユースケース。
;;; param 名は symbol (lexical var), type は string。
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

;;; Attributes 経由 — macro の 3rd option
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

;;; 基底クラス継承経路は macro でも OK — 2nd 引数の第 1 要素が base
(deftest d777-macro-inheritance
  (progn
    (dotnet:define-class "DotclTest.MacroException" ("System.Exception"))
    (let* ((obj (dotnet:new "DotclTest.MacroException"))
           (type (dotnet:invoke obj "GetType"))
           (base (dotnet:invoke type "get_BaseType")))
      (dotnet:invoke base "get_FullName")))
  "System.Exception")

;;; Void 戻り + 副作用 — method 本体で他メソッドを呼べる
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
;;; D778 — Step 5c: type short-name resolution + require integration

;;; BCL 型の symbol short-name が有効 (Int32 / String / Void / Object)
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

;;; symbol と string 混在可
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

;;; BCL BaseType を symbol で指定
(deftest d778-shortname-base
  (progn
    (dotnet:define-class "DotclTest.ShortException" (Exception))
    (let* ((obj (dotnet:new "DotclTest.ShortException"))
           (type (dotnet:invoke obj "GetType"))
           (base (dotnet:invoke type "get_BaseType")))
      (dotnet:invoke base "get_FullName")))
  "System.Exception")

;;; 未知の short-name symbol は展開時エラー
(deftest d778-unknown-shortname-rejected
  (signals-error
    (macroexpand-1 '(dotnet:define-class "DotclTest.Bad" (NoSuchType)))
    error)
  t)

;;; ユーザ拡張: テーブルに追加すれば自分の alias が使える。
;;; setf は macro 展開より前に評価される必要があるので top-level に置く。
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
;;; D783 — Step 7a: ctor body (Lisp lambda invoked after base.ctor)

;;; ctor body が呼ばれ、self 経由で自分のフィールドを初期化できる
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

;;; ctor body と method が共存して、method が ctor で初期化された値を読める
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

;;; 2 回 new したらそれぞれ独立に ctor body が走る
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

;;; nil ctor-body は省略と同じ (デフォルト空 ctor)
(deftest d783-nil-ctor-body
  (progn
    (dotnet:%define-class "DotclTest.CtorClassD" nil nil nil nil nil)
    (not (null (dotnet:new "DotclTest.CtorClassD"))))
  t)

;;; non-function の ctor body はエラー
(deftest d783-non-function-ctor-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadCtorClass" nil nil nil nil
                          "not a function")
    error)
  t)

;;; macro (:ctor ...) 経由で同じことができる
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
;;; D785 — Step 7b: auto-properties (private backing field + public get/set)

;;; Int プロパティの set/get roundtrip (DOTNET:INVOKE が get_X/set_X を
;;; 見つけて InvokeMember で呼ぶ)
(deftest d785-int-property-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.PropClassA" nil nil nil nil nil
      '(("Count" "System.Int32")))
    (let ((obj (dotnet:new "DotclTest.PropClassA")))
      (dotnet:%set-invoke obj "Count" 42)
      (dotnet:invoke obj "Count")))
  42)

;;; String プロパティ
(deftest d785-string-property-roundtrip
  (progn
    (dotnet:%define-class "DotclTest.PropClassB" nil nil nil nil nil
      '(("Message" "System.String")))
    (let ((obj (dotnet:new "DotclTest.PropClassB")))
      (dotnet:%set-invoke obj "Message" "hello properties")
      (dotnet:invoke obj "Message")))
  "hello properties")

;;; プロパティ型が reflection から public property として見える
(deftest d785-property-visible-via-reflection
  (progn
    (dotnet:%define-class "DotclTest.PropClassC" nil nil nil nil nil
      '(("Tag" "System.String")))
    (let* ((obj (dotnet:new "DotclTest.PropClassC"))
           (type (dotnet:invoke obj "GetType"))
           (props (dotnet:invoke type "GetProperties")))
      ;; GetProperties で少なくとも 1 つは取れる
      (< 0 (dotnet:invoke props "get_Length"))))
  t)

;;; 重複 property 名は拒否
(deftest d785-duplicate-property-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadPropClass" nil nil nil nil nil
      '(("Dup" "System.Int32") ("Dup" "System.String")))
    error)
  t)

;;; macro (:properties ...) 経由
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
;;; D786 — Step 7c: virtual method override via DefineMethodOverride

;;; 基本: System.Object.ToString() を上書き。virtual dispatch が override を選ぶ。
(deftest d786-override-tostring
  (progn
    (dotnet:%define-class "DotclTest.OverrideA" nil nil nil
      (list (list "ToString" "System.String" nil
                  (lambda (self) (declare (ignore self)) "custom-tostring")
                  t)))
    (let ((obj (dotnet:new "DotclTest.OverrideA")))
      (dotnet:invoke obj "ToString")))
  "custom-tostring")

;;; :override t 無しのメソッドは new の shadow なので base を型経由で呼ぶと base が返る
;;; のが本来だが、ここでは単純に override 版と non-override 版で挙動が分かれることを確認
;;; しない (dotnet:invoke は名前解決ベース)。代わりに override 時は Virtual bit が
;;; 立つことを reflection で確認する。
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

;;; 引数付き override — System.Object.Equals(Object)
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

;;; 非 virtual なメソッド (Object.GetType) を override しようとするとエラー
(deftest d786-override-non-virtual-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadOverrideA" nil nil nil
      (list (list "GetType" "System.Type" nil
                  (lambda (self) (declare (ignore self)) nil)
                  t)))
    error)
  t)

;;; 存在しないメソッド名を override しようとするとエラー
(deftest d786-override-missing-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadOverrideB" nil nil nil
      (list (list "NoSuchMethod" "System.String" nil
                  (lambda (self) (declare (ignore self)) "x")
                  t)))
    error)
  t)

;;; 戻り値型が base と合わないとエラー
(deftest d786-override-return-type-mismatch-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadOverrideC" nil nil nil
      (list (list "ToString" "System.Int32" nil
                  (lambda (self) (declare (ignore self)) 0)
                  t)))
    error)
  t)

;;; 祖父型の virtual を override できる (Exception を経由して Object.ToString)
(deftest d786-override-from-grandparent
  (progn
    (dotnet:%define-class "DotclTest.OverrideExc" "System.Exception" nil nil
      (list (list "ToString" "System.String" nil
                  (lambda (self) (declare (ignore self)) "exc-override")
                  t)))
    (let ((obj (dotnet:new "DotclTest.OverrideExc")))
      (dotnet:invoke obj "ToString")))
  "exc-override")

;;; macro (:override t) 経路
(deftest d786-macro-override
  (progn
    (dotnet:define-class "DotclTest.MacroOverride" (Object)
      (:methods
        ("ToString" () :returns String :override t
          "macro-override")))
    (let ((obj (dotnet:new "DotclTest.MacroOverride")))
      (dotnet:invoke obj "ToString")))
  "macro-override")

;;; macro で :override 未指定は通常メソッド (既存 d777 との両立確認)
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
;;; D787 — Step 7d: interface implementations

;;; IDisposable を実装。型は IDisposable を is-a として見える。
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

;;; 実装メソッドの Lisp body が interface dispatch 経由で呼ばれる
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
      ;; Clone は ICloneable.Clone の実装。直接呼べる。
      (dotnet:invoke obj "Clone")))
  "cloned-via-iface")

;;; 実装メソッドは Virtual|Final (sealed override) で emit される
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

;;; 複数 interface 同時実装
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

;;; 非 interface を :implements に渡すとエラー (System.Object はクラス)
(deftest d787-non-interface-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadIface1" nil nil nil nil nil nil
      '("System.Object"))
    error)
  t)

;;; 重複 interface はエラー
(deftest d787-duplicate-interface-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadIface2" nil nil nil nil nil nil
      '("System.IDisposable" "System.IDisposable"))
    error)
  t)

;;; interface method に合致しないメソッドは従来通り普通の public method で
;;; 通る (Foo はどの interface にもない名前なので virtual にならない)
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

;;; macro (:implements ...) 経路 — symbol short-name も OK
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

;;; macro で複数 interface + properties + methods 統合
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
;;; D788 — Step 7e: events (delegate field + add_/remove_ accessors + EventBuilder)

;;; 基本: event が reflection で見える
(deftest d788-event-visible-via-reflection
  (progn
    (dotnet:%define-class "DotclTest.EventA" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.EventA"))
           (type (dotnet:invoke obj "GetType"))
           (events (dotnet:invoke type "GetEvents")))
      (dotnet:invoke events "get_Length")))
  1)

;;; add_Name / remove_Name accessor が emit される
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

;;; 非 delegate 型は reject
(deftest d788-event-non-delegate-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadEventA" nil nil nil nil nil nil nil
      '(("Foo" "System.String")))
    error)
  t)

;;; 重複 event 名 reject
(deftest d788-event-duplicate-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadEventB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")
        ("Clicked" "System.EventHandler")))
    error)
  t)

;;; add_Name が methods と名前衝突したら reject
(deftest d788-event-method-collision-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadEventC" nil nil nil
      (list (list "add_Clicked" "System.Void" '("System.EventHandler")
                  (lambda (self h) (declare (ignore self h)) nil)))
      nil nil nil
      '(("Clicked" "System.EventHandler")))
    error)
  t)

;;; INotifyPropertyChanged を実装して PropertyChanged event を付ける
;;; → add_/remove_PropertyChanged が interface slot を満たす
;;; (Type.GetType は assembly-qualified name が無いと System.ObjectModel の
;;;  INotifyPropertyChanged を解決できないので、ここでは GetInterfaces を
;;;  reflection して Fullname で照合する)
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

;;; interface slot にフィットすると add_/remove_ は Virtual|Final
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

;;; dotnet:add-event / remove-event がクラッシュせず通る
;;; (実際に handler が呼ばれるかは D789 の raiser 実装後でないと確認できない)
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

;;; macro (:events ...) 経路 — symbol short-name も OK
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

;;; macro で INotifyPropertyChanged 完全統合 (interface + event + property)
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
;;; D789 — Step 7f: event raiser auto-generation (OnName)

;;; OnName が public method として emit される (sender-pattern)
(deftest d789-raiser-method-exists
  (progn
    (dotnet:%define-class "DotclTest.RaiserA" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.RaiserA"))
           (type (dotnet:invoke obj "GetType"))
           (mi (dotnet:invoke type "GetMethod" "OnClicked")))
      (not (null mi))))
  t)

;;; OnName は virtual
(deftest d789-raiser-is-virtual
  (progn
    (dotnet:%define-class "DotclTest.RaiserB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let* ((obj (dotnet:new "DotclTest.RaiserB"))
           (type (dotnet:invoke obj "GetType"))
           (mi (dotnet:invoke type "GetMethod" "OnClicked")))
      (dotnet:invoke mi "get_IsVirtual")))
  t)

;;; sender-pattern: OnName(args) のパラメタ数は delegate Invoke の params - 1
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

;;; handler が非 null の時 raiser で発火し side-effect が観測できる
;;; (dotnet:add-event で handler を載せ、OnClicked を呼び、closure の
;;;  counter が増える)
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

;;; handler が null の時は OnName を呼んでもクラッシュしない
(deftest d789-raiser-null-handler-is-noop
  (progn
    (dotnet:%define-class "DotclTest.FireB" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let ((obj (dotnet:new "DotclTest.FireB")))
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      t))
  t)

;;; D794 (#160 fix): bare Lisp lambda を渡しても remove-event が
;;; 実際に delegate を取り外せる。handler 同一性をキャッシュ経由で解決。
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

;;; 複数 handler のうち特定 1 つだけ remove できる
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

;;; 未登録 handler の remove は noop (エラーにならない)
(deftest d794-remove-unregistered-noop
  (progn
    (dotnet:%define-class "DotclTest.RemoveC" nil nil nil nil nil nil nil
      '(("Clicked" "System.EventHandler")))
    (let ((obj (dotnet:new "DotclTest.RemoveC"))
          (never-added (lambda (s a) (declare (ignore s a)) nil)))
      (dotnet:remove-event obj "Clicked" never-added)
      t))
  t)

;;; raiser 名が methods と衝突したら reject (OnClicked が予約済み)
(deftest d789-raiser-method-collision-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadRaiser" nil nil nil
      (list (list "OnClicked" "System.Void" '("System.EventArgs")
                  (lambda (self e) (declare (ignore self e)) nil)))
      nil nil nil
      '(("Clicked" "System.EventHandler")))
    error)
  t)

;;; 複数 handler: Combine されて全部呼ばれる
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

;;; 真の INotifyPropertyChanged: property setter 風に OnPropertyChanged を
;;; 呼んで handler が発火する end-to-end テスト
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
;;; D790 — Step 7g: :notify t で setter が OnPropertyChanged を自動呼び出し

;;; :notify t 指定で property set 時に PropertyChanged が発火する
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

;;; :notify t でも通常の get/set は壊れない
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

;;; :notify 指定 property と :notify 未指定 property が混在できる
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
      (dotnet:%set-invoke obj "Internal" 10)  ; notify なし → 発火しない
      (dotnet:%set-invoke obj "Title" "x")    ; notify あり → 発火
      fired))
  1)

;;; PropertyChanged event を宣言しないで :notify t はエラー
(deftest d790-notify-without-event-rejected
  (signals-error
    (dotnet:%define-class "DotclTest.BadNotify" nil nil nil nil nil
      '(("Title" "System.String" t))
      nil nil)
    error)
  t)

;;; 複数 :notify property — 各 set で正しい name が通知される
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

;;; 完全な MVVM scaffold — SetX wrapper なしで boilerplate が消える
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
;;; 以下、D785 integration（先頭 comment は旧版のまま）

;;; properties + ctor + methods 統合 — ViewModel 相当のパターン
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
;;; D892 — dotnet:ref: indexer sugar

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
;;; D893 — dotnet:using: IDisposable resource cleanup macro

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
