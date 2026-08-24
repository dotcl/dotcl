# Defining .NET classes

`dotnet:define-class` emits a real .NET type at run time. Not a proxy and not a
dictionary pretending to be an object: the CLR sees an ordinary class, so a
framework that reflects over your type, subclasses it, or hands it to a DI
container finds what it expects. That is what lets a Lisp function serve an
ASP.NET route or back a MAUI page.

```lisp
(require "dotnet-class")

(dotnet:define-class "Demo.Counter" ()
  (:fields ("Count" Int32))
  (:ctor ((start Int32))
    (dotnet:%set-invoke self "Count" start))
  (:methods
    ("Bump" ((by Int32)) :returns Int32
      (dotnet:%set-invoke self "Count" (+ (dotnet:invoke self "Count") by))
      (dotnet:invoke self "Count"))
    ("ToString" () :returns String :override t
      (format nil "Counter(~a)" (dotnet:invoke self "Count")))))

(let ((c (dotnet:new "Demo.Counter" 10)))
  (dotnet:invoke c "Bump" 5)        ; => 15
  (dotnet:invoke c "ToString"))     ; => "Counter(15)"
```

The emitted type really is `Demo.Counter`: `GetType()` on the instance answers
with that name, and `ToString` overrides the one on `System.Object`.

## The shape of a definition

```
(dotnet:define-class "Full.TypeName" (Base)
  clause...)
```

The supers list names the base class. A type name is either a **string**, used
verbatim, or a **symbol**, looked up in `dotnet::*type-aliases*` — a table of
common BCL short names. An unknown symbol is an error at expansion time, so a
typo does not survive to run time. Add your own:

```lisp
(setf (gethash "CONTROLLERBASE" dotnet::*type-aliases*)
      "Microsoft.AspNetCore.Mvc.ControllerBase")
```

### Clauses

| clause | what it emits |
| --- | --- |
| `(:fields ("Name" Type) ...)` | instance fields. Read with `dotnet:invoke`, write with `dotnet:%set-invoke` |
| `(:ctor (params) body...)` | a constructor. `self` is bound to the new instance; the body runs after the base constructor |
| `(:methods ("Name" (params) :returns Type body...) ...)` | instance methods |
| `(:properties ("Name" Type [:notify t]))` | a property with a backing field |
| `(:events ("Name" DelegateType) ...)` | a private delegate field plus the `add_`/`remove_` pair |
| `(:implements IFoo IBar)` | interface implementations |
| `(:attributes ("System.ObsoleteAttribute" "message"))` | attributes on the type |

In a method spec the name is a string, the parameters are symbols bound as
lexical variables in the body, and the body is an implicit `progn` whose last
value is converted to the declared return type (`Void` discards it).

`:override t` after `:returns` emits the method as an override of a matching
virtual method on the base hierarchy — that is how `ToString` above replaces
`System.Object`'s.

A method whose name and signature match a declared interface is emitted as that
interface's implementation automatically; the same applies to the event
accessors, so `INotifyPropertyChanged` wires itself up when you declare the
matching event. `:notify t` on a property then makes its setter raise
`PropertyChanged` for you.

### Parameters the caller reads back (`ref` / `out`)

A parameter type ending in `&` is by-reference — the spelling .NET itself uses,
so `"System.Int32&"` is C#'s `ref int`. A Lisp function is called with values, so
such a parameter arrives as a **cell**: read it with `dotnet:deref`, and whatever
the body leaves in it is what the caller sees after the call. Leaving it alone
leaves the caller's value alone.

```lisp
(dotnet:define-class "Demo.Counter" ()
  (:methods ("Bump" ((n "System.Int32&")) :returns Void :static t
              (setf (dotnet:deref n) (+ 1 (dotnet:deref n))))))
```

`out` is the same thing to the CLR; the difference is only that the caller is not
expected to have set a value first.

This is the implementing side. For *calling* a .NET method that has `out`/`ref`
parameters, see `dotnet:call-out` in
[Calling .NET from Lisp](dotnet-package.md), which returns those parameters as
additional values.

### Constructors take arguments

A constructor's parameter list is typed like a method's, and `(:base ...)`
forwards to the base constructor:

```lisp
(:ctor ((message String)) (:base message))
```

This is what makes constructor injection work: a class defined this way is
constructed by `ActivatorUtilities.CreateInstance` — the mechanism ASP.NET uses
for controllers — with the registered service passed to the constructor. See
[`samples/AspNetLispDemo`](../samples/AspNetLispDemo) for that in place.

## Where the type lives

The type is emitted into a dynamic assembly in the running process, so it exists
from the moment the form is evaluated. Two consequences worth knowing:

- A host that scans loaded assemblies for your type has to scan **after** the
  Lisp side has run. The ASP.NET sample registers its dynamic assembly as an
  MVC `ApplicationPart` for exactly this reason.
- Redefining a class emits a *new* type. Instances already made keep the old
  one, and a framework that cached the old type (a routing table, say) keeps
  using it until it is rebuilt.

Emitting a type needs run-time code generation, so `define-class` is not
available where that is forbidden (NativeAOT, IL2CPP, the browser). Precompiled
Lisp still runs there — see [Using libraries](libraries.md) and the
`PrecompiledLispDemo*` samples.

## Producing a .dll for C# to reference

`dotnet:library` aggregates several definitions into one assembly written to
disk, so a C# project can `<Reference>` it like any other library. It takes the
same clause syntax as `define-class`, plus `:module` (a static-function holder),
`:enum`, `:constants`, and `:struct`:

```lisp
(dotnet:library ("MyPack" :version "1.2.3.0" :path "out/MyPack.dll")
  (:class "MyPack.Greeter" ()
    (:methods ("Hello" ((who String)) :returns String
      (concatenate 'string "Hi " who))))
  (:module "MyPack.MathOps"
    (:functions ("Square" ((x Int32)) :returns Int32 (* x x)))))
```

Enums, constant holders and field-only structs come out **standalone** — pure
metadata with no reference to `DotCL.Runtime` — so a consumer that only touches
those needs nothing from dotcl at run time.
