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
verbatim, or a **symbol**, looked up in `dotnet:*type-aliases*`; a table of
common BCL short names. An unknown symbol is an error at expansion time, so a
typo does not survive to run time. Add your own:

```lisp
(setf (gethash "CONTROLLERBASE" dotnet:*type-aliases*)
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
| `(:attributes ("System.ObsoleteAttribute" "message" :diagnostic-id "X1"))` | attributes on the type |

In a method spec the name is a string, the parameters are symbols bound as
lexical variables in the body, and the body is an implicit `progn` whose last
value is converted to the declared return type (`Void` discards it).

An attribute spec is `(type-name constructor-arg... :member value...)`. The
arguments before the first keyword go to the attribute's constructor; from the
first keyword on, each pair names a settable property or a public field. Most
.NET attributes are shaped that way: a short constructor plus named properties.
So `[Service(Name = "Worker")]` is written `("Android.App.ServiceAttribute"
:name "Worker")`. A member name matches case-insensitively with dashes removed,
so `:main-launcher`, `:mainlauncher` and `"MainLauncher"` all reach
`MainLauncher`. Naming a read-only property, or one that does not exist, is an
error rather than a silent omission. The same spec shape works in a method's
`:attributes` option.

`:override t` after `:returns` emits the method as an override of a matching
virtual method on the base hierarchy; that is how `ToString` above replaces
`System.Object`'s.

A method whose name and signature match a declared interface is emitted as that
interface's implementation automatically; the same applies to the event
accessors, so `INotifyPropertyChanged` wires itself up when you declare the
matching event. `:notify t` on a property then makes its setter raise
`PropertyChanged` for you.

### Parameters the caller reads back (`ref` / `out`)

A parameter type ending in `&` is by-reference, the spelling .NET itself uses,
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
constructed by `ActivatorUtilities.CreateInstance`, the mechanism ASP.NET uses
for controllers, with the registered service passed to the constructor. See
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
Lisp still runs there; see [Using libraries](libraries.md) and the
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

Enums, constant holders and field-only structs come out **standalone**, pure
metadata with no reference to `DotCL.Runtime`, so a consumer that only touches
those needs nothing from dotcl at run time.

Classes and modules are facades: their method bodies call back into Lisp, and
saving the library does not carry the Lisp bodies with it. The process that
uses the .dll installs them with `dotnet:register-class-handlers`, which takes
the same clauses as `define-class` but defines no type:

```lisp
(dotnet:register-class-handlers "MyPack.Greeter" ()
  (:methods ("Hello" ((who String)) :returns String
    (concatenate 'string "Hi " who))))
(dotnet:register-class-handlers "MyPack.MathOps" ()
  (:functions ("Square" ((x Int32)) :returns Int32 (* x x))))
```

It emits nothing, so it also works on a runtime without code generation, where
`define-class` is unavailable.

### Functions compiled into the .dll

A `:functions` entry named by a symbol instead of a string is compiled into the
.dll itself. Its C# name is the symbol in PascalCase (`sum-squares` becomes
`SumSquares`; `:name "Other"` after `:returns` overrides it), and `(:defun ...)`
adds a helper that is compiled in but not exported:

```lisp
(defpackage :fib-lib (:use :cl))
(in-package :fib-lib)
(dotnet:library ("FibLib" :path "out/FibLib.dll")
  (:defun square (x) (* x x))
  (:module "FibLib.MathOps"
    (:functions
      (fib ((n Int64)) :returns Int64
        (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
      (sum-squares ((a Int64) (b Int64)) :returns Int64
        (+ (square a) (square b))))))
```

A C# project that references `FibLib.dll` and `DotCL.Runtime` calls
`MathOps.Fib(30)` with no further setup: the .dll boots the runtime when it is
first touched and defines its functions in their package (created if needed).
No Lisp source and no core are loaded. A Lisp error the body does not handle
reaches C# as a `DotCL.LispErrorException`.

Because the consumer has only `DotCL.Runtime`, a body may call the library's own
functions and the functions `DotCL.Runtime` implements. A standard function
implemented in Lisp in the core (`signum`, `union`, `map`, `#'+`, for example) is
copied into the .dll together with the core functions it calls in turn, each under
a name in a package of the library's own (`FibLib/CORE`), so loading the .dll into
a dotcl that has the core leaves the core's definitions as they were. After a
build, `dotnet::*library-core-functions*` lists the functions that were copied.

Built with `dotcl:build-library` (below), a body may also call the functions of
the files it builds from, or of the ASDF system and the systems it depends on: each
one reached is compiled in from its source under its own name, with what it calls
in turn. `dotnet::*library-source-functions*` lists them after a build.

`dotnet:library` signals an error naming the call, and the chain of calls that
reached it, when a body calls a function defined outside the library and its
sources, `eval`,
`compile`, `load`, `intern` or `macroexpand`, a generic function, or a core
function that reads a special variable (`y-or-n-p` reads `*query-io*`). The core
functions are copied from `cil-stdlib.lisp` next to the loaded core, so building
such a library needs a dotcl that has it there. Macros are expanded when the
library is built, so any macro may be used.

The compiled code calls into `DotCL.Runtime` directly, so the .dll works with the
`DotCL.Runtime` it was built with. A different version works only if it still has
every member the .dll names; see "The runtime surface of a compiled-in .dll" below.

### Packing the library as a NuGet package

Adding `:package` to the library spec also writes a `.nupkg`, so a C# project
needs one line instead of a `<Reference>` with a path:

```lisp
(dotnet:library ("FibLib" :version "1.0.0.0" :path "out/FibLib.dll"
                 :package "out/feed" :authors "Me" :description "Fibonacci")
  ...)
```

```xml
<PackageReference Include="FibLib" Version="1.0.0" />
```

`:package` is `t` (write next to the .dll) or the output directory. `:package-id`
and `:package-version` override the id (the assembly name) and the version (the
assembly version without a trailing `.0`); `:authors` and `:description` fill the
nuspec. `(dotnet:pack-library "out/FibLib.dll" :output "out/feed")` packs a .dll
that is already built. The package holds `lib/netstandard2.0/FibLib.dll`, the
sidecar `FibLib.xml` when there is one, and a nuspec that depends on exactly the
`DotCL.Runtime` version of the dotcl that wrote it (`[0.1.35]`), since the .dll
calls into that version directly. No .NET SDK is needed to write it.

The consumer's feed must carry `DotCL.Runtime` as well as `FibLib`: put both
packages in one directory and add it as a source (`dotnet nuget add source`, or a
`nuget.config`).

### Building the library from a project or the command line

The library does not have to write itself with `:path`. A .NET project can build
it like any other assembly with the `DotCL.Sdk` MSBuild SDK:

```xml
<Project Sdk="DotCL.Sdk">
  <ItemGroup>
    <LispLibrary Include="plugin.lisp" />
    <ProjectReference Include="../Contracts/Contracts.csproj" />
  </ItemGroup>
</Project>
```

`dotnet build` loads every reference of the project (`ProjectReference`,
`Reference`, `PackageReference`) so the files can name their types, then loads
the `LispLibrary` files in order. The one `dotnet:library` form among them is
written as the project's assembly (its `:path` is ignored), so its name has to be
the project's `AssemblyName`. `DotCL.Runtime` comes as a package dependency and
is copied beside the DLL; `dotcl.core` and the contrib tree are not, since the
DLL runs without them. A C# project can then `ProjectReference` the Lisp one.
The SDK version goes in `global.json` (`"msbuild-sdks": { "DotCL.Sdk": "VERSION" }`)
or in the attribute (`Sdk="DotCL.Sdk/VERSION"`); `LispLibrary` needs a release
after 0.1.35.
See [SDK and MSBuild properties](sdk.md#lisp-libraries).

The same build without MSBuild:

```sh
dotcl build-library plugin.lisp --reference Contracts.dll --output out/LispPlugin.dll
```

and from Lisp, `(dotcl:build-library :files '("plugin.lisp") :references
'("Contracts.dll") :output "out/LispPlugin.dll")`. All three run the same code.

A function of these files that a library body calls does not have to be written
inside `dotnet:library`: the build compiles it in from the file, with the
functions it calls in turn (see "Functions compiled into the .dll" above).

### Building the library from an ASDF system

A library whose code is an ASDF system, maybe with systems of its own to depend
on, names its `dotnet:library` form as a `(:dotnet-library ...)` component and
`dotnet-library-op` as its build operation (both from the `dotnet-library`
contrib):

```lisp
(defsystem "word-stats"
  :defsystem-depends-on ("dotnet-library")
  :build-operation "dotnet-library-op"
  :build-pathname "bin/WordStatsPlugin"     ; optional
  :depends-on ("text-util")
  :serial t
  :components ((:file "package")
               (:file "stats")
               (:dotnet-library "plugin")))
```

`(asdf:make "word-stats")` loads the system, then `plugin.lisp`, whose
`dotnet:library` form is written to `bin/WordStatsPlugin.dll` (the
`:build-pathname`, relative to the system; without one, the form's own `:path`).
Loading the system (`asdf:load-system`) leaves `plugin.lisp` alone and writes
nothing. Every function of `word-stats` and of `text-util` that the library's
bodies reach, and what those reach, is compiled into the DLL from its source; the
rest of the systems is not. The DLL needs `DotCL.Runtime` only, as above.

The same build, with an output path that wins over the system's:

```sh
dotcl build-library --system word-stats.asd --search-path ../text-util \
  --reference Contracts.dll --output out/WordStatsPlugin.dll
```

```lisp
;; word-stats.asd, ../text-util/ and Contracts.dll are your own project's files
(dotcl:build-library :system "word-stats.asd" :search-paths '("../text-util/")
                     :references '("Contracts.dll") :output "out/WordStatsPlugin.dll")
```

and in a project, `<LispSystem Include="word-stats.asd" />` with a
`<DotclAsdSearchPath Include="../text-util/" />` for each directory a dependency
is found in ([SDK and MSBuild properties](sdk.md#lisp-libraries)). `:system` and
`--system` take a system name too, found the way ASDF finds it.

What a function of the system can do is what a body can (the rules above). A
function is refused, with the chain of calls that reached it, when it reads a
special variable of the system (`defvar`, `defparameter`), when its definition is
not a top-level `defun` (a `defun` inside a `let`, a `defstruct` accessor), or
when it is a generic function. A `defun` that a macro or a top-level `macrolet`
writes counts as one. Constants (`defconstant`) and macros are used when the
library is built. All the systems end up in the one DLL; a library made of a
DLL per system is not built yet.

Nothing has to initialise the library: the DLL boots the runtime from a module
initializer the first time one of its types is touched, so the caller just uses
the types.

### Classes compiled into the .dll

The same goes for a class: in `dotnet:library`, a `:methods` entry named by a
symbol is compiled in as a function whose first argument is `self`, and the public
instance method calls it directly. A class with such a method also compiles its
`(:ctor ...)` bodies in (`(:bake t)` among the class options does so for a class
with none). Fields and properties are read and written through `self` with
`dotnet:invoke` and `dotnet:%set-invoke`; a property's get/set are plain IL and
need nothing from Lisp. A parameter or return type may name a class of the same
library, the class itself included.

```lisp
(dotnet:library ("ClsLib" :path "out/ClsLib.dll")
  (:class "ClsLib.Counter" ()
    (:fields ("Count" Int32))
    (:ctor ((start Int32))
      (dotnet:%set-invoke self "Count" start))
    (:methods
      (add ((n Int32)) :returns Int32
        (dotnet:%set-invoke self "Count" (+ (dotnet:invoke self "Count") n))
        (dotnet:invoke self "Count")))))
```

```csharp
var c = new Counter(5);
c.Add(3);   // 8
```

The restrictions are those of the functions above. A string-named method stays a
facade that needs the Lisp loaded. Two methods of one class cannot share a symbol.

### Callbacks and events

A delegate type may appear in the signature of a compiled-in function or method,
and a class may declare events. Both directions work with `DotCL.Runtime` alone.

A delegate that C# passes in (`Func<..>`, `Action<..>`, a delegate type of the
library or of the host) is a function designator in the body: `funcall`, `apply`
and the mapping functions call it, the arguments are converted to the delegate's
parameter types, and the result comes back converted (`nil` for a void delegate).
Passing it on to a C# method, or returning it, hands over the very same delegate.
An exception the delegate throws reaches the caller as it was thrown.

A function a body returns where the declared type is a delegate type (a `lambda`,
a closure over the body's variables, `#'name` of one of the library's functions)
reaches C# as a delegate of that type. An error the function does not handle
leaves as a `DotCL.LispErrorException` when C# calls the delegate.

```lisp
(dotnet:library ("CbLib" :path "out/CbLib.dll")
  (:defun square (x) (* x x))
  (:delegate "CbLib.Step" ((n Int32) (label String)) :returns Void)
  (:class "CbLib.Worker" ()
    (:events ("Progress" "System.Action`1[System.Int32]")
             ("Stepped" "CbLib.Step")
             ("Finished" "System.EventHandler"))
    (:methods
      (run ((n Int32)) :returns Void
        (dotimes (i n)
          (dotnet:invoke self "OnProgress" i)
          (dotnet:invoke self "OnStepped" i "s"))
        (dotnet:invoke self "OnFinished" (dotnet:static "System.EventArgs" "Empty")))
      (watch ((src "System.ComponentModel.INotifyPropertyChanged")) :returns Void
        (dotnet:add-event src "PropertyChanged" (lambda (sender e) ...)))))
  (:module "CbLib.Ops"
    (:functions
      (apply-fn ((f "System.Func`2[System.Int32,System.Int32]") (x Int32)) :returns Int32
        (funcall f x))
      (make-counter () :returns "System.Func`1[System.Int32]"
        (let ((n 0)) (lambda () (incf n))))
      (squarer () :returns "System.Func`2[System.Int32,System.Int32]" #'square))))
```

```csharp
Ops.ApplyFn(x => x * 2, 21);              // 42
var next = Ops.MakeCounter(); next(); next();   // 1, 2
var w = new Worker();
w.Progress += i => Console.WriteLine(i);
w.Finished += (sender, e) => Console.WriteLine("done");
w.Run(3);
```

An `:events` entry takes a delegate type, a .NET one or a `(:delegate ...)` of the
same library (listed before the class that uses it), and emits the event with its
`add` / `remove` accessors. The event also gets a public `On<Name>` method that
raises it from the body with `dotnet:invoke`: for an `EventHandler`-shaped delegate
(first parameter `object`) it takes the remaining parameters and passes `self` as
the sender, otherwise it takes all of them. Only a delegate that returns `void` can
be an event. A body subscribes to the event of a C# object with `dotnet:add-event`
and `dotnet:remove-event`.

A delegate type whose parameters the conversion cannot carry (`ref` / `out` / `in`,
a pointer, a ref struct such as `Span<T>`), an open generic delegate type, and a
`System.Delegate` return type are errors when the library is built, and the message
names the method, the parameter and the reason.

### The runtime surface of a compiled-in .dll

A .dll with compiled-in bodies names a set of `DotCL.Runtime` types, methods and
fields (its "surface"). `(dotcl:library-runtime-surface "X.dll")` lists it, and
`(dotcl:library-runtime-surface "X.dll" t)` puts the group of each entry in front.
The groups:

- **boundary**: the object model (`LispObject`, `Symbol`, `LispFunction`,
  `Nil.Instance`, `T.Instance`, `Fixnum.Make` / `Value`, the `DoubleFloat`,
  `SingleFloat` and `LispString` constructors, `Startup.SymInPkg` / `Keyword`),
  `DotclHost.Initialize`, the entry points of the public wrappers
  (`DynamicClassBuilder.InvokeBaked`, `InvokeBakedMethod`, `ResolveBakedFunction`,
  `EnsureBakedPackage`, `DispatchLispMethod`, `DispatchLispStatic`) and
  `LibrarySurface.Check`. Kept as public API.
- **abi**: `LibraryAbi`, the calling convention of compiled code as a library
  sees it. Kept as public API.
- **primitive**: `Runtime` methods, one per Common Lisp operation the compiler
  turns into a direct call (`Runtime.Add`, `Car`, `Length`, `Format`, `Sort`,
  `IsTrueLt` ...). Their meaning is fixed by the language; their C# names and
  signatures are not.
- **special**: what the code of a control or value operator calls
  (`MultipleValues`, `HandlerClusterStack`, `CatchTagStack`, `ThrowReturn` ...).
  Compiled-in code has none of it either: `catch` / `throw`, `handler-case`,
  `handler-bind`, `restart-case`, `unwind-protect`, `multiple-value-bind` and the
  like go through `LibraryAbi` too.
- **internal**: anything else. Compiled-in code has none: the check above fails
  if a library names one.

When dotnet:library compiles bodies into a .dll, every call the compiler would
make to its own calling convention is written as a call to `LibraryAbi` instead:

| `LibraryAbi` | in place of |
|---|---|
| `Enter`, `EnterUnit`, `Leave`, `Resume`, `ResumeBelow`, `UnlinkIfTop`, `Poll`, the `Frame` struct | `LispFunction.Enter0` ... `Enter4`, `Enter0U` ... `Enter4U` and the frame (a library's frames do not record the arguments), `ConditionSystem.PollInterrupt` |
| `Call` (0 to 4 arguments, 5 to 8, an array; each with a value mode) | `LispFunction.Call*`, `Invoke*`, `InvokeFast*`, `InvokeOnce*` |
| `TailPrep`, `TailDirect<T>`, `TailClosure<T>`, `Environment`, `SelfTailEntry`, `SelfTailPrepare`, `Lend` | `LispFunction.TailPrep0` ... `TailPrep8`, `TailDirect*`, `TailClosure*`, `SelfTail*`, `TailLendMarker` |
| `TakeMode`, `PassMode`, `ResetValues`, `ResetValuesOnExit`, `Primary` | `MultipleValues.TakeMode` / `PassMode` / `Reset` / `ResetOnExit`, `Runtime.UnwrapMv` |
| `NameSite`, `SymbolSite`, `SiteSymbol`, `FunctionOf`, `SetfFunctionOf`, `DefineFunction` | `Startup.SymSite` / `SymFnSite`, `CilAssembler.GetFunctionBySymbol` / `RegisterFunctionOnSymbol*` |
| `Function`, `Closure`, `DirectClosure`, `SetEntry`, `WithEntry`, `ClosureCheck`, `SetLambdaList` | the `LispFunction` constructors, `MakeDirectClosure`, `SetDirectDelegate`, `SetNativeDelegate`, `SetSelfFramedEntry`, `SetArityEntry`, `MarkLeafEntry`, `MarkMvModeEntry`, `WithClosureEntry`, `WithMvModeEntry`, `ClosureEntryCheck`, `Runtime.SetLambdaListSource` |
| `CheckArity`, `CheckArityMin`, `CheckKeys`, `RestArgs`, `KeyArg`, `Absent`, `IsAbsent` | `Runtime.CheckArityExact` / `CheckArityMin` / `CheckNoUnknownKeys` / `CollectRestArgs` / `FindKeyArg`, `AbsentKey` |
| `Preintern`, `SplitData`, `ReadData`, `ShareData` | `FaslData.Preintern` / `SplitParts` / `ReadValue` / `Share` |
| `PushCatch`, `PushCatchIndex`, `PopCatch`, `HasCatch`, `MakeThrow`, `CatchMatches`, `ThrownValue`, `ThrownTag` | `CatchTagStack.*`, `Runtime.MakeCatchThrow`, `ControlFlowFilters.CatchTagMatches`, `CatchThrowException` |
| `Binding`, `PushHandlers`, `PopHandlers`, `HandlerCaseClause`, `HandlerCaseCondition` | `HandlerBinding`, `HandlerClusterStack.*`, `ControlFlowFilters.HandlerCase*` |
| `Restart`, `SetRestartName` / `Report` / `Interactive` / `Test`, `PushRestarts`, `PopRestarts`, `RestartMatches`, `RestartArguments` | `LispRestart`, `RestartClusterStack.*`, `ControlFlowFilters.RestartCaseTag`, `RestartInvocationException` |
| `ClaimThrow`, `CanReturnThrow`, `ReturnThrow`, `IsThrowReturned`, `TakeThrown` | `ThrowReturn.*` (a library's THROW is always an exception) |
| `Held`, `Hold`, `Restore`, `Values0`, `Values` (2 to 8, an array), `Capture`, `CapturePair`, `PairNth`, `Nth`, `CallBound`, `PrimaryValue`, `PrimaryAlone`, `SavedCount`, `SavedValues`, `RestoreSaved` | `MvHeld`, `MultipleValues.*` |

Calls to a typed entry of another function are ordinary calls, not tail calls,
and no body is compiled as a leaf (a body whose frame the runtime builds only
when it signals). The rest of the compiled code is unchanged. Code compiled any
other way (the core, a fasl, the REPL) still calls the runtime directly.

Measured on the examples in `test/save-class-lib/check.sh` (types, methods and
fields named; `AbiLib` exercises optional, key and rest arguments, calls of six
arguments, multiple values, a closure, `handler-case`, `handler-bind`, `catch` /
`throw`, `unwind-protect`, `restart-case`, non-local exits and tail calls):

| .dll | surface | boundary | abi | primitive | special |
|---|---:|---:|---:|---:|---:|
| enum, const, struct, exception, interface, delegate libraries | 0 | | | | |
| class facade (`MyLibPack`) | 3 | 3 | | | |
| static functions (`FibLib`) | 50 | 20 | 17 | 13 | |
| classes with method and constructor bodies (`ClsLib`) | 56 | 20 | 27 | 9 | |
| bodies that call core functions (`ShakeLib`) | 99 | 26 | 36 | 37 | |
| calling convention and control operators (`AbiLib`) | 128 | 21 | 71 | 36 | 0 |

Each such .dll records the runtime it was built with as assembly metadata:
`dotcl-baked-runtime-version` (the `DotCL.Runtime` informational version),
`dotcl-baked-runtime-source` (the digest of the runtime source) and
`dotcl-runtime-surface` (`sha256:<hash of the sorted surface> n=<count>`).

When the .dll is first used, its module initializer compares the recorded source
digest with the running runtime's. If they are equal nothing more happens. If they
differ, every runtime member the .dll names is resolved against the running
runtime: when all are present the .dll runs without any message, and when some
are missing the initializer throws an `InvalidOperationException` that names the
version it was built with and the first missing members. The record is
declared, not enforced: a different version is never refused for being different.
