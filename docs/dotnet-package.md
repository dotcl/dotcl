# Calling .NET from Lisp

Everything in this guide is in the `dotnet:` package, which is there from the
start -- with two exceptions, `dotnet:ref` and `dotnet:using`, which come from a
contrib and are marked where they appear. Every form below was run against dotcl
and shows its real result.

The snippets share a few names. `sb` and `uri` are set up once here; the other
names (`list`, `dict`, `grid`, `path`, `buf`, `octets`, `doubles`, `a`, `b`) stand
for objects of the kind the surrounding text describes, which you create
yourself:

```lisp
(defvar sb (dotnet:new "System.Text.StringBuilder"))
(defvar uri (dotnet:new "System.Uri" "http://example.com/a/b"))
```

Numbers have rules of their own, which .NET numeric types arrive as which Lisp
types, when an integer argument is rejected, and how `System.Decimal` works.
A ratio crosses to a `double` or an `object` parameter as a double, a bignum
crosses to `object` as an `Int64`, `UInt64` or `BigInteger` by its range, and
to `decimal` a ratio or bignum must be exact.
See [Numbers across the .NET boundary](numbers.md).

## Objects, methods, properties

```lisp
(dotnet:new "System.Text.StringBuilder")          ; => #<DOTNET System.Text.StringBuilder >
(dotnet:invoke sb "Append" "hi")                  ; sb.Append("hi")
(dotnet:invoke sb "ToString")                     ; => "hi"
```

`dotnet:invoke` reaches methods and properties through the same call; a property
needs **no `get_` prefix**, and it is a place:

```lisp
(dotnet:invoke uri "Host")                        ; uri.Host        => "example.com"
(setf (dotnet:invoke sb "Capacity") 64)           ; sb.Capacity = 64
```

Statics take the type name in place of the object, and are also places:

```lisp
(dotnet:static "System.Math" "Sqrt" 2d0)          ; => 1.4142135623730951d0
(dotnet:static "System.Int32" "MaxValue")         ; => 2147483647
(setf (dotnet:static "System.Environment" "ExitCode") 0)
```

The receiver may be a Lisp string, character or number, since that is what .NET
calls hand back:

```lisp
(dotnet:invoke "abc" "ToUpper")                   ; => "ABC"
(dotnet:invoke 42 "ToString")                     ; => "42"
```

## Chains

`a.B.C(x).D` nests inside-out when written with `dotnet:invoke` alone. `dotnet:->`
takes the steps in call order instead; a step is a member name, or
`(member-name arg...)` to pass arguments:

```lisp
(dotnet:-> uri "Host" ("Substring" 0 7) "ToUpper")     ; => "EXAMPLE"
(setf (dotnet:-> sb "Capacity") 128)                   ; the chain is a place
(setf (dotnet:-> list ("Item" 0)) 99)                  ; indexed property
```

`dotnet:doto` applies several members to one object and returns that object:

```lisp
(dotnet:doto (dotnet:new "System.Text.StringBuilder")
             ("Append" "a") ("Append" 1))              ; => the builder, now "a1"
```

Member names are strings because the Lisp reader upcases bare symbols while .NET
member names are case-sensitive. A symbol is accepted and contributes its name
verbatim, so `|Host|` works and `host` correctly does not.

## Typed calls

A call whose receiver and arguments carry their .NET type is resolved when it is
compiled and becomes a direct call: no member lookup, no argument array, no
reflection at run time. The type is given with `(the (dotnet "T") x)`, or is
known from `dotnet:new`, `dotnet:box`, or an earlier typed call in a chain:

```lisp
(dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Append"
               (the (dotnet "System.String") "hi"))           ; sb.Append("hi")
(dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Length") ; => 2
```

`dotnet:static` with a literal type and member name does the same when every
argument is typed. With no arguments (a property, a field, or a method that
takes none) there is nothing to declare, so it is always resolved this way:

```lisp
(dotnet:static "System.Math" "Max" (the (dotnet "System.Int32") a)
                                   (the (dotnet "System.Int32") b))  ; Math.Max(int, int)
(dotnet:static "System.DateTime" "Now")                         ; DateTime.Now
(dotnet:invoke (dotnet:static "System.Text.Encoding" "UTF8") "GetByteCount"
               (the (dotnet "System.String") "abc"))            ; => 3
```

The declared types choose the overload, as in C#. A receiver or argument that is a
Lisp value (a string, number or character) is converted to the declared type the
same way as in the ordinary call, so `(the (dotnet "System.String") s)` accepts a
Lisp string. A call that does not resolve
(an argument without a type, a name the type does not have, a type defined at
run time) is made the ordinary way and gives the same result, only slower.
One difference remains: an exception the .NET method throws reaches the handler
as an `error` carrying the .NET type (`dotnet:exception-typep` sees it), but its
message does not start with the `DOTNET:STATIC Type.Member` context.

## Indexers

*(`dotnet:ref` needs `(require "dotnet-class")` -- see the end of the next
section.)*

`dotnet:ref` is the indexer without spelling out `get_Item` / `set_Item`, and it
is a place. Anything with an indexer works; `List<T>`, `Dictionary<K,V>`, and
plain arrays:

```lisp
(require "dotnet-class")                  ; once per process
(dotnet:ref list 0)                       ; => 10
(setf (dotnet:ref list 1) 99)
(setf (dotnet:ref dict "a") 1)            ; Dictionary<string,int>
(setf (dotnet:ref (dotnet:new-array "System.Int32" 1 2 3) 2) 42)
```

Multi-dimensional indexers take several keys: `(dotnet:ref grid row col)`.

`dotnet:make-array` takes one size per axis or a list of sizes, and
`:initial-element` / `:initial-contents` fill it as `make-array` does (a flat
sequence for rank 1, nested sequences for higher ranks).

`length`, `elt`, `coerce`, `aref`, `row-major-aref` (and its `setf`),
`array-rank`, `array-dimensions`, `array-dimension`, `array-total-size`,
`array-in-bounds-p` and `array-row-major-index` accept a .NET array; a jagged
array counts as rank 1. A wrong number of subscripts or an out-of-range
subscript signals an ordinary Lisp error naming the rank or the bound.

## Disposables

`dotnet:using` is C#'s `using`: each variable is bound in turn and `Dispose` is
called on the way out, including when the body signals. Nested bindings are
disposed innermost first. A NIL resource is skipped, and an error from the body
wins over one from `Dispose`.

```lisp
(require "dotnet-class")                  ; once per process
(dotnet:using ((stream (dotnet:new "System.IO.StreamReader" path)))
  (dotnet:invoke stream "ReadToEnd"))
```

Both of these come from the `dotnet-class` contrib, so they need
`(require "dotnet-class")`; the same one `dotnet:define-class` lives in.

## Types

```lisp
(dotnet:resolve-type "System.Uri")                              ; a System.Type
(dotnet:make-generic-type "System.Collections.Generic.List"
                          (list "System.Int32"))                ; List<int>
```

Type arguments may themselves be resolved types, so nested generics compose
instead of being spelled as one assembly-qualified string:

```lisp
(dotnet:make-generic-type "System.Action"
  (list (dotnet:make-generic-type "System.Collections.Generic.List"
                                  (list "System.String"))))     ; Action<List<string>>
```

A resolved type is accepted anywhere a type name is; `dotnet:new`,
`dotnet:make-delegate`, `dotnet:static-generic`, and as a method specializer.

The CLR boxes a `Nullable<T>` as its underlying `T`, so a value arrives as that
`T` and an empty `Nullable<T>` is `NIL`. Construct the empty value with no
argument, then test it with `null`; `HasValue` and `Value` are not on the boxed
value:

```lisp
(dotnet:new (dotnet:make-generic-type "System.Nullable" (list "System.Int32")))   ; => NIL
(dotnet:new (dotnet:make-generic-type "System.Nullable" (list "System.Int32")) 5) ; => Int32 5
```

## Collections and LINQ

```lisp
(let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                               (list "System.Int32")))))
  (dotnet:doto l ("Add" 3) ("Add" 1))
  (list (dotnet:-> l "Count") (dotnet:-> l ("Item" 0))))        ; => (2 3)

(dotnet:new-array "System.String" "a" "b")                      ; => String[]
```

Extension methods resolve after instance methods, so LINQ reads normally. A Lisp
lambda becomes the delegate, and the overload is chosen by its argument count;
one argument for the plain form, two for the indexed one:

```lisp
(dotnet:-> l ("Where" (lambda (x) (oddp x)))
             ("Select" (lambda (x) (* x 10)))
             "ToList")                                          ; => List<int>
```

The collection arguments pin the element type parameters (`TFirst`, `TSecond` in
`Zip`), and a lambda whose result type is still open is called once with default
arguments to read the type of its first value. `Select`, `Zip`, `Join`,
`GroupJoin` and the `SelectMany` forms therefore take bare lambdas:

```lisp
(dotnet:-> l ("Zip" l (lambda (a b) (+ a b))) "ToList")
(dotnet:-> l ("SelectMany" (lambda (x) (list x x))) "ToList")
(dotnet:-> l ("SelectMany" (lambda (x i) (list x i))) "ToList")
```

A collection selector may return a .NET enumerable, a Lisp list or a Lisp
vector; each is converted to the `IEnumerable<T>` the delegate asks for. If the
result type is still not evident (the first returned value is NIL, say), it
stays `object`, as `Select` has always used. `Cast` and `OfType` take their one
type argument after the receiver, as a name or a `System.Type`:

```lisp
(dotnet:-> l ("Cast" "System.Object") "Count")
(dotnet:-> l ("OfType" (dotnet:resolve-type "System.Int32")) "Count")
```

A generic method called with `dotnet:static` or `dotnet:invoke` also infers its
type arguments when the values pin them: a typed delegate contributes its own
delegate type, a Lisp string contributes `System.String`, and a fixnum that fits
`System.Int32` contributes `Int32` where the parameter is the bare type
parameter. The call is made only when one instantiation leaves exactly one
overload, so `Task.Run` with `Func<int>`, `List<T>.ConvertAll` with a
`Converter<int,string>`, `Tuple.Create` with an integer and a string, and
`Task.FromResult` with an integer need no explicit form. NIL, an integer wider
than `Int32`, or any other value that fits more than one instantiation or
overload still needs `dotnet:static-generic` or `dotnet:invoke-generic`.

## Enums, out parameters, exceptions

```lisp
(dotnet:enum-or "System.IO.FileAccess" "Read" "Write")          ; => ReadWrite

(logior (dotnet:static "System.IO.FileAccess" "Read")
        (dotnet:static "System.IO.FileAccess" "Write"))        ; => ReadWrite

(multiple-value-list (dotnet:call-out "System.Int32" "TryParse" "42"))
;; => (T 42): return value first, then each out/ref parameter
```

`LOGIOR`, `LOGAND`, `LOGXOR`, `LOGTEST`, `LOGANDC1` and `LOGANDC2` accept values
of one `[Flags]` enum type, and integers, and return that enum type. A
non-`[Flags]` enum or a mix of two enum types is a `TYPE-ERROR`.

A COM object reached through IDispatch carries no signature, so there every
argument is passed, out positions included, and each one's value after the call
follows the return value.

A .NET exception arrives as a Lisp condition that remembers its CLR type:

```lisp
(handler-case (dotnet:static "System.Int32" "Parse" "nope")
  (error (e) (dotnet:exception-typep e "System.FormatException")))    ; => T

(block nil
  (dotnet:handler-bind (("System.FormatException" (e) (return-from nil :format-error)))
    (dotnet:static "System.Int32" "Parse" "nope")))                   ; => :FORMAT-ERROR
```

## Spans and memory

A parameter typed `Span<T>`, `ReadOnlySpan<T>`, `Memory<T>` or `ReadOnlyMemory<T>`
takes a Lisp vector directly. When the vector's storage has element type `T`, the
span is a view of that storage, not a copy, so a method that writes into a span
writes into the vector:

```lisp
(let* ((buf (make-array 16 :element-type '(unsigned-byte 8)))
       (stream (dotnet:new "System.IO.FileStream" path "Open" "Read")))
  (dotnet:invoke stream "Read" buf))            ; FileStream.Read(Span<byte>) fills BUF

(dotnet:invoke (dotnet:static "System.Text.Encoding" "UTF8") "GetBytes" "abc" buf)
(multiple-value-list (dotnet:call-out "System.Int32" "TryParse" octets))  ; ReadOnlySpan<byte>
(dotnet:static-generic "System.MemoryExtensions" "Reverse" '("System.Double") doubles)
```

| Lisp vector | storage |
|---|---|
| string | `char` |
| `(unsigned-byte 8)` | `byte` |
| `(unsigned-byte 16)` | `ushort` |
| `(signed-byte 32)` | `int` |
| `(signed-byte 64)`, `fixnum` | `long` |
| `(unsigned-byte 64)` | `ulong` |
| `single-float` / `double-float` | `float` / `double` |

`(unsigned-byte 32)`, `(signed-byte 8)` and `(signed-byte 16)` vectors are stored
wider than their element type, except those made by `dotcl:make-pinned-vector`.
The span covers the vector's active part: up to its fill pointer, starting at its
displaced offset. A .NET array or memory object passes as itself.

A read-only span or memory can also take a list, a general vector, or a numeric
vector of another width; it then receives a copy. A writable one needs the
storage itself, since writes into a copy would be lost.

An array parameter (`T[]`, or `System.Array`) works the same way for a vector
whose storage is that array: a one-dimensional vector without a fill pointer or
displacement is passed as its storage, so `Stream.Read(byte[], int, int)`,
`Array.Copy` or `TextReader.Read(char[], int, int)` write into the vector. A vector
with such storage and a fill pointer or a displacement signals a `type-error`
rather than receiving a copy whose writes would be lost; pass a simple vector
(`copy-seq`, then `replace` the result back). A list, a general vector, or a vector
of another element type (including `(unsigned-byte 4)`, whose storage is a `byte[]`) is still
copied in. An array a method keeps (`new MemoryStream(byte[])`) is the vector's
storage until `adjust-array` replaces it. `dotnet:new` chooses among constructors
the same way, so `(dotnet:new "System.IO.MemoryStream" octets)` is a stream over
`octets` and `(dotnet:new "System.IO.MemoryStream" 16)` one with capacity 16.
Constructors taking a span or a pointer are not called.

A method that returns a span gives back a fresh Lisp vector of the element type
(a string for `char`):

```lisp
(dotnet:static "System.MemoryExtensions" "Trim" "  hi  ")    ; => "hi"
```

When overloads differ only in a span, memory or string/array parameter, the one the
arguments fit is used; overloads with no span or memory parameter are preferred, then
span ones, then memory ones. Span arguments and results go through generated code, so
on a build without `System.Reflection.Emit` (netstandard2.0, `DotclNoEmit`) such a call
signals an error; `Memory<T>` parameters work there. Other ref struct types
(`Utf8JsonReader`, ...) cannot be passed. If storage is replaced by `adjust-array`, a
`Memory<T>` a method kept no longer refers to the vector.

## Delegates and callbacks

```lisp
(dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]" (lambda (x) (* x 2)))
```

A Lisp error inside a callback is **contained** at the boundary: it is reported
to `*error-output*` and the delegate returns its return type's default. That is
what keeps a .NET-driven loop (a UI event, a game loop) alive when Lisp signals.
When Lisp itself triggered the callback, bind
`dotcl:*foreign-callback-propagate*` to have the error reach your handler
instead:

```lisp
(let ((fn (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                (lambda (x) (declare (ignore x)) (error "boom"))))
      (dotcl:*foreign-callback-propagate* t))
  (handler-case (dotnet:invoke fn "Invoke" 5)
    (error (e) (princ-to-string e))))                           ; => "boom"
```

`dotcl:*foreign-callback-handler*` is the other hook: a function of one argument
(the condition) whose value becomes the callback's result.

Non-local exits are not affected by containment; `return-from` and `throw` out
of a callback unwind to their target as usual.

### What crosses a .NET frame

There are three kinds of boundary, and they differ in what an error does.

- **A library function with a compiled-in body** (a `dotnet:library` function or
  method named by a symbol), called from C# or from Lisp. Lisp's condition
  system is not in the way: called from Lisp, an enclosing `handler-case`,
  `handler-bind` (with the special bindings around the call) and `restart-case`
  all see the error, and `throw` to an enclosing `catch` works. Called from C#
  with no Lisp frame outside, the error leaves as a `DotCL.LispErrorException`
  (see below).
- **A method of a `dotnet:define-class` class whose body is a Lisp lambda** and
  **a delegate or event handler built from a Lisp function**. The boundary
  establishes its own `ERROR` handler, so an enclosing `handler-bind` does not
  see the error first; by default it is reported and the caller gets the return
  type's default. With `dotcl:*foreign-callback-propagate*` true the error is
  signalled again after the callback's frames have unwound, so an enclosing
  `handler-case` or `handler-bind` sees it, with its condition type intact and
  with the special bindings outside the callback, but without the restarts the
  callback established. A `warning` is not an `ERROR` and reaches enclosing
  handlers either way.
- **Non-local exits** (`throw`, `return-from`, `go`) cross both kinds of boundary
  as ordinary .NET exceptions, running `unwind-protect` cleanups on the way. Two
  consequences: a C# frame between the callback and its target that has a
  catch-all (`catch (Exception)`) swallows the exit, and the target is then not
  reached; and a callback that runs on another thread cannot throw to a `catch`
  of the thread that started it, because catch tags belong to a thread, so that
  `throw` is a `CONTROL-ERROR` in the callback (contained as above). A `throw`
  to a tag nobody catches is a `CONTROL-ERROR` at the `throw`, never a stray
  exception in the C# caller.

### Errors leaving a library

A library function with a compiled-in body that is called from C# throws a
subclass of `DotCL.LispErrorException`, chosen by the type of the condition. All
of them keep the condition in `Condition`, and an existing
`catch (LispErrorException)` still catches them:

| condition type | exception |
|---|---|
| `division-by-zero` | `LispDivisionByZeroException` (a `LispArithmeticErrorException`) |
| `arithmetic-error` | `LispArithmeticErrorException` |
| `unbound-variable` | `LispUnboundVariableException` (a `LispCellErrorException`) |
| `undefined-function` | `LispUndefinedFunctionException` (a `LispCellErrorException`) |
| `cell-error` | `LispCellErrorException` |
| `type-error` | `LispTypeErrorException` |
| `end-of-file` | `LispEndOfFileException` (a `LispStreamErrorException`) |
| `stream-error` | `LispStreamErrorException` |
| `file-error` | `LispFileErrorException` |
| `control-error` | `LispControlErrorException` |
| `program-error` | `LispProgramErrorException` |
| anything else | `LispErrorException` |

A library can also name an exception type of its own. A `:class` that derives from
`System.Exception` and says `(:maps-condition type-error)` is thrown, built with the
condition's report as its message, for a condition of that type, in place of the
runtime's class; the condition is in `Exception.Data["dotcl.condition"]`. Such a
class needs nothing from this runtime, so it cannot derive from `LispErrorException`
(.NET has single inheritance), which is why it carries the condition in `Data`. For
the same reason there is no mapping to `InvalidCastException` or another framework
exception: that type could not also be a `LispErrorException`. A library that wants
a framework type defines its own class deriving from it and maps the condition to
it.

## Tasks

```lisp
(dotnet:await (dotnet:static-generic "System.Threading.Tasks.Task" "FromResult"
                                     (list "System.Int32") 7))  ; => 7
(dotnet:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 1))   ; => NIL
```

`dotnet:await` blocks the calling thread until the task completes.

## UI threads

Lisp runs on a worker thread with a large stack, because deeply nested macro
expansion needs one. UI toolkits that accept work on the process main thread
only, macOS AppKit above all, get it back with `dotcl:call-on-main-thread`:

```lisp
(dotcl:main-thread-p)                                    ; => NIL
(dotcl:call-on-main-thread (lambda () (dotcl:main-thread-p)))  ; => T
```

The call blocks until the function returns and passes its value back, so
wrapping a GUI's event loop in it hands the main thread to the toolkit for as
long as the application runs. An error signalled inside reaches the caller's
handlers rather than the debugger. Windows and X11 impose no such requirement,
and the wrapper costs nothing there.

For WinForms and WPF on Windows, `dotnet:ui-invoke` and `dotnet:ui-post`
marshal onto a dedicated STA thread with a message loop instead.

## Dispatching on .NET types

A .NET object's class participates in CLOS dispatch. A specializer may be a type
name string, a form producing a `System.Type`, or a symbol naming the type; the
class is registered on the spot, so no instance need exist first:

```lisp
(defmethod kind ((x "System.Text.StringBuilder")) :stringbuilder)
(defmethod kind ((x "System.Collections.IEnumerable")) :enumerable)
(defmethod kind ((x (dotnet:resolve-type "System.Collections.Generic.List`1"))) :any-list)
```

Precedence follows the .NET type: concrete classes first, then interfaces
(most derived first), and a closed generic type is more specific than its open
definition. So `List<int>` picks `:any-list` above, `String[]` picks
`:enumerable`, and a method on `List<int>` itself would beat both.

`(typep obj 'class-name)` and `subtypep` agree with that ordering, and
`dotnet:class-for-type` returns the class object for a type.

## Coming from C#: what to write instead

The forms below are the first thing a C# programmer writes that does not do what
it looks like, with what to write. Each one was run.

| In C# | Written naively | What to write |
|---|---|---|
| `await client.GetStringAsync(url)` | `(dotnet:invoke client "GetStringAsync" url)` returns the `Task`, not the string; the first use of it fails ("not a sequence", "not a number"). | `(dotnet:await (dotnet:invoke client "GetStringAsync" url))` |
| `Task.Run(() => 42).Result` | `(dotnet:static "System.Threading.Tasks.Task" "Run" (lambda () 42))` binds the lambda to `Run(Action)`, so the result is a plain `Task` with no `Result` ("Method ... Result not found"). | `(dotnet:await (dotnet:static-generic "System.Threading.Tasks.Task" "Run" '("System.Int32") (lambda () 42)))` |
| `new byte[64]` | `(dotnet:new-array "System.Byte" 64)` is an array of one element, the number 64 (`new-array` takes the elements). A later `Stream.Read(buf, 0, 64)` fails with "Offset and length were out of bounds". | `(dotnet:make-array "System.Byte" 64)`, or a Lisp vector `(make-array 64 :element-type '(unsigned-byte 8))`, which passes as `byte[]` |
| `dict.TryGetValue(k, out v)` | `(dotnet:invoke dict "TryGetValue" k)` ("Method ... not found": the out parameter is missing). | `(multiple-value-list (dotnet:call-out dict "TryGetValue" k))` gives `(T v)`; `call-out` also takes an object, not only a type name |
| `list[0]`, `dict["a"]` | `dotnet:ref` is not defined until the module that provides it is loaded (`Symbol "REF" is not external`). | `(require "dotnet-class")` once, then `(dotnet:ref list 0)` |
| `foreach (var m in matches)` | `dolist` does not accept a .NET collection or array ("not of type LIST"), and neither do `find`, `position`, `remove-if`, `subseq`, `reduce` and the other sequence functions ("not a sequence"). `length`, `elt`, `coerce` to `list` / `vector`, `map`, `some` and `loop ... across` read a .NET array or collection as a sequence, and `aref` indexes an array. | `(dolist (m (coerce matches 'list)) ...)`; the same coercion lets the other sequence functions take the elements. Or index it with `aref` / `(dotnet:ref coll i)`, or walk `GetEnumerator` with `MoveNext` / `Current` |
| `JsonSerializer.Serialize(new[] {1, 2, 3})` | A Lisp list is not a .NET collection: `JsonConvert.SerializeObject` of one writes its cons cells (`{"Car":1,"Cdr":...}`), and `JsonSerializer.Serialize` finds no overload. | build a .NET array or list, and name the type for a generic method: `(dotnet:static-generic "System.Text.Json.JsonSerializer" "Serialize" '("System.Int32[]") (dotnet:new-array "System.Int32" 1 2 3))` |
| `a == b` on two enum values | `(eql (dotnet:static "System.DayOfWeek" "Monday") (dotnet:static "System.DayOfWeek" "Monday"))` is NIL: each call boxes a new object. | `(dotnet:invoke a "Equals" b)` |
| `Span<T>` returned by a method (`AsSpan`) | "Method ... not found": a `Span<T>` cannot be held in a Lisp variable. | pass the array to the method that wants the span instead (see "Spans and memory") |
| `app.MapGet("/", () => "hi")` (ASP.NET Core minimal API) | With only `Microsoft.AspNetCore` loaded, `MapGet` (an extension method in `Microsoft.AspNetCore.Routing`) is "no method of that name". Route and query parameters are bound by the delegate's parameter names, and `Func<string, string>` names its parameter `arg`, not the `{n}` of the route. | `(dotnet:load-assembly "Microsoft.AspNetCore.Routing")`, then `(dotnet:invoke app "MapGet" "/" (dotnet:make-delegate "System.Func`1[System.String]" (lambda () "hi")))`; a `RequestDelegate` or `Func<HttpContext, IResult>` works too. Then `(dotnet:invoke (dotnet:invoke app "Urls") "Add" "http://127.0.0.1:5000")` and `(dotnet:invoke app "Start")` |

## What can I call?

Members are named with strings, so the name, its casing, the overloads and the
optional parameters all have to come from somewhere. `dotnet:members` asks the
runtime:

```lisp
(dotnet:members "System.Net.Sockets.TcpClient" :prefix "Connect")
;; => one entry per Connect and ConnectAsync overload; reflection order is
;;    unspecified, so this listing is representative:
;;    (:name "Connect" :kind :method :static nil
;;     :signature "(IPAddress, Int32) => Void"
;;     :declaring-type "System.Net.Sockets.TcpClient")
;;    (:name "Connect" :kind :method :static nil
;;     :signature "(IPAddress[], Int32) => Void"
;;     :declaring-type "System.Net.Sockets.TcpClient")
;;    ...
```

The first argument is a type name, a resolved `System.Type`, or any .NET object
-- an object answers for its runtime type, so a value already in hand can be
asked what it takes:

```lisp
(let ((sb (dotnet:new "System.Text.StringBuilder")))
  (dotnet:members sb :prefix "AppendLine"))
```

Keyword arguments narrow the listing: `:kind` (`:method` `:property` `:field`
`:event` `:constructor`), `:prefix` (case-insensitive), `:static` (`:both`, the
default, `:static`, `:instance`), `:inherited` (default `t`) and `:extensions`
(default `t`).

Two details make the output match what you are about to write. Property
accessors appear under the names `dotnet:invoke` takes, not as bare property
names:

```lisp
(dotnet:members "System.Text.StringBuilder" :prefix "set_")
;; => set_Capacity (Int32) => Void / set_Chars (Int32, Char) => Void / ...
```

And an optional trailing parameter is marked with `?`, because omitting one is
what makes a call fail to resolve. A type that lives in an assembly not loaded
yet needs that assembly first:

```lisp
(dotnet:load-assembly "Microsoft.AspNetCore.Http.Abstractions")
(dotnet:members "Microsoft.AspNetCore.Http.HttpResponse" :prefix "Write")
;; => WriteAsync (String, CancellationToken?) => Task
;;    WriteAsync (String, Encoding, CancellationToken?) => Task
```

Extension methods are included, with the receiver dropped from the parameter
list so the signature reads as the call: `WriteAsync` above is one, and so is
LINQ over a string.

## Loading assemblies

```lisp
(dotnet:load-assembly "System.Text.Json")
;; => "System.Text.Json, Version=10.0.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51"
```

The return value is the loaded assembly's full name string, which identifies the
version that was resolved. Types from a loaded assembly resolve by name
afterwards. For NuGet packages, see
[Using libraries](libraries.md).

## Conditions in a .NET host

The other direction: a .NET application that embeds dotcl (`DotclHost`) and runs
Lisp code from its own entry points. A Lisp condition that no Lisp handler takes
has to become something the host can catch, and `SetThrowingDebuggerHook` is what
binds `*debugger-hook*` to do that instead of stopping in the interactive
debugger -- which a server or a game loop has no console to answer.

What it throws is a `DotclConditionException`, and the condition survives on it:
`Condition` is the live condition object, `ConditionType` its Lisp type name,
`Message` its report string, and `ClrException` the .NET exception it wraps when
the failure came from a .NET call. A condition a Lisp handler takes never reaches
the host at all.

```csharp
DotclHost.EnsureCore();
DotclHost.SetThrowingDebuggerHook();

try { DotclHost.EvalString("(error \"boom ~a\" 42)"); }
catch (DotclConditionException e)
{
    // e.ConditionType is "SIMPLE-ERROR", e.Message is "boom 42",
    // e.Condition is the condition itself -- hand it back to Lisp to read
    // its slots, and e.ClrException is non-null if .NET raised the failure.
}
```

Choosing a **restart** is a different job, and the catch block is too late for
it: by the time the exception reaches the host, the Lisp stack the restart lives
on has already unwound. The decision has to happen while that stack is still
standing, so make the hook itself ask the host:

```csharp
// The host decides which restart to use; Lisp invokes it in place.
DotclHost.Register("host-restart-for", args => "USE-VALUE");
DotclHost.EvalString(@"(setq *debugger-hook*
  (lambda (c hook)
    (declare (ignore hook))
    (let ((r (find-restart (intern (host-restart-for (princ-to-string c)) :cl-user) c)))
      (when r (invoke-restart r 99)))))");

// The restart returns 99 to the RESTART-CASE, so nothing is thrown at all.
var v = DotclHost.EvalString("(restart-case (error \"needs a value\") (use-value (x) x))");
```

A failure the runtime raises itself -- a .NET method that threw, reached through
`dotnet:invoke`, or a type error from `car` -- runs a `*debugger-hook*` of your own
(like the restart-choosing one above) as `error` does, but not the hook
`SetThrowingDebuggerHook` installs: it unwinds as a `LispErrorException`. With that
hook installed, the
`DotclHost` entry points (`EvalString`, `EvalStringMv`, `Call`, `CallMv`,
`LoadLispFile`) turn such a failure into a `DotclConditionException` on its way
out, so one `catch` covers both kinds. `ClrException` is the original .NET
exception here too, and `InnerException` is the `LispErrorException` that carried
it. Only the outermost entry converts: when Lisp code calls back into the host
and the host calls into Lisp again, the inner failure stays a
`LispErrorException` until it has passed the Lisp frames in between, whose
handlers still see the original condition. A host that has bound
`*debugger-hook*` to a hook of its own that returns or is NIL gets the
`LispErrorException` unchanged.

`SetThrowingDebuggerHook(false)` selects the older behaviour, where the hook
throws an `InvalidOperationException` whose message is `"TYPE: report"` and which
carries nothing else, and a runtime-raised failure still arrives as a
`LispErrorException`.

## Stack size when embedding

The `dotcl` executable runs Lisp on a thread it creates with a **256 MB** stack
(`runtime/Program.cs`). A host that calls `DotclHost` from its own threads gets
whatever those threads have, which for a .NET thread pool worker or a plain
`new Thread(...)` is the default **1 MB**. Nothing in `DotclHost` changes that,
and nothing warns about it.

It matters because the recursion depth the two allow is not in the same range.
Compiling deeply nested code, expanding a macro that recurses over a large form,
and walking a deep list in Lisp all recurse in .NET frames. A workload that the
`dotcl` executable runs without noticing can end a 1 MB host thread with a
`StackOverflowException` -- which .NET does not let anything catch: the process
goes, with no condition, no handler, and no `finally`.

So: if the Lisp you run is anything more than short calls into shallow
functions, **run it on a thread you gave a large stack**, and keep using that
one thread.

```csharp
// 256 MB, the same figure the dotcl executable uses.
Exception? failure = null;
var lisp = new Thread(() => {
    try
    {
        DotclHost.EnsureCore();
        DotclHost.EvalString("(load \"app.lisp\")");
    }
    catch (Exception e) { failure = e; }
}, 256 * 1024 * 1024);
lisp.Start();
lisp.Join();
if (failure != null) throw failure;
```

Reusing one such thread is worth more than the stack alone: dotcl keeps
per-thread state (special-variable bindings, handler and restart clusters, the
catch-tag stack), so a host that hops between pool threads gets a fresh dynamic
environment on each hop, and a `handler-bind` established on one thread is not
in scope on the next.

### On a `RunOnLispThread` helper

`DotclHost` could own that thread and expose `RunOnLispThread(Action)`. It is
deliberately **not** added yet, and the reason is that it would be the second
answer to a question dotcl already answers once: `dotcl:call-on-main-thread`
exists because macOS AppKit accepts work from thread 0 and nothing else, so the
executable keeps the real main thread as a work queue and Lisp submits to it.

A host embedding dotcl in a UI application needs both properties at once -- a
large stack and, on macOS, thread 0 -- and thread 0 cannot be created after the
fact. A helper that owns "the Lisp thread" without knowing about that constraint
would be right for a console host and wrong for a UI one, and the two would then
disagree about which thread is "the" Lisp thread.

The decision is to design the two together, as one story about which thread Lisp
runs on, rather than add the easy half now. Until then the snippet above is the
supported shape, and it is what the executable itself does.
