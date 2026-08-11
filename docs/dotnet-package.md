# Calling .NET from Lisp

Everything in this guide is in the `dotnet:` package, which is available without
loading anything. Every form below was run against dotcl and shows its real
result.

Numbers have rules of their own — which .NET numeric types arrive as which Lisp
types, when an integer argument is rejected, and how `System.Decimal` works.
See [Numbers across the .NET boundary](numbers.md).

## Objects, methods, properties

```lisp
(dotnet:new "System.Text.StringBuilder")          ; => #<DOTNET System.Text.StringBuilder>
(dotnet:invoke sb "Append" "hi")                  ; sb.Append("hi")
(dotnet:invoke sb "ToString")                     ; => "hi"
```

`dotnet:invoke` reaches methods and properties through the same call — a property
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
(setf (dotnet:-> lst ("Item" 0)) 99)                   ; indexed property
```

`dotnet:doto` applies several members to one object and returns that object:

```lisp
(dotnet:doto (dotnet:new "System.Text.StringBuilder")
             ("Append" "a") ("Append" 1))              ; => the builder, now "a1"
```

Member names are strings because the Lisp reader upcases bare symbols while .NET
member names are case-sensitive. A symbol is accepted and contributes its name
verbatim, so `|Host|` works and `host` correctly does not.

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

A resolved type is accepted anywhere a type name is — `dotnet:new`,
`dotnet:make-delegate`, `dotnet:static-generic`, and as a method specializer.

## Collections and LINQ

```lisp
(let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                               (list "System.Int32")))))
  (dotnet:doto l ("Add" 3) ("Add" 1))
  (list (dotnet:-> l "Count") (dotnet:-> l ("Item" 0))))        ; => (2 3)

(dotnet:new-array "System.String" "a" "b")                      ; => String[]
```

Extension methods resolve after instance methods, so LINQ reads normally. A Lisp
lambda becomes the delegate, and the overload is chosen by its argument count —
one argument for the plain form, two for the indexed one:

```lisp
(dotnet:-> l ("Where" (lambda (x) (oddp x)))
             ("Select" (lambda (x) (* x 10)))
             "ToList")                                          ; => List<object>
```

`Select<TSource,TResult>` cannot infer `TResult` from a Lisp closure, so it comes
back as `object`. Use `dotnet:static-generic` when a specific instantiation
matters.

## Enums, out parameters, exceptions

```lisp
(dotnet:enum-or "System.IO.FileAccess" "Read" "Write")          ; => ReadWrite

(multiple-value-list (dotnet:call-out "System.Int32" "TryParse" "42"))
;; => (T 42)   — return value first, then each out/ref parameter
```

A .NET exception arrives as a Lisp condition that remembers its CLR type:

```lisp
(handler-case (dotnet:static "System.Int32" "Parse" "nope")
  (error (e) (dotnet:exception-typep e "System.FormatException")))    ; => T

(block nil
  (dotnet:handler-bind (("System.FormatException" (e) (return-from nil :format-error)))
    (dotnet:static "System.Int32" "Parse" "nope")))                   ; => :FORMAT-ERROR
```

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
(let ((dotcl:*foreign-callback-propagate* t))
  (handler-case (dotnet:invoke fn "Invoke" 5)
    (error (e) (princ-to-string e))))                           ; => "boom"
```

`dotcl:*foreign-callback-handler*` is the other hook: a function of one argument
(the condition) whose value becomes the callback's result.

Non-local exits are not affected by containment — `return-from` and `throw` out
of a callback unwind to their target as usual.

## Tasks

```lisp
(dotnet:await (dotnet:static-generic "System.Threading.Tasks.Task" "FromResult"
                                     (list "System.Int32") 7))  ; => 7
(dotnet:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 1))   ; => NIL
```

`dotnet:await` blocks the calling thread until the task completes.

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

## Loading assemblies

```lisp
(dotnet:load-assembly "System.Text.Json")     ; => T
```

Types from a loaded assembly resolve by name afterwards. For NuGet packages, see
[Using libraries](libraries.md).
