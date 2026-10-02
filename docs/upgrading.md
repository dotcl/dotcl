# Upgrading a .NET host

Notes for C# code that references `DotCL.Runtime` directly: an application that
hosts dotcl, a library that calls into it, or a tool built against its types.
Lisp code is not affected by anything here; Lisp-level changes are in the
release notes.

Each section lists what changed in the public C# surface of `DotCL.Runtime`, the
code that stops compiling or running, and what to write instead. The before and
after snippets were compiled against the two releases they name.

## 0.1.30 to 0.1.31

Every change below breaks binary compatibility, so **rebuild your host against
0.1.31**. An assembly built against 0.1.30 and run with 0.1.31 fails when it
reaches one of these members, for example:

```
System.MissingFieldException: Field not found: 'DotCL.Symbol.Function'.
```

Lisp FASLs compiled by 0.1.30 do not reference these members and are not
affected by this.

Most code needs only the rebuild. The source changes are below. The last two
sections are different: they compile unchanged and change behavior.

### `Symbol.Function` and `Symbol.SetfFunction` are properties

They were public fields. Reading and assigning them is unchanged, but a property
cannot be passed by `ref` or `out`, so code that used the field with
`Interlocked`, `Volatile` or a `ref` parameter no longer compiles (CS0206).

Before (0.1.30):

```csharp
public static LispObject? SwapFunction(Symbol sym, LispObject f)
    => Interlocked.Exchange(ref sym.Function, f);
```

After (0.1.31):

```csharp
static readonly object FunctionLock = new();

public static LispObject? SwapFunction(Symbol sym, LispObject f)
{
    lock (FunctionLock)
    {
        var old = sym.Function;
        sym.Function = f;
        return old;
    }
}
```

The lock only orders callers that take it. If you need no atomic swap, a plain
`sym.Function = f;` is enough.

### `LispMethod.Qualifiers` is `LispObject[]`

It was `Symbol[]`. A method qualifier can be any object other than a list, and
qualifiers the runtime does not represent as a `Symbol` (`T`, or a number) used to
be dropped, turning the method into a primary one. Code that reads the property
into a `Symbol[]` no longer compiles (CS0266).

Before (0.1.30):

```csharp
public static Symbol[] QualifiersOf(LispMethod m) => m.Qualifiers;
```

After (0.1.31):

```csharp
public static LispObject[] QualifiersOf(LispMethod m) => m.Qualifiers;

// For display: QualifierName gives the name of a symbol or T, and null for
// any other qualifier.
public static string DescribeQualifiers(LispMethod m)
    => string.Join(" ", m.Qualifiers.Select(q => LispMethod.QualifierName(q) ?? q.ToString()));
```

Do not cast each element to `Symbol`: the cast fails on a qualifier that is not
one. `LispMethod.QualifierSame(a, b)` compares two qualifiers the way the runtime
decides whether two methods are the same method: symbols and `T` by name,
anything else by EQL.

The constructor's second parameter changed the same way, to `LispObject[]`.
Passing a `Symbol[]` still compiles, because arrays of a class convert to arrays
of its base class:

```csharp
// compiles against both releases
new LispMethod(specializers, new Symbol[] { around }, function);
// the 0.1.31 spelling
new LispMethod(specializers, new LispObject[] { around }, function);
```

### `LispProcess.Launch` has two more optional parameters

`Launch` gained `Encoding? encoding = null` and `bool binary = false` after
`environment`. Calls written for 0.1.30 compile unchanged, and with both left at
their defaults the child process is set up as before. Only the rebuild is
needed.

### Runtime errors arrive as `DotclConditionException` under the typed hook

This one compiles unchanged and behaves differently. In a host that called
`DotclHost.SetThrowingDebuggerHook()`, a Lisp `error` already reached C# as a
`DotclConditionException`, but an error the runtime raises itself, such as
`(car 5)` or a .NET exception thrown through `dotnet:invoke`, still arrived as a
`LispErrorException`. In 0.1.31 both kinds arrive as `DotclConditionException`
at the outermost host entry point (`EvalString`, `Call`, `LoadLispFile` and their
multiple-value forms). A `catch (LispErrorException)` written for the runtime
errors is no longer reached, so whatever it did is skipped.

Before (0.1.30):

```csharp
DotclHost.SetThrowingDebuggerHook();
try { DotclHost.Call("HOST-CAR", 5); }
catch (DotclConditionException e) { Console.WriteLine($"lisp error {e.ConditionType}"); }
catch (LispErrorException e) { Console.WriteLine($"runtime error {e.Message}"); }
```

With 0.1.30 `(car 5)` takes the second clause; with 0.1.31 it takes the first.

After (0.1.31):

```csharp
DotclHost.SetThrowingDebuggerHook();
try { DotclHost.Call("HOST-CAR", 5); }
catch (DotclConditionException e)
{
    // e.ConditionType is TYPE-ERROR here. For a failed dotnet:invoke,
    // e.ClrException is the original .NET exception; e.InnerException is the
    // LispErrorException the runtime raised.
    Console.WriteLine($"{e.ConditionType}: {e.Message}");
}
```

Nothing changes for a host that installs no hook, or that installed it with
`SetThrowingDebuggerHook(false)`, or for a host call nested inside Lisp code
(Lisp handlers in between still see the original condition).

### Names passed to `DotclHost` are read as the reader reads them

This one also compiles unchanged and behaves differently. `Call`, `CallMv`,
`GetSpecial`, `SetSpecial`, `Register` and the `CurrentPackage` setter take a
name, and 0.1.30 matched it exactly against the symbol name, so
`(defun fact ...)` was called as `Call("FACT")`. In 0.1.31 the string is read
the way the Lisp reader reads a symbol: unescaped letters follow the current
readtable's case (upcased under the standard readtable), `|...|` and `\` keep
the case, and `pkg:name` / `pkg::name` qualify, with the package name read the
same way. Which package an unqualified name is looked up in does not change.

`Call("FACT")` keeps working, and `Call("fact")` now works too. The one call that
changes meaning is a lowercase name written to reach a symbol whose name is
lowercase:

Before (0.1.30):

```csharp
DotclHost.EvalString("(defun |lower| () :lowercase)");
DotclHost.Call("lower");     // the symbol named "lower"
```

After (0.1.31):

```csharp
DotclHost.Call("|lower|");   // the symbol named "lower"
DotclHost.Call("lower");     // now the symbol LOWER
```

### `DotclHost.Call` and `DotclHost.EvalString` return the primary value

This one also compiles unchanged and behaves differently. When the Lisp
function (or the last form) returned several values, 0.1.30 handed back an
internal wrapper object for them; 0.1.31 returns the first value itself, as a
single-value position in Lisp receives it. When it returned no values at all,
the result is `NIL`.

```csharp
var q = DotclHost.Call("FLOOR", 7, 2);   // 0.1.31: the Fixnum 3
bool isFixnum = q is Fixnum;             // 0.1.30: false (a wrapper), 0.1.31: true
```

Code that passed the result through `DotclHost.ToClr` already saw the first
value and needs no change. To get every value, use `DotclHost.CallMv` and
`DotclHost.EvalStringMv`, which return them as an array.
