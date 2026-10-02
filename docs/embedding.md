# Embedding dotcl

How a .NET program hosts Lisp: start the runtime, run Lisp code, pass values
both ways, and handle what comes back. Everything here is on the static class
`DotCL.DotclHost` in the `DotCL.Runtime` package.

```sh
dotnet add package DotCL.Runtime
```

## Starting the runtime

```csharp
using DotCL;

DotclHost.EnsureCore();
```

`EnsureCore` boots the runtime and loads the core image that the package copies
next to your application. It is idempotent: a library or plugin that needs Lisp
can call it without knowing whether the host already did. There is one Lisp
image per process; see [Hosting dotcl safely](hosting-safely.md).

`EnsureCore` finds the core with `FindCore()`, which looks next to the entry
assembly and under `share/dotcl/` (on Android, in the APK's assets). When the
core lives somewhere else, boot explicitly:

```csharp
DotclHost.Initialize();
DotclHost.LoadCore(pathToCore);   // or LoadCore(byte[]) for a core held in memory
```

Load the core once per process. Loading it a second time redefines the
standard functions and fails ("package COMMON-LISP is locked", wrapped in a
`TargetInvocationException`); that is what `EnsureCore` guards against.

## Running Lisp code

- `EvalString(source)` reads and evaluates every form in the string and returns
  the value of the last one.
- `LoadLispFile(path)` loads a `.lisp` file or a compiled `.fasl`, as `load` does.
- `Call(name, args...)` calls a Lisp function and returns its primary value.
  `ToClr` / `ToClr<T>` turn that into a .NET value.
- `EvalStringMv` and `CallMv` return every value as an array, for functions
  such as `gethash` whose second value matters.

```csharp
DotclHost.EvalString("(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))");
long f = DotclHost.ToClr<long>(DotclHost.Call("fact", 20));
```

### Names are read as Lisp reads them

The name given to `Call`, `GetSpecial`, `SetSpecial` and `Register` is read
the way the Lisp reader reads a symbol in source. `"fact"` and `"FACT"` both
name the function `(defun fact ...)` defined; a name that really is lower case
is written `"|fact|"`, as in Lisp. A qualified name works as in source too:
`"mylib:entry"` for an exported symbol, `"mylib::helper"` for an internal one.
An unqualified name is looked up in `DotclHost.CurrentPackage`, which is
`CL-USER` unless you change it. When a name is not found, the error says which
package does have it.

`Register` always defines the function in `CL-USER`, whatever
`CurrentPackage` is. Code in a package that does not use `CL-USER` calls it as
`cl-user::greet`, or imports the symbol.

## Passing values

- Arguments to `Call` are converted to Lisp: strings, numbers, `bool` and
  `null` become their Lisp counterparts; any other .NET object is passed as
  itself, and Lisp code can call its methods.
- `ToClr<T>(value)` converts a result to `T`, with the same rules the runtime
  uses for .NET method arguments. `ToClr(value)` picks the natural type.
- A .NET array or list passed to `Call` stays a .NET object. Use
  `ToLispList` / `ToLispVector` when the Lisp function expects a sequence,
  and `ToClrList<T>` / `ToClrArray<T>` on the way back.
- `GetSpecial` / `SetSpecial` read and set a special variable by name.

[Numbers across the boundary](numbers.md) lists which .NET numeric type
arrives as which Lisp type.

## Calling back into C#

```csharp
DotclHost.Register("greet", args => $"Hello, {args[0]}!");
DotclHost.EvalString("(greet \"Lisp\")");
```

`Register` makes a .NET delegate callable as a Lisp function. Arguments arrive
converted as `ToClr` converts them; the return value is converted back. It
generates no code, so it works where run-time code generation is off.

Lisp code can also call any .NET API directly, without registering anything:
see [Calling .NET from Lisp](dotnet-package.md).

## Errors

An error that no Lisp handler takes goes to the Lisp debugger, as it would at
the REPL. In a host that is rarely what you want: with a console attached the
debugger waits for input, and without one it prints a banner and throws a
`DebuggerDeclinedException`. Call `SetThrowingDebuggerHook()` once after
booting, and an unhandled condition comes back to the caller as a
`DotclConditionException` instead:

```csharp
DotclHost.EnsureCore();
DotclHost.SetThrowingDebuggerHook();

try
{
    DotclHost.EvalString("(error \"bad input: ~a\" 42)");
}
catch (DotclConditionException e) when (e.ClrException is System.IO.IOException io)
{
    // The Lisp code called a .NET method that threw; the original exception is here.
}
catch (DotclConditionException e)
{
    Console.WriteLine(e.Message);   // "bad input: 42", the condition's report
}
```

- `Message` is the condition's report, as the REPL would print it.
- `Condition` is the condition object itself. Hand it back to Lisp to read its
  slots or to invoke one of its restarts.
- `ClrException` is the .NET exception, when the condition came from a .NET
  method that threw under `dotnet:invoke` or `dotnet:static`.

With the hook installed, errors the runtime raises itself (a type error from
`car`, a .NET method that threw) arrive as `DotclConditionException` too. In
0.1.30 they arrived as `LispErrorException`; catch both if you support that
release.

An exception thrown by a function you `Register`ed is not wrapped: it reaches
your `catch` as itself, unchanged. Lisp code between the two can still catch it
with `handler-case`, as an `error`.

A name that `Call` cannot find throws `InvalidOperationException`, whatever the
hook.

## Output

`SetStandardOutput(writer)` and `SetErrorOutput(writer)` send what Lisp writes
to `*standard-output*` and `*error-output*` to a `TextWriter` of yours: a log,
a text box, a test buffer. `null` restores the process's own streams.

## Without run-time code generation

NativeAOT, Unity IL2CPP and WebGL builds cannot generate code at run time.
dotcl runs there in two parts:

- **Your Lisp, compiled ahead of time.** At build time the compiler turns your
  Lisp and the core into `.fasl` files, which are ordinary .NET assemblies. The
  compiler generates code, so this step runs on your build machine, not on the
  target.
- **The `netstandard2.0` build of `DotCL.Runtime`,** which has no code
  generator. It runs the precompiled code, and it evaluates anything new
  (`EvalString`, a `defun`, a `defmacro`, `defclass`) with an interpreter
  instead of compiling it.

Under NativeAOT a `.fasl` cannot be loaded from a file at run time, so the
build links the precompiled assemblies into the executable and the host starts
them by assembly name:

```csharp
DotclHost.Initialize();
DotclHost.RunLinkedModuleByName("dotclcore");   // the core
DotclHost.Register("host-log", args => { Console.WriteLine(args[0]); return null; });
DotclHost.RunLinkedModuleByName("appfasl");     // your Lisp
var fib = DotclHost.ToClr<int>(DotclHost.Call("fib", 20));
DotclHost.EvalString("(defun cube (x) (* x x x))");   // interpreted
```

`Initialize` comes first, as always. `Register` any host functions before
starting your Lisp if its top-level forms call them while it loads.

The [PrecompiledLispDemoAot](../samples/PrecompiledLispDemoAot/) sample is a
complete project: its csproj builds the `netstandard2.0` runtime and the two
Lisp assemblies, and `dotnet publish -p:PublishAot=true` produces a native
executable. [PrecompiledLispDemoWebGL](../samples/PrecompiledLispDemoWebGL/)
does the same for a Unity IL2CPP WebGL build.

To check on ordinary CoreCLR that your Lisp never needs the code generator,
load the precompiled files and set `DotclHost.PrecompiledOnly = true`. Running
compiled code and `Register` keep working; any attempt to generate code
(evaluating a compound form, `dotnet:define-class`, a native FFI thunk) throws
instead. [PrecompiledLispDemo](../samples/PrecompiledLispDemo/) does this:

```csharp
DotclHost.Initialize();
DotclHost.LoadCore("dotcl.core");
DotclHost.PrecompiledOnly = true;
DotclHost.LoadLispFile("app.fasl");
```

## Shipping Lisp with your application

For anything beyond a few `EvalString` calls, let the build compile your Lisp.
Point the project at an ASDF system and the build compiles it, with its
dependencies, to FASLs in the output directory, plus a manifest listing them:

```xml
<ItemGroup>
  <PackageReference Include="DotCL.Runtime" Version="0.1.30" />
</ItemGroup>
<PropertyGroup>
  <DotclProjectAsd>$(MSBuildProjectDirectory)/app.asd</DotclProjectAsd>
</PropertyGroup>
```

The build writes the FASLs and the manifest to `dotcl-fasl/` in the output
directory. At startup, load them and call your entry point:

```csharp
DotclHost.Initialize();
DotclHost.LoadFromManifest(Path.Combine(AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt"));
DotclHost.Call("APP:APP-MAIN");
```

`LoadFromManifest` loads the core and every FASL the manifest names, skipping
anything already loaded, so an application and the Lisp libraries it
references can each load their own manifest.
[SDK and MSBuild properties](sdk.md) lists the properties and the other two ways
to wire a project; [Getting started](getting-started.md) builds one from
`dotnet new dotcl-app`. To ship FASLs precompiled to native code as well, see
[Ahead-of-time compiled FASLs](readytorun.md).
