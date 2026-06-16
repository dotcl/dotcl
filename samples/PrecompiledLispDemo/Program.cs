using DotCL;

// PrecompiledLispDemo — a plain .NET console host that embeds dotcl, loads a
// PRECOMPILED Lisp image (app.fasl) and runs it with NO runtime code generation,
// calling between C# and Lisp in both directions. After the precompiled image is
// loaded we turn on DotclHost.PrecompiledOnly, which makes any attempt to JIT
// (eval/compile, define-class, FFI) throw — the same constraint an AOT/IL2CPP
// target imposes. This proves precompiled dotcl runs emit-less.

string dir = AppContext.BaseDirectory;

DotclHost.Initialize();
// Boot the FASL core (compiler+stdlib). A FASL loads via ModuleInit — already
// compiled IL — so booting does not generate any code.
DotclHost.LoadCore(Path.Combine(dir, "dotcl.core"));

// From boot onward, assert the runtime never generates code. Registering host
// functions and loading the precompiled app below all happen under this flag.
DotclHost.PrecompiledOnly = true;
Console.WriteLine("PrecompiledOnly = true (runtime code generation is now forbidden)\n");

// Lisp -> C#: expose a host function the Lisp code calls back into.
DotclHost.Register("host-log", args =>
{
    Console.WriteLine($"    [C# host-log] {args[0]}");
    return null; // NIL
});

// Load the precompiled application image (ModuleInit — no code generation).
DotclHost.LoadLispFile(Path.Combine(dir, "app.fasl"));

// C# -> Lisp: call the precompiled functions.
Console.WriteLine($"fib(20)          = {DotclHost.ToClr<long>(DotclHost.Call("FIB", 20))}");
Console.WriteLine($"sum-squares(100) = {DotclHost.ToClr<long>(DotclHost.Call("SUM-SQUARES", 100))}");

// greet runs precompiled Lisp that calls back into C# (host-log).
long len = DotclHost.ToClr<long>(DotclHost.Call("GREET", "dotcl"));
Console.WriteLine($"greet returned name length = {len}\n");

// Demonstrate the contract: eval (which would generate code) is now blocked.
try
{
    DotclHost.EvalString("(+ 1 2)");
    Console.WriteLine("eval succeeded — UNEXPECTED under PrecompiledOnly");
}
catch (Exception e)
{
    Console.WriteLine($"eval blocked as expected: {e.Message.Split('\n')[0]}");
}

Console.WriteLine("\nDone — ran precompiled Lisp with no runtime code generation.");
