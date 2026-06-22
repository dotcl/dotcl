using DotCL;

// PrecompiledLispDemoAot — the strongest shippable-runtime proof: precompiled Lisp + a dynamic
// emit-free evaluator running inside a NativeAOT native binary, where
// Reflection.Emit is not merely unused but PHYSICALLY ABSENT (IsDynamicCodeSupported
// is False and Assembly.LoadFrom throws PlatformNotSupportedException).
//
// Linking the emit-free netstandard2.0 runtime on plain CoreCLR would only prove
// the runtime doesn't *use* emit — Reflection.Emit could still exist there. Here
// the whole thing is AOT-compiled, so it proves the runtime survives a platform
// that forbids codegen outright. The two Lisp images (the runtime core and this app)
// are referenced as fixed-name assemblies (dotclcore / appfasl), baked into the
// native image by the AOT compiler, and kept whole via <TrimmerRootAssembly>. We
// boot them by stable name through DotclHost.RunLinkedModuleByName — which uses
// Assembly.Load on the already-linked assembly, never Assembly.LoadFrom.

DotclHost.Initialize();
Console.WriteLine(
    $"IsDynamicCodeSupported = {System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported}");

// Boot the FASL core (compiler+stdlib) and the app image — both build-time-linked,
// no Assembly.LoadFrom, no code generation. The names match :module-name in
// precompile.lisp and the <Reference>/<TrimmerRootAssembly> Include in the csproj.
DotclHost.RunLinkedModuleByName("dotclcore");
Console.WriteLine("Core booted (build-time linked, no Assembly.LoadFrom).");

// Lisp -> C#: expose a host function the Lisp code calls back into.
DotclHost.Register("host-log", args =>
{
    Console.WriteLine($"    [C# host-log] {args[0]}");
    return null; // NIL
});

DotclHost.RunLinkedModuleByName("appfasl");
Console.WriteLine("App fasl linked.\n");

// C# -> Lisp: call the precompiled functions.
Console.WriteLine($"fib(20)          = {DotclHost.ToClr<long>(DotclHost.Call("FIB", 20))}");
Console.WriteLine($"sum-squares(100) = {DotclHost.ToClr<long>(DotclHost.Call("SUM-SQUARES", 100))}");
// greet runs precompiled Lisp that calls back into C# (host-log).
Console.WriteLine($"greet len        = {DotclHost.ToClr<long>(DotclHost.Call("GREET", "dotcl"))}\n");

// Runtime eval WITHOUT an emitter: compound forms route through the tree-walk
// interpreter (%mini-eval), itself precompiled in the core. This is the hybrid an
// AOT/IL2CPP target wants — precompiled hot paths plus a dynamic evaluator that
// never generates code.
Console.WriteLine("Emit-free eval (IsDynamicCodeSupported=False):");
Console.WriteLine($"  (+ 1 2)                       = {DotclHost.ToClr<long>(DotclHost.EvalString("(+ 1 2)"))}");
Console.WriteLine($"  (loop for i to 4 collect i)   = {DotclHost.ToClr<object>(DotclHost.EvalString("(loop for i to 4 collect i)"))}");
// eval can call the precompiled application functions too:
Console.WriteLine($"  (fib 10)  [calls compiled fn] = {DotclHost.ToClr<long>(DotclHost.EvalString("(fib 10)"))}");

// Define a NEW function, macro, and class at run time — interpreted, never
// compiled — then use them:
DotclHost.EvalString("(defun cube (n) (* n n n))");
Console.WriteLine($"  defun cube; (cube 5)          = {DotclHost.ToClr<long>(DotclHost.Call("CUBE", 5))}");
DotclHost.EvalString("(defmacro twice (x) `(* 2 ,x))");
Console.WriteLine($"  defmacro twice; (twice ..)    = {DotclHost.ToClr<long>(DotclHost.EvalString("(twice (cube 3))"))}");
DotclHost.EvalString("(defclass pt () ((x :initarg :x :accessor pt-x)))");
DotclHost.EvalString("(defmethod sq ((p pt)) (* (pt-x p) (pt-x p)))");
Console.WriteLine($"  defclass/defmethod; (sq ..)   = {DotclHost.ToClr<long>(DotclHost.EvalString("(sq (make-instance 'pt :x 7))"))}");

Console.WriteLine("\nDone — precompiled Lisp + emit-free eval (defun/defmacro/CLOS) in a NativeAOT binary, no Reflection.Emit.");
