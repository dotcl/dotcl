namespace DotCL.Emitter;

/// <summary>
/// The dispatch half of <see cref="DynamicClassBuilder"/>: the table of Lisp
/// bodies a generated type's methods call into, and the two entry points the
/// emitted IL calls. Kept apart from the emitting half because a saved facade
/// assembly references these members, and such a facade can be loaded by a
/// runtime that cannot emit. Everything here must therefore build without
/// Reflection.Emit.
/// </summary>
public static partial class DynamicClassBuilder
{
    // Global dispatch table: Lisp lambda bodies keyed by (typeFullName, dispatchKey).
    // dispatchKey = methodName for no-param methods; methodName + "#" + "|"-joined
    // FullNames for parameterized methods. Populated at DefineClass time (or by
    // RegisterHandlers for a type that was emitted elsewhere) and consulted by
    // DispatchLispMethod on every invocation. Keeping the lambda alive keeps its
    // lexical closure alive. Concurrent: the generated methods are called from
    // whatever threads .NET code runs on (a web server's request threads) while
    // another thread may define or register a type, and a plain Dictionary read
    // during a resize missed keys that were there.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<(string, string), LispObject>
        _methodHandlers = new();

    // Reserved method-table key for the ctor body dispatch. Chosen so it can
    // never collide with a user-defined method (.ctor isn't a valid CLR method
    // name that MethodBuilder would accept for DefineMethod).
    internal const string CtorKey = ".ctor";

    // Build the runtime dispatch key for a method / ctor.
    // No-param methods use just the name so existing single-overload code is unaffected.
    internal static string MethodDispatchKey(string methodName, IReadOnlyList<Type> paramTypes)
        => paramTypes.Count == 0
           ? methodName
           : methodName + "#" + string.Join("|", paramTypes.Select(t => t.FullName!));

    /// <summary>
    /// Install Lisp bodies for a type without defining the type. The type is one
    /// whose IL was emitted in another process (a saved facade assembly): its
    /// method bodies already call <see cref="DispatchLispMethod"/> with these
    /// keys, and the keys carry no assembly identity, so registering under the
    /// same full name is all it takes for the loaded facade to reach Lisp.
    /// A later registration for the same key replaces the earlier one, as a
    /// re-definition does.
    /// </summary>
    public static void RegisterHandlers(string typeFullName,
        IEnumerable<(string Key, LispObject Body)> handlers)
    {
        foreach (var (key, body) in handlers)
            _methodHandlers[(typeFullName, key)] = body;
    }

    /// <summary>
    /// Runtime entry point called by the emitted method body. Looks up the
    /// Lisp lambda registered for (typeFullName, methodName), marshals self
    /// and args through DotNetToLisp, funcalls the lambda, and marshals the
    /// result back through LispToDotNet for the declared <paramref name="returnType"/>.
    /// </summary>
    public static object? DispatchLispMethod(
        string typeFullName, string methodName, Type returnType,
        object? self, object?[] args)
    {
        if (!_methodHandlers.TryGetValue((typeFullName, methodName), out var lispFn))
            throw new InvalidOperationException(
                $"DispatchLispMethod: no Lisp handler registered for {typeFullName}.{methodName}");

        var lispArgs = new LispObject[args.Length + 1];
        lispArgs[0] = Runtime.DotNetToLisp(self);
        for (int i = 0; i < args.Length; i++)
            lispArgs[i + 1] = Runtime.DotNetToLisp(args[i]);

        // Cross the C#->Lisp boundary through InvokeForeignCallback so a Lisp error
        // in the override body is handled (dotcl:*foreign-callback-handler*) rather
        // than escaping as TargetInvocationException and crashing the .NET caller.
        var result = Runtime.InvokeForeignCallback(lispFn, lispArgs);

        if (returnType == typeof(void)) return null;
        return Runtime.LispToDotNet(result, returnType);
    }

    /// <summary>
    /// Runtime entry point for an emitted <c>public static</c> method body.
    /// Like <see cref="DispatchLispMethod"/> but with no <c>self</c>: looks up
    /// the Lisp function registered for (typeFullName, methodName), marshals the
    /// args, funcalls it through InvokeForeignCallback (so a Lisp error is
    /// handled rather than crashing the .NET caller), and marshals the result
    /// back for the declared <paramref name="returnType"/>.
    /// </summary>
    public static object? DispatchLispStatic(
        string typeFullName, string methodName, Type returnType, object?[] args)
    {
        if (!_methodHandlers.TryGetValue((typeFullName, methodName), out var lispFn))
            throw new InvalidOperationException(
                $"DispatchLispStatic: no Lisp handler registered for {typeFullName}.{methodName}");

        var lispArgs = new LispObject[args.Length];
        for (int i = 0; i < args.Length; i++)
            lispArgs[i] = Runtime.DotNetToLisp(args[i]);

        var result = Runtime.InvokeForeignCallback(lispFn, lispArgs);

        if (returnType == typeof(void)) return null;
        return Runtime.LispToDotNet(result, returnType);
    }
}
