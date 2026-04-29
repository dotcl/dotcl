using System.Linq.Expressions;
using System.Runtime.CompilerServices;

namespace DotCL;

internal static class DotNetEvents
{
    // Lisp handler → (delegate type → cached Delegate). Weak-keyed on the
    // LispObject so a handler's delegate pool is GC'd when the handler
    // itself becomes unreachable. Lets AddEvent and RemoveEvent share the
    // same Delegate instance for a given (handler, delegateType) pair so
    // bare Lisp lambdas passed to remove-event correctly detach the handler
    // that add-event installed (#160).
    private static readonly ConditionalWeakTable<LispObject, Dictionary<Type, Delegate>>
        _delegateCache = new();

    // (dotnet:add-event obj "Click" (lambda (sender e) ...))
    public static LispObject AddEvent(LispObject[] args)
    {
        if (args.Length != 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:ADD-EVENT: expected 3 arguments (object event-name handler)"));

        var target = args[0] is LispDotNetObject dno ? dno.Value
            : throw new LispErrorException(new LispProgramError(
                "DOTNET:ADD-EVENT: first argument must be a .NET object"));

        string eventName = args[1] is LispString ls ? ls.Value : args[1].ToString()!;
        LispObject handler = args[2];

        var ev = target.GetType().GetEvent(eventName)
            ?? throw new LispErrorException(new LispProgramError(
                $"DOTNET:ADD-EVENT: no event '{eventName}' on {target.GetType().Name}"));

        var del = MakeDelegate(ev.EventHandlerType!, handler);
        ev.GetAddMethod()!.Invoke(target, new[] { del });
        return Nil.Instance;
    }

    // (dotnet:remove-event obj "Click" handler)
    // handler may be either the bare Lisp closure originally passed to
    // add-event (resolved via the cache populated by MakeDelegate) or a
    // LispDotNetObject wrapping a Delegate (legacy path).
    public static LispObject RemoveEvent(LispObject[] args)
    {
        if (args.Length != 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:REMOVE-EVENT: expected 3 arguments (object event-name handler)"));

        var target = args[0] is LispDotNetObject dno ? dno.Value
            : throw new LispErrorException(new LispProgramError(
                "DOTNET:REMOVE-EVENT: first argument must be a .NET object"));

        string eventName = args[1] is LispString ls ? ls.Value : args[1].ToString()!;
        LispObject handler = args[2];

        var ev = target.GetType().GetEvent(eventName)
            ?? throw new LispErrorException(new LispProgramError(
                $"DOTNET:REMOVE-EVENT: no event '{eventName}' on {target.GetType().Name}"));

        Delegate? del = null;
        if (handler is LispDotNetObject wrap && wrap.Value is Delegate d)
        {
            del = d;
        }
        else if (_delegateCache.TryGetValue(handler, out var byType)
                 && byType.TryGetValue(ev.EventHandlerType!, out var cached))
        {
            del = cached;
        }

        if (del != null)
            ev.GetRemoveMethod()!.Invoke(target, new[] { del });

        return Nil.Instance;
    }

    // Called from generated delegates on the event's thread
    public static void DispatchEvent(LispObject fn, object?[] rawArgs)
    {
        try
        {
            var lispArgs = rawArgs.Select(Runtime.DotNetToLisp).ToArray();
            Runtime.Funcall(fn, lispArgs);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"DOTNET:ADD-EVENT handler error: {ex.Message}");
        }
    }

    // Build a delegate of eventHandlerType that calls the Lisp function.
    // Returns the same Delegate on repeat calls with the same (fn,
    // delegateType) pair so remove-event can find what add-event installed.
    internal static Delegate MakeDelegate(Type delegateType, LispObject fn)
    {
        var byType = _delegateCache.GetValue(fn, _ => new Dictionary<Type, Delegate>());
        lock (byType)
        {
            if (byType.TryGetValue(delegateType, out var cached))
                return cached;

            var invokeMethod = delegateType.GetMethod("Invoke")!;
            var paramTypes = invokeMethod.GetParameters()
                .Select(p => p.ParameterType).ToArray();

            var parameters = paramTypes
                .Select((t, i) => Expression.Parameter(t, $"a{i}"))
                .ToArray();

            // Build: object?[] args = new object?[] { a0, a1, ... }
            var argsArray = Expression.NewArrayInit(typeof(object),
                parameters.Select(p => (Expression)Expression.Convert(p, typeof(object))));

            // Build: DispatchEvent(fn, args)
            var fnConst = Expression.Constant(fn, typeof(LispObject));
            var dispatch = Expression.Call(
                typeof(DotNetEvents).GetMethod(nameof(DispatchEvent))!,
                fnConst, argsArray);

            var lambda = Expression.Lambda(delegateType, dispatch, parameters);
            var del = lambda.Compile();
            byType[delegateType] = del;
            return del;
        }
    }
}
