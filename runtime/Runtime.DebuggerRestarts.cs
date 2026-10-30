namespace DotCL;

public static partial class Runtime
{
    private static readonly System.Func<LispObject[], LispObject> s_inactiveRestart =
        _ => Nil.Instance;

    /// <summary>Read one form from *QUERY-IO* and evaluate it.</summary>
    private static readonly LispFunction s_restartValuePrompt = new LispFunction(
        _ =>
        {
            var stream = DynamicBindings.TryGet(Startup.Sym("*QUERY-IO*"), out var io)
                ? io
                : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
            var form = ReadFromStream(stream, T.Instance, Nil.Instance);
            return List(Eval(form));
        }, "%RESTART-VALUE", -1);

    private static readonly LispFunction s_returnNothing = new LispFunction(
        _ => MultipleValues.Values0(), "RETURN-NOTHING", -1);

    private static bool IsRestartTag(object tag, LispRestart[] restarts)
    {
        foreach (var restart in restarts)
            if (ReferenceEquals(restart.Tag, tag))
                return true;
        return false;
    }

    private static LispObject SignalWithRestarts(
        LispCondition condition, LispRestart[] restarts,
        System.Func<RestartInvocationException, LispObject> dispatch)
    {
        RestartClusterStack.PushCluster(restarts);
        try
        {
            throw ConditionSystem.ErrorExit(condition);
        }
        catch (RestartInvocationException rie) when (IsRestartTag(rie.Tag, restarts))
        {
            return dispatch(rie);
        }
        finally
        {
            RestartClusterStack.PopCluster();
        }
    }

    internal static LispObject UnboundVariableError(Symbol sym, bool global = false)
    {
        var use = new LispRestart("USE-VALUE", s_inactiveRestart,
            "Use specified value.") { InteractiveFunction = s_restartValuePrompt };
        var store = new LispRestart("STORE-VALUE", s_inactiveRestart,
            "Set specified value and use it.") { InteractiveFunction = s_restartValuePrompt };
        var restarts = new[] { use, store };
        return SignalWithRestarts(new LispUnboundVariable(sym), restarts, rie =>
        {
            var value = rie.Arguments.Length > 0 ? rie.Arguments[0] : Nil.Instance;
            if (ReferenceEquals(rie.Tag, store.Tag))
            {
                if (global) sym.Value = value;
                else DynamicBindings.Set(sym, value);
            }
            return value;
        });
    }

    internal static LispFunction UndefinedFunctionError(LispObject name)
    {
        var use = new LispRestart("USE-VALUE", s_inactiveRestart,
            "Call specified function.") { InteractiveFunction = s_restartValuePrompt };
        var value = new LispRestart("RETURN-VALUE", s_inactiveRestart,
            "Return specified values.") { InteractiveFunction = s_restartValuePrompt };
        var nothing = new LispRestart("RETURN-NOTHING", s_inactiveRestart,
            "Return zero values.");
        var retry = new LispRestart("CONTINUE", s_inactiveRestart,
            $"Retry calling {name}.");
        var restarts = new[] { retry, use, value, nothing };
        return (LispFunction)SignalWithRestarts(new LispUndefinedFunction(name), restarts, rie =>
        {
            if (ReferenceEquals(rie.Tag, use.Tag))
            {
                var designator = rie.Arguments.Length > 0 ? rie.Arguments[0] : Nil.Instance;
                return CoerceToFunction(designator);
            }
            if (ReferenceEquals(rie.Tag, value.Tag))
                return ReturnValuesFunction(rie.Arguments);
            if (ReferenceEquals(rie.Tag, retry.Tag))
                return RetryCallFunction(name);
            return s_returnNothing;
        });
    }

    private static LispFunction RetryCallFunction(LispObject name) =>
        new LispFunction(args => ((LispFunction)Fdefinition(name)).Invoke(args),
            "CONTINUE", -1);

    private static LispFunction ReturnValuesFunction(LispObject[] values) =>
        new LispFunction(_ =>
        {
            MultipleValues.Set(values);
            return values.Length > 0 ? values[0] : MultipleValues.Values0();
        }, "RETURN-VALUE", -1);
}
