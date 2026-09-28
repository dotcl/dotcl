namespace DotCL;

/// <summary>
/// The history variables a read-eval-print loop maintains: CLHS 25.1.1's
/// <c>*</c> <c>**</c> <c>***</c> for values, <c>+</c> <c>++</c> <c>+++</c> for
/// forms, <c>/</c> <c>//</c> <c>///</c> for the list of all values, and
/// <c>-</c> for the form being evaluated now.
///
/// Here rather than inside the loop because there are two loops: the top level
/// one and the nested one the debugger runs. Both update the same variables,
/// as they do in SBCL, so that a value computed at a debugger prompt can be
/// picked up with <c>*</c> afterwards -- the reason for reaching for it there
/// is the same reason as at the top level.
///
/// The variables are assigned, not bound. A binding would give the loop a
/// dynamic extent that the debugger's loop, and any form the reader types,
/// would have to nest inside; and the point of these variables is that their
/// value outlives the evaluation that set it.
/// </summary>
public static class ReplHistory
{
    /// <summary>
    /// Note the form that is about to be evaluated, and clear the
    /// multiple-value channel so that <see cref="AfterEval"/> reads the values
    /// of this evaluation rather than whatever ran before it.
    /// </summary>
    public static void BeforeEval(LispObject form)
    {
        Set("-", form);
        MultipleValues.Reset();
    }

    /// <summary>
    /// Advance the history with the values of the evaluation that has just
    /// finished, given its primary result.
    ///
    /// Call this before printing that result. Printing can run Lisp -- a
    /// PRINT-OBJECT method -- and running Lisp publishes over the
    /// multiple-value channel this reads.
    ///
    /// An evaluation that exits non-locally never reaches here, which is what
    /// leaves the history untouched after an error: the caller's handler skips
    /// it. <c>-</c> keeps the form that failed until the next one replaces it.
    /// </summary>
    public static void AfterEval(LispObject primary)
        => Record(List(MultipleValues.Of(primary)));

    /// <summary>
    /// Advance the history, given the values as a list. <c>/</c> becomes that
    /// list, <c>*</c> its first element -- NIL when there are none, as after
    /// (VALUES) -- and <c>+</c> takes whatever <c>-</c> was set to before the
    /// evaluation.
    /// </summary>
    public static void Record(LispObject values)
    {
        // An empty list of values reaches a caller outside Lisp as a null,
        // since that is what NIL marshals to across the .NET boundary. Storing
        // one in a variable would leave it holding something that is not a
        // Lisp object at all, and the next read of / would call it unbound.
        values ??= Nil.Instance;
        Rotate("***", "**", "*", values is Cons c ? c.Car : Nil.Instance);
        Rotate("///", "//", "/", values);
        Rotate("+++", "++", "+", Get("-"));
    }

    /// <summary>Shift the three variables along and put VALUE in the newest.</summary>
    private static void Rotate(string oldest, string middle, string newest, LispObject value)
    {
        Set(oldest, Get(middle));
        Set(middle, Get(newest));
        Set(newest, value);
    }

    /// <summary>
    /// The value, or NIL where there is none. These are proclaimed special
    /// with a value at startup, so the fallback is for an image that has taken
    /// one of them apart: a REPL that cannot print a result because its
    /// history variable is unbound would be a poor way to find that out.
    /// </summary>
    private static LispObject Get(string name)
        => DynamicBindings.TryGet(Startup.Sym(name), out var value) ? value : Nil.Instance;

    private static void Set(string name, LispObject value)
        => DynamicBindings.Set(Startup.Sym(name), value);

    private static LispObject List(LispObject[] values)
    {
        LispObject list = Nil.Instance;
        for (int i = values.Length - 1; i >= 0; i--) list = new Cons(values[i], list);
        return list;
    }
}
