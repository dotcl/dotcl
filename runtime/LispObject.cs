namespace DotCL;

// DebuggerDisplay makes every Lisp value read as its printed form in the VS
// Locals/Watch/hover windows (e.g. (1 2 3), 42, "foo") instead of the boxed
// .NET type. ToString() on each subclass yields that form and is side-effect
// free and depth-guarded (see Cons). The debugger applies a base type's
// DebuggerDisplay to derived instances lacking their own.
[System.Diagnostics.DebuggerDisplay("{ToString(),nq}")]
public abstract class LispObject
{
    public abstract override string ToString();
}

// Debug-only representation of a boxed lexical variable (one that is both
// mutated and captured by a closure). In normal builds such a variable lives in
// a LispObject[1] heap cell; under DOTCL_EMIT_PDB the compiler emits this named
// class instead, so the VS Locals window shows the variable's value directly
// (via DebuggerDisplay) rather than a one-element array. It is purely a
// display/representation choice and is never mixed with LispObject[] boxes
// within a single (debug) compilation. Not a LispObject — it holds one.
[System.Diagnostics.DebuggerDisplay("{Value}")]
public sealed class LispBox
{
    public LispObject Value;
    public LispBox(LispObject value) { Value = value; }
}

public class Cons : LispObject
{
    public LispObject Car { get; set; }
    public LispObject Cdr { get; set; }

    public Cons(LispObject car, LispObject cdr)
    {
        Car = car;
        Cdr = cdr;
        DotCL.Diagnostics.AllocCounter.Inc("Cons");
    }

    [ThreadStatic] private static int _printDepth;
    private const int MaxPrintDepth = 256;

    public override string ToString()
    {
        if (_printDepth >= MaxPrintDepth) return "(...)";
        _printDepth++;
        try
        {
            var parts = new List<string>();
            LispObject current = this;
            var visited = new HashSet<Cons>(ReferenceEqualityComparer.Instance);
            while (current is Cons c)
            {
                if (!visited.Add(c)) { parts.Add("..."); break; }
                parts.Add(c.Car.ToString());
                current = c.Cdr;
            }
            if (current is Nil || (current is Cons))
                return $"({string.Join(" ", parts)})";
            else
                return $"({string.Join(" ", parts)} . {current})";
        }
        finally { _printDepth--; }
    }
}

public class Nil : LispObject
{
    public static readonly Nil Instance = new Nil();
    private Nil() { }
    public override string ToString() => "NIL";
}

public class T : LispObject
{
    public static readonly T Instance = new T();
    private T() { }
    public override string ToString() => "T";
}
