namespace DotCL;

public abstract class LispObject
{
    public abstract override string ToString();
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
