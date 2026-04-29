namespace DotCL;

/// <summary>
/// Wrapper that carries multiple return values through the stack.
/// Created by (values ...) with 0 or 2+ args; consumed by multiple-value-list etc.
/// At non-MV consumption points, the compiler inserts UnwrapMv to extract the primary value.
/// </summary>
public class MvReturn : LispObject
{
    public readonly LispObject[] Values;
    public MvReturn(LispObject[] values)
    {
        Values = values;
        DotCL.Diagnostics.AllocCounter.Inc("MvReturn");
    }
    public override string ToString() =>
        Values.Length > 0 ? Values[0].ToString() : "NIL";
}

public static class MultipleValues
{
    [ThreadStatic]
    private static LispObject[]? _values;
    [ThreadStatic]
    private static int _count;
    [ThreadStatic]
    private static LispObject[]? _primaryCache;

    public static void Set(params LispObject[] vals)
    {
        _values = vals;
        _count = vals.Length;
    }

    public static LispObject[] Get()
    {
        if (_values == null || _count <= 0)
            return Array.Empty<LispObject>();
        var result = new LispObject[_count];
        Array.Copy(_values, result, _count);
        return result;
    }

    public static int Count => _count;

    public static LispObject Primary(LispObject value)
    {
        if (value is MvReturn mv)
            value = mv.Values.Length > 0 ? mv.Values[0] : Nil.Instance;
        var cache = _primaryCache ??= new LispObject[1];
        cache[0] = value;
        _values = cache;
        _count = 1;
        return value;
    }

    public static LispObject Values(params LispObject[] vals)
    {
        Set(vals);
        if (vals.Length == 1)
            return vals[0]; // Single value: no wrapper
        // 0 or 2+ values: return MvReturn for stack-based propagation
        return new MvReturn(vals);
    }

    public static void Reset()
    {
        _count = -1; // Sentinel: no explicit values call yet
        // Don't null _values — Get() checks _count first, saves a ThreadStatic write
    }

    // Save/restore for unwind-protect: preserve body's secondary values across cleanup
    public static int SaveCount() => _count;
    public static LispObject[]? SaveValues() => _values;
    public static void RestoreSaved(int savedCount, LispObject[]? savedValues)
    {
        _count = savedCount;
        _values = savedValues;
    }
}
