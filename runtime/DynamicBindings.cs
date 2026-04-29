namespace DotCL;

public static class DynamicBindings
{
    public static readonly LispObject Unbound = new UnboundSentinel();

    private const int InitialCapacity = 64;

    [ThreadStatic] private static Symbol?[]? _syms;
    [ThreadStatic] private static LispObject?[]? _vals;
    [ThreadStatic] private static int _top;

    private static void Grow()
    {
        var newSize = _syms == null ? InitialCapacity : _syms.Length * 2;
        var newSyms = new Symbol?[newSize];
        var newVals = new LispObject?[newSize];
        if (_syms != null)
        {
            Array.Copy(_syms, newSyms, _top);
            Array.Copy(_vals!, newVals, _top);
        }
        _syms = newSyms;
        _vals = newVals;
    }

    public static LispObject Get(Symbol sym)
    {
        var syms = _syms;
        if (syms != null)
        {
            var vals = _vals!;
            for (int i = _top - 1; i >= 0; i--)
            {
                if (ReferenceEquals(syms[i], sym))
                {
                    var val = vals[i];
                    if (val == null)
                        throw new LispErrorException(new LispUnboundVariable(sym));
                    return val;
                }
            }
        }
        if (sym.Value == null)
            throw new LispErrorException(new LispUnboundVariable(sym));
        return sym.Value;
    }

    public static bool TryGet(Symbol sym, out LispObject value)
    {
        var syms = _syms;
        if (syms != null)
        {
            var vals = _vals!;
            for (int i = _top - 1; i >= 0; i--)
            {
                if (ReferenceEquals(syms[i], sym))
                {
                    var val = vals[i];
                    if (val != null) { value = val; return true; }
                    value = null!; return false;
                }
            }
        }
        if (sym.Value != null) { value = sym.Value; return true; }
        value = null!;
        return false;
    }

    public static void Push(Symbol sym, LispObject value)
    {
        if (_syms == null || _top >= _syms.Length) Grow();
        _syms![_top] = sym;
        _vals![_top] = value is UnboundSentinel ? null : value;
        _top++;
    }

    public static void Pop(Symbol sym)
    {
        var syms = _syms;
        if (syms == null || _top == 0) return;
        var vals = _vals!;
        for (int i = _top - 1; i >= 0; i--)
        {
            if (ReferenceEquals(syms[i], sym))
            {
                // Swap with top entry so we can decrement _top without a shift
                int last = _top - 1;
                if (i < last)
                {
                    syms[i] = syms[last];
                    vals[i] = vals[last];
                }
                _top--;
                // Clear freed slot to prevent GC rooting
                syms[_top] = null;
                vals[_top] = null;
                return;
            }
        }
        // Not found: defensive no-op (double-pop)
    }

    public static LispObject Set(Symbol sym, LispObject value)
    {
        var syms = _syms;
        if (syms != null)
        {
            var vals = _vals!;
            for (int i = _top - 1; i >= 0; i--)
            {
                if (ReferenceEquals(syms[i], sym))
                {
                    vals[i] = value;
                    return value;
                }
            }
        }
        sym.Value = value;
        return value;
    }

    public static LispObject SetIfUnbound(Symbol sym, LispObject value)
    {
        if (TryGet(sym, out var existing)) return existing;
        sym.Value = value;
        return value;
    }

    public static LispObject Makunbound(Symbol sym)
    {
        var syms = _syms;
        if (syms != null)
        {
            var vals = _vals!;
            for (int i = _top - 1; i >= 0; i--)
            {
                if (ReferenceEquals(syms[i], sym))
                {
                    vals[i] = null; // null = unbound in flat stack
                    return sym;
                }
            }
        }
        sym.Value = null;
        return sym;
    }

    public static LispObject ProgvBind(LispObject symsObj, LispObject valsObj)
    {
        var syms = Runtime.ToList(symsObj);
        var vals = Runtime.ToList(valsObj);
        for (int i = 0; i < syms.Count; i++)
        {
            if (syms[i] is not Symbol sym)
                throw new LispErrorException(new LispError($"PROGV: not a symbol: {syms[i]}"));
            var val = i < vals.Count ? vals[i] : Unbound;
            Push(sym, val);
        }
        return symsObj;
    }

    /// <summary>
    /// Snapshot the current thread's dynamic bindings for child-thread inheritance.
    /// Scans from top; first occurrence of each symbol is the effective binding.
    /// </summary>
    public static Dictionary<Symbol, LispObject>? Snapshot()
    {
        if (_top == 0) return null;
        var snap = new Dictionary<Symbol, LispObject>();
        var syms = _syms!;
        var vals = _vals!;
        for (int i = _top - 1; i >= 0; i--)
        {
            var sym = syms[i];
            if (sym != null && !snap.ContainsKey(sym))
            {
                var val = vals[i];
                if (val != null) snap[sym] = val;
            }
        }
        return snap.Count > 0 ? snap : null;
    }

    public static void Restore(Dictionary<Symbol, LispObject>? snapshot)
    {
        if (snapshot == null) return;
        foreach (var (sym, val) in snapshot)
            Push(sym, val);
    }

    public static LispObject ProgvUnbind(LispObject symsObj)
    {
        var syms = Runtime.ToList(symsObj);
        for (int i = syms.Count - 1; i >= 0; i--)
        {
            if (syms[i] is Symbol sym)
                Pop(sym);
        }
        return Nil.Instance;
    }
}

// Sentinel class for unbound dynamic bindings
public class UnboundSentinel : LispObject
{
    public override string ToString() => "#<UNBOUND>";
}
