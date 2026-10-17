namespace DotCL;

public static class DynamicBindings
{
    public static readonly LispObject Unbound = new UnboundSentinel();

    private const int InitialCapacity = 64;

    // One object per thread holds the whole binding stack, so an operation
    // reads one thread-static instead of three (on macOS each thread-static
    // read is a call into the TLS resolver, which showed in compile profiles).
    //
    // COUNTS[sym.BindIndex] is how many entries for that symbol this thread's
    // stack holds. A read of a symbol that has been bound somewhere but is not
    // bound on this thread right now -- most reads of the compiler's own
    // specials, which outer code bound once or not at all -- answers from the
    // global value without scanning. The stack is deep while compiling (about
    // 160 entries on average for a library load), and those reads scanned all
    // of it.
    private sealed class BindingStack
    {
        public Symbol?[] Syms = new Symbol?[InitialCapacity];
        public LispObject?[] Vals = new LispObject?[InitialCapacity];
        public int Top;
        public int[] Counts = new int[256];
    }

    [ThreadStatic] private static BindingStack? _stack;

    private static int _nextBindIndex;

    private static BindingStack Current => _stack ??= new BindingStack();

    private static int BindIndexOf(Symbol sym)
    {
        int idx = sym.BindIndex;
        if (idx != 0) return idx;
        int fresh = Interlocked.Increment(ref _nextBindIndex);
        int prior = Interlocked.CompareExchange(ref sym.BindIndex, fresh, 0);
        return prior == 0 ? fresh : prior;
    }

    /// <summary>The binding stack of this thread when SYM may be bound on it,
    /// else null (the symbol's global value is the one in effect).</summary>
    private static BindingStack? StackBinding(Symbol sym)
    {
        if (!sym.EverDynamicallyBound) return null;
        var st = _stack;
        if (st == null) return null;
        int idx = sym.BindIndex;
        var counts = st.Counts;
        if ((uint)idx >= (uint)counts.Length || counts[idx] == 0) return null;
        return st;
    }

    private static void Grow(BindingStack st)
    {
        var newSize = st.Syms.Length * 2;
        var newSyms = new Symbol?[newSize];
        var newVals = new LispObject?[newSize];
        Array.Copy(st.Syms, newSyms, st.Top);
        Array.Copy(st.Vals, newVals, st.Top);
        st.Syms = newSyms;
        st.Vals = newVals;
    }

    private static void Uncount(BindingStack st, Symbol sym)
    {
        int idx = sym.BindIndex;
        if ((uint)idx < (uint)st.Counts.Length && st.Counts[idx] > 0) st.Counts[idx]--;
    }

    public static LispObject GetConstant(Symbol sym)
        => sym.Value ?? throw new LispErrorException(new LispUnboundVariable(sym));

    public static LispObject Get(Symbol sym)
    {
        var st = StackBinding(sym);
        if (st == null)
        {
            var v = sym.Value;
            if (v == null) throw new LispErrorException(new LispUnboundVariable(sym));
            return v;
        }
        var syms = st.Syms;
        var vals = st.Vals;
        for (int i = st.Top - 1; i >= 0; i--)
        {
            if (ReferenceEquals(syms[i], sym))
            {
                var val = vals[i];
                if (val == null)
                    throw new LispErrorException(new LispUnboundVariable(sym));
                return val;
            }
        }
        if (sym.Value == null)
            throw new LispErrorException(new LispUnboundVariable(sym));
        return sym.Value;
    }

    public static bool TryGet(Symbol sym, out LispObject value)
    {
        var st = StackBinding(sym);
        if (st != null)
        {
            var syms = st.Syms;
            var vals = st.Vals;
            for (int i = st.Top - 1; i >= 0; i--)
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
        var st = Current;
        int idx = BindIndexOf(sym);
        if (idx >= st.Counts.Length)
        {
            var bigger = new int[Math.Max(idx + 1, st.Counts.Length * 2)];
            Array.Copy(st.Counts, bigger, st.Counts.Length);
            st.Counts = bigger;
        }
        // Counted before the flag is set, both before the entry is visible:
        // from here on, reads of SYM on this thread must scan.
        st.Counts[idx]++;
        sym.EverDynamicallyBound = true;
        if (st.Top >= st.Syms.Length) Grow(st);
        st.Syms[st.Top] = sym;
        st.Vals[st.Top] = value is UnboundSentinel ? null : value;
        st.Top++;
    }

    public static void Pop(Symbol sym)
    {
        var st = _stack;
        if (st == null || st.Top == 0) return;
        var syms = st.Syms;
        var vals = st.Vals;
        for (int i = st.Top - 1; i >= 0; i--)
        {
            if (ReferenceEquals(syms[i], sym))
            {
                // Swap with top entry so we can decrement Top without a shift
                int last = st.Top - 1;
                if (i < last)
                {
                    syms[i] = syms[last];
                    vals[i] = vals[last];
                }
                st.Top--;
                // Clear freed slot to prevent GC rooting
                syms[st.Top] = null;
                vals[st.Top] = null;
                Uncount(st, sym);
                return;
            }
        }
        // Not found: defensive no-op (double-pop)
    }

    public static LispObject Set(Symbol sym, LispObject value)
    {
        var st = StackBinding(sym);
        if (st != null)
        {
            var syms = st.Syms;
            var vals = st.Vals;
            for (int i = st.Top - 1; i >= 0; i--)
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

    /// <summary>
    /// SETQ's assignment: SET, but refusing a constant variable. CLHS 3.1.2.1.1.3
    /// makes assigning one an error, and it was silently allowed -- (setq +c+ 99)
    /// changed the value out from under code that had already been compiled with
    /// the old one, and (setq nil 1) / (setq t 1) reported success while doing
    /// nothing. DEFCONSTANT itself goes through Set, not here.
    /// </summary>
    public static LispObject SetChecked(Symbol sym, LispObject value)
    {
        if (sym.IsConstant) throw Runtime.ConstantAssignmentError(sym, "SETQ");
        return Set(sym, value);
    }

    public static LispObject SetIfUnbound(Symbol sym, LispObject value)
    {
        if (TryGet(sym, out var existing)) return existing;
        sym.Value = value;
        return value;
    }

    public static LispObject Makunbound(Symbol sym)
    {
        var st = StackBinding(sym);
        if (st != null)
        {
            var syms = st.Syms;
            var vals = st.Vals;
            for (int i = st.Top - 1; i >= 0; i--)
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
                throw new LispErrorException(new LispTypeError(
                    $"PROGV: not a symbol: {syms[i]}", syms[i], Startup.Sym("SYMBOL")));
            var val = i < vals.Count ? vals[i] : Unbound;
            Push(sym, val);
        }
        return symsObj;
    }

    /// <summary>
    /// Snapshot the current thread's dynamic bindings so they can be re-installed on
    /// another thread (used when a .NET callback / async continuation must run with
    /// the specials that were in effect where it was registered: see Runtime.DotNet).
    /// NOT used for MAKE-THREAD: new Lisp threads deliberately do not inherit bindings.
    /// Scans from top; first occurrence of each symbol is the effective binding.
    /// </summary>
    public static Dictionary<Symbol, LispObject>? Snapshot()
    {
        var st = _stack;
        if (st == null || st.Top == 0) return null;
        var snap = new Dictionary<Symbol, LispObject>();
        var syms = st.Syms;
        var vals = st.Vals;
        for (int i = st.Top - 1; i >= 0; i--)
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

    /// <summary>Current binding-stack depth. Pair with <see cref="TruncateTo"/> to
    /// install a snapshot temporarily (e.g. around an async continuation that runs
    /// on a different thread) and unwind it afterwards.</summary>
    public static int Depth => _stack?.Top ?? 0;

    /// <summary>
    /// The bindings currently on this thread's stack, innermost first, as
    /// (symbol, value, depth): for the debugger, which shows what specials are
    /// in effect and (via DEPTH) which of them a given frame established.
    ///
    /// Every binding is listed, not just the effective one per symbol: a shadowed
    /// outer binding is exactly what a reader of a nested LET wants to see, and
    /// collapsing them would hide it. An unbound binding (PROGV with no value)
    /// reports the Unbound sentinel rather than being skipped.
    /// </summary>
    public static List<(Symbol Symbol, LispObject Value, int Depth)> CurrentBindings()
    {
        var result = new List<(Symbol, LispObject, int)>();
        var st = _stack;
        if (st == null) return result;
        var syms = st.Syms;
        var vals = st.Vals;
        for (int i = st.Top - 1; i >= 0; i--)
        {
            var sym = syms[i];
            if (sym != null) result.Add((sym, vals[i] ?? Unbound, i));
        }
        return result;
    }

    /// <summary>Pop bindings down to DEPTH (no-op if already at or below it).</summary>
    public static void TruncateTo(int depth)
    {
        if (depth < 0) depth = 0;
        var st = _stack;
        if (st == null) return;
        var syms = st.Syms;
        var vals = st.Vals;
        while (st.Top > depth)
        {
            st.Top--;
            var sym = syms[st.Top];
            if (sym != null) Uncount(st, sym);
            syms[st.Top] = null;
            vals[st.Top] = null;
        }
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

// The value a typed entry of a &key function passes to the function's shared
// body for a keyword the call did not supply. The shared body replaces it with
// the key's default before any user code runs, so Lisp code never sees it.
public sealed class AbsentKey : LispObject
{
    public static readonly AbsentKey Instance = new();
    private AbsentKey() { }
    public static bool Is(LispObject o) => ReferenceEquals(o, Instance);
    public override string ToString() => "#<ABSENT-KEY>";
}
