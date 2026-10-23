namespace DotCL;

public static class DynamicBindings
{
    public static readonly LispObject Unbound = new UnboundSentinel();

    private const int InitialCapacity = 64;

    // One object per thread holds the whole binding stack, so an operation
    // reads one thread-static instead of several.
    //
    // TOPS[sym.BindIndex] is 1 + the position of SYM's innermost binding on
    // this thread's stack, or 0 when this thread has none; each entry keeps the
    // TOPS value it replaced (PREV), so unbinding restores it. A read is one
    // index, with no scan of the stack.
    public sealed class BindingStack
    {
        internal Entry[] E = new Entry[InitialCapacity];
        internal int Top;
        internal int[] Tops = new int[256];
    }

    // A struct array: a store into LispObject?[] goes through the covariant
    // array-store check, and Push / Pop are on the path of every special LET.
    internal struct Entry
    {
        public Symbol? Sym;
        public LispObject? Val;
        public int Prev;
    }

    [ThreadStatic] private static BindingStack? _stack;

    private static int _nextBindIndex;

    private static BindingStack Current => _stack ??= new BindingStack();

    // The *On entries take this thread's stack from a local the compiled function
    // fetched once, instead of reading the thread-static at every operation.
    public static BindingStack Here() => Current;

    private static int BindIndexOf(Symbol sym)
    {
        int idx = sym.BindIndex;
        if (idx != 0) return idx;
        int fresh = Interlocked.Increment(ref _nextBindIndex);
        int prior = Interlocked.CompareExchange(ref sym.BindIndex, fresh, 0);
        return prior == 0 ? fresh : prior;
    }

    /// <summary>The binding stack of this thread when SYM is bound on it, with
    /// POS the position of the innermost binding; else null (the symbol's
    /// global value is the one in effect).</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static BindingStack? StackBinding(Symbol sym, out int pos)
    {
        pos = -1;
        if (!sym.EverDynamicallyBound) return null;
        var st = _stack;
        if (st == null) return null;
        int idx = sym.BindIndex;
        var tops = st.Tops;
        if ((uint)idx >= (uint)tops.Length) return null;
        pos = tops[idx] - 1;
        return pos < 0 ? null : st;
    }

    private static void Grow(BindingStack st)
    {
        var bigger = new Entry[st.E.Length * 2];
        Array.Copy(st.E, bigger, st.Top);
        st.E = bigger;
    }

    /// <summary>Recompute TOPS and PREV from the entries, after an entry was
    /// removed from below the top.</summary>
    private static void Rebuild(BindingStack st, Symbol removed)
    {
        var tops = st.Tops;
        var e = st.E;
        int r = removed.BindIndex;
        if ((uint)r < (uint)tops.Length) tops[r] = 0;
        for (int j = 0; j < st.Top; j++)
        {
            var s = e[j].Sym;
            if (s != null) tops[s.BindIndex] = 0;
        }
        for (int j = 0; j < st.Top; j++)
        {
            var s = e[j].Sym;
            if (s == null) continue;
            e[j].Prev = tops[s.BindIndex];
            tops[s.BindIndex] = j + 1;
        }
    }

    public static LispObject GetConstant(Symbol sym)
        => sym.Value ?? throw new LispErrorException(new LispUnboundVariable(sym));

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject Get(Symbol sym)
    {
        if (!sym.EverDynamicallyBound && sym.Value is { } g) return g;
        return GetSlow(sym);
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static LispObject GetSlow(Symbol sym)
    {
        var st = StackBinding(sym, out int pos);
        var v = st == null ? sym.Value : st.E[pos].Val;
        if (v == null) throw new LispErrorException(new LispUnboundVariable(sym));
        return v;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject GetOn(Symbol sym, BindingStack st)
    {
        if (sym.EverDynamicallyBound)
        {
            int idx = sym.BindIndex;
            var tops = st.Tops;
            if ((uint)idx < (uint)tops.Length && tops[idx] > 0)
                return st.E[tops[idx] - 1].Val ?? UnboundError(sym);
        }
        return sym.Value ?? UnboundError(sym);
    }

    // For a function that binds nothing: ST is a local of the caller, filled
    // with this thread's stack at the first read of a variable that has been
    // bound, so the call reads the thread-static at most once.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject GetLazy(Symbol sym, ref BindingStack? st)
    {
        if (sym.EverDynamicallyBound)
        {
            var s = st ??= Here();
            int idx = sym.BindIndex;
            var tops = s.Tops;
            if ((uint)idx < (uint)tops.Length && tops[idx] > 0)
                return s.E[tops[idx] - 1].Val ?? UnboundError(sym);
        }
        return sym.Value ?? UnboundError(sym);
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject SetCheckedLazy(Symbol sym, LispObject value, ref BindingStack? st)
    {
        if (sym.IsConstant) return SetCheckedSlow(sym, value);
        if (sym.EverDynamicallyBound)
        {
            var s = st ??= Here();
            int idx = sym.BindIndex;
            var tops = s.Tops;
            if ((uint)idx < (uint)tops.Length && tops[idx] > 0)
            {
                s.E[tops[idx] - 1].Val = value;
                return value;
            }
        }
        sym.Value = value;
        return value;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static LispObject UnboundError(Symbol sym)
        => throw new LispErrorException(new LispUnboundVariable(sym));

    public static bool TryGet(Symbol sym, out LispObject value)
    {
        var st = StackBinding(sym, out int pos);
        var v = st == null ? sym.Value : st.E[pos].Val;
        value = v!;
        return v != null;
    }

    public static void Push(Symbol sym, LispObject value) => PushOn(sym, value, Current);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static void PushOn(Symbol sym, LispObject value, BindingStack st)
    {
        int idx = sym.BindIndex;
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        if ((uint)(idx - 1) < (uint)(tops.Length - 1) && (uint)top < (uint)e.Length
            && sym.EverDynamicallyBound && value is not UnboundSentinel)
        {
            ref var slot = ref e[top];
            slot.Sym = sym;
            slot.Val = value;
            slot.Prev = tops[idx];
            tops[idx] = top + 1;
            st.Top = top + 1;
            return;
        }
        PushSlow(sym, value, st);
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static void PushSlow(Symbol sym, LispObject value, BindingStack st)
    {
        int idx = BindIndexOf(sym);
        if (idx >= st.Tops.Length)
        {
            var bigger = new int[Math.Max(idx + 1, st.Tops.Length * 2)];
            Array.Copy(st.Tops, bigger, st.Tops.Length);
            st.Tops = bigger;
        }
        if (!sym.EverDynamicallyBound) sym.EverDynamicallyBound = true;
        int top = st.Top;
        if (top >= st.E.Length) Grow(st);
        ref var slot = ref st.E[top];
        slot.Sym = sym;
        slot.Val = value is UnboundSentinel ? null : value;
        slot.Prev = st.Tops[idx];
        st.Tops[idx] = top + 1;
        st.Top = top + 1;
    }

    public static void Pop(Symbol sym)
    {
        var st = StackBinding(sym, out int pos);
        if (st == null) return; // Not bound here: defensive no-op (double-pop)
        PopAt(st, sym, pos);
    }

    public static void PopOn(Symbol sym, BindingStack st)
    {
        int idx = sym.BindIndex;
        var tops = st.Tops;
        if ((uint)idx >= (uint)tops.Length || tops[idx] == 0) return;
        PopAt(st, sym, tops[idx] - 1);
    }

    private static void PopAt(BindingStack st, Symbol sym, int pos)
    {
        var e = st.E;
        int last = st.Top - 1;
        if (pos == last)
        {
            st.Tops[sym.BindIndex] = e[last].Prev;
            e[last] = default;
            st.Top = last;
            return;
        }
        // Out of order (a fasl compiled when frames popped first-pushed first).
        Array.Copy(e, pos + 1, e, pos, last - pos);
        e[last] = default;
        st.Top = last;
        Rebuild(st, sym);
    }

    public static LispObject Set(Symbol sym, LispObject value)
    {
        var st = StackBinding(sym, out int pos);
        if (st != null) st.E[pos].Val = value;
        else sym.Value = value;
        return value;
    }

    /// <summary>
    /// SETQ's assignment: SET, but refusing a constant variable. CLHS 3.1.2.1.1.3
    /// makes assigning one an error, and it was silently allowed -- (setq +c+ 99)
    /// changed the value out from under code that had already been compiled with
    /// the old one, and (setq nil 1) / (setq t 1) reported success while doing
    /// nothing. DEFCONSTANT itself goes through Set, not here.
    /// </summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject SetChecked(Symbol sym, LispObject value)
    {
        if (!sym.EverDynamicallyBound && !sym.IsConstant) { sym.Value = value; return value; }
        return SetCheckedSlow(sym, value);
    }

    public static LispObject SetCheckedOn(Symbol sym, LispObject value, BindingStack st)
    {
        if (sym.IsConstant) throw Runtime.ConstantAssignmentError(sym, "SETQ");
        if (sym.EverDynamicallyBound)
        {
            int idx = sym.BindIndex;
            var tops = st.Tops;
            if ((uint)idx < (uint)tops.Length && tops[idx] > 0)
            {
                st.E[tops[idx] - 1].Val = value;
                return value;
            }
        }
        sym.Value = value;
        return value;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static LispObject SetCheckedSlow(Symbol sym, LispObject value)
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
        var st = StackBinding(sym, out int pos);
        if (st != null) st.E[pos].Val = null; // null = unbound in flat stack
        else sym.Value = null;
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
        var e = st.E;
        for (int i = st.Top - 1; i >= 0; i--)
        {
            var sym = e[i].Sym;
            if (sym != null && !snap.ContainsKey(sym))
            {
                var val = e[i].Val;
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
        var e = st.E;
        for (int i = st.Top - 1; i >= 0; i--)
        {
            var sym = e[i].Sym;
            if (sym != null) result.Add((sym, e[i].Val ?? Unbound, i));
        }
        return result;
    }

    /// <summary>The depth a LET binding specials saves before its pushes, so its
    /// cleanup is one <see cref="UnbindTo"/> instead of one Pop per symbol.</summary>
    public static int BindDepth() => _stack?.Top ?? 0;

    public static int BindDepthOn(BindingStack st) => st.Top;

    public static void UnbindTo(int depth)
    {
        var st = _stack;
        if (st == null) return;
        UnbindToOn(depth, st);
    }

    public static void UnbindToOn(int depth, BindingStack st)
    {
        int top = st.Top;
        if (top <= depth) return;
        var e = st.E;
        var tops = st.Tops;
        do
        {
            top--;
            ref var slot = ref e[top];
            var sym = slot.Sym;
            if (sym != null) tops[sym.BindIndex] = slot.Prev;
            slot = default;
        } while (top > depth);
        st.Top = top;
    }

    /// <summary>Pop bindings down to DEPTH (no-op if already at or below it).</summary>
    public static void TruncateTo(int depth) => UnbindTo(depth < 0 ? 0 : depth);

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
public sealed class UnboundSentinel : LispObject
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
