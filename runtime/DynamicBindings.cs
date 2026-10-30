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
        => sym.Value ?? Runtime.UnboundVariableError(sym, global: true);

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
        return v ?? Runtime.UnboundVariableError(sym);
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
        => Runtime.UnboundVariableError(sym);

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

    /// <summary>Save the group depth and make room for COUNT entries once.</summary>
    public static int BindGroupDepth(int count) => BindGroupDepthOn(count, Current);

    public static int BindGroupDepthOn(int count, BindingStack st)
    {
        int need = st.Top + count;
        if (need > st.E.Length)
        {
            int cap = st.E.Length;
            while (cap < need) cap *= 2;
            var bigger = new Entry[cap];
            Array.Copy(st.E, bigger, st.Top);
            st.E = bigger;
        }
        return st.Top;
    }

    public static void PushGroup2(Symbol s1, LispObject v1, Symbol s2, LispObject v2)
        => PushGroup2On(s1, v1, s2, v2, Current);

    public static void PushGroup2On(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                    BindingStack st)
    {
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        int i1 = s1.BindIndex, i2 = s2.BindIndex;
        if ((uint)(top + 2) <= (uint)e.Length
            && (uint)(i1 - 1) < (uint)(tops.Length - 1)
            && (uint)(i2 - 1) < (uint)(tops.Length - 1)
            && s1.EverDynamicallyBound && s2.EverDynamicallyBound
            && v1 is not UnboundSentinel && v2 is not UnboundSentinel)
        {
            ref var a = ref e[top];
            a.Sym = s1; a.Val = v1; a.Prev = tops[i1]; tops[i1] = top + 1;
            ref var b = ref e[top + 1];
            b.Sym = s2; b.Val = v2; b.Prev = tops[i2]; tops[i2] = top + 2;
            st.Top = top + 2;
            return;
        }
        PushOn(s1, v1, st); PushOn(s2, v2, st);
    }

    public static void PushGroup3(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                  Symbol s3, LispObject v3)
        => PushGroup3On(s1, v1, s2, v2, s3, v3, Current);

    public static void PushGroup3On(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                    Symbol s3, LispObject v3, BindingStack st)
    {
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        int i1 = s1.BindIndex, i2 = s2.BindIndex, i3 = s3.BindIndex;
        if ((uint)(top + 3) <= (uint)e.Length
            && (uint)(i1 - 1) < (uint)(tops.Length - 1)
            && (uint)(i2 - 1) < (uint)(tops.Length - 1)
            && (uint)(i3 - 1) < (uint)(tops.Length - 1)
            && s1.EverDynamicallyBound && s2.EverDynamicallyBound
            && s3.EverDynamicallyBound
            && v1 is not UnboundSentinel && v2 is not UnboundSentinel
            && v3 is not UnboundSentinel)
        {
            ref var a = ref e[top];
            a.Sym = s1; a.Val = v1; a.Prev = tops[i1]; tops[i1] = top + 1;
            ref var b = ref e[top + 1];
            b.Sym = s2; b.Val = v2; b.Prev = tops[i2]; tops[i2] = top + 2;
            ref var c = ref e[top + 2];
            c.Sym = s3; c.Val = v3; c.Prev = tops[i3]; tops[i3] = top + 3;
            st.Top = top + 3;
            return;
        }
        PushOn(s1, v1, st); PushOn(s2, v2, st); PushOn(s3, v3, st);
    }

    public static void PushGroup4(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                  Symbol s3, LispObject v3, Symbol s4, LispObject v4)
        => PushGroup4On(s1, v1, s2, v2, s3, v3, s4, v4, Current);

    public static void PushGroup4On(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                    Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                    BindingStack st)
    {
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        int i1 = s1.BindIndex, i2 = s2.BindIndex, i3 = s3.BindIndex, i4 = s4.BindIndex;
        if ((uint)(top + 4) <= (uint)e.Length
            && (uint)(i1 - 1) < (uint)(tops.Length - 1)
            && (uint)(i2 - 1) < (uint)(tops.Length - 1)
            && (uint)(i3 - 1) < (uint)(tops.Length - 1)
            && (uint)(i4 - 1) < (uint)(tops.Length - 1)
            && s1.EverDynamicallyBound && s2.EverDynamicallyBound
            && s3.EverDynamicallyBound && s4.EverDynamicallyBound
            && v1 is not UnboundSentinel && v2 is not UnboundSentinel
            && v3 is not UnboundSentinel && v4 is not UnboundSentinel)
        {
            ref var a = ref e[top];
            a.Sym = s1; a.Val = v1; a.Prev = tops[i1]; tops[i1] = top + 1;
            ref var b = ref e[top + 1];
            b.Sym = s2; b.Val = v2; b.Prev = tops[i2]; tops[i2] = top + 2;
            ref var c = ref e[top + 2];
            c.Sym = s3; c.Val = v3; c.Prev = tops[i3]; tops[i3] = top + 3;
            ref var d = ref e[top + 3];
            d.Sym = s4; d.Val = v4; d.Prev = tops[i4]; tops[i4] = top + 4;
            st.Top = top + 4;
            return;
        }
        PushOn(s1, v1, st); PushOn(s2, v2, st);
        PushOn(s3, v3, st); PushOn(s4, v4, st);
    }

    public static void PushGroup5(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                  Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                  Symbol s5, LispObject v5)
        => PushGroup5On(s1, v1, s2, v2, s3, v3, s4, v4, s5, v5, Current);

    public static void PushGroup5On(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                    Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                    Symbol s5, LispObject v5, BindingStack st)
    {
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        int i1 = s1.BindIndex, i2 = s2.BindIndex, i3 = s3.BindIndex, i4 = s4.BindIndex,
            i5 = s5.BindIndex;
        if ((uint)(top + 5) <= (uint)e.Length
            && (uint)(i1 - 1) < (uint)(tops.Length - 1)
            && (uint)(i2 - 1) < (uint)(tops.Length - 1)
            && (uint)(i3 - 1) < (uint)(tops.Length - 1)
            && (uint)(i4 - 1) < (uint)(tops.Length - 1)
            && (uint)(i5 - 1) < (uint)(tops.Length - 1)
            && s1.EverDynamicallyBound && s2.EverDynamicallyBound
            && s3.EverDynamicallyBound && s4.EverDynamicallyBound
            && s5.EverDynamicallyBound
            && v1 is not UnboundSentinel && v2 is not UnboundSentinel
            && v3 is not UnboundSentinel && v4 is not UnboundSentinel
            && v5 is not UnboundSentinel)
        {
            ref var a = ref e[top];
            a.Sym = s1; a.Val = v1; a.Prev = tops[i1]; tops[i1] = top + 1;
            ref var b = ref e[top + 1];
            b.Sym = s2; b.Val = v2; b.Prev = tops[i2]; tops[i2] = top + 2;
            ref var c = ref e[top + 2];
            c.Sym = s3; c.Val = v3; c.Prev = tops[i3]; tops[i3] = top + 3;
            ref var d = ref e[top + 3];
            d.Sym = s4; d.Val = v4; d.Prev = tops[i4]; tops[i4] = top + 4;
            ref var f = ref e[top + 4];
            f.Sym = s5; f.Val = v5; f.Prev = tops[i5]; tops[i5] = top + 5;
            st.Top = top + 5;
            return;
        }
        PushOn(s1, v1, st); PushOn(s2, v2, st); PushOn(s3, v3, st);
        PushOn(s4, v4, st); PushOn(s5, v5, st);
    }

    public static void PushGroup6(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                  Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                  Symbol s5, LispObject v5, Symbol s6, LispObject v6)
        => PushGroup6On(s1, v1, s2, v2, s3, v3, s4, v4, s5, v5, s6, v6, Current);

    public static void PushGroup6On(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                    Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                    Symbol s5, LispObject v5, Symbol s6, LispObject v6,
                                    BindingStack st)
    {
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        int i1 = s1.BindIndex, i2 = s2.BindIndex, i3 = s3.BindIndex, i4 = s4.BindIndex,
            i5 = s5.BindIndex, i6 = s6.BindIndex;
        if ((uint)(top + 6) <= (uint)e.Length
            && (uint)(i1 - 1) < (uint)(tops.Length - 1)
            && (uint)(i2 - 1) < (uint)(tops.Length - 1)
            && (uint)(i3 - 1) < (uint)(tops.Length - 1)
            && (uint)(i4 - 1) < (uint)(tops.Length - 1)
            && (uint)(i5 - 1) < (uint)(tops.Length - 1)
            && (uint)(i6 - 1) < (uint)(tops.Length - 1)
            && s1.EverDynamicallyBound && s2.EverDynamicallyBound
            && s3.EverDynamicallyBound && s4.EverDynamicallyBound
            && s5.EverDynamicallyBound && s6.EverDynamicallyBound
            && v1 is not UnboundSentinel && v2 is not UnboundSentinel
            && v3 is not UnboundSentinel && v4 is not UnboundSentinel
            && v5 is not UnboundSentinel && v6 is not UnboundSentinel)
        {
            ref var a = ref e[top];
            a.Sym = s1; a.Val = v1; a.Prev = tops[i1]; tops[i1] = top + 1;
            ref var b = ref e[top + 1];
            b.Sym = s2; b.Val = v2; b.Prev = tops[i2]; tops[i2] = top + 2;
            ref var c = ref e[top + 2];
            c.Sym = s3; c.Val = v3; c.Prev = tops[i3]; tops[i3] = top + 3;
            ref var d = ref e[top + 3];
            d.Sym = s4; d.Val = v4; d.Prev = tops[i4]; tops[i4] = top + 4;
            ref var f = ref e[top + 4];
            f.Sym = s5; f.Val = v5; f.Prev = tops[i5]; tops[i5] = top + 5;
            ref var g = ref e[top + 5];
            g.Sym = s6; g.Val = v6; g.Prev = tops[i6]; tops[i6] = top + 6;
            st.Top = top + 6;
            return;
        }
        PushOn(s1, v1, st); PushOn(s2, v2, st); PushOn(s3, v3, st);
        PushOn(s4, v4, st); PushOn(s5, v5, st); PushOn(s6, v6, st);
    }

    public static void PushGroup7(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                  Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                  Symbol s5, LispObject v5, Symbol s6, LispObject v6,
                                  Symbol s7, LispObject v7)
        => PushGroup7On(s1, v1, s2, v2, s3, v3, s4, v4, s5, v5, s6, v6, s7, v7, Current);

    public static void PushGroup7On(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                    Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                    Symbol s5, LispObject v5, Symbol s6, LispObject v6,
                                    Symbol s7, LispObject v7, BindingStack st)
    {
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        int i1 = s1.BindIndex, i2 = s2.BindIndex, i3 = s3.BindIndex, i4 = s4.BindIndex,
            i5 = s5.BindIndex, i6 = s6.BindIndex, i7 = s7.BindIndex;
        if ((uint)(top + 7) <= (uint)e.Length
            && (uint)(i1 - 1) < (uint)(tops.Length - 1)
            && (uint)(i2 - 1) < (uint)(tops.Length - 1)
            && (uint)(i3 - 1) < (uint)(tops.Length - 1)
            && (uint)(i4 - 1) < (uint)(tops.Length - 1)
            && (uint)(i5 - 1) < (uint)(tops.Length - 1)
            && (uint)(i6 - 1) < (uint)(tops.Length - 1)
            && (uint)(i7 - 1) < (uint)(tops.Length - 1)
            && s1.EverDynamicallyBound && s2.EverDynamicallyBound
            && s3.EverDynamicallyBound && s4.EverDynamicallyBound
            && s5.EverDynamicallyBound && s6.EverDynamicallyBound
            && s7.EverDynamicallyBound
            && v1 is not UnboundSentinel && v2 is not UnboundSentinel
            && v3 is not UnboundSentinel && v4 is not UnboundSentinel
            && v5 is not UnboundSentinel && v6 is not UnboundSentinel
            && v7 is not UnboundSentinel)
        {
            ref var a = ref e[top];
            a.Sym = s1; a.Val = v1; a.Prev = tops[i1]; tops[i1] = top + 1;
            ref var b = ref e[top + 1];
            b.Sym = s2; b.Val = v2; b.Prev = tops[i2]; tops[i2] = top + 2;
            ref var c = ref e[top + 2];
            c.Sym = s3; c.Val = v3; c.Prev = tops[i3]; tops[i3] = top + 3;
            ref var d = ref e[top + 3];
            d.Sym = s4; d.Val = v4; d.Prev = tops[i4]; tops[i4] = top + 4;
            ref var f = ref e[top + 4];
            f.Sym = s5; f.Val = v5; f.Prev = tops[i5]; tops[i5] = top + 5;
            ref var g = ref e[top + 5];
            g.Sym = s6; g.Val = v6; g.Prev = tops[i6]; tops[i6] = top + 6;
            ref var h = ref e[top + 6];
            h.Sym = s7; h.Val = v7; h.Prev = tops[i7]; tops[i7] = top + 7;
            st.Top = top + 7;
            return;
        }
        PushOn(s1, v1, st); PushOn(s2, v2, st); PushOn(s3, v3, st);
        PushOn(s4, v4, st); PushOn(s5, v5, st); PushOn(s6, v6, st);
        PushOn(s7, v7, st);
    }

    public static void PushGroup8(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                  Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                  Symbol s5, LispObject v5, Symbol s6, LispObject v6,
                                  Symbol s7, LispObject v7, Symbol s8, LispObject v8)
        => PushGroup8On(s1, v1, s2, v2, s3, v3, s4, v4, s5, v5, s6, v6, s7, v7, s8, v8, Current);

    public static void PushGroup8On(Symbol s1, LispObject v1, Symbol s2, LispObject v2,
                                    Symbol s3, LispObject v3, Symbol s4, LispObject v4,
                                    Symbol s5, LispObject v5, Symbol s6, LispObject v6,
                                    Symbol s7, LispObject v7, Symbol s8, LispObject v8,
                                    BindingStack st)
    {
        int top = st.Top;
        var tops = st.Tops;
        var e = st.E;
        int i1 = s1.BindIndex, i2 = s2.BindIndex, i3 = s3.BindIndex, i4 = s4.BindIndex,
            i5 = s5.BindIndex, i6 = s6.BindIndex, i7 = s7.BindIndex, i8 = s8.BindIndex;
        if ((uint)(top + 8) <= (uint)e.Length
            && (uint)(i1 - 1) < (uint)(tops.Length - 1)
            && (uint)(i2 - 1) < (uint)(tops.Length - 1)
            && (uint)(i3 - 1) < (uint)(tops.Length - 1)
            && (uint)(i4 - 1) < (uint)(tops.Length - 1)
            && (uint)(i5 - 1) < (uint)(tops.Length - 1)
            && (uint)(i6 - 1) < (uint)(tops.Length - 1)
            && (uint)(i7 - 1) < (uint)(tops.Length - 1)
            && (uint)(i8 - 1) < (uint)(tops.Length - 1)
            && s1.EverDynamicallyBound && s2.EverDynamicallyBound
            && s3.EverDynamicallyBound && s4.EverDynamicallyBound
            && s5.EverDynamicallyBound && s6.EverDynamicallyBound
            && s7.EverDynamicallyBound && s8.EverDynamicallyBound
            && v1 is not UnboundSentinel && v2 is not UnboundSentinel
            && v3 is not UnboundSentinel && v4 is not UnboundSentinel
            && v5 is not UnboundSentinel && v6 is not UnboundSentinel
            && v7 is not UnboundSentinel && v8 is not UnboundSentinel)
        {
            ref var a = ref e[top];
            a.Sym = s1; a.Val = v1; a.Prev = tops[i1]; tops[i1] = top + 1;
            ref var b = ref e[top + 1];
            b.Sym = s2; b.Val = v2; b.Prev = tops[i2]; tops[i2] = top + 2;
            ref var c = ref e[top + 2];
            c.Sym = s3; c.Val = v3; c.Prev = tops[i3]; tops[i3] = top + 3;
            ref var d = ref e[top + 3];
            d.Sym = s4; d.Val = v4; d.Prev = tops[i4]; tops[i4] = top + 4;
            ref var f = ref e[top + 4];
            f.Sym = s5; f.Val = v5; f.Prev = tops[i5]; tops[i5] = top + 5;
            ref var g = ref e[top + 5];
            g.Sym = s6; g.Val = v6; g.Prev = tops[i6]; tops[i6] = top + 6;
            ref var h = ref e[top + 6];
            h.Sym = s7; h.Val = v7; h.Prev = tops[i7]; tops[i7] = top + 7;
            ref var j = ref e[top + 7];
            j.Sym = s8; j.Val = v8; j.Prev = tops[i8]; tops[i8] = top + 8;
            st.Top = top + 8;
            return;
        }
        PushOn(s1, v1, st); PushOn(s2, v2, st); PushOn(s3, v3, st);
        PushOn(s4, v4, st); PushOn(s5, v5, st); PushOn(s6, v6, st);
        PushOn(s7, v7, st); PushOn(s8, v8, st);
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

    private static List<LispObject> ProgvList(LispObject obj)
    {
        var result = new List<LispObject>();
        var cur = obj;
        while (cur is Cons c) { result.Add(c.Car); cur = c.Cdr; }
        if (cur is not Nil)
            throw new LispErrorException(new LispTypeError(
                $"PROGV: not a proper list: {obj}", obj, Startup.Sym("LIST")));
        return result;
    }

    public static LispObject ProgvBind(LispObject symsObj, LispObject valsObj)
    {
        var syms = ProgvList(symsObj);
        var vals = ProgvList(valsObj);
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
