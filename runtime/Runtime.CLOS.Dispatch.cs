using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    /// <summary>A generic function that dispatches through DISPATCHGF, with the
    /// arity-1 entry point installed. Every GF whose dispatch is the standard one is
    /// built here, so the entry points a GF carries are decided in one place.</summary>
    internal static unsafe GenericFunction NewDispatchingGF(Symbol name, int arity)
    {
        GenericFunction? gf = null;
        gf = new GenericFunction(name, arity, args => GFEntryArgs(gf!, args));
        gf.SetDirectDelegate(GFEntries.E1.CreateDelegate(typeof(Func<LispObject, LispObject>), gf));
        gf.SetDirectDelegate(GFEntries.E2.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject>), gf));
        gf.SetDirectDelegate(GFEntries.E3.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject>), gf));
        gf.SetDirectDelegate(GFEntries.E4.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject>), gf));
        gf.MarkMvModeEntry(1);
        gf.MarkMvModeEntry(2);
        gf.MarkMvModeEntry(3);
        gf.MarkMvModeEntry(4);
        nint entry = arity switch
        {
            1 => (nint)(delegate*<LispFunction, LispObject, LispObject>)&GFEntry1,
            2 => (nint)(delegate*<LispFunction, LispObject, LispObject, LispObject>)&GFEntry2,
            3 => (nint)(delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject>)&GFEntry3,
            4 => (nint)(delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject>)&GFEntry4,
            _ => 0,
        };
        gf.SetSelfFramedEntry(entry, arity);
        return gf;
    }

    private static class GFEntries
    {
        private static System.Reflection.MethodInfo M(string name) =>
            typeof(Runtime).GetMethod(name, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)!;
        internal static readonly System.Reflection.MethodInfo E1 = M(nameof(GFEntry1));
        internal static readonly System.Reflection.MethodInfo E2 = M(nameof(GFEntry2));
        internal static readonly System.Reflection.MethodInfo E3 = M(nameof(GFEntry3));
        internal static readonly System.Reflection.MethodInfo E4 = M(nameof(GFEntry4));
    }

    // A generic function links its own debugger frame, as a self-framed DEFUN body
    // does (Function.SelfFramed.cs).
    // The entries take the caller's value mode and hand it to the method on the
    // loose-cache path (arity 1 and 2) or the plain primary chain (3 and 4) only;
    // every other path drops it.
    private static unsafe LispObject GFEntry1(LispFunction self, LispObject a)
    {
        int mode = MultipleValues.PeekMode();
        var n = self.FrameNameField;
        if (n == null) { MultipleValues.DropMode(mode); LispFunction.DropBorrow(); return DispatchGF1(Unsafe.As<GenericFunction>(self), a, mode); }
        Unsafe.SkipInit(out LispFunction.Frame fr);
        LispFunction.Enter1(ref fr, n, a);
        var gf = Unsafe.As<GenericFunction>(self);
        LispObject r;
        if (gf.DispatchFunction == null && !AnyInvocationProtocolCustomized && LooseFast1(gf, a) is { } sl)
        {
            if (fr.Depth >= 0 && sl.Entry1 != 0)
            {
                if (mode != 0 && !sl.Fn.MvModeEntry(1)) MultipleValues.DropMode(mode);
                r = ((delegate*<LispObject, LispObject>)sl.Entry1)(a); GC.KeepAlive(sl);
            }
            else
            {
                MultipleValues.DropMode(mode);
                r = mode != 0 ? sl.Fn.InvokeFast1M(a, mode) : sl.Fn.InvokeArgc(1, a, null, null, null);
            }
        }
        else { MultipleValues.DropMode(mode); r = DispatchGF1(gf, a, mode); }
        LispFunction.Leave(ref fr);
        return r;
    }

    /// <summary>A one-argument call of a generic function through the call site's
    /// cache CELL. An argument of the class the site first saw runs its method body
    /// on a frame of the caller's (FR), as GFENTRY1 runs it on its own; another class
    /// goes through the generic function's loose cache, as in GFENTRY1. MODE is the
    /// caller's value mode, or -1 when the call passes none. Null when neither
    /// applies; the caller then calls FN as usual.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject? GFCallIC1(GFCallCache cell, LispFunction fn, LispObject a, int mode,
                                               ref LispFunction.Frame fr)
    {
        var e = cell.E;
        if (e != null && ReferenceEquals(e.Gf, fn) && e.Epoch == GenericFunction.MethodEpoch
            && (e.Kind == GFCallCache.KindFixnum ? a is Fixnum
                : e.Kind == GFCallCache.KindInstance ? a is LispInstance inst && ReferenceEquals(inst.Class, e.C0)
                : e.Kind == GFCallCache.KindStruct && LispStruct.TryAs(a, out var st) && ReferenceEquals(st.TypeName, e.StructName)
                  && e.ClassEpoch == ClassRegistry.Epoch))
        {
            var sl = e.Slot;
            if (sl.EqlKeys != null && (sl = sl.PickEql(a)) == null) return null;
            LispObject r;
            LispFunction.Enter1(ref fr, e.Name, a);
            if (fr.Depth >= 0 && sl.Entry1 != 0)
            {
                if (mode >= 0 && sl.Fn.MvModeEntry(1)) MultipleValues.SetMode(mode);
                r = ((delegate*<LispObject, LispObject>)sl.Entry1)(a); GC.KeepAlive(sl);
            }
            else r = GFCallIC1Invoke(sl, a, mode);
            LispFunction.Leave(ref fr);
            return r;
        }
        return GFCallIC1Miss(cell, fn, a, mode);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject GFCallIC1Invoke(GenericFunction.LooseSlot sl, LispObject a, int mode) =>
        mode >= 0 ? sl.Fn.InvokeFast1M(a, mode) : sl.Fn.InvokeArgc(1, a, null, null, null);

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static unsafe LispObject? GFCallIC1Miss(GFCallCache cell, LispFunction fn, LispObject a, int mode)
    {
        var e = cell.E;
        if (e != null && ReferenceEquals(e.Gf, fn) && e.Epoch == GenericFunction.MethodEpoch)
        {
            var gf = e.Gf;
            int classEpoch = ClassRegistry.Epoch;
            var ca = LooseArgClass(a, classEpoch);
            var sl = gf.LooseFront;
            if (sl == null || !ReferenceEquals(sl.C0, ca))
            {
                sl = LooseProbe1(gf, ca);
                if (sl == null) return null;
            }
            // Now and then the class at hand takes the entry, so that a site whose
            // first class was not its usual one still ends up with the usual one.
            if ((++cell.Misses & GFCallCache.RefillMask) == 0)
                cell.E = new GFCallCache.Entry(gf, sl, a, e.Epoch, classEpoch, e.Name);
            if (sl.EqlKeys != null && (sl = sl.PickEql(a)) == null) return null;
            Unsafe.SkipInit(out LispFunction.Frame fr);
            LispFunction.Enter1(ref fr, e.Name, a);
            LispObject r;
            if (fr.Depth >= 0 && sl.Entry1 != 0)
            {
                if (mode >= 0 && sl.Fn.MvModeEntry(1)) MultipleValues.SetMode(mode);
                r = ((delegate*<LispObject, LispObject>)sl.Entry1)(a); GC.KeepAlive(sl);
            }
            else r = mode >= 0 ? sl.Fn.InvokeFast1M(a, mode) : sl.Fn.InvokeArgc(1, a, null, null, null);
            LispFunction.Leave(ref fr);
            return r;
        }
        if (cell.Fails < GFCallCache.MaxFails) GFCallIC1Fill(cell, fn, a);
        return null;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void GFCallIC1Fill(GFCallCache cell, LispFunction fn, LispObject a)
    {
        if (fn is not GenericFunction gf || gf.DispatchFunction != null || AnyInvocationProtocolCustomized
            || gf.FrameNameField is not { } name)
        {
            cell.GiveUp();
            return;
        }
        int epoch = GenericFunction.MethodEpoch, classEpoch = ClassRegistry.Epoch;
        var ca = LooseArgClass(a, classEpoch);
        var sl = gf.LooseFront;
        if (sl == null || !ReferenceEquals(sl.C0, ca)) sl = LooseProbe1(gf, ca);
        if (sl == null)
        {
            if (++cell.Fails >= GFCallCache.MaxFails) cell.GiveUp();
            return;
        }
        cell.Fails = 0;
        cell.E = new GFCallCache.Entry(gf, sl, a, epoch, classEpoch, name);
    }

    private static unsafe LispObject GFEntry2(LispFunction self, LispObject a, LispObject b)
    {
        int mode = MultipleValues.TakeMode();
        var n = self.FrameNameField;
        if (n == null) { LispFunction.DropBorrow(); return DispatchGF2(Unsafe.As<GenericFunction>(self), a, b, mode); }
        Unsafe.SkipInit(out LispFunction.Frame fr);
        LispFunction.Enter2(ref fr, n, a, b);
        var gf = Unsafe.As<GenericFunction>(self);
        LispObject r;
        if (gf.DispatchFunction == null && !AnyInvocationProtocolCustomized && LooseFast(gf, a, b, 2) is { } sl)
        {
            if (fr.Depth >= 0 && sl.Entry2 != 0)
            {
                if (mode != 0 && sl.Fn.MvModeEntry(2)) MultipleValues.SetMode(mode);
                r = ((delegate*<LispObject, LispObject, LispObject>)sl.Entry2)(a, b); GC.KeepAlive(sl);
            }
            else r = mode != 0 ? sl.Fn.InvokeFast2M(a, b, mode) : sl.Fn.InvokeArgc(2, a, b, null, null);
        }
        else r = DispatchGF2(gf, a, b, mode);
        LispFunction.Leave(ref fr);
        return r;
    }

    private static LispObject GFEntry3(LispFunction self, LispObject a, LispObject b, LispObject c)
    {
        int mode = MultipleValues.TakeMode();
        var n = self.FrameNameField;
        if (n == null) { LispFunction.DropBorrow(); return DispatchGF3(Unsafe.As<GenericFunction>(self), a, b, c, mode); }
        Unsafe.SkipInit(out LispFunction.Frame fr);
        LispFunction.Enter3(ref fr, n, a, b, c);
        var r = DispatchGF3(Unsafe.As<GenericFunction>(self), a, b, c, mode);
        LispFunction.Leave(ref fr);
        return r;
    }

    private static LispObject GFEntry4(LispFunction self, LispObject a, LispObject b, LispObject c, LispObject d)
    {
        int mode = MultipleValues.TakeMode();
        var n = self.FrameNameField;
        if (n == null) { LispFunction.DropBorrow(); return DispatchGF4(Unsafe.As<GenericFunction>(self), a, b, c, d, mode); }
        Unsafe.SkipInit(out LispFunction.Frame fr);
        LispFunction.Enter4(ref fr, n, a, b, c, d);
        var r = DispatchGF4(Unsafe.As<GenericFunction>(self), a, b, c, d, mode);
        LispFunction.Leave(ref fr);
        return r;
    }

    private static LispObject GFEntryArgs(GenericFunction gf, LispObject[] args)
    {
        var n = gf.FrameNameField;
        if (n == null) { LispFunction.DropBorrow(); return DispatchGF(gf, args); }
        Unsafe.SkipInit(out LispFunction.Frame fr);
        LispFunction.EnterArgs(ref fr, n, args);
        var r = DispatchGF(gf, args);
        LispFunction.Leave(ref fr);
        return r;
    }

    /// <summary>One-argument entry point for generic-function calls, installed as the
    /// GF's arity-1 direct delegate so INVOKE1 reaches dispatch without building an
    /// argument array.
    ///
    /// Runs every cached shape that needs nothing but the single argument: a primary
    /// chain (with or without :before/:after), an EQL-specialized entry, and the slot
    /// reader shortcut. Everything else: a miss, :around methods, built-in
    /// combinations, an effective method built by the protocol, keyword checking,
    /// arity errors: builds the array and goes through DISPATCHGF unchanged, so this
    /// adds a fast path rather than a second dispatcher.</summary>
    private static LispObject DispatchGF1(GenericFunction gf, LispObject a, int mode = 0)
    {
        if (gf.DispatchFunction is LispFunction df1) return df1.Invoke(new[] { a });
        // The protocol route needs the argument array anyway; the check itself is one
        // static bool read until someone specialises one of the three.
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a });
        if (LooseFast1(gf, a) is { } sl)
            return mode != 0 ? sl.Fn.InvokeFast1M(a, mode) : sl.Fn.InvokeArgc(1, a, null, null, null);
        var entry = LooseEntry(gf, a, null, null, null, 1);
        if (entry != null)
        {
            if (entry.FallbackOnly && gf.FallbackFunction is { } fb1) return fb1.Invoke1(a);
            if (entry.PlainPrimaryChain)
            {
                NoteLoose(gf, entry, 1);
                return InvokeChainLoose(gf, entry.Primary, a, null, null, null, 1);
            }
            if (entry.PrimaryChainWithBeforeAfter)
                return InvokeCombinationLoose(gf, entry, a, null, null, null, 1);
            // EQL-specialized generic functions are cached only for one required
            // argument, so this is the only arity that can take them without an array.
            if (entry.HasEqlSpecializers)
            {
                NoteLooseEql(gf, entry);
                var eqlChain = EqlLooseChain(entry, a);
                if (eqlChain != null) return InvokeChainLoose(gf, eqlChain, a, null, null, null, 1);
            }
            // A cached slot-reader shortcut: the array path would only hand args[0] to
            // the same SLOTVALUEDIRECT. It is reached by every call of an accessor whose
            // generic function holds anything besides reader methods, since the
            // compile-time READERIC shortcut stands down for such a function.
            else if (entry.ReaderSlotIndex >= 0 && entry.EffectiveMethodFunction == null
                     && a is LispInstance readerInst)
                return SlotValueDirect(readerInst, entry.ReaderSlotIndex,
                                       entry.ReaderSlotName!, entry.ReaderSlotName!.Name);
        }
        return DispatchGF(gf, new[] { a });
    }

    /// <summary>The method chain a warm EQL-specialized cache entry runs for A, or null
    /// when the entry is not one the loose-argument path can take (:around/:before/:after,
    /// a built-in combination, or one of the slot reader/writer shortcuts) or no method
    /// applies. A matching EQL method runs at the head of the precomputed [eql-method,
    /// non-EQL primaries...] chain; with no match the non-EQL primaries are the whole
    /// chain. Mirrors the entry-selection half of DISPATCHGF's cache-hit path.</summary>
    private static List<LispMethod>? EqlLooseChain(CachedDispatch entry, LispObject a)
    {
        if (entry.EqlValues == null || entry.EqlChains == null) return null;
        if (entry.IsBuiltinCombination || entry.Around.Count != 0
            || entry.Before.Count != 0 || entry.After.Count != 0
            || entry.ReaderSlotIndex >= 0 || entry.WriterSlotIndex >= 0
            || entry.EffectiveMethodFunction != null)
            return null;
        var eqlValues = entry.EqlValues;
        for (int i = 0; i < eqlValues.Length; i++)
        {
            // Same comparison the array path uses: identity, then fixnum value,
            // then the general EQL for the mixed cases.
            var v = eqlValues[i];
            if (ReferenceEquals(a, v)
                || (a is Fixnum fa
                    ? v is Fixnum fv && fa.Value == fv.Value
                    : v is not Fixnum && IsTrueEql(a, v)))
                return entry.EqlChains[i];
        }
        // No EQL value matched: the class-specialized primaries are what applies.
        // An empty set is "no applicable method", which DISPATCHGF reports.
        return entry.Primary.Count > 0 ? entry.Primary : null;
    }

    /// <summary>Two-argument twin of DISPATCHGF1.</summary>
    private static LispObject DispatchGF2(GenericFunction gf, LispObject a, LispObject b, int mode = 0)
    {
        if (gf.DispatchFunction is LispFunction df2) return df2.Invoke(new[] { a, b });
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a, b });
        if (LooseFast(gf, a, b, 2) is { } sl)
            return mode != 0 ? sl.Fn.InvokeFast2M(a, b, mode) : sl.Fn.InvokeArgc(2, a, b, null, null);
        var entry = LooseEntry(gf, a, b, null, null, 2);
        if (entry != null)
        {
            if (entry.FallbackOnly && gf.FallbackFunction is { } fb2) return fb2.Invoke2(a, b);
            if (entry.PlainPrimaryChain)
            {
                NoteLoose(gf, entry, 2);
                return InvokeChainLoose(gf, entry.Primary, a, b, null, null, 2);
            }
            if (entry.PrimaryChainWithBeforeAfter)
                return InvokeCombinationLoose(gf, entry, a, b, null, null, 2);
        }
        return DispatchGF(gf, new[] { a, b });
    }

    /// <summary>Three-argument twin of DISPATCHGF1.</summary>
    private static LispObject DispatchGF3(GenericFunction gf, LispObject a, LispObject b, LispObject c, int mode = 0)
    {
        if (gf.DispatchFunction is LispFunction df3) return df3.Invoke(new[] { a, b, c });
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a, b, c });
        var entry = LooseEntry(gf, a, b, c, null, 3);
        if (entry != null)
        {
            if (entry.PlainPrimaryChain)
                return InvokeChainLoose(gf, entry.Primary, a, b, c, null, 3, mode);
            if (entry.PrimaryChainWithBeforeAfter)
                return InvokeCombinationLoose(gf, entry, a, b, c, null, 3);
        }
        return DispatchGF(gf, new[] { a, b, c });
    }

    /// <summary>Run a one-primary-with-:before/:after combination on loose arguments.
    ///
    /// The auxiliary methods run purely for effect on the same arguments the primary
    /// gets -- CLHS 7.6.6.2: :before methods most specific first, then the primary,
    /// then :after methods least specific first, and the value is the primary's.
    /// None of them can reach CALL-NEXT-METHOD past the single primary, so the whole
    /// combination needs no arguments array; INVOKECHAINLOOSE handles the primary and
    /// its next-method state, and the auxiliaries invoke directly by arity.</summary>
    private static LispObject InvokeCombinationLoose(
        GenericFunction gf, CachedDispatch entry, LispObject a, LispObject? b,
        LispObject? c, LispObject? d, int argc)
    {
        ref var nm = ref Nm;
        var savedGf = nm.Gf;
        nm.Gf = gf;
        try
        {
            var before = entry.Before;
            for (int i = 0; i < before.Count; i++) InvokeLoose(before[i], a, b, c, d, argc);
            if (before.Count != 0) MultipleValues.Reset();
            var result = InvokeChainLoose(gf, entry.Primary, a, b, c, d, argc);
            var after = entry.After;
            if (after.Count == 0) return result;
            var held = MultipleValues.Hold(result);
            for (int i = 0; i < after.Count; i++) InvokeLoose(after[i], a, b, c, d, argc);
            MultipleValues.RestoreHeld(held);
            return result;
        }
        finally
        {
            nm.Gf = savedGf;
        }
    }

    /// <summary>Invoke one method on loose arguments, by arity.</summary>
    private static void InvokeLoose(
        LispMethod m, LispObject a, LispObject? b, LispObject? c, LispObject? d, int argc)
    {
        var fn = m.Function;
        if (m.IgnoresNextMethodState) { fn.InvokeArgc(argc, a, b, c, d); return; }
        // An auxiliary method has no next method, whatever the state around it has.
        ref var nm = ref Nm;
        var savedMethod = nm.Method;
        var savedChain = nm.Chain;
        var savedArgs = nm.Args;
        var savedFallback = nm.Fallback;
        var savedNmp = nm.CapturedNmp;
        var savedCnm = nm.CapturedCnm;
        var savedArg0 = nm.Arg0;
        var savedArg1 = nm.Arg1;
        var savedArg2 = nm.Arg2;
        var savedArg3 = nm.Arg3;
        var savedArgc = nm.Argc;
        nm.Method = m;
        nm.Chain = null;
        nm.Args = null;
        nm.Arg0 = a;
        nm.Arg1 = b;
        nm.Arg2 = c;
        nm.Arg3 = d;
        nm.Argc = argc;
        nm.Fallback = null;
        nm.CapturedNmp = _defaultNmp;
        nm.CapturedCnm = null;
        try { fn.InvokeArgc(argc, a, b, c, d); }
        finally
        {
            nm.Method = savedMethod;
            nm.Chain = savedChain;
            nm.Args = savedArgs;
            nm.Arg0 = savedArg0;
            nm.Arg1 = savedArg1;
            nm.Arg2 = savedArg2;
            nm.Arg3 = savedArg3;
            nm.Argc = savedArgc;
            nm.Fallback = savedFallback;
            nm.CapturedNmp = savedNmp;
            nm.CapturedCnm = savedCnm;
        }
    }

    private static void InvokeAux(LispMethod m, LispObject[] args)
    {
        if (m.IgnoresNextMethodState) { m.Function.Invoke(args); return; }
        ref var nm = ref Nm;
        var savedMethod = nm.Method;
        var savedChain = nm.Chain;
        var savedArgs = nm.Args;
        var savedFallback = nm.Fallback;
        var savedNmp = nm.CapturedNmp;
        var savedCnm = nm.CapturedCnm;
        nm.Method = m;
        nm.Chain = null;
        nm.Args = args;
        nm.Fallback = null;
        nm.CapturedNmp = _defaultNmp;
        nm.CapturedCnm = null;
        try { m.Function.Invoke(args); }
        finally
        {
            nm.Method = savedMethod;
            nm.Chain = savedChain;
            nm.Args = savedArgs;
            nm.Fallback = savedFallback;
            nm.CapturedNmp = savedNmp;
            nm.CapturedCnm = savedCnm;
        }
    }

    /// <summary>Four-argument twin of DISPATCHGF1. The arity cliff was real: a
    /// generic function of 1-3 arguments allocated nothing on a warm cache while
    /// one of 4 allocated 72 B a call, purely because the loose path stopped at
    /// three. SHARED-INITIALIZE reaches it on every (instance slot-names . initargs)
    /// call with one initarg pair.</summary>
    private static LispObject DispatchGF4(
        GenericFunction gf, LispObject a, LispObject b, LispObject c, LispObject d, int mode = 0)
    {
        if (gf.DispatchFunction is LispFunction df4) return df4.Invoke(new[] { a, b, c, d });
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a, b, c, d });
        var entry = LooseEntry(gf, a, b, c, d, 4);
        if (entry != null)
        {
            if (entry.PlainPrimaryChain)
                return InvokeChainLoose(gf, entry.Primary, a, b, c, d, 4, mode);
            if (entry.PrimaryChainWithBeforeAfter)
                return InvokeCombinationLoose(gf, entry, a, b, c, d, 4);
        }
        return DispatchGF(gf, new[] { a, b, c, d });
    }

    /// <summary>The cache entry for these loose arguments, from the front cache or the
    /// table behind it, or null when there is none or when this call must go through
    /// DISPATCHGF anyway (keyword checking, or an argument count DISPATCHGF reports).
    /// The caller decides whether the entry's shape is one it can run.
    ///
    /// The classes of the dispatched arguments are computed once, up front: an entry
    /// holds the classes of the required arguments (an entry may hold fewer classes
    /// than the call has arguments, so the rest are ignored, the same rule DISPATCHGF's
    /// own scan uses).</summary>
    private static CachedDispatch? LooseEntry(
        GenericFunction gf, LispObject a, LispObject? b, LispObject? c, LispObject? d, int argc)
    {
        var dcache = gf.DispatchCache;
        if (dcache == null) return null;
        // Keyword checking and an argument count DISPATCHGF reports as an error
        // (INVOKE2 on a one-argument generic function would otherwise match an entry
        // keyed on its one argument) both leave KNOWN at 0.
        int known = gf.LooseKnown(argc);
        if (known == 0) return null;
        int epoch = ClassRegistry.Epoch;
        var ca = LooseArgClass(a, epoch);
        var cb = known > 1 ? LooseArgClass(b!, epoch) : null;
        var cc = known > 2 ? LooseArgClass(c!, epoch) : null;
        var cd = known > 3 ? LooseArgClass(d!, epoch) : null;
        for (int i = 0; i < dcache.Length; i++)
        {
            var entry = dcache[i];
            var types = entry.ArgTypes;
            int n = types.Length;
            if (n == 0 || n > known) continue;
            if (!ReferenceEquals(types[0], ca)) continue;
            if (n > 1 && !ReferenceEquals(types[1], cb)) continue;
            if (n > 2 && !ReferenceEquals(types[2], cc)) continue;
            if (n > 3 && !ReferenceEquals(types[3], cd)) continue;
            // Nothing else in the cache can match these classes.
            return entry;
        }
        var table = gf.DispatchByClasses;
        if (table == null || table.Width > known || table.Epoch != GenericFunction.MethodEpoch)
            return null;
        int w = table.Width;
        return table.Lookup(ca, w > 1 ? cb : null, w > 2 ? cc : null, w > 3 ? cd : null);
    }

    /// <summary>The entry for these arguments in GF's LOOSECACHE, or null.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static GenericFunction.LooseSlot? LooseFast(GenericFunction gf, LispObject a, LispObject? b, int argc)
    {
        var lc = gf.LooseCache;
        if (lc == null) return null;
        int known = gf.LooseKnown(argc);
        if (known == 0 || known > 2) return null;
        int epoch = ClassRegistry.Epoch;
        var ca = LooseArgClass(a, epoch);
        var cb = known > 1 ? LooseArgClass(b!, epoch) : null;
        int mask = lc.Length - 1;
        int i = GenericFunction.LooseIndex(ca, cb, mask);
        for (int n = 0; n < GenericFunction.LooseProbes; n++, i = (i + 1) & mask)
        {
            var sl = lc[i];
            if (sl == null) return null;
            if (ReferenceEquals(sl.C0, ca) && ReferenceEquals(sl.C1, cb))
                return sl.EqlKeys == null ? sl : sl.PickEql(a);
        }
        return null;
    }

    /// <summary>LOOSEFAST for one argument. The slot of the first class looked up
    /// in the cache is tried before the hash probe.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static GenericFunction.LooseSlot? LooseFast1(GenericFunction gf, LispObject a)
    {
        var ca = LooseArgClass(a, ClassRegistry.Epoch);
        var sl = gf.LooseFront;
        if (sl == null || !ReferenceEquals(sl.C0, ca))
        {
            sl = LooseProbe1(gf, ca);
            if (sl == null) return null;
        }
        return sl.EqlKeys == null ? sl : sl.PickEql(a);
    }

    private static GenericFunction.LooseSlot? LooseProbe1(GenericFunction gf, LispClass? ca)
    {
        var lc = gf.LooseCache;
        if (lc == null || gf.LooseKnown(1) != 1) return null;
        int mask = lc.Length - 1;
        int i = GenericFunction.LooseIndex(ca, null, mask);
        for (int n = 0; n < GenericFunction.LooseProbes; n++, i = (i + 1) & mask)
        {
            var sl = lc[i];
            if (sl == null) return null;
            if (ReferenceEquals(sl.C0, ca) && sl.C1 == null)
            {
                if (gf.LooseFront == null) gf.SetLooseFront(sl, lc);
                return sl;
            }
        }
        return null;
    }

    /// <summary>Whether a warm EQL-specialized entry can live in the loose cache: the
    /// shape EQLLOOSECHAIN takes, every chain's first method called as it is, and
    /// EQL values whose EQL is identity or a fixnum compare.</summary>
    private static bool EqlLooseEligible(CachedDispatch entry)
    {
        if (entry.EqlValues == null || entry.EqlChains == null || entry.ArgTypes.Length != 1) return false;
        if (entry.IsBuiltinCombination || entry.Around.Count != 0
            || entry.Before.Count != 0 || entry.After.Count != 0
            || entry.ReaderSlotIndex >= 0 || entry.WriterSlotIndex >= 0
            || entry.EffectiveMethodFunction != null)
            return false;
        if (entry.Primary.Count > 0 && !entry.Primary[0].IgnoresNextMethodState) return false;
        if (entry.Primary.Count == 0 && entry.EqlChains.Length == 0) return false;
        foreach (var v in entry.EqlValues)
            if (!(v is Fixnum || v is Symbol))
                return false;
        foreach (var chain in entry.EqlChains)
            if (!chain[0].IgnoresNextMethodState) return false;
        return true;
    }

    private static void NoteLooseEql(GenericFunction gf, CachedDispatch entry)
    {
        if (entry.Loose != null || gf.LooseCache == null || gf.LooseKnown(1) != 1
            || !EqlLooseEligible(entry))
            return;
        var c0 = entry.ArgTypes[0];
        var chains = entry.EqlChains!;
        var hits = new GenericFunction.LooseSlot[chains.Length];
        for (int i = 0; i < chains.Length; i++)
            hits[i] = new GenericFunction.LooseSlot(c0, null, chains[i][0].Function);
        bool noDefault = entry.Primary.Count == 0;
        var fn = noDefault ? chains[0][0].Function : entry.Primary[0].Function;
        var sl = new GenericFunction.LooseSlot(c0, fn, noDefault, (LispObject[])entry.EqlValues!.Clone(), hits);
        entry.Loose = sl;
        gf.AddLoose(sl);
    }

    private static void NoteLoose(GenericFunction gf, CachedDispatch entry, int argc)
    {
        var first = entry.Primary[0];
        if (!first.IgnoresNextMethodState) return;
        var types = entry.ArgTypes;
        int known = gf.LooseKnown(argc);
        if (known > 2 || types.Length != known) return;
        var fn = first.Function;
        var sl = entry.Loose;
        if (sl == null || !ReferenceEquals(sl.Fn, fn))
            entry.Loose = sl = new GenericFunction.LooseSlot(types[0], known > 1 ? types[1] : null, fn);
        gf.AddLoose(sl);
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static LispClass? LooseArgClass(LispObject obj, int epoch)
    {
        if (obj is LispInstance inst) return inst.Class;
        if (LispStruct.TryAs(obj, out var st) && st.TypeName.ClassMemo is { } memo && memo.Epoch == epoch)
            return memo.AsClass;
        if (obj is Fixnum && _dcFixnum is { } fc) return fc;
        return LooseBuiltinClass(obj);
    }

    // The common built-in arguments from ArgDispatchClass's memos, without its type
    // switch. Kept out of line so the instance and structure tests above stay small.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static LispClass? LooseBuiltinClass(LispObject obj)
    {
        if (obj is Cons) { if (_dcCons is { } c) return c; }
        else if (obj is Nil) { if (_dcNull is { } c) return c; }
        else if (obj is Symbol) { if (_dcSymbol is { } c) return c; }
        else if (obj is Fixnum) { if (_dcFixnum is { } c) return c; }
        return ArgDispatchClass(obj);
    }

    /// <summary>Run CHAIN[0] on ARGC loose arguments with the next-method state a method
    /// body expects. The counterpart of INVOKESTANDARDCOMBINATION for the callers that
    /// hold the arguments rather than an array. A one-method chain has nowhere for
    /// CALL-NEXT-METHOD to go, so the body must see the default closures; a longer one
    /// leaves the captured slots empty, which is how CAPTUREDCNM knows to build from the
    /// chain state (an EQL-specialized method followed by the class-specialized primaries
    /// is the case that has more than one). The arguments of the current invocation are
    /// published lazily: only CALL-NEXT-METHOD with no arguments needs them as a list,
    /// and it materialises them from the loose slots through CURRENTGFARGS.</summary>
    private static LispObject InvokeChainLoose(
        GenericFunction gf, List<LispMethod> primary, LispObject a, LispObject? b,
        LispObject? c, LispObject? d, int argc, int mode = 0)
    {
        var first = primary[0];
        // A body that cannot reach the next-method state is called as it is: nothing
        // it runs reads the state, and the rest of the chain is reachable only through
        // CALL-NEXT-METHOD from it.
        if (first.IgnoresNextMethodState)
        {
            if (mode == 0) return first.Function.InvokeArgc(argc, a, b, c, d);
            return argc == 3 ? first.Function.InvokeFast3M(a, b!, c!, mode)
                             : first.Function.InvokeFast4M(a, b!, c!, d!, mode);
        }
        ref var nm = ref Nm;
        var savedGf = nm.Gf;
        var savedMethod = nm.Method;
        var savedChain = nm.Chain;
        var savedIndex = nm.Index;
        var savedArgs = nm.Args;
        var savedFallback = nm.Fallback;
        var savedCapturedNmp = nm.CapturedNmp;
        var savedCapturedCnm = nm.CapturedCnm;
        var savedArg0 = nm.Arg0;
        var savedArg1 = nm.Arg1;
        var savedArg2 = nm.Arg2;
        var savedArg3 = nm.Arg3;
        var savedArgc = nm.Argc;
        nm.Gf = gf;
        nm.Method = first;
        nm.Chain = primary;
        nm.Index = 1;
        nm.Args = null;
        nm.Arg0 = a;
        nm.Arg1 = b;
        nm.Arg2 = c;
        nm.Arg3 = d;
        nm.Argc = argc;
        nm.Fallback = null;
        // Leave the slot empty so a capture builds from this chain state.
        bool single = primary.Count == 1;
        nm.CapturedNmp = single ? _defaultNmp : null;
        nm.CapturedCnm = null;
        try
        {
            var fn = first.Function;
            return argc switch
            {
                1 => fn.Invoke1(a),
                2 => fn.Invoke2(a, b!),
                3 => fn.Invoke3(a, b!, c!),
                _ => fn.Invoke4(a, b!, c!, d!)
            };
        }
        finally
        {
            nm.Gf = savedGf;
            nm.Method = savedMethod;
            nm.Chain = savedChain;
            nm.Index = savedIndex;
            nm.Args = savedArgs;
            nm.Arg0 = savedArg0;
            nm.Arg1 = savedArg1;
            nm.Arg2 = savedArg2;
            nm.Arg3 = savedArg3;
            nm.Argc = savedArgc;
            nm.Fallback = savedFallback;
            nm.CapturedNmp = savedCapturedNmp;
            nm.CapturedCnm = savedCapturedCnm;
        }
    }

    private static LispObject DispatchGF(GenericFunction gf, LispObject[] args)
    {
        // A discriminating function installed on this generic function replaces
        // dispatch entirely -- applicable methods, the cache and the combination are
        // all its business now. Checked at every entry point, arity fast paths
        // included, or an installed function would be bypassed by exactly the calls
        // that are most common. Null on every generic function until something
        // installs one, so the cost on the usual path is a field read.
        if (gf.DispatchFunction is LispFunction dfN) return dfN.Invoke(args);
        return DispatchGFCore(gf, args);
    }

    /// <summary>dotcl.s own dispatch with the discriminating-function check skipped.
    /// COMPUTE-DISCRIMINATING-FUNCTION.s default method hands this back, so a user
    /// method that calls CALL-NEXT-METHOD and installs the result gets standard
    /// dispatch instead of looping back through the function it just installed.</summary>
    internal static LispObject DispatchGFCore(GenericFunction gf, LispObject[] args)
    {
        // Arity check: signal program-error for too few/too many arguments
        int requiredCount = gf.LambdaListInfoSet ? gf.RequiredCount : (gf.Arity >= 0 ? gf.Arity : 0);
        // A generic function with no required parameters has nothing to dispatch on:
        // every call sees the same applicable set, so the cache key is the empty class
        // vector and the entry matches unconditionally. Only when the lambda list is
        // actually known -- with LAMBDALISTINFOSET false and no arity, REQUIREDCOUNT is
        // 0 because nothing was recorded, not because the function takes no arguments,
        // and an empty key would then match calls that really do dispatch.
        bool zeroDispatch = gf.LambdaListInfoSet && gf.RequiredCount == 0;
        if (requiredCount > 0 && args.Length < requiredCount)
            throw new LispErrorException(new LispProgramError(
                $"{gf.Name.Name}: too few arguments ({args.Length}), expected at least {requiredCount}"));
        // Check max args: if GF has no &rest and no &key, reject excess arguments
        if (gf.LambdaListInfoSet && !gf.HasRest && !gf.HasKey)
        {
            int maxArgs = gf.RequiredCount + gf.OptionalCount;
            if (args.Length > maxArgs)
                throw new LispErrorException(new LispProgramError(
                    $"{gf.Name.Name}: too many arguments ({args.Length}), expected at most {maxArgs}"));
        }

        // Cache check: N-way polymorphic inline cache. Scan the recent
        // dispatches for one whose argument classes match this call, then fall into
        // the (unchanged) hit logic below. The old single-entry cache missed on every
        // alternation between 2+ classes at a call site (3.5-4.6x on poly sites).
        CachedDispatch? cached = null;
        {
            var dcache = gf.DispatchCache;
            if (dcache != null)
            {
                foreach (var entry in dcache)
                {
                    var cachedTypes = entry.ArgTypes;
                    if (cachedTypes.Length > args.Length) continue;
                    bool match = true;
                    for (int i = 0; i < cachedTypes.Length; i++)
                    {
                        if (!ReferenceEquals(ArgDispatchClass(args[i]), cachedTypes[i]))
                        { match = false; break; }
                    }
                    if (match) { cached = entry; break; }
                }
            }
            // Not among the recent ones: the per-class table remembers the rest, so
            // the applicable methods are not recomputed for a class seen before.
            cached ??= DispatchTableHit(gf, args);
        }
        if (cached != null)
        {
                if (cached.FallbackOnly)
                    return gf.FallbackFunction is { } cachedFallback
                        ? cachedFallback.Invoke(args) : CallNoApplicableMethod(gf, args);
                // The AMOP route: this generic function's effective method for these
                // argument classes was built by the protocol on the miss below.
                if (cached.EffectiveMethodFunction is { } cachedEmf)
                    return cachedEmf.Invoke(args);
                // Item2: specialized standard slot reader: read the slot directly,
                // skipping keyword/eql checks and effective-method construction. The cache
                // entry was only stored for this shape (1 dispatch arg, single accessor
                // primary, no aux methods, standard metaclass, instance-allocated slot), so
                // args[0] is a LispInstance of ArgTypes[0] and ReaderSlotIndex is its slot.
                if (cached.ReaderSlotIndex >= 0 && args[0] is LispInstance readerInst)
                    return SlotValueDirect(readerInst, cached.ReaderSlotIndex,
                                           cached.ReaderSlotName!, cached.ReaderSlotName!.Name);
                // Item2b: specialized standard slot writer: write the slot directly.
                // (setf accessor) is arity 2: object = args[1], new value = args[0].
                if (cached.WriterSlotIndex >= 0 && args.Length >= 2 && args[1] is LispInstance writerInst)
                    return SetSlotValueDirect(writerInst, cached.WriterSlotIndex,
                                              cached.WriterSlotName!.Name, args[0]);
                // Keyword validation must run on the cache-hit path too: a warm
                // monomorphic cache otherwise skips the unknown-keyword check that the
                // cache-miss path performs (ANSI DEFMETHOD.ERROR.14/15).
                //
                // Only when a keyword was actually passed. The applicable list below
                // is collected into a fresh List whose sole use is this check, and the
                // check answers immediately when the call has no keyword portion --
                // so a plain call to a generic function that merely HAS &key was
                // building a list, and its backing array, for nothing.
                if (gf.LambdaListInfoSet && gf.HasKey && !gf.HasAllowOtherKeys
                    && args.Length > gf.RequiredCount + gf.OptionalCount)
                    ValidateGenericKeywords(gf, cached, args);
                // For EQL specializers: check if any EQL method matches (takes
                // priority: the cache is only stored for single-required-arg GFs,
                // where an applicable EQL method is always the most specific and at
                // most one EQL value can match; see the cache-store comment).
                if (cached.HasEqlSpecializers && cached.EqlValues != null)
                {
                    int eqlIdx = -1;
                    var eqlValues = cached.EqlValues;
                    var a0 = args[0];
                    for (int i = 0; i < eqlValues.Length; i++)
                    {
                        // Fast eql: identity (symbols/keywords/interned values) or
                        // fixnum value compare; a fixnum is never eql to a
                        // non-fixnum, so only mixed non-fixnum pairs (floats,
                        // chars, ...) take the generic IsTrueEql.
                        var v = eqlValues[i];
                        if (ReferenceEquals(a0, v)
                            || (a0 is Fixnum fa
                                ? v is Fixnum fv && fa.Value == fv.Value
                                : v is not Fixnum && IsTrueEql(a0, v)))
                        { eqlIdx = i; break; }
                    }
                    if (eqlIdx >= 0)
                    {
                        var eqlMatch = cached.EqlMethods![eqlIdx];
                        // Fast path: no around/before/after and no non-EQL primaries
                        // -> invoke EQL method directly with minimal overhead
                        if (cached.Around.Count == 0 && cached.Before.Count == 0
                            && cached.After.Count == 0 && cached.Primary.Count == 0)
                        {
                            var savedGf = _currentGF;
                            var savedMethod = _currentMethod;
                            var savedChain = _nextMethodChain;
                            var savedIndex = _nextMethodIndex;
                            var savedArgs = _currentGFArgs;
                            var savedFallback = _nextMethodFallback;
                            var savedNmp = _capturedNmp;
                            var savedCnm = _capturedCnm;
                            _currentGF = gf;
                            _currentMethod = eqlMatch;
                            _nextMethodChain = null;
                            _nextMethodIndex = 0;
                            _currentGFArgs = args;
                            _nextMethodFallback = null;
                            // A capture here is this invocation's, not an enclosing one.
                            _capturedNmp = null;
                            _capturedCnm = null;
                            try
                            {
                                return eqlMatch.Function.Invoke(args);
                            }
                            finally
                            {
                                _currentGF = savedGf;
                                _currentMethod = savedMethod;
                                _nextMethodChain = savedChain;
                                _nextMethodIndex = savedIndex;
                                _currentGFArgs = savedArgs;
                                _nextMethodFallback = savedFallback;
                                _capturedNmp = savedNmp;
                                _capturedCnm = savedCnm;
                            }
                        }
                        // General case: use the precomputed [eql-method, non-EQL
                        // primaries...] chain: no per-hit list building.
                        var eqlPrimary = cached.EqlChains![eqlIdx];
                        if (cached.Around.Count > 0)
                            return InvokeAroundCombination(gf, cached, eqlPrimary, args);
                        return InvokeStandardCombination(gf, cached.Before, eqlPrimary, cached.After, args);
                    }
                    // No EQL match: fall through to cached non-EQL result
                }
                // EQL cache with no matching EQL method and no non-EQL primaries:
                // mirror the miss path (no-applicable -> fallback fn or error;
                // applicable-but-no-primary -> error). Non-EQL caches always have
                // >= 1 primary (the miss path throws before storing otherwise).
                if (cached.HasEqlSpecializers && cached.Primary.Count == 0)
                {
                    if (cached.Around.Count == 0 && cached.Before.Count == 0
                        && cached.After.Count == 0)
                    {
                        if (gf.FallbackFunction != null)
                            return gf.FallbackFunction.Invoke(args);
                        return CallNoApplicableMethod(gf, args);
                    }
                    throw new LispErrorException(new LispError(
                        $"No primary method for generic function {gf.Name.Name}"));
                }
                // Cache hit: reuse sorted method lists
                if (cached.IsBuiltinCombination && cached.BuiltinCombined != null)
                    return InvokeBuiltinCombination(gf, cached.BuiltinCombined, cached.BuiltinAround!, args);
                if (cached.IsBuiltinCombination && cached.Applicable != null)
                    return DispatchBuiltinCombination(gf, cached.Applicable, args);
                if (cached.Around.Count > 0)
                    return InvokeAroundCombination(gf, cached, cached.Primary, args);
                return InvokeStandardCombination(gf, cached.Before, cached.Primary, cached.After, args);
        }

        // A generic function whose invocation protocol someone specialised is dispatched
        // through it. The answer is cached per argument-class vector like everything
        // else here, so the protocol runs on a miss rather than on every call. If it
        // cannot answer (no applicable methods, or the pieces are missing), fall through
        // to dotcl's own dispatch, which reports no-applicable-method the usual way.
        if (UsesInvocationProtocol(gf))
        {
            var protocolEmf = EffectiveMethodThroughProtocol(gf, args);
            if (protocolEmf != null)
            {
                var protocolTypes = new LispClass?[Math.Min(args.Length, Math.Max(1, requiredCount))];
                for (int i = 0; i < protocolTypes.Length; i++)
                    protocolTypes[i] = ArgDispatchClass(args[i]);
                AddDispatchCache(gf, new CachedDispatch
                {
                    ArgTypes = protocolTypes,
                    Around = new List<LispMethod>(),
                    Before = new List<LispMethod>(),
                    Primary = new List<LispMethod>(),
                    After = new List<LispMethod>(),
                    EffectiveMethodFunction = protocolEmf
                });
                return protocolEmf.Invoke(args);
            }
        }

        // Find applicable methods
        var applicable = new List<LispMethod>();
        bool hasEqlSpec = false;
        foreach (var method in gf.Methods)
        {
            if (IsMethodApplicable(method, args))
                applicable.Add(method);
            if (!hasEqlSpec)
                foreach (var spec in method.Specializers)
                    if (EqlSpecializerValue(spec) != null) { hasEqlSpec = true; break; }
        }

        if (applicable.Count == 0)
        {
            // Fall back to the saved original ordinary function (e.g. the C# built-in
            // for CL functions like CLOSE or STREAM-ELEMENT-TYPE when called with a
            // type that has no user-defined Gray-stream method).
            if (gf.FallbackFunction != null)
            {
                if (!hasEqlSpec)
                {
                    int n = zeroDispatch ? 0 : Math.Max(1, requiredCount);
                    var types = new LispClass?[n];
                    for (int i = 0; i < n && i < args.Length; i++)
                        types[i] = ArgDispatchClass(args[i]);
                    AddDispatchCache(gf, new CachedDispatch
                    {
                        ArgTypes = types,
                        FallbackOnly = true,
                        Around = new List<LispMethod>(),
                        Before = new List<LispMethod>(),
                        Primary = new List<LispMethod>(),
                        After = new List<LispMethod>()
                    });
                }
                return gf.FallbackFunction.Invoke(args);
            }
            return CallNoApplicableMethod(gf, args);
        }

        // Keyword argument validation (CLHS 7.6.5)
        ValidateGenericKeywords(gf, applicable, args);

        // Built-in operator method combinations (+, NCONC, APPEND, AND, OR, PROGN, MIN, MAX, LIST)
        if (gf.MethodCombination != null)
        {
            // Check for long-form method combination first
            if (TryGetMC(_longFormMCRegistry, gf.MethodCombination, out var longFormMC))
            {
                return DispatchLongFormCombination(gf, longFormMC, applicable, args);
            }

            // Sorted once here; the cache entry keeps the result for the hits.
            PartitionBuiltinCombination(gf, applicable, args, out var combined, out var around);
            // Cache for built-in combination
            if (!hasEqlSpec)
            {
                // Width 0 for a no-required-parameter GF: see ZERODISPATCH above.
                int n = zeroDispatch ? 0 : Math.Max(1, requiredCount);
                var types = new LispClass?[n];
                for (int i = 0; i < n && i < args.Length; i++)
                    types[i] = ArgDispatchClass(args[i]);
                AddDispatchCache(gf, new CachedDispatch
                {
                    ArgTypes = types,
                    Applicable = applicable,
                    BuiltinCombined = combined,
                    BuiltinAround = around,
                    HasEqlSpecializers = false,
                    IsBuiltinCombination = true,
                    Around = new List<LispMethod>(),
                    Before = new List<LispMethod>(),
                    Primary = new List<LispMethod>(),
                    After = new List<LispMethod>()
                });
            }
            return InvokeBuiltinCombination(gf, combined, around, args);
        }

        // STANDARD method combination: partition by qualifier
        var aroundMethods = new List<LispMethod>();
        var beforeMethods = new List<LispMethod>();
        var primaryMethods = new List<LispMethod>();
        var afterMethods = new List<LispMethod>();

        foreach (var m in applicable)
        {
            if (m.Qualifiers.Length == 0)
                primaryMethods.Add(m);
            else if (m.Qualifiers.Length != 1)
                continue;
            else if (LispMethod.QualifierName(m.Qualifiers[0]) == "BEFORE")
                beforeMethods.Add(m);
            else if (LispMethod.QualifierName(m.Qualifiers[0]) == "AFTER")
                afterMethods.Add(m);
            else if (LispMethod.QualifierName(m.Qualifiers[0]) == "AROUND")
                aroundMethods.Add(m);
        }

        if (primaryMethods.Count == 0 && gf.FallbackFunction == null)
            throw new LispErrorException(new LispError(
                $"No primary method for generic function {gf.Name.Name}"));

        // Sort: more specific first
        var bySpecificity = new SpecificityComparer(args, false);
        primaryMethods.Sort(bySpecificity);
        if (gf.FallbackFunction != null)
            primaryMethods.Add(FallbackMethodOf(gf, requiredCount));
        beforeMethods.Sort(bySpecificity);
        afterMethods.Sort(new SpecificityComparer(args, true)); // reverse for :after
        aroundMethods.Sort(bySpecificity);

        // Update monomorphic cache for STANDARD combination.
        //
        // EQL-specialized GFs need care: the cache key is the argument CLASSES
        // only, so two calls with the same classes but different EQL values
        // (e.g. (cv :seq x) vs (cv :other x)) share the single cache slot. A
        // class-keyed hit therefore must re-check the EQL methods against the
        // actual argument values: that is what the hit path's EqlMethods scan
        // does. Storing such a cache is only correct when the hit path's
        // reconstruction ("matching EQL method is the most specific primary,
        // prepended to the cached non-EQL primaries") provably matches CLHS
        // 7.6.6 ordering:
        //   - exactly 1 required (dispatch) position: with a single position an
        //     applicable EQL method always beats every class-specialized method,
        //     and two distinct EQL methods can never both be applicable: so
        //     "prepend the (unique) EQL match" IS the sorted order. With 2+
        //     positions a class method can out-rank an EQL method (leftmost
        //     comparison), so those GFs stay uncached.
        //   - all EQL methods unqualified: the hit path files the EQL match as a
        //     primary; an EQL :before/:after/:around would be misfiled.
        // GFs outside this shape keep the old behavior (recompute per call).
        if (!hasEqlSpec)
        {
            // Width 0 for a no-required-parameter GF: see ZERODISPATCH above.
            int n = zeroDispatch ? 0 : Math.Max(1, requiredCount);
            var types = new LispClass?[n];
            for (int i = 0; i < n && i < args.Length; i++)
                types[i] = ArgDispatchClass(args[i]);
            // Item2: specialize a standard slot READER: one dispatch arg, a single
            // accessor primary with no before/after/around, a standard-metaclass instance
            // and an instance-allocated slot. The hit path then reads the slot directly
            // (SlotValueDirect), which is exactly what the reader method's body does, minus
            // effective-method construction and the lambda call. requiredCount==1 excludes
            // (setf accessor) writers (arity 2). InvalidateCache on any method add clears it.
            int readerIdx = -1, writerIdx = -1;
            Symbol? readerName = null, writerName = null;
            if (primaryMethods.Count == 1
                && aroundMethods.Count == 0 && beforeMethods.Count == 0 && afterMethods.Count == 0
                && primaryMethods[0].AccessorSlot is { } asd)
            {
                if (requiredCount == 1
                    && args.Length >= 1 && args[0] is LispInstance rinst && !UsesSlotProtocol(rinst.Class)
                    && rinst.Class.TryGetSlotIndex(asd.Name, out int sidx)
                    && !rinst.Class.EffectiveSlots[sidx].IsClassAllocation)
                {
                    readerIdx = sidx;
                    readerName = asd.Name;
                }
                // Item2b: (setf accessor) writer: object is the last required arg
                // (args[requiredCount-1]), new value is args[0]. Write the slot directly.
                else if (requiredCount == 2
                    && args.Length >= 2 && args[1] is LispInstance winst && !UsesSlotProtocol(winst.Class)
                    && winst.Class.TryGetSlotIndex(asd.Name, out int widx)
                    && !winst.Class.EffectiveSlots[widx].IsClassAllocation)
                {
                    writerIdx = widx;
                    writerName = asd.Name;
                }
            }
            AddDispatchCache(gf, new CachedDispatch
            {
                ArgTypes = types,
                Around = aroundMethods,
                Before = beforeMethods,
                Primary = primaryMethods,
                After = afterMethods,
                HasEqlSpecializers = false,
                ReaderSlotIndex = readerIdx,
                ReaderSlotName = readerName,
                WriterSlotIndex = writerIdx,
                WriterSlotName = writerName
            });
        }
        else if (requiredCount == 1)
        {
            var eqlMethods = new List<LispMethod>();
            bool cacheable = true;
            foreach (var m in gf.Methods)
            {
                bool hasEql = false;
                foreach (var spec in m.Specializers)
                    if (EqlSpecializerValue(spec) != null) { hasEql = true; break; }
                if (!hasEql) continue;
                if (m.Qualifiers.Length != 0) { cacheable = false; break; }
                eqlMethods.Add(m);
            }
            if (cacheable)
            {
                // Cached partitions hold only the non-EQL methods; the hit path
                // re-checks EqlMethods per call and prepends the match. The
                // current call's own invocation below still uses the full
                // sorted primaryMethods (EQL included), so copy-on-filter.
                var nonEqlPrimary = new List<LispMethod>(primaryMethods.Count);
                foreach (var m in primaryMethods)
                {
                    bool hasEql = false;
                    foreach (var spec in m.Specializers)
                        if (EqlSpecializerValue(spec) != null) { hasEql = true; break; }
                    if (!hasEql) nonEqlPrimary.Add(m);
                }
                // Precompute per-EQL-method dispatch data: the EQL value (the
                // single required arg means the EQL specializer is at position
                // 0) and the effective primary chain [eql-method, non-EQL
                // primaries...], so a warm hit allocates nothing.
                var eqlArr = eqlMethods.ToArray();
                var eqlValues = new LispObject[eqlArr.Length];
                var eqlChains = new List<LispMethod>[eqlArr.Length];
                for (int i = 0; i < eqlArr.Length; i++)
                {
                    eqlValues[i] = EqlSpecializerValue(eqlArr[i].Specializers[0])!;
                    var chain = new List<LispMethod>(1 + nonEqlPrimary.Count) { eqlArr[i] };
                    chain.AddRange(nonEqlPrimary);
                    eqlChains[i] = chain;
                }
                var types = new LispClass?[1];
                if (args.Length > 0) types[0] = ArgDispatchClass(args[0]);
                AddDispatchCache(gf, new CachedDispatch
                {
                    ArgTypes = types,
                    Around = aroundMethods,
                    Before = beforeMethods,
                    Primary = nonEqlPrimary,
                    After = afterMethods,
                    HasEqlSpecializers = true,
                    EqlMethods = eqlArr,
                    EqlValues = eqlValues,
                    EqlChains = eqlChains
                });
            }
        }

        // Build effective method chain
        if (aroundMethods.Count > 0)
        {
            // :around wraps everything
            return InvokeAroundCombination(gf, aroundMethods, beforeMethods, primaryMethods,
                                           afterMethods, args);
        }
        else
        {
            return InvokeStandardCombination(gf, beforeMethods, primaryMethods, afterMethods, args);
        }
    }
}
