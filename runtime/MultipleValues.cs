namespace DotCL;

/// <summary>
/// The value a form returns when it produces other than exactly one value. Up to two
/// values live in fields; three or more carry an array. TRUNCATE, FLOOR, GETHASH and
/// the rest of the two-value functions are what this class is mostly used for, and the
/// array those built was 40 of the 64 bytes a multiple-value return cost.
/// </summary>
public sealed class MvReturn : LispObject
{
    private readonly LispObject? _v0;
    private readonly LispObject? _v1;
    /// <summary>The values, for COUNT >= 3. Null for the inline shapes.</summary>
    private readonly LispObject[]? _rest;

    /// <summary>How many values this carries. Derived rather than stored: an extra
    /// int field would push the object from 40 to 48 bytes, and this is the object a
    /// multiple-value return allocates.</summary>
    public int Count => _rest != null ? _rest.Length : _v1 != null ? 2 : _v0 != null ? 1 : 0;

    public MvReturn(LispObject[] values)
    {
        switch (values.Length)
        {
            case 0: break;
            case 1: _v0 = values[0]; break;
            case 2: _v0 = values[0]; _v1 = values[1]; break;
            default: _rest = values; break;
        }
        DotCL.Diagnostics.AllocCounter.Inc("MvReturn");
    }

    /// <summary>Two values without an array to carry them.</summary>
    public MvReturn(LispObject v0, LispObject v1)
    {
        _v0 = v0; _v1 = v1;
        DotCL.Diagnostics.AllocCounter.Inc("MvReturn");
    }

    /// <summary>The I-th value, or NIL past the end (CLHS: missing values are NIL).</summary>
    public LispObject this[int i]
        => _rest != null
            ? (i >= 0 && i < _rest.Length ? _rest[i] : Nil.Instance)
            : i == 0 ? (_v0 ?? Nil.Instance)
            : i == 1 && Count > 1 ? (_v1 ?? Nil.Instance)
            : Nil.Instance;

    /// <summary>The primary value: the first one, or NIL when there are none.</summary>
    public LispObject PrimaryValue => Count > 0 ? this[0] : Nil.Instance;

    /// <summary>The values as an array. Builds one for the inline shapes, so read
    /// COUNT and the indexer instead wherever the values are taken one at a time.</summary>
    public LispObject[] ToArray()
    {
        if (_rest != null) return _rest;
        return Count switch
        {
            0 => Array.Empty<LispObject>(),
            1 => new[] { _v0! },
            _ => new[] { _v0!, _v1! },
        };
    }

    public override string ToString() => Count > 0 ? this[0].ToString()! : "NIL";
}

/// <summary>The values of a form, held in fields across code that may publish
/// values of its own (an UNWIND-PROTECT cleanup, the rest of a
/// MULTIPLE-VALUE-PROG1). N is the count, -1 when nothing was published, -2 when
/// the form's MvReturn carries them.</summary>
public struct MvHeld
{
    internal int N;
    internal LispObject? V0, V1, V2, V3;
    internal LispObject[]? Rest;
}

public static class MultipleValues
{
    [ThreadStatic]
    private static LispObject[]? _values;
    // Lives in CallTls (see there): read and written around every call.
    private static ref int _count => ref CallTls.MvCount;
    [ThreadStatic]
    private static LispObject[]? _primaryCache;
    /// <summary>The two values of a (VALUES A B) whose thread state was published
    /// without an array. Live only while _values is null and _count says how many;
    /// every reader copies them out, so there is no buffer for a later VALUES to
    /// overwrite under someone.</summary>
    [ThreadStatic]
    private static LispObject? _pair0;
    [ThreadStatic]
    private static LispObject? _pair1;

    public static void Set(params LispObject[] vals)
    {
        _values = vals;
        _count = vals.Length;
    }

    /// <summary>Publish two values without an array. The array a (VALUES A B) used to
    /// build was 40 bytes on every TRUNCATE, FLOOR, GETHASH ... and nothing reads it as
    /// an array unless MULTIPLE-VALUE-LIST or an unwind-protect asks.</summary>
    public static void SetPair(LispObject a, LispObject b)
    {
        _values = null;
        _pair0 = a;
        _pair1 = b;
        _count = 2;
    }

    /// <summary>Publish one value without an array, for the same reason SETPAIR
    /// exists. GET already reads a one-value pair state, so this is the same
    /// representation with the second slot unused.</summary>
    public static void SetOne(LispObject a)
    {
        _values = null;
        _pair0 = a;
        _count = 1;
    }

    public static LispObject[] Get()
    {
        MaterializeSnap();
        if (_count <= 0) return Array.Empty<LispObject>();
        if (_values == null)
            return _count == 1 ? new[] { _pair0! } : new[] { _pair0!, _pair1! };
        var result = new LispObject[_count];
        Array.Copy(_values, result, _count);
        return result;
    }

    public static int Count { get { MaterializeSnap(); return _count; } }

    public static LispObject Primary(LispObject value)
    {
        if (value is MvReturn mv)
            value = mv.PrimaryValue;
        var cache = _primaryCache ??= new LispObject[1];
        // A plain `cache[0] = value` is a COVARIANT array store: .NET arrays are
        // covariant, so the store must prove the value fits the array's actual
        // element type, and the JIT cannot see that type through a thread-static
        // field. PRIMARY runs after every call in a single-value context, and
        // that one check was 7.6% of a Release profile (99.8% of all
        // CastHelpers.StelemRef time in it). The array is created right here as
        // LispObject[1] and never escapes as anything else, so the check can
        // only ever pass.
#if NETSTANDARD2_0
        cache[0] = value;
#else
        System.Runtime.CompilerServices.Unsafe.Add(
            ref System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(cache), 0) = value;
#endif
        _values = cache;
        _count = 1;
        return value;
    }

    // (values) with no arguments: one shared marker. It carries no values, so
    // sharing it is safe (an MvReturn that escapes -- which the defensive
    // unwraps around the runtime show does happen -- cannot be mutated through
    // this one), and it saves both the empty array the call site used to build
    // and the wrapper.
    private static readonly MvReturn _noValues = new MvReturn(Array.Empty<LispObject>());

    public static LispObject Values0()
    {
        _values = Array.Empty<LispObject>();
        _count = 0;
        return _noValues;
    }

    /// <summary>Two values, published and wrapped without an array. This is the shape
    /// TRUNCATE, FLOOR, ROUND, GETHASH, INTERN ... return, so it is where the 40-byte
    /// array a multiple-value return used to carry is worth removing.</summary>
    public static LispObject Values2(LispObject a, LispObject b)
    {
        SetPair(a, b);
        return new MvReturn(a, b);
    }

    /// <summary>The single-value half of VALUES2: publish A as the only value and
    /// return it, which is exactly what VALUES2 followed by RUNTIME.UNWRAPMV does --
    /// UnwrapMv takes the primary out of the MvReturn and collapses the thread state
    /// to that one value. A caller in single-value position wants only that, so the
    /// MvReturn VALUES2 allocates for it is dead the instant it is made (40 B on
    /// every GETHASH, FLOOR, TRUNCATE, ROUND, CEILING ...). The peephole rewrites
    /// the call pair to the twin entry that ends here instead.
    ///
    /// B is still passed so the two entries read the same at the call site and a
    /// caller cannot accidentally pair the wrong secondary with the primary.</summary>
    public static LispObject Values2Primary(LispObject a, LispObject b) => Primary(a);

    /// <summary>VALUES2 for a call whose values are read out of the thread state by
    /// the very next instruction (CAPTUREFORBIND, NTHVALUEOF): those readers take a
    /// pair state whose first value is the primary as this call's values, so the
    /// MvReturn is not needed.</summary>
    public static LispObject Values2Pair(LispObject a, LispObject b)
    {
        SetPair(a, b);
        return a;
    }

    public static LispObject Values(params LispObject[] vals)
    {
        Set(vals);
        if (vals.Length == 1)
            return vals[0]; // Single value: no wrapper
        // 0 or 2+ values: return MvReturn for stack-based propagation
        return new MvReturn(vals);
    }

    // --- MULTIPLE-VALUE-BIND support -------------------------------------
    //
    // Binding N values used to go through MULTIPLE-VALUE-LIST, i.e. a fresh cons
    // per value on every call -- and MULTIPLE-VALUE-BIND is how (gethash ...),
    // (floor ...) and friends are normally consumed. These two take the values
    // out of the thread state into a reusable per-thread snapshot instead, so
    // binding allocates nothing.
    //
    // The snapshot is safe to reuse because it is read immediately: the
    // expansion is (let* ((p (capture form)) (v0 (nth 0)) (v1 (nth 1))) ...),
    // and nothing between those reads can produce values. A nested
    // MULTIPLE-VALUE-BIND inside the body has already had its outer values
    // copied into ordinary locals by then.
    [ThreadStatic] private static LispObject[]? _bindSnap;
    private static ref int _bindCount => ref CallTls.BindCount;

    /// <summary>Take the values just returned by FORM into the bind snapshot and
    /// answer the primary value. Mirrors MultipleValuesList1's normalisation:
    /// an MvReturn carries them directly, otherwise the thread state is only this
    /// call's when its count is set and its first value is the primary.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject CaptureForBind(LispObject primary)
    {
        // A capture-mode return already put the values here.
#if NETSTANDARD2_0
        if (_count == CountSnap && ReferenceEquals(_bindSnap![0], primary)) { _count = -1; return primary; }
#else
        if (_count == CountSnap && ReferenceEquals(SnapAt(0), primary)) { _count = -1; return primary; }
#endif
        return CaptureForBindSlow(primary);
    }

    private static LispObject CaptureForBindSlow(LispObject primary)
    {
        MaterializeSnap();
        LispObject[]? vals = null;
        MvReturn? mvSrc = null;
        bool fromPair = false;
        int count;
        if (primary is MvReturn mv)
        {
            mvSrc = mv;
            count = mv.Count;
        }
        else
        {
            int c = _count;
            // Two values published without an array (SETPAIR): copy them from the
            // fields, which is what the array branch below does with its buffer.
            if (c > 0 && _values == null && ReferenceEquals(_pair0, primary))
            {
                count = c;
                fromPair = true;
            }
            else if (c > 0 && _values != null && c <= _values.Length
                && ReferenceEquals(_values[0], primary))
            {
                vals = _values;
                count = c;
            }
            else if (c == 0)
            {
                count = 0;
            }
            else
            {
                // Sentinel (no VALUES call) or a stale buffer: one value.
                count = 1;
            }
        }
        var pair0 = _pair0;
        var pair1 = _pair1;
        Reset(); // consume, so an outer binder does not see these
        var snap = _bindSnap;
        if (snap == null || snap.Length < count)
            snap = NewSnap(count);
        if (mvSrc != null)
        {
            for (int i = 0; i < count; i++) snap[i] = mvSrc[i];
        }
        else if (fromPair)
        {
            snap[0] = pair0!;
            if (count > 1) snap[1] = pair1!;
        }
        else if (vals == null)
        {
            if (count == 1) snap[0] = primary;
        }
        else
        {
            for (int i = 0; i < count; i++) snap[i] = vals[i];
        }
        _bindCount = count;
        return count > 0 ? snap[0] : Nil.Instance;
    }

    /// <summary>CAPTUREFORBIND right after a VALUES2PAIR entry, which left the pair
    /// state with PRIMARY as its first value.</summary>
    public static LispObject CapturePair(LispObject primary)
    {
        var snap = SnapFor(2);
        snap[0] = primary;
        snap[1] = _pair1!;
        _bindCount = 2;
        _count = -1;
        return primary;
    }

    /// <summary>NTHVALUEOF right after a VALUES2PAIR entry.</summary>
    public static LispObject PairNth(LispObject primary, int n)
    {
        _count = -1;
        return n == 0 ? primary : n == 1 ? _pair1! : Nil.Instance;
    }

    /// <summary>Fill the bind snapshot from a LIST of values and answer the
    /// primary. The tree-walk interpreter takes this route: it evaluates the
    /// value form with the host's MULTIPLE-VALUE-LIST (its own way of keeping
    /// values across an evaluation step) and hands the list over, since the
    /// thread state cannot survive the interpreter's own work between producing
    /// the values and binding them.</summary>
    public static LispObject CaptureListForBind(LispObject list)
    {
        int count = 0;
        for (var p = list; p is Cons c; p = c.Cdr) count++;
        var snap = _bindSnap;
        if (snap == null || snap.Length < count)
            snap = NewSnap(count);
        int i = 0;
        for (var p = list; p is Cons c; p = c.Cdr) snap[i++] = c.Car;
        _bindCount = count;
        Reset();
        return count > 0 ? snap[0] : Nil.Instance;
    }

    /// <summary>Nth value of the last CaptureForBind, NIL past the end (CLHS:
    /// missing values are NIL).</summary>
    public static LispObject BindNth(LispObject index)
    {
        long i = index is Fixnum f ? f.Value : 0;
        var snap = _bindSnap;
        if (snap == null || i < 0 || i >= _bindCount) return Nil.Instance;
        return snap[(int)i] ?? Nil.Instance;
    }

    /// <summary>BINDNTH with a literal index.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject BindNthI(int i)
    {
        if ((uint)i >= (uint)_bindCount) return Nil.Instance;
#if NETSTANDARD2_0
        return _bindSnap![i] ?? Nil.Instance;
#else
        return SnapAt(i) ?? Nil.Instance;
#endif
    }

    public static void Reset()
    {
        _count = -1; // Sentinel: no explicit values call yet
        // Don't null _values: Get() checks _count first, saves a ThreadStatic write
    }

    /// <summary>RESET on the way out of a BLOCK (RETURN-FROM with a constant or a
    /// variable). A separate entry so the loop pass that moves RESET in front of a
    /// loop leaves this one on the exit path, where it runs once.</summary>
    public static void ResetOnExit() => _count = -1;

    /// <summary>VALUE as the only value of a runtime function that called Lisp code
    /// (a :TEST, a :KEY, a handler, a method) and returns something of its own: the
    /// channel still holds whatever that code returned, and a caller would take it
    /// as this call's values when its first value is VALUE.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject Alone(LispObject value)
    {
        _count = -1;
        return value;
    }

    // A runtime entry that publishes its values in the channel and returns the
    // primary bare is safe only where the values are read at once. Compiled
    // code calls it through these three, as it calls GETHASH and its twins.

    /// <summary>The values a runtime entry just published for PRIMARY, as an
    /// MvReturn: what compiled code expects from a call whose values it may pass on.</summary>
    public static LispObject Boxed(LispObject primary)
    {
        if (primary is MvReturn) return primary;
        if (_count == 2 && _values == null && ReferenceEquals(_pair0, primary))
            return new MvReturn(primary, _pair1!);
        int n = OwnCount(primary);
        if (n == 1 || n < 0) { _count = -1; return primary; }
        return new MvReturn(Get());
    }

    /// <summary>BOXED followed by RUNTIME.UNWRAPMV: the primary value, alone.</summary>
    public static LispObject PrimaryAlone(LispObject primary)
    {
        _count = -1;
        return primary is MvReturn mv ? mv.PrimaryValue : primary;
    }

    /// <summary>The values a runtime entry just published for PRIMARY, as the
    /// pair state CAPTUREPAIR and PAIRNTH read.</summary>
    public static LispObject Paired(LispObject primary)
    {
        if (primary is MvReturn mv)
        {
            primary = mv.PrimaryValue;
            SetPair(primary, mv.Count > 1 ? mv[1] : Nil.Instance);
            return primary;
        }
        if (_count == 2 && _values == null && ReferenceEquals(_pair0, primary)) return primary;
        int n = OwnCount(primary);
        SetPair(primary, n > 1 ? Get()[1] : Nil.Instance);
        return primary;
    }

    /// <summary>
    /// The values a call produced, given its primary result. Valid only
    /// immediately after the call, and only when the channel was RESET just
    /// before it -- otherwise a previous (VALUES) is still current and a
    /// function that returned one value the ordinary way looks like it
    /// returned none.
    ///
    /// Three shapes reach this. MvReturn is the boxed form the protocol uses
    /// when values cross a boundary that cannot carry the channel. A negative
    /// count means nobody published, which is how a function returning one
    /// value the ordinary way looks. Otherwise the channel is what the callee
    /// published, including the zero-length case for (VALUES), but only when its
    /// first value IS the primary: values an inner form published before the
    /// call returned something else (a constant after a non-local exit, under
    /// the interpreter) are not this call's. That is the test
    /// MULTIPLE-VALUE-LIST makes (OwnCount), so the host and Lisp agree.
    /// </summary>
    public static LispObject[] Of(LispObject primary)
    {
        if (primary is MvReturn mv) return mv.ToArray();
        if (OwnCount(primary) < 0) return new[] { primary };
        return Get();
    }

    // --- Call-site value modes (what the caller will do with the values) ---
    //
    // A compiled call whose result is consumed at once can say how: PRIMARY (the
    // caller takes the primary value only, as RUNTIME.UNWRAPMV would) or CAPTURE
    // (the caller reads the values out of the thread state straight away, as
    // CAPTUREFORBIND does). It passes the mode to LispFunction.InvokeNM, which
    // hands it on only when the entry it is about to call is a compiled body that
    // reads it (the entry's bit in LispFunction's mode mask), immediately before
    // calling that entry. The body takes the mode with TAKEMODE as its first
    // instruction, so the slot is non-zero only between those two points: no
    // other Lisp code can run there and see a mode meant for someone else. A
    // body that read a mode returns its tail (VALUES ...) without an MvReturn:
    // the primary value alone for PRIMARY, the values published in the thread
    // state for CAPTURE.
    public const int ModeNone = 0, ModePrimary = 1, ModeCapture = 2;

    /// <summary>The mode the caller of the current body asked for, consumed.</summary>
    public static int TakeMode()
    {
        int m = CallTls.Mode;
        if (m != 0) CallTls.Mode = 0;
        return m;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    internal static void SetMode(int mode) => CallTls.Mode = mode;

    // A generic function's entry leaves the caller's mode in place for the method
    // body it calls, and drops it on every other way out. Linking its frame keeps
    // the mode only when LispFunction.EnterSlow returns normally.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    internal static int PeekMode() => CallTls.Mode;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    internal static void DropMode(int mode) { if (mode != 0) CallTls.Mode = 0; }

    /// <summary>Hand MODE on to the body called next, which must take it as its
    /// first instruction: the typed entry of a &amp;key or &amp;optional function
    /// passing its own caller's mode to the shared body it calls directly.</summary>
    public static void PassMode(int mode) => CallTls.Mode = mode;

    // A capture-mode return writes its values straight into the bind snapshot
    // (see CAPTUREFORBIND) and says so with this count: the caller's capture is
    // the very next thing that runs, so CAPTUREFORBIND then has nothing to copy,
    // and the thread state needs no reference stores of its own. Every other
    // reader of the state first turns it back into an ordinary array state
    // (MATERIALIZESNAP), so it is never misread as "no values".
    private const int CountSnap = -4;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static LispObject[] SnapFor(int n)
    {
        var snap = _bindSnap;
        if (snap == null || snap.Length < n) snap = NewSnap(n);
        return snap;
    }

    // At least 8 long, so a capture of up to 4 values needs no length check.
    // Pinned, so that compiled code reads it through the address in CallTls
    // without looking the GC thread static up.
    private static unsafe LispObject[] NewSnap(int n)
    {
#if NETSTANDARD2_0
        return _bindSnap = new LispObject[n < 8 ? 8 : n];
#else
        var a = GC.AllocateArray<LispObject>(n < 8 ? 8 : n, pinned: true);
        _bindSnap = a;
        CallTls.Snap = (nint)System.Runtime.CompilerServices.Unsafe.AsPointer(
            ref System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(a));
        return a;
#endif
    }

#if !NETSTANDARD2_0
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static unsafe ref LispObject SnapAt(int i) =>
        ref System.Runtime.CompilerServices.Unsafe.Add(
            ref System.Runtime.CompilerServices.Unsafe.AsRef<LispObject>((void*)CallTls.Snap), i);
#endif

    // The snapshot is only ever a LispObject[], so the covariance check of a
    // plain element store can only pass.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static void SnapSet(LispObject[] snap, int i, LispObject v)
    {
#if NETSTANDARD2_0
        snap[i] = v;
#else
        System.Runtime.CompilerServices.Unsafe.Add(
            ref System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(snap), i) = v;
#endif
    }

    private static void MaterializeSnap()
    {
        if (_count != CountSnap) return;
        int n = _bindCount;
        var vals = new LispObject[n];
        Array.Copy(_bindSnap!, vals, n);
        _values = vals;
        _count = n;
    }

    /// <summary>The primary-mode return: A, with the state saying "no values
    /// published", which every reader takes as the one value it is handed --
    /// what UNWRAPMV's collapse to one value said before. Leaving the state as
    /// it was would let values published earlier in the body, whose first
    /// happens to be A, pass for this call's own further up.</summary>
    private static LispObject PrimaryOnly(LispObject a)
    {
        _count = -1;
        return a;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static LispObject Capture2(LispObject a, LispObject b)
    {
#if NETSTANDARD2_0
        var snap = SnapFor(2);
        snap[0] = a; snap[1] = b;
#else
        if (CallTls.Snap == 0) NewSnap(2);
        SnapAt(0) = a; SnapAt(1) = b;
#endif
        _bindCount = 2;
        _count = CountSnap;
        return a;
    }

    /// <summary>(VALUES A B) where a value mode applies (see TAKEMODE).</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject Values2Mode(LispObject a, LispObject b, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(a);
        if (mode == ModeCapture) return Capture2(a, b);
        return Values2(a, b);
    }

    private static LispObject Capture3(LispObject a, LispObject b, LispObject c)
    {
        var snap = SnapFor(3);
        SnapSet(snap, 0, a); SnapSet(snap, 1, b); SnapSet(snap, 2, c);
        _bindCount = 3;
        _count = CountSnap;
        return a;
    }

    private static LispObject Capture4(LispObject a, LispObject b, LispObject c, LispObject d)
    {
        var snap = SnapFor(4);
        SnapSet(snap, 0, a); SnapSet(snap, 1, b); SnapSet(snap, 2, c); SnapSet(snap, 3, d);
        _bindCount = 4;
        _count = CountSnap;
        return a;
    }

    public static LispObject Values3Mode(LispObject a, LispObject b, LispObject c, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(a);
        if (mode == ModeCapture) return Capture3(a, b, c);
        return Values(a, b, c);
    }

    public static LispObject Values4Mode(LispObject a, LispObject b, LispObject c, LispObject d, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(a);
        if (mode == ModeCapture) return Capture4(a, b, c, d);
        return Values(a, b, c, d);
    }

    public static LispObject Values5Mode(LispObject a, LispObject b, LispObject c, LispObject d,
                                         LispObject e, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(a);
        if (mode != ModeCapture) return Values(a, b, c, d, e);
        var s = SnapFor(5);
        s[0] = a; s[1] = b; s[2] = c; s[3] = d; s[4] = e;
        return CaptureN(5, a);
    }

    public static LispObject Values6Mode(LispObject a, LispObject b, LispObject c, LispObject d,
                                         LispObject e, LispObject f, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(a);
        if (mode != ModeCapture) return Values(a, b, c, d, e, f);
        var s = SnapFor(6);
        s[0] = a; s[1] = b; s[2] = c; s[3] = d; s[4] = e; s[5] = f;
        return CaptureN(6, a);
    }

    public static LispObject Values7Mode(LispObject a, LispObject b, LispObject c, LispObject d,
                                         LispObject e, LispObject f, LispObject g, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(a);
        if (mode != ModeCapture) return Values(a, b, c, d, e, f, g);
        var s = SnapFor(7);
        s[0] = a; s[1] = b; s[2] = c; s[3] = d; s[4] = e; s[5] = f; s[6] = g;
        return CaptureN(7, a);
    }

    public static LispObject Values8Mode(LispObject a, LispObject b, LispObject c, LispObject d,
                                         LispObject e, LispObject f, LispObject g, LispObject h, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(a);
        if (mode != ModeCapture) return Values(a, b, c, d, e, f, g, h);
        var s = SnapFor(8);
        s[0] = a; s[1] = b; s[2] = c; s[3] = d; s[4] = e; s[5] = f; s[6] = g; s[7] = h;
        return CaptureN(8, a);
    }

    /// <summary>(VALUES ...) of more than eight values where a value mode applies.</summary>
    public static LispObject ValuesArrayMode(LispObject[] vals, int mode)
    {
        if (mode == ModePrimary) return PrimaryOnly(vals[0]);
        if (mode != ModeCapture) return Values(vals);
        Array.Copy(vals, SnapFor(vals.Length), vals.Length);
        return CaptureN(vals.Length, vals[0]);
    }

    /// <summary>VALUES-LIST where a value mode applies.</summary>
    public static LispObject ValuesListMode(LispObject list, int mode)
    {
        int n = 0;
        var cur = list;
        while (cur is Cons c) { n++; cur = c.Cdr; }
        if (cur is not Nil)
            throw new LispErrorException(new LispTypeError("VALUES-LIST: not a proper list", list));
        if (n == 0)
        {
            if (mode == ModePrimary) return PrimaryOnly(Nil.Instance);
            _values = Array.Empty<LispObject>();
            _count = 0;
            return Nil.Instance;
        }
        var first = ((Cons)list).Car;
        if (mode == ModePrimary || n == 1) return PrimaryOnly(first);
        var s = SnapFor(n);
        cur = list;
        for (int i = 0; i < n; i++) { var c = (Cons)cur; s[i] = c.Car; cur = c.Cdr; }
        return CaptureN(n, first);
    }

    /// <summary>MULTIPLE-VALUE-CALL of FN on the values CAPTUREFORBIND has just put in
    /// the bind snapshot, with the caller's value mode.</summary>
    public static LispObject CallBound(LispObject fn, int mode)
    {
        var s = _bindSnap;
        int n = _bindCount;
        LispObject a = Nil.Instance, b = Nil.Instance, c = Nil.Instance, d = Nil.Instance;
        LispObject[]? args = null;
        if (n > 4) { args = new LispObject[n]; Array.Copy(s!, args, n); }
        else
        {
            if (n > 0) a = s![0];
            if (n > 1) b = s![1];
            if (n > 2) c = s![2];
            if (n > 3) d = s![3];
        }
        var f = Runtime.CoerceToFunction(fn);
        return n switch
        {
            0 => f.Invoke0M(mode),
            1 => f.Invoke1M(a, mode),
            2 => f.Invoke2M(a, b, mode),
            3 => f.Invoke3M(a, b, c, mode),
            4 => f.Invoke4M(a, b, c, d, mode),
            _ => f.Invoke(args!),
        };
    }

    private static LispObject CaptureN(int n, LispObject a)
    {
        _bindCount = n;
        _count = CountSnap;
        return a;
    }

    /// <summary>The bind snapshot and its count when a capture-mode return put
    /// the values of the call whose primary value is PRIMARY there, consumed;
    /// otherwise null (and the state is left as it was).</summary>
    internal static LispObject[]? TakeSnap(LispObject primary, out int count)
    {
        if (_count == CountSnap)
        {
            var snap = _bindSnap!;
            if (ReferenceEquals(snap[0], primary))
            {
                count = _bindCount;
                _count = -1;
                return snap;
            }
        }
        count = 0;
        return null;
    }

    /// <summary>How many values the thread state holds for a call whose primary
    /// value is PRIMARY: the published count when the state is that call's (its
    /// first value is PRIMARY), 0 after (VALUES), and -1 when the state is not
    /// that call's -- the same test CAPTUREFORBIND makes. Read with OWNNTH,
    /// which copies nothing.</summary>
    internal static int OwnCount(LispObject primary)
    {
        MaterializeSnap();
        int c = _count;
        if (c > 0)
        {
            var vals = _values;
            if (vals == null) return ReferenceEquals(_pair0, primary) ? c : -1;
            return c <= vals.Length && ReferenceEquals(vals[0], primary) ? c : -1;
        }
        return c == 0 ? 0 : -1;
    }

    private static readonly LispObject[] s_heldInPrimary = new LispObject[1];

    /// <summary>The values of a call whose primary value is PRIMARY, for RESTOREHELD to
    /// publish again after code that may publish its own.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    internal static LispObject[]? Hold(LispObject primary)
    {
        if (primary is MvReturn) return s_heldInPrimary;
        if (_count == -1) return null;
        return HoldSlow(primary);
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static LispObject[]? HoldSlow(LispObject primary)
    {
        int n = OwnCount(primary);
        return n == 0 || n > 1 ? Get() : null;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    internal static void RestoreHeld(LispObject[]? held)
    {
        if (held == null) _count = -1;
        else if (!ReferenceEquals(held, s_heldInPrimary)) Set(held);
    }

    internal static LispObject OwnNth(int i)
    {
        var vals = _values;
        if (vals == null) return (i == 0 ? _pair0 : _pair1) ?? Nil.Instance;
        return vals[i] ?? Nil.Instance;
    }

    /// <summary>The values of the form whose value is PRIMARY: the MvReturn, or the
    /// thread state when it is that form's (OWNCOUNT's test).</summary>
    public static MvHeld HoldValues(LispObject primary)
    {
        MvHeld h = default;
        if (primary is MvReturn) { h.N = -2; return h; }
        int c = _count;
        LispObject[]? vals;
        if (c == CountSnap) { c = _bindCount; vals = _bindSnap!; }
        else vals = _values;
        if (c == 0) { h.N = 0; return h; }
        if (c < 0) { h.N = -1; return h; }
        if (vals == null)
        {
            if (!ReferenceEquals(_pair0, primary)) { h.N = -1; return h; }
            h.N = c; h.V0 = primary; h.V1 = c > 1 ? _pair1 : null;
            return h;
        }
        if (c > vals.Length || !ReferenceEquals(vals[0], primary)) { h.N = -1; return h; }
        h.N = c;
        if (c > 4)
        {
            // A snapshot or a longer buffer is reused; a published array of exactly C is not.
            if (vals.Length == c && _count != CountSnap) h.Rest = vals;
            else { h.Rest = new LispObject[c]; Array.Copy(vals, h.Rest, c); }
            return h;
        }
        h.V0 = primary;
        if (c > 1) h.V1 = vals[1];
        if (c > 2) h.V2 = vals[2];
        if (c > 3) h.V3 = vals[3];
        return h;
    }

    /// <summary>Publish H again and return the form's value, in the form MODE asks
    /// for (see TAKEMODE). -1: no mode applies, PRIMARY is returned as it is.</summary>
    public static LispObject RestoreValues(MvHeld h, LispObject primary, int mode)
    {
        int n = h.N;
        if (n == -2)
        {
            var mv = (MvReturn)primary;
            if (mode == ModePrimary) { _count = -1; return mv.PrimaryValue; }
            n = mv.Count;
            if (n > 2) { Set(mv.ToArray()); return primary; }
            if (n == 2) SetPair(mv[0], mv[1]);
            else if (n == 1) SetOne(mv[0]);
            else { _values = Array.Empty<LispObject>(); _count = 0; }
            return primary;
        }
        if (n == -1 || mode == ModePrimary) { _count = -1; return primary; }
        switch (n)
        {
            case 0: _values = Array.Empty<LispObject>(); _count = 0; break;
            case 1: SetOne(h.V0!); break;
            case 2: SetPair(h.V0!, h.V1!); break;
            case 3: case 4:
                var snap = SnapFor(n);
                snap[0] = h.V0!; snap[1] = h.V1!; snap[2] = h.V2!;
                if (n == 4) snap[3] = h.V3!;
                _bindCount = n;
                _count = CountSnap;
                break;
            default: Set(h.Rest!); break;
        }
        if (mode != ModeNone || n <= 1) return primary;
        // The caller passed no mode: the values leave in an MvReturn, as a tail VALUES does.
        return n switch
        {
            2 => Values2(h.V0!, h.V1!),
            3 => Values(h.V0!, h.V1!, h.V2!),
            4 => Values(h.V0!, h.V1!, h.V2!, h.V3!),
            _ => Values(h.Rest!),
        };
    }

    // Save/restore for unwind-protect: preserve body's secondary values across cleanup
    public static int SaveCount() { MaterializeSnap(); return _count; }
    /// <summary>The values to restore after an unwind-protect cleanup. A pair published
    /// without an array materialises one here: the cleanup can publish values of its own,
    /// and the fields would then hold those instead of the body.s.</summary>
    public static LispObject[]? SaveValues()
    {
        MaterializeSnap();
        return _values ?? (_count > 0 ? Get() : null);
    }
    public static void RestoreSaved(int savedCount, LispObject[]? savedValues)
    {
        _count = savedCount;
        _values = savedValues;
    }
}
