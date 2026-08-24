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

public static class MultipleValues
{
    [ThreadStatic]
    private static LispObject[]? _values;
    [ThreadStatic]
    private static int _count;
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
    /// build was 40 bytes on every TRUNCATE, FLOOR, GETHASH … and nothing reads it as
    /// an array unless MULTIPLE-VALUE-LIST or an unwind-protect asks.</summary>
    public static void SetPair(LispObject a, LispObject b)
    {
        _values = null;
        _pair0 = a;
        _pair1 = b;
        _count = 2;
    }

    public static LispObject[] Get()
    {
        if (_count <= 0) return Array.Empty<LispObject>();
        if (_values == null)
            return _count == 1 ? new[] { _pair0! } : new[] { _pair0!, _pair1! };
        var result = new LispObject[_count];
        Array.Copy(_values, result, _count);
        return result;
    }

    public static int Count => _count;

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
    /// TRUNCATE, FLOOR, ROUND, GETHASH, INTERN … return, so it is where the 40-byte
    /// array a multiple-value return used to carry is worth removing.</summary>
    public static LispObject Values2(LispObject a, LispObject b)
    {
        SetPair(a, b);
        return new MvReturn(a, b);
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
    [ThreadStatic] private static int _bindCount;

    /// <summary>Take the values just returned by FORM into the bind snapshot and
    /// answer the primary value. Mirrors MultipleValuesList1's normalisation:
    /// an MvReturn carries them directly, otherwise the thread state is only this
    /// call's when its count is set and its first value is the primary.</summary>
    public static LispObject CaptureForBind(LispObject primary)
    {
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
            snap = _bindSnap = new LispObject[count < 8 ? 8 : count];
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
            snap = _bindSnap = new LispObject[count < 8 ? 8 : count];
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

    public static void Reset()
    {
        _count = -1; // Sentinel: no explicit values call yet
        // Don't null _values — Get() checks _count first, saves a ThreadStatic write
    }

    // Save/restore for unwind-protect: preserve body's secondary values across cleanup
    public static int SaveCount() => _count;
    /// <summary>The values to restore after an unwind-protect cleanup. A pair published
    /// without an array materialises one here: the cleanup can publish values of its own,
    /// and the fields would then hold those instead of the body.s.</summary>
    public static LispObject[]? SaveValues() => _values ?? (_count > 0 ? Get() : null);
    public static void RestoreSaved(int savedCount, LispObject[]? savedValues)
    {
        _count = savedCount;
        _values = savedValues;
    }
}
