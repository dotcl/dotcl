using System.Collections.Concurrent;
using System.Runtime.CompilerServices;
using static DotCL.NumStorage;

namespace DotCL;

public class LispStruct : LispObject
{
    public Symbol TypeName { get; }

    /// <summary>The slot values, boxed, one array entry per slot.
    ///
    /// Private on purpose: every read and write goes through GetSlot / SetSlot /
    /// SlotCount below. While this is the only storage a structure has, those
    /// are one-line forwarders and the indirection buys nothing -- but a slot
    /// whose declared type has a machine representation does not want to live
    /// in a LispObject[] at all, and a field that 50 call sites index directly
    /// cannot grow a second storage behind it. The accessors are where that
    /// choice will be made once, instead of at each of the 50.</summary>
    private readonly LispObject[] _slots;

    /// <summary>Raw storage for the slots whose declared type fits an int64.
    /// Null when the structure has none, which is every structure that did not
    /// use :TYPE -- those pay nothing for this, not even a null field read on
    /// the fast paths, because the null check is the fast path's first test.
    ///
    /// Parallel to _slots rather than replacing it: _slots keeps its full
    /// length so a slot index means the same thing everywhere, and _layout says
    /// which entries live over here instead. An entry in _slots for a raw slot
    /// is never read.</summary>
    internal readonly long[]? _longs;

    /// <summary>Per-slot storage map, or null when every slot is boxed and the
    /// structure has never been redefined. Index by slot number: -1 = boxed
    /// (use _slots), >= 0 = the position in _longs. Shared by every instance of
    /// the structure type, so an instance carries one reference rather than a
    /// copy.
    ///
    /// Each entry is (VERSION &lt;&lt; 16) | POSITION, with POSITION = SlotBoxed
    /// meaning the slot lives in _slots. VERSION says which definition of the
    /// structure built this instance, and it is repeated in every entry rather
    /// than kept in a field of its own for two reasons: a field would grow
    /// every structure object by 8 bytes (four references fit an aligned 56, an
    /// int does not), and a reader that has to check the version has already
    /// loaded the entry -- the check is then a compare against a value in a
    /// register instead of a second load. Measured: separate storage cost the
    /// struct-slots kernel 5.58 -> 8.36 against C#, the packed form nothing.</summary>
    internal readonly int[]? _layout;

    /// <summary>A layout entry is (VERSION &lt;&lt; 16) | (KIND &lt;&lt; 15) |
    /// POSITION, with POSITION = SlotBoxed meaning "in _slots".
    ///
    /// Everything a raw read needs is in that one int, and the bits are laid
    /// out so the reader tests all of it at once: XOR the entry with the
    /// caller's version bits (and with SlotKindDouble when it wants a double)
    /// and what comes back is the position if every field agreed, and a number
    /// past the end of the raw array if any of them did not. One load, one
    /// XOR, one unsigned compare -- and that compare is the bounds check the
    /// array access needed anyway.</summary>
    internal const int SlotPosMask = 0x7FFF;

    /// <summary>The POSITION field of a layout entry that says "not raw".</summary>
    internal const int SlotBoxed = SlotPosMask;

    /// <summary>The KIND bit of a layout entry: set for a raw double.</summary>
    internal const int SlotKindDouble = 0x8000;

    /// <summary>Where the layout version sits in a layout entry, and in the
    /// packed slot constant a call site emits.</summary>
    internal const int LayoutVersionShift = 16;

    /// <summary>The version field of a layout entry or a packed constant.</summary>
    internal const int LayoutVersionMask = ~0xFFFF;

    /// <summary>A double lives in the same long[] as its IEEE bits rather than
    /// in an array of its own. One raw array means one field on every structure
    /// instead of two, and one bounds-checked access rather than a choice
    /// between two arrays on the hot path; the conversion is
    /// BitConverter.Int64BitsToDouble, which is a register move, not a call.
    /// Which of the two a slot holds is a bit of its layout entry.</summary>
    internal const byte RawLong = 0;
    internal const byte RawDouble = 1;
    internal const byte RawSingle = 2;

    /// <summary>The bit of a layout entry that marks a raw single-float: the
    /// sign bit, which the version (15 bits from bit 16) never reaches. Its
    /// bits sit in the low 32 bits of the long. A reader expecting another kind
    /// XORs it into a negative number, which its unsigned range test rejects,
    /// so it falls back to the boxed path the way a kind mismatch always has.</summary>
    internal const int SlotKindSingle = unchecked((int)0x80000000);

    internal const byte RawU64 = 3;

    /// <summary>Both kind bits: a raw (unsigned-byte 64), its bits in the
    /// long. A reader expecting any other kind is left with one of the two
    /// bits set, which its range test rejects.</summary>
    internal const int SlotKindU64 = SlotKindDouble | SlotKindSingle;

    public int SlotCount => _slots.Length;

    /// <summary>The most slots an instance keeps in its own fields (LispStruct1..4)
    /// instead of an array. Only a structure with no layout entry (no raw slot,
    /// never redefined) is built that way; _slots is then a shared array that
    /// only carries the count.</summary>
    internal const int MaxInlineSlots = 4;

    private static readonly LispObject[][] s_inlineCounts =
        { new LispObject[0], new LispObject[1], new LispObject[2], new LispObject[3], new LispObject[4] };

    private protected LispStruct(Symbol typeName, int inlineCount)
    {
        TypeName = typeName;
        _slots = s_inlineCounts[inlineCount];
        DotCL.Diagnostics.AllocCounter.Inc("LispStruct");
    }

    /// <summary>Whether this is a LispStruct1..4 (boxed slots in fields).</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool IsInline() => GetType() != typeof(LispStruct) && GetType() != typeof(LispStructR);

    /// <summary>Whether this is a LispStructR (raw slots in fields).</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal bool IsRawInline() => GetType() == typeof(LispStructR);

    /// <summary>The most raw slots an instance keeps in its own fields
    /// (LispStructR) instead of a long[].</summary>
    internal const int MaxInlineRawSlots = 4;

    /// <summary>The representation bit of LispStructR in a mask (IsKnown).</summary>
    internal const int RawInlineMask = 32;

    /// <summary>False if the runtime does not lay out _r0.._r3 contiguously, in
    /// which case no LispStructR instance is ever built.</summary>
    private static readonly bool s_rawInlineOk = CheckRawInlineLayout();

    private static bool CheckRawInlineLayout()
    {
        var r = new LispStructR(null!, s_inlineCounts[0], Array.Empty<int>());
        return Unsafe.ByteOffset(ref r._r0, ref r._r1) == (IntPtr)8
               && Unsafe.ByteOffset(ref r._r0, ref r._r2) == (IntPtr)16
               && Unsafe.ByteOffset(ref r._r0, ref r._r3) == (IntPtr)24;
    }

    /// <summary>Whether OBJ is a LispStructR and MASK allows one.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool IsRawInline(LispObject? obj, int mask)
        => (mask & RawInlineMask) != 0 && obj is not null && obj.GetType() == typeof(LispStructR);

    /// <summary>The raw cell at POS (below MaxInlineRawSlots) of a LispStructR.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref long RawInlineCell(LispObject s, int pos)
        => ref Unsafe.Add(ref Unsafe.As<LispStructR>(s)._r0, pos);

    /// <summary>OBJ as a LispStructR, when MASK allows one, whose slot PACKED is
    /// raw of KIND at the version PACKED carries; the position of its cell in POS.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool TryRawInline(LispObject? obj, int mask, int packed, int kind, out int pos)
    {
        if (IsRawInline(obj, mask))
        {
            var layout = Unsafe.As<LispStruct>(obj)!._layout!;
            int idx = packed & SlotPosMaskIndex;
            if ((uint)idx < (uint)layout.Length)
            {
                pos = layout[idx] ^ (packed & LayoutVersionMask) ^ kind;
                if ((uint)pos < (uint)MaxInlineRawSlots) return true;
            }
        }
        pos = 0;
        return false;
    }

    private const int SlotPosMaskIndex = 0xFFFF;

    /// <summary>The storage of raw slot POS, wherever this instance keeps it.</summary>
    private ref long RawCell(int pos)
    {
        var longs = _longs;
        if (longs != null) return ref longs[pos];
        if (!IsRawInline() || (uint)pos >= (uint)MaxInlineRawSlots)
            throw new InvalidOperationException("structure has no raw storage at " + pos);
        return ref RawInlineCell(this, pos);
    }

    /// <summary>The number of raw slots in a layout, when an instance of it is
    /// built as a LispStructR; else 0.</summary>
    internal static int RawInlineCountOf(int[] map)
    {
        if (!s_rawInlineOk) return 0;
        int k = 0;
        foreach (var e in map) if ((e & SlotPosMask) != SlotBoxed) k++;
        return k <= MaxInlineRawSlots ? k : 0;
    }

    private static bool RawFits(int entry, LispObject v)
        => (entry & SlotKindU64) == SlotKindU64 ? Runtime.IsU64(v)
            : (entry & SlotKindDouble) != 0 ? v is DoubleFloat
            : entry < 0 ? v is SingleFloat
            : v is Fixnum;

    private static long RawBitsOf(int entry, LispObject v)
        => (entry & SlotKindU64) == SlotKindU64 ? Runtime.Low64(v)
            : (entry & SlotKindDouble) != 0 ? BitConverter.DoubleToInt64Bits(((DoubleFloat)v).Value)
            : entry < 0 ? Compat.SingleToInt32Bits(((SingleFloat)v).Value)
            : ((Fixnum)v).Value;

    /// <summary>A LispStructR of TYPENAME from the first N of A..D, when every
    /// slot of its layout is raw and every value fits; else null.</summary>
    internal static LispStruct? TryCreateAllRaw(Symbol typeName, int n, LispObject a, LispObject b,
                                                LispObject c, LispObject d)
    {
        if (typeName.StructLayout is not { } map || map.Length != n || RawInlineCountOf(map) != n)
            return null;
        if (!RawFits(map[0], a) || (n > 1 && !RawFits(map[1], b))
            || (n > 2 && !RawFits(map[2], c)) || (n > 3 && !RawFits(map[3], d)))
            return null;
        var s = new LispStructR(typeName, NoBoxedSlots(n), map);
        RawInlineCell(s, map[0] & SlotPosMask) = RawBitsOf(map[0], a);
        if (n > 1) RawInlineCell(s, map[1] & SlotPosMask) = RawBitsOf(map[1], b);
        if (n > 2) RawInlineCell(s, map[2] & SlotPosMask) = RawBitsOf(map[2], c);
        if (n > 3) RawInlineCell(s, map[3] & SlotPosMask) = RawBitsOf(map[3], d);
        return s;
    }

    /// <summary>A LispStructR of TYPENAME holding SLOTS under MAP, or null when
    /// a value does not fit its raw slot (the array form keeps it boxed).</summary>
    private static LispStruct? TryCreateRawInline(Symbol typeName, LispObject[] slots, int[] map)
    {
        int k = RawInlineCountOf(map);
        if (k == 0) return null;
        for (int i = 0; i < slots.Length; i++)
        {
            int entry = map[i];
            if ((entry & SlotPosMask) == SlotBoxed) continue;
            if (!RawFits(entry, slots[i])) return null;
        }
        var s = new LispStructR(typeName, k == slots.Length ? NoBoxedSlots(slots.Length) : slots, map);
        for (int i = 0; i < slots.Length; i++)
        {
            int entry = map[i];
            int pos = entry & SlotPosMask;
            if (pos == SlotBoxed) continue;
            RawInlineCell(s, pos) = RawBitsOf(entry, slots[i]);
        }
        return s;
    }

    /// <summary>A LispStructR built by compiled code from raw values R0..R3 (by raw
    /// position), or null when TYPENAME's registered layout is not version VERSION.</summary>
    internal static LispStruct? TryMakeRawInline(Symbol typeName, LispObject[]? slots, long r0, long r1,
                                                  long r2, long r3, int version, int n, int nraw)
    {
        if (!s_rawInlineOk || nraw < 1 || nraw > MaxInlineRawSlots
            || typeName.StructLayout is not { } map || map.Length != n || n == 0
            || ((map[0] >> LayoutVersionShift) & SlotPosMask) != (version & SlotPosMask))
            return null;
        return new LispStructR(typeName, slots ?? NoBoxedSlots(n), map)
            { _r0 = r0, _r1 = r1, _r2 = r2, _r3 = r3 };
    }

    /// <summary>Whether OBJ is a structure, and OBJ as one. Use this rather than
    /// `is LispStruct`: on a class that has subclasses that is a runtime call
    /// whenever the object is not exactly a LispStruct, which on the hot paths is
    /// every inline instance and every non-structure.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool TryAs(LispObject? obj, out LispStruct s)
    {
        // Each test spelled as obj.GetType() == typeof(...): that is the shape the
        // JIT folds to a compare of the method table.
        if (obj is not null
            && (obj.GetType() == typeof(LispStruct) || obj.GetType() == typeof(LispStruct1)
                || obj.GetType() == typeof(LispStruct2) || obj.GetType() == typeof(LispStruct3)
                || obj.GetType() == typeof(LispStruct4) || obj.GetType() == typeof(LispStructR)))
        {
            s = Unsafe.As<LispStruct>(obj);
            return true;
        }
        s = null!;
        return false;
    }

    /// <summary>TryAs for an array-backed instance only (the only kind with raw
    /// slots): one compare, and small enough to stay inlined in large callers.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool TryAsArrayBacked(LispObject? obj, out LispStruct s)
    {
        if (obj is not null && obj.GetType() == typeof(LispStruct))
        {
            s = Unsafe.As<LispStruct>(obj);
            return true;
        }
        s = null!;
        return false;
    }

    /// <summary>The type name of OBJ when it is a structure, else null: the
    /// compiled inline TYPEP.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Symbol? TypeNameOf(LispObject? obj) => TryAs(obj, out var s) ? s.TypeName : null;

    /// <summary>Type name of OBJ when exactly array-backed, else the general test.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Symbol? TypeNameOfStruct(LispObject? obj)
        => obj is not null && obj.GetType() == typeof(LispStruct)
            ? Unsafe.As<LispStruct>(obj).TypeName : TypeNameOf(obj);

    /// <summary>Type name of OBJ when exactly a LispStructR, else the general test.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Symbol? TypeNameOfRawInline(LispObject? obj)
        => obj is not null && obj.GetType() == typeof(LispStructR)
            ? Unsafe.As<LispStruct>(obj).TypeName : TypeNameOf(obj);

    /// <summary>Whether OBJ has one of the representations in MASK: bit 0 is
    /// exact LispStruct, bit N is LispStructN, bit 5 is LispStructR.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool IsKnown(LispObject? obj, int mask, out LispStruct s)
    {
        if (obj is not null)
        {
            if ((mask & 2) != 0 && obj.GetType() == typeof(LispStruct1)) { s = Unsafe.As<LispStruct>(obj); return true; }
            if ((mask & 4) != 0 && obj.GetType() == typeof(LispStruct2)) { s = Unsafe.As<LispStruct>(obj); return true; }
            if ((mask & 8) != 0 && obj.GetType() == typeof(LispStruct3)) { s = Unsafe.As<LispStruct>(obj); return true; }
            if ((mask & 16) != 0 && obj.GetType() == typeof(LispStruct4)) { s = Unsafe.As<LispStruct>(obj); return true; }
            if ((mask & 1) != 0 && obj.GetType() == typeof(LispStruct)) { s = Unsafe.As<LispStruct>(obj); return true; }
            if ((mask & RawInlineMask) != 0 && obj.GetType() == typeof(LispStructR)) { s = Unsafe.As<LispStruct>(obj); return true; }
        }
        s = null!;
        return false;
    }

    /// <summary>The type name of OBJ when its representation is in MASK, else
    /// the general test.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Symbol? TypeNameOfKnown(LispObject? obj, int mask)
        => IsKnown(obj, mask, out var s) ? s.TypeName : TypeNameOf(obj);

    /// <summary>Whether this inline instance has slot INDEX, from its type alone:
    /// with INDEX a constant at the call site this is at most two compares of a
    /// method table already loaded, and no load of the count.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool HasInlineSlot(int index) => index switch
    {
        0 => true,
        1 => GetType() != typeof(LispStruct1),
        2 => GetType() == typeof(LispStruct3) || GetType() == typeof(LispStruct4),
        3 => GetType() == typeof(LispStruct4),
        _ => false,
    };

    /// <summary>Slot INDEX of an inline instance; INDEX is below SlotCount.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private LispObject InlineGet(int index) => index switch
    {
        0 => Unsafe.As<LispStruct1>(this)._s0,
        1 => Unsafe.As<LispStruct2>(this)._s1,
        2 => Unsafe.As<LispStruct3>(this)._s2,
        _ => Unsafe.As<LispStruct4>(this)._s3,
    };

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void InlineSet(int index, LispObject value)
    {
        switch (index)
        {
            case 0: Unsafe.As<LispStruct1>(this)._s0 = value; break;
            case 1: Unsafe.As<LispStruct2>(this)._s1 = value; break;
            case 2: Unsafe.As<LispStruct3>(this)._s2 = value; break;
            default: Unsafe.As<LispStruct4>(this)._s3 = value; break;
        }
    }

    /// <summary>A new structure of TYPENAME holding SLOTS (handed over, as to the
    /// public constructor): in its own fields when it is short and its type has
    /// no layout entry, else exactly what the constructor builds.</summary>
    internal static LispStruct Create(Symbol typeName, LispObject[] slots)
    {
        int n = slots.Length;
        if (typeName.StructLayout is { } registered && registered.Length == n)
            return TryCreateRawInline(typeName, slots, registered) ?? new LispStruct(typeName, slots);
        if (n == 0 || n > MaxInlineSlots)
            return new LispStruct(typeName, slots);
        return NewInline(typeName, slots);
    }

    /// <summary>COPY-STRUCTURE: an instance of SOURCE's shape and layout.</summary>
    internal static LispStruct Copy(LispStruct source)
    {
        if (source.IsInline())
            return source._slots.Length switch
            {
                1 => new LispStruct1(source.TypeName, source.InlineGet(0)),
                2 => new LispStruct2(source.TypeName, source.InlineGet(0), source.InlineGet(1)),
                3 => new LispStruct3(source.TypeName, source.InlineGet(0), source.InlineGet(1),
                                     source.InlineGet(2)),
                _ => new LispStruct4(source.TypeName, source.InlineGet(0), source.InlineGet(1),
                                     source.InlineGet(2), source.InlineGet(3)),
            };
        if (source.IsRawInline())
        {
            var map = source._layout!;
            var boxed = source._slots;
            if (RawInlineCountOf(map) != boxed.Length)
            {
                var b = new LispObject[boxed.Length];
                Array.Copy(boxed, b, boxed.Length);
                boxed = b;
            }
            var r = Unsafe.As<LispStructR>(source);
            return new LispStructR(source.TypeName, boxed, map)
                { _r0 = r._r0, _r1 = r._r1, _r2 = r._r2, _r3 = r._r3 };
        }
        // Array.Clone goes through MemberwiseClone, a runtime call that reads the
        // array's type at run time; allocating and copying is the same work.
        var src = source.SlotsSnapshot();
        var dst = new LispObject[src.Length];
        Array.Copy(src, dst, src.Length);
        return new LispStruct(source, dst);
    }

    private static LispStruct NewInline(Symbol typeName, LispObject[] v) => v.Length switch
    {
        1 => new LispStruct1(typeName, v[0]),
        2 => new LispStruct2(typeName, v[0], v[1]),
        3 => new LispStruct3(typeName, v[0], v[1], v[2]),
        _ => new LispStruct4(typeName, v[0], v[1], v[2], v[3]),
    };

    /// <summary>Whether a 1- to 4-slot structure of TYPENAME is built inline
    /// (no layout entry), for the compiled constructors.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool BuildsInline(Symbol typeName) => typeName.StructLayout == null;

    public LispObject GetSlot(int index)
    {
        if (_layout != null)
        {
            int entry = _layout[index];
            int pos = entry & SlotPosMask;
            if (pos != SlotBoxed)
            {
                long bits = RawCell(pos);
                return (entry & SlotKindU64) == SlotKindU64
                    ? Runtime.BoxU64(bits)
                    : (entry & SlotKindDouble) != 0
                    ? new DoubleFloat(BitConverter.Int64BitsToDouble(bits))
                    : entry < 0
                    ? new SingleFloat(Compat.Int32BitsToSingle((int)bits))
                    : Fixnum.Make(bits);
            }
        }
        // An inline instance's _slots holds only nulls, so the array read is its
        // bounds check too, and a plain instance pays no type test.
        var v = _slots[index];
        return v is not null || !IsInline() ? v! : InlineGet(index);
    }

    public void SetSlot(int index, LispObject value)
    {
        if (_layout != null)
        {
            int entry = _layout[index];
            int pos = entry & SlotPosMask;
            if (pos != SlotBoxed)
            {
                // A raw slot holds a machine number. Anything else means the
                // declared type was violated somewhere the compiler did not
                // check (safety 0, or a path that predates the check), and the
                // boxed storage cannot take it because there is none -- so it
                // is reported rather than silently dropped.
                if ((entry & SlotKindU64) == SlotKindU64)
                {
                    if (!Runtime.IsU64(value))
                        throw new LispErrorException(new LispTypeError(
                            $"{TypeName}: slot {index} holds a raw (unsigned-byte 64), got {value}",
                            value, Runtime.SizedByte("UNSIGNED-BYTE", 64)));
                    RawCell(pos) = Runtime.Low64(value);
                    return;
                }
                if ((entry & SlotKindDouble) != 0)
                {
                    if (value is not DoubleFloat d)
                        throw new LispErrorException(new LispTypeError(
                            $"{TypeName}: slot {index} holds a raw double, got {value}",
                            value, Startup.Sym("DOUBLE-FLOAT")));
                    RawCell(pos) = BitConverter.DoubleToInt64Bits(d.Value);
                    return;
                }
                if (entry < 0)
                {
                    if (value is not SingleFloat sf)
                        throw new LispErrorException(new LispTypeError(
                            $"{TypeName}: slot {index} holds a raw single-float, got {value}",
                            value, Startup.Sym("SINGLE-FLOAT")));
                    RawCell(pos) = Compat.SingleToInt32Bits(sf.Value);
                    return;
                }
                if (value is not Fixnum f)
                    throw new LispErrorException(new LispTypeError(
                        $"{TypeName}: slot {index} holds a raw integer, got {value}",
                        value, Startup.Sym("FIXNUM")));
                RawCell(pos) = f.Value;
                return;
            }
        }
        var slots = _slots;
        if (!IsInline()) slots[index] = value;
        else if ((uint)index >= (uint)slots.Length) throw new IndexOutOfRangeException();
        else InlineSet(index, value);
    }

    /// <summary>The layout entry for slot INDEX: version and position in one
    /// int. A structure with no layout answers "boxed, version 0" -- which is
    /// exactly what a call site compiled against a structure that was never
    /// redefined expects, so the common case needs no extra test.
    ///
    /// Callers decode this once and pass the POSITION to the raw accessors
    /// below, instead of indexing the layout a second time per access the way
    /// the separate RawPos / GetSlotRaw pair did.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal int LayoutEntry(int index) => _layout == null ? SlotBoxed : _layout[index];

    /// <summary>The value of a boxed slot, when PACKED's version matches this
    /// instance's layout; else null and the caller takes its slow path.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal LispObject? BoxedSlotOrNull(int packed)
    {
        int idx = packed & 0xFFFF;
        if (IsInline())
            return (packed & 0x7FFF0000) == 0 && HasInlineSlot(idx) ? InlineGet(idx) : null;
        var layout = _layout;
        if (layout != null)
        {
            if ((uint)idx >= (uint)layout.Length) return null;
            int entry = layout[idx];
            if (((entry ^ packed) & 0x7FFF0000) != 0 || (entry & SlotPosMask) != SlotBoxed) return null;
        }
        else if ((packed & 0x7FFF0000) != 0) return null;
        var slots = _slots;
        return (uint)idx < (uint)slots.Length ? slots[idx] : null;
    }

    /// <summary>Store into a boxed slot, as BoxedSlotOrNull reads; false when
    /// the slot is raw or the version differs.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal bool TrySetBoxedSlot(int packed, LispObject value)
    {
        int idx = packed & 0xFFFF;
        if (IsInline())
        {
            if ((packed & 0x7FFF0000) != 0 || !HasInlineSlot(idx)) return false;
            InlineSet(idx, value);
            return true;
        }
        var layout = _layout;
        if (layout != null)
        {
            if ((uint)idx >= (uint)layout.Length) return false;
            int entry = layout[idx];
            if (((entry ^ packed) & 0x7FFF0000) != 0 || (entry & SlotPosMask) != SlotBoxed) return false;
        }
        else if ((packed & 0x7FFF0000) != 0) return false;
        var slots = _slots;
        if ((uint)idx >= (uint)slots.Length) return false;
        slots[idx] = value;
        return true;
    }

    /// <summary>The raw int64 at POS, when POS is a position in the raw array.
    /// POS comes from a layout entry XORed with what the caller expected, so
    /// anything the caller got wrong -- the version, the kind, or a slot that
    /// is not raw at all -- lands outside the array and answers false. The
    /// range test is the bounds check the read needed anyway.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal bool TryRawLong(int pos, out long value)
    {
        var longs = _longs;
        if (longs != null && (uint)pos < (uint)longs.Length)
        {
            value = longs[pos];
            return true;
        }
        value = 0;
        return false;
    }

    /// <summary>Store an int64 at POS, or answer false. See TryRawLong.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal bool TrySetRaw(int pos, long value)
    {
        var longs = _longs;
        if (longs != null && (uint)pos < (uint)longs.Length)
        {
            longs[pos] = value;
            return true;
        }
        return false;
    }

    /// <summary>RawLong / RawDouble / RawSingle for a slot the layout says is raw.</summary>
    internal byte RawKind(int index)
    {
        int entry = LayoutEntry(index);
        return (entry & SlotKindU64) == SlotKindU64 ? RawU64
            : (entry & SlotKindDouble) != 0 ? RawDouble : entry < 0 ? RawSingle : RawLong;
    }

    /// <summary>Which definition of the structure built this instance. 0 for a
    /// structure whose DEFSTRUCT has never been re-evaluated with a different
    /// slot layout, which is what a compiled call site assumes unless it says
    /// otherwise. Read off any entry, since every entry carries it; only the
    /// error path needs this, because the check itself compares the entry the
    /// reader already loaded.</summary>
    internal int LayoutVersion
        => _layout == null || _layout.Length == 0 ? 0 : (_layout[0] >> LayoutVersionShift) & SlotPosMask;

    /// <summary>The slot values as one array, for the callers that need to hand
    /// the whole set somewhere: PRINT, COPY-STRUCTURE, the FASL literal writer.
    /// Callers must treat it as a snapshot and not write through it -- writing
    /// goes through SetSlot, which is what a second storage would have to
    /// intercept.</summary>
    public LispObject[] SlotsSnapshot()
    {
        if (_layout == null && !IsInline()) return _slots;
        // A structure with raw or inline slots has no single array holding every value,
        // so the snapshot is built. The callers are the cold ones -- printing,
        // COPY-STRUCTURE, the FASL literal writer, EQUALP -- and they want the
        // values, not the storage.
        var all = new LispObject[_slots.Length];
        for (int i = 0; i < all.Length; i++) all[i] = GetSlot(i);
        return all;
    }

    // Intern cache for EQ-preserving FASL deserialization.
    // Uses WeakReference values so GC can collect structs no longer referenced elsewhere.
    private static readonly ConcurrentDictionary<string, WeakReference<LispStruct>> _internCache = new();

    /// <summary>Slot layouts by structure name, kept on the name
    /// (Symbol.StructLayout). A DEFSTRUCT with at least one slot whose declared
    /// type fits an int64 registers one at load time; every other structure
    /// reads a null layout.
    ///
    /// The lookup happens once per structure CREATED, not per slot accessed --
    /// the instance carries the layout reference afterwards. That is why the
    /// split lives in this constructor rather than in MakeStruct: a FASL
    /// literal, COPY-STRUCTURE and the reader all build structures through
    /// here, and a structure built by one of them has to have the same shape as
    /// one built by the constructor function.</summary>
    /// <summary>Record which slots of TYPENAME are raw, and which definition of
    /// TYPENAME this is. POS has one entry per slot: the position in the raw
    /// array, or -1 for a boxed slot. KIND says what the bits at that position
    /// mean. VERSION is folded into every entry.
    ///
    /// A structure with no raw slot at version 0 is removed rather than
    /// recorded: that is every structure in a program that uses no :TYPE and
    /// redefines nothing, and a null layout is what keeps its slot access down
    /// to one array read.</summary>
    /// <summary>The shape this had before layouts carried a version. Kept
    /// because a shipped FASL calls what it was compiled against.</summary>
    public static void RegisterLayout(Symbol typeName, int[] pos, byte[] kind)
        => RegisterLayout(typeName, pos, kind, 0);

    public static void RegisterLayout(Symbol typeName, int[] pos, byte[] kind, int version)
    {
        bool anyRaw = false;
        // A position has 15 bits and a version 15 more. Nothing real comes
        // close, and a structure that did would lose only the raw storage --
        // the slot falls back to the boxed path it had before any of this.
        foreach (var p in pos) if (p >= 0 && p < SlotBoxed) { anyRaw = true; break; }
        int ver = version & SlotPosMask;
        if (anyRaw || ver != 0)
        {
            var entries = new int[pos.Length];
            for (int i = 0; i < pos.Length; i++)
                entries[i] = (ver << LayoutVersionShift)
                             | (pos[i] >= 0 && pos[i] < SlotBoxed
                                ? pos[i] | (kind[i] == RawDouble ? SlotKindDouble
                                            : kind[i] == RawSingle ? SlotKindSingle
                                            : kind[i] == RawU64 ? SlotKindU64 : 0)
                                : SlotBoxed);
            typeName.StructLayout = entries;
        }
        else typeName.StructLayout = null;
    }

    /// <summary>A layout map with every slot boxed, carrying VERSION. Used when
    /// an instance cannot take the raw storage its type registered -- a value
    /// of the wrong type, stored where no check ran -- and the version still
    /// has to be remembered. Null at version 0, where a null layout says the
    /// same thing and costs less.</summary>
    private static int[]? BoxedMap(int slotCount, int version)
    {
        if (version == 0) return null;
        var m = new int[slotCount];
        int entry = (version << LayoutVersionShift) | SlotBoxed;
        for (int i = 0; i < slotCount; i++) m[i] = entry;
        return m;
    }

    public LispStruct(Symbol typeName, LispObject[] slots)
        : this(typeName, slots,
               typeName.StructLayout is { } registered
               && registered.Length == slots.Length ? registered : null)
    {
    }

    /// <summary>A copy of SOURCE holding VALUES, keeping SOURCE's layout.
    ///
    /// COPY-STRUCTURE of an instance built by an older definition has to stay
    /// that definition's shape. Rebuilding it against what the structure means
    /// NOW would leave the values at the positions the old definition gave them
    /// while the copy claimed the current version -- which is the silent misread
    /// the version exists to stop, reintroduced by the one operation that
    /// carries values from one instance to another.</summary>
    internal LispStruct(LispStruct source, LispObject[] values)
        : this(source.TypeName, values, source._layout)
    {
    }

    private LispStruct(Symbol typeName, LispObject[] slots, int[] map, long[] longs)
    {
        TypeName = typeName;
        _slots = slots;
        _layout = map;
        _longs = longs;
        DotCL.Diagnostics.AllocCounter.Inc("LispStruct");
    }

    private protected LispStruct(Symbol typeName, LispObject[] slots, int[] map, bool rawInline)
    {
        TypeName = typeName;
        _slots = slots;
        _layout = map;
        DotCL.Diagnostics.AllocCounter.Inc("LispStruct");
    }

    // One array of nulls per length, the _slots of a structure whose slots are all raw:
    // nothing reads or writes _slots at a raw slot.
    private static LispObject[]?[] s_noBoxedSlots = new LispObject[]?[8];

    private static LispObject[] NoBoxedSlots(int n)
    {
        var cache = s_noBoxedSlots;
        if (n < cache.Length && cache[n] is { } a) return a;
        var arr = new LispObject[n];
        if (n < cache.Length) cache[n] = arr;
        return arr;
    }

    /// <summary>A structure built by compiled code that computed its raw slots as raw
    /// values: RAWS holds them in raw position order, SLOTS the boxed ones (null at a
    /// raw slot, or SLOTS null when every slot is raw). KINDS has two bits per slot,
    /// the layout the call site was compiled against: 0 boxed, 1 integer, 2 double,
    /// 3 single-float. When the registered layout is still that VERSION the raw array
    /// becomes the storage; otherwise the values are boxed and laid out as now
    /// registered.</summary>
    internal static LispStruct MakeRaw(Symbol typeName, LispObject[]? slots, long[] raws,
                                       int version, long kinds, int n)
    {
        if (typeName.StructLayout is { } map && map.Length == n && n > 0
            && ((map[0] >> LayoutVersionShift) & SlotPosMask) == (version & SlotPosMask))
        {
            int k = RawInlineCountOf(map);
            if (k != 0 && k == raws.Length)
            {
                var inl = new LispStructR(typeName, slots ?? NoBoxedSlots(n), map);
                for (int i = 0; i < k; i++) RawInlineCell(inl, i) = raws[i];
                return inl;
            }
            return new LispStruct(typeName, slots ?? NoBoxedSlots(n), map, raws);
        }
        var boxed = new LispObject[n];
        int r = 0;
        for (int i = 0; i < n; i++)
        {
            switch ((int)((kinds >> (2 * i)) & 3))
            {
                case 0: boxed[i] = slots![i]; break;
                case 1: boxed[i] = Fixnum.Make(raws[r++]); break;
                case 2: boxed[i] = new DoubleFloat(BitConverter.Int64BitsToDouble(raws[r++])); break;
                default: boxed[i] = new SingleFloat(Compat.Int32BitsToSingle((int)raws[r++])); break;
            }
        }
        return Create(typeName, boxed);
    }

    private LispStruct(Symbol typeName, LispObject[] slots, int[]? map)
    {
        TypeName = typeName;
        _slots = slots;
        if (map != null)
        {
            _layout = map;
            int n = 0;
            for (int i = 0; i < slots.Length; i++) if ((map[i] & SlotPosMask) != SlotBoxed) n++;
            // A structure that is only here to carry a version has no raw
            // storage at all.
            _longs = n > 0 ? new long[n] : null;
            for (int i = 0; i < slots.Length; i++)
            {
                int entry = map[i];
                int pos = entry & SlotPosMask;
                if (pos == SlotBoxed) continue;
                // A slot value of the wrong type here means the declaration was
                // violated before the checks existed, or under safety 0. Keep
                // the value rather than lose it: the raw storage is dropped for
                // this instance and it behaves exactly as it did before.
                if ((entry & SlotKindU64) == SlotKindU64)
                {
                    if (!Runtime.IsU64(slots[i]))
                    {
                        _longs = null;
                        _layout = BoxedMap(slots.Length, (entry >> LayoutVersionShift) & SlotPosMask);
                        break;
                    }
                    _longs![pos] = Runtime.Low64(slots[i]);
                }
                else if ((entry & SlotKindDouble) != 0)
                {
                    if (slots[i] is not DoubleFloat d)
                    {
                        // Dropping the raw storage must not drop the version
                        // with it: the instance still came from this definition
                        // and accessors compiled against it are still right
                        // about where its slots are.
                        _longs = null;
                        _layout = BoxedMap(slots.Length, (entry >> LayoutVersionShift) & SlotPosMask);
                        break;
                    }
                    _longs![pos] = BitConverter.DoubleToInt64Bits(d.Value);
                }
                else if (entry < 0)
                {
                    if (slots[i] is not SingleFloat sf)
                    {
                        _longs = null;
                        _layout = BoxedMap(slots.Length, (entry >> LayoutVersionShift) & SlotPosMask);
                        break;
                    }
                    _longs![pos] = Compat.SingleToInt32Bits(sf.Value);
                }
                else
                {
                    if (slots[i] is not Fixnum f)
                    {
                        _longs = null;
                        _layout = BoxedMap(slots.Length, (entry >> LayoutVersionShift) & SlotPosMask);
                        break;
                    }
                    _longs![pos] = f.Value;
                }
            }
        }
        DotCL.Diagnostics.AllocCounter.Inc("LispStruct");
    }

    /// <summary>Forget the structures a module of this name interned (see
    /// LispInstance.EvictInternedModule).</summary>
    public static void EvictInternedModule(string moduleName)
    {
        var prefix = moduleName + ".";
        foreach (var k in _internCache.Keys)
            if (k.StartsWith(prefix, StringComparison.Ordinal)) _internCache.TryRemove(k, out _);
    }

    /// <summary>
    /// Intern a struct by content key for EQ preservation across FASL loads.
    /// Called from IL emitted by EmitLoadConstInline.
    /// </summary>
    public static LispObject Intern(string key, LispObject typeNameObj, LispObject[] slots)
    {
        if (_internCache.TryGetValue(key, out var weakRef) && weakRef.TryGetTarget(out var existing))
            return existing;
        var typeSym = (Symbol)typeNameObj;
        var result = Create(typeSym, slots);
        var newWeak = new WeakReference<LispStruct>(result);
        _internCache[key] = newWeak;
        return result;
    }

    /// <summary>
    /// Pre-register the original struct in the intern cache at compile time,
    /// so that same-process fasl loads return the original object (preserving EQ).
    /// </summary>
    public static void PreRegisterIntern(string key, LispStruct original)
    {
        _internCache.TryAdd(key, new WeakReference<LispStruct>(original));
    }

    /// <summary>
    /// Evaluate a make-load-form creation form at FASL load time, then intern
    /// the resulting struct by key for EQ preservation across loads.
    /// Called from IL emitted by the make-load-form protocol path in EmitLoadConstInline.
    /// </summary>
    public static LispObject InternViaEval(string key, LispObject form)
    {
        if (_internCache.TryGetValue(key, out var weakRef) && weakRef.TryGetTarget(out var existing))
            return existing;
        var obj = Runtime.TryCallConstantForm(form, out var called)
            ? Runtime.UnwrapMv(called) : Runtime.Eval(form);
        if (LispStruct.TryAs(obj, out var result))
        {
            _internCache[key] = new WeakReference<LispStruct>(result);
            return result;
        }
        return obj;
    }

    [ThreadStatic] private static HashSet<LispStruct>? _printing;

    public override string ToString()
    {
        _printing ??= new HashSet<LispStruct>(ReferenceEqualityComparer.Instance);
        if (!_printing.Add(this))
            return "#S(...)";
        try
        {
            var parts = new string[SlotCount];
            for (int i = 0; i < SlotCount; i++)
                parts[i] = GetSlot(i).ToString();
            if (parts.Length == 0)
                return $"#S({TypeName.Name})";
            return $"#S({TypeName.Name} {string.Join(" ", parts)})";
        }
        finally { _printing.Remove(this); }
    }
}

/// <summary>A structure of one to four slots, kept in these fields. Each level
/// adds one, so slot K is the same field whichever of them the instance is.</summary>
internal class LispStruct1 : LispStruct
{
    internal LispObject _s0;
    internal LispStruct1(Symbol typeName, LispObject a) : base(typeName, 1) { _s0 = a; }
    private protected LispStruct1(Symbol typeName, int n, LispObject a) : base(typeName, n) { _s0 = a; }
}

internal class LispStruct2 : LispStruct1
{
    internal LispObject _s1;
    internal LispStruct2(Symbol typeName, LispObject a, LispObject b) : base(typeName, 2, a) { _s1 = b; }
    private protected LispStruct2(Symbol typeName, int n, LispObject a, LispObject b) : base(typeName, n, a) { _s1 = b; }
}

internal class LispStruct3 : LispStruct2
{
    internal LispObject _s2;
    internal LispStruct3(Symbol typeName, LispObject a, LispObject b, LispObject c)
        : base(typeName, 3, a, b) { _s2 = c; }
    private protected LispStruct3(Symbol typeName, int n, LispObject a, LispObject b, LispObject c)
        : base(typeName, n, a, b) { _s2 = c; }
}

internal sealed class LispStruct4 : LispStruct3
{
    internal LispObject _s3;
    internal LispStruct4(Symbol typeName, LispObject a, LispObject b, LispObject c, LispObject d)
        : base(typeName, 4, a, b, c) { _s3 = d; }
}

/// <summary>A structure whose raw slots (one to four) are kept in these fields
/// instead of a long[]: raw position K is _rK. Boxed slots, if any, stay in
/// _slots.</summary>
internal sealed class LispStructR : LispStruct
{
    internal long _r0, _r1, _r2, _r3;
    internal LispStructR(Symbol typeName, LispObject[] slots, int[] map) : base(typeName, slots, map, true) { }
}

public sealed class LispVector : LispObject
{
    internal LispObject[] _elements;
    private int _fillPointer;
    private bool _hasFillPointer;
    private int _declaredSize; // for displaced arrays: the declared size (not backed by _elements)
    internal int[]? _dimensions; // null = 1D vector, non-null = multi-dimensional array

    // Displaced array support: when non-null, element access delegates to _displacedTo at _displacedOffset
    internal LispVector? _displacedTo;
    private int _displacedOffset;

    // Adjustable flag: set by make-array :adjustable t
    private bool _isAdjustable;

    // Packed bit storage: used when ElementTypeName == "BIT" and not displaced
    internal ulong[]? _bitData;

    // Unboxed numeric storage: used when the element type is a bounded integer
    // type that maps to a fixed-width backing (see NumKindForElementType) and
    // the array is not displaced. Cuts memory traffic (2000x2000 (unsigned-byte
    // 16) = 8MB of ushort vs 32MB of object references), removes the GC write
    // barrier per store and the Fixnum object per element. _numKind selects the
    // concrete array type of _numData.
    internal Array? _numData;   // byte[] | ushort[] | int[] | long[] | float[] | double[] | char[] | uint[] | sbyte[] | short[] | ulong[] per _numKind
    internal int _numLen;       // _numData.Length (Array.Length on the abstract
                                // static type is a runtime call, not ldlen: hot
                                // aref paths bounds-check against this instead)
    internal byte _numKind;    // 0=none 1=u8 2=u16 3=i32 4=i64 5=f4(float[]) 6=f8(double[]) 7=char[]
                               // 8=u32(uint[]) 9=s8(sbyte[]) 10=s16(short[]): foreign-width
                               // kinds, only made by DOTCL:MAKE-PINNED-VECTOR (see
                               // ForeignWidthKind); MAKE-ARRAY never picks them.
                               // 11=u64(ulong[]): (unsigned-byte 64), the value's bits.
                               // Not a raw kind (IsRawNumKind): a value at or above
                               // 2^63 does not fit the int64 the raw paths carry, so
                               // it has its own entries (U64Get/U64Set, Runtime.ArefU64*).

    // Element type: "T" (general), "CHARACTER"/"BASE-CHAR"/"STANDARD-CHAR" (string-like), "NIL" (bit vector of nil), etc.
    public string ElementTypeName
    {
        get => _elementTypeName;
        private set { _elementTypeName = value; _numNarrow = NarrowWidthFor(value); }
    }
    private string _elementTypeName = "T";

    // The element type's width when it is narrower than the storage it gets:
    // +N for (unsigned-byte N), -N for (signed-byte N), 0 when the storage's own
    // range is the element type's ((unsigned-byte 8) in a byte[], FIXNUM in a
    // long[], ...). (unsigned-byte 32) lives in a long[] and (signed-byte 8) in
    // an int[], so the storage alone would take values the element type does
    // not admit; NumSet checks this width before it stores.
    internal sbyte _numNarrow;

    /// <summary>The _numNarrow of element type name ET (see there).</summary>
    internal static sbyte NarrowWidthFor(string et)
    {
        const string ub = "UNSIGNED-BYTE-";
        const string sb = "SIGNED-BYTE-";
        if (et.StartsWith(ub, StringComparison.Ordinal)
            && TryParseWidth(et, ub.Length, out int un))
            return un >= 1 && un <= 63 && un != 8 && un != 16 ? (sbyte)un : (sbyte)0;
        if (et.StartsWith(sb, StringComparison.Ordinal)
            && TryParseWidth(et, sb.Length, out int sn))
            return sn >= 1 && sn <= 63 && sn != 32 ? (sbyte)(-sn) : (sbyte)0;
        return 0;
    }

    // The digits of ET from START. Runs for every vector made, so no substring.
    static bool TryParseWidth(string et, int start, out int n)
    {
        n = 0;
        if (start >= et.Length || et.Length - start > 9) return false;
        for (int i = start; i < et.Length; i++)
        {
            int d = et[i] - '0';
            if ((uint)d > 9) return false;
            n = n * 10 + d;
        }
        return true;
    }

    /// <summary>True when V is in the range of the narrow width W (+N unsigned,
    /// -N signed; see _numNarrow).</summary>
    internal static bool FitsNarrow(long v, sbyte w)
        => w > 0 ? ((ulong)v >> w) == 0
                 : (v >> (-w - 1)) is 0 or -1;

    /// <summary>Storage kind for a bounded-integer element type name, or 0 for
    /// none. Names arrive normalized from ParseElementTypeName ("UNSIGNED-BYTE-8",
    /// "SIGNED-BYTE-32", "FIXNUM", ...). (unsigned-byte 64) is kept as its bits
    /// in a ulong[] (kind 11); a value at or above 2^63 is boxed as a bignum when
    /// it is read through the generic paths.</summary>
    internal static byte NumKindForElementType(string et)
    {
        // Characters pack into a char[]: 2 bytes per element instead of an
        // 8-byte reference, and no LispChar per store. The win is the same one
        // the integer kinds get, and it is the storage a string-building
        // (vector-push-extend into an adjustable character vector) pays on
        // every character.
        if (et == "CHARACTER" || et == "BASE-CHAR" || et == "STANDARD-CHAR") return 7;
        if (et == "FIXNUM") return 4;
        if (et == "SINGLE-FLOAT") return 5;
        if (et == "DOUBLE-FLOAT") return 6;
        const string ub = "UNSIGNED-BYTE-";
        const string sb = "SIGNED-BYTE-";
        if (et.StartsWith(ub, StringComparison.Ordinal)
            && TryParseWidth(et, ub.Length, out int un) && un >= 1)
            return un <= 8 ? (byte)1 : un <= 16 ? (byte)2 : un <= 31 ? (byte)3
                 : un <= 63 ? (byte)4 : un == 64 ? (byte)11 : (byte)0;
        if (et.StartsWith(sb, StringComparison.Ordinal)
            && TryParseWidth(et, sb.Length, out int sn) && sn >= 1)
            return sn <= 32 ? (byte)3 : sn <= 64 ? (byte)4 : (byte)0;
        return 0;
    }

    private Array AllocNum(int size)
    {
        _numLen = size;
        return _numKind switch
        {
            1 => new byte[size],
            2 => new ushort[size],
            3 => new int[size],
            4 => new long[size],
            5 => new float[size],
            7 => new char[size],
            8 => new uint[size],
            9 => new sbyte[size],
            10 => new short[size],
            11 => new ulong[size],
            _ => new double[size],
        };
    }

    /// <summary>The storage kind whose element width is the one foreign code
    /// expects for ELEMENTTYPE, when that differs from what MAKE-ARRAY picks:
    /// (unsigned-byte 32) is kept in a long[] and (signed-byte 8/16) in an
    /// int[], which is right for Lisp but not for a buffer whose address is
    /// handed to C. 0 when NumKindForElementType's choice already has the
    /// foreign width. Used only for pinned vectors.</summary>
    internal static byte ForeignWidthKind(string et)
    {
        if (et == "UNSIGNED-BYTE-32") return 8;
        const string sb = "SIGNED-BYTE-";
        if (et.StartsWith(sb, StringComparison.Ordinal)
            && TryParseWidth(et, sb.Length, out int sn) && sn >= 1)
            return sn <= 8 ? (byte)9 : sn <= 16 ? (byte)10 : (byte)0;
        return 0;
    }

    /// <summary>A simple vector of ELEMENTTYPE whose numeric backing is STORAGE,
    /// an array the caller allocated (a pinned one, for DOTCL:MAKE-PINNED-VECTOR).
    /// STORAGE must be of StorageTypeFor(ELEMENTTYPE, FOREIGNWIDTH). Its contents
    /// become the vector's contents.</summary>
    internal static LispVector WithNumStorage(string elementType, Array storage, bool foreignWidth)
    {
        var v = new LispVector(0, Nil.Instance, elementType);
        if (v._numData == null || storage.GetType() != StorageTypeFor(elementType, foreignWidth))
            throw new InvalidOperationException(
                $"WithNumStorage: {storage.GetType()} is not the storage of element type {elementType}");
        byte fw = foreignWidth ? ForeignWidthKind(elementType) : (byte)0;
        if (fw != 0) v._numKind = fw;
        v._numData = storage;
        v._numLen = storage.Length;
        v._fillPointer = storage.Length;
        v._declaredSize = storage.Length;
        return v;
    }

    /// <summary>The concrete .NET array type a numeric vector of ELEMENTTYPE is
    /// stored in (with FOREIGNWIDTH, the foreign-width kind where there is one),
    /// or null when the element type has no numeric storage.</summary>
    internal static Type? StorageTypeFor(string elementType, bool foreignWidth)
    {
        byte kind = foreignWidth ? ForeignWidthKind(elementType) : (byte)0;
        if (kind == 0) kind = NumKindForElementType(elementType);
        return kind switch
        {
            0 => null,
            1 => typeof(byte[]), 2 => typeof(ushort[]), 3 => typeof(int[]),
            4 => typeof(long[]), 5 => typeof(float[]), 6 => typeof(double[]),
            7 => typeof(char[]), 8 => typeof(uint[]), 9 => typeof(sbyte[]),
            10 => typeof(short[]), _ => typeof(ulong[]),
        };
    }

    // True when the numeric backing holds floats (kind 5=float[], 6=double[])
    // rather than integers. Integer kinds cross the compiler boundary as a raw
    // long (NumGet/NumSet); float kinds as a raw double (NumGetF/NumSetF).
    internal bool IsFloatNumKind => _numKind == 5 || _numKind == 6;

    // True when the backing is one the raw long/double element paths understand.
    // Characters live in _numData too, but they are neither, so the compiler's
    // unboxed aref helpers must not touch them.
    // Kinds 1-10 except 7: as cheap as the test before kind 11 existed, which
    // matters because the per-element helpers (ArefNumL ...) inline it.
    internal bool IsRawNumKind => (uint)(_numKind - 1) < 10u && _numKind != 7;

    // (unsigned-byte 64) kept as bits in a ulong[]. Note that the CLR lets a
    // ulong[] pass an "is long[]" test, so code that picks the backing by its
    // .NET array type has to ask this first.
    internal bool IsU64Kind => _numKind == 11;

    // A pinned vector whose storage is a foreign-width kind (8-10). The
    // compiler's hoisted element buffers are typed per MAKE-ARRAY's storage
    // (long[] for (unsigned-byte 32), int[] for (signed-byte 8)), so their
    // fetches decline these and the per-element helpers serve them.
    internal bool IsForeignWidthKind => _numKind >= 8 && _numKind <= 10;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal char CharGet(int i) => ArrayCast<char>(_numData)[i];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void CharSet(int i, char c) => ArrayCast<char>(_numData)[i] = c;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal long NumGet(int i) => _numKind switch
    {
        1 => ArrayCast<byte>(_numData)[i],
        2 => ArrayCast<ushort>(_numData)[i],
        3 => ArrayCast<int>(_numData)[i],
        _ => ArrayAs<long>(_numData) is { } l ? l[i] : NumGetForeignWidth(i),
    };

    // Kinds 8-10 (pinned vectors only), kept off the inlined NumGet/NumSet.
    private long NumGetForeignWidth(int i) => _numKind switch
    {
        8 => ArrayCast<uint>(_numData)[i],
        9 => ArrayCast<sbyte>(_numData)[i],
        _ => ArrayCast<short>(_numData)[i],
    };

    private void NumSetForeignWidth(int i, long v)
    {
        switch (_numKind)
        {
            case 8:
                if ((ulong)v > uint.MaxValue) throw NumRangeError(v);
                ArrayCast<uint>(_numData)[i] = (uint)v; return;
            case 9:
                if (v < sbyte.MinValue || v > sbyte.MaxValue) throw NumRangeError(v);
                ArrayCast<sbyte>(_numData)[i] = (sbyte)v; return;
            default:
                if (v < short.MinValue || v > short.MaxValue) throw NumRangeError(v);
                ArrayCast<short>(_numData)[i] = (short)v; return;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void NumSet(int i, long v)
    {
        // Range-checked store: silently wrapping a value that exceeds the
        // storage width would corrupt data, so an element-type violation
        // (undefined behavior per CLHS) signals loudly instead.
        if (_numNarrow != 0 && !FitsNarrow(v, _numNarrow)) throw NumRangeError(v);
        switch (_numKind)
        {
            case 1:
                if ((ulong)v > byte.MaxValue) throw NumRangeError(v);
                ArrayCast<byte>(_numData)[i] = (byte)v; return;
            case 2:
                if ((ulong)v > ushort.MaxValue) throw NumRangeError(v);
                ArrayCast<ushort>(_numData)[i] = (ushort)v; return;
            case 3:
                if (v < int.MinValue || v > int.MaxValue) throw NumRangeError(v);
                ArrayCast<int>(_numData)[i] = (int)v; return;
            default:
                if (ArrayAs<long>(_numData) is { } l) l[i] = v;
                else NumSetForeignWidth(i, v);
                return;
        }
    }

    // Raw float element access (kind 5=float[] / 6=double[]). single-float
    // storage narrows the double to float on store; NaN/inf are legal values
    // so there is no range check (unlike the integer NumSet).
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal double NumGetF(int i) => _numKind == 5
        ? ArrayCast<float>(_numData)[i]
        : ArrayCast<double>(_numData)[i];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void NumSetF(int i, double v)
    {
        if (_numKind == 5) ArrayCast<float>(_numData)[i] = (float)v;
        else ArrayCast<double>(_numData)[i] = v;
    }

    // The bits of an (unsigned-byte 64) element (kind 11).
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal long U64Get(int i) => (long)ArrayCast<ulong>(_numData)[i];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void U64Set(int i, long bits) => ArrayCast<ulong>(_numData)[i] = (ulong)bits;

    // Box the element at index i per the backing kind: Fixnum for integer
    // kinds, SingleFloat/DoubleFloat for float kinds.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal LispObject NumBox(int i) => _numKind switch
    {
        7 => LispChar.Make(CharGet(i)),
        11 => Runtime.BoxU64(U64Get(i)),
        5 => new SingleFloat((float)NumGetF(i)),
        6 => new DoubleFloat(NumGetF(i)),
        _ => Fixnum.Make(NumGet(i)),
    };

    // Store a boxed value into numeric backing at (already bounds-checked)
    // index i, dispatching on the backing kind. Returns false when val's type
    // is incompatible with the backing so the caller can fall to the general
    // (boxed) slow path. Integer range violations still throw loudly via NumSet.
    internal bool TryNumStore(int i, LispObject val)
    {
        if (_numKind == 7)
        {
            if (val is LispChar lc) { CharSet(i, lc.Value); return true; }
            return false;
        }
        if (IsFloatNumKind)
        {
            // Only a float of the array's own format is an element of it. An
            // integer or the other format is a type error, as it is in SBCL;
            // converting it would store a value the program never computed.
            if (_numKind == 6)
            {
                if (val is DoubleFloat df) { NumSetF(i, df.Value); return true; }
                return false;
            }
            if (val is SingleFloat sf) { NumSetF(i, sf.Value); return true; }
            return false;
        }
        if (_numKind == 11)
        {
            if (!Runtime.IsU64(val)) return false;
            U64Set(i, Runtime.Low64(val));
            return true;
        }
        if (val is Fixnum nf) { NumSet(i, nf.Value); return true; }
        return false;
    }

    // TryNumStore for a store that has no other path to fall to: ADJUST-ARRAY's
    // :INITIAL-ELEMENT used to leave the new elements 0 when it was not an
    // element of the array.
    private void NumStoreOrThrow(int i, LispObject val)
    {
        if (!TryNumStore(i, val))
            throw new LispErrorException(new LispTypeError(
                $"array of element-type {ElementTypeName}: cannot store", val));
    }

    private Exception NumRangeError(long v) =>
        new LispErrorException(new LispTypeError(
            $"array of element-type {ElementTypeName}: value {v} does not fit", Fixnum.Make(v)));

    // Pack a boxed element array into numeric storage (mirror of PackBits).
    // C# null slots (uninitialized) become 0; any other non-fixnum is an
    // element-type violation and signals.
    private Array PackNum(LispObject[] elements)
    {
        _numData = AllocNum(elements.Length);
        for (int i = 0; i < elements.Length; i++)
        {
            var e = elements[i];
            if (e is null || e is Nil) continue; // uninitialized slot stays 0
            if (!TryNumStore(i, e))
                throw new LispErrorException(new LispTypeError(
                    $"array of element-type {ElementTypeName}: cannot store", e));
        }
        return _numData;
    }

    /// <summary>The multi-dimensional counterpart of the (size, initialElement,
    /// elementType) constructor: fills the packed storage directly instead of packing a
    /// boxed array the caller built. MAKE-ARRAY built a LispObject[SIZE] for every
    /// array, including the ones whose element type has narrow storage, and the
    /// constructor then packed it and dropped it: so a 200x200x200 (integer 0 1000)
    /// array allocated 64 MB of boxed references on the way to its 16 MB ushort[].
    /// </summary>
    public LispVector(int size, LispObject initialElement, string elementType, int[] dimensions)
        : this(size, initialElement, elementType)
    {
        _dimensions = dimensions;
    }

    /// <summary>What an element of general storage holds when no :initial-element
    /// was given. A (complex single-float) or (complex double-float) array starts
    /// out as complex zeros, the way a double-float array starts out as 0.0d0:
    /// an array of that element type must never hold NIL, and code that hands
    /// the storage to foreign routines as work space reads every element. A bit
    /// array's is the 0 bit, so a grown bit array stays a bit array.</summary>
    internal static LispObject DefaultElement(string elementType)
    {
        if (elementType == "BIT") return Fixnum.Make(0);
        return Runtime.ComplexElementPartClass(elementType) switch
        {
            'D' => LispComplex.OfDoubles(0.0, 0.0),
            'S' => LispComplex.Of(SingleFloat.Box(0f), SingleFloat.Box(0f)),
            _ => Nil.Instance
        };
    }

    public LispVector(int size, LispObject initialElement, string elementType)
    {
        ElementTypeName = elementType;
        if (elementType == "BIT")
        {
            _bitData = new ulong[(size + 63) / 64];
            _elements = Array.Empty<LispObject>();
            // NIL is "no :initial-element was given" (the storage is already zero);
            // anything else has to be a bit, like every other store into this array.
            if (initialElement is not Nil)
            {
                if (initialElement is not Fixnum f || (ulong)f.Value > 1UL)
                    throw BitTypeError(initialElement);
                if (f.Value == 1)
                    Compat.Fill(_bitData, ulong.MaxValue);
            }
        }
        else if ((_numKind = NumKindForElementType(elementType)) != 0)
        {
            _numData = AllocNum(size);
            _elements = Array.Empty<LispObject>();
            if (_numKind == 7)
            {
                if (initialElement is LispChar ic)
                {
                    if (ic.Value != '\0')
                        Compat.Fill((char[])_numData, ic.Value);
                }
                else if (initialElement is not Nil)
                    throw new LispErrorException(new LispTypeError(
                        $"array of element-type {elementType}: cannot store", initialElement));
            }
            else if (IsFloatNumKind)
            {
                if (size > 0 && initialElement is not Nil)
                {
                    if (!TryNumStore(0, initialElement))
                        throw new LispErrorException(new LispTypeError(
                            $"array of element-type {elementType}: cannot store", initialElement));
                    double d = NumGetF(0);
                    if (d != 0.0)
                        for (int i = 1; i < size; i++) NumSetF(i, d);
                }
            }
            else if (_numKind == 11)
            {
                if (size > 0 && initialElement is not Nil)
                {
                    if (!TryNumStore(0, initialElement))
                        throw new LispErrorException(new LispTypeError(
                            $"array of element-type {elementType}: cannot store", initialElement));
                    long bits = U64Get(0);
                    if (bits != 0)
                        for (int i = 1; i < size; i++) U64Set(i, bits);
                }
            }
            else if (initialElement is Fixnum nf)
            {
                if (nf.Value != 0)
                    for (int i = 0; i < size; i++) NumSet(i, nf.Value);
            }
            else if (initialElement is not Nil)
                throw new LispErrorException(new LispTypeError(
                    $"array of element-type {elementType}: cannot store", initialElement));
        }
        else
        {
            _elements = new LispObject[size];
            Compat.Fill(_elements, initialElement);
        }
        _fillPointer = size;
        _declaredSize = size;
        _hasFillPointer = false;
        DotCL.Diagnostics.AllocCounter.Inc("LispVector");
    }

    /// <summary>A simple vector of SIZE zero elements with LIKE's element type and
    /// numeric storage kind (1-6 or 11), without reading the type's name again.</summary>
    internal LispVector(int size, LispVector like)
    {
        _elementTypeName = like._elementTypeName;
        _numNarrow = like._numNarrow;
        _numKind = like._numKind;
        _numData = AllocNum(size);
        _elements = Array.Empty<LispObject>();
        _fillPointer = size;
        _declaredSize = size;
        _hasFillPointer = false;
        DotCL.Diagnostics.AllocCounter.Inc("LispVector");
    }

    // This exact signature (one parameter) is required by CilAssembler's hardcoded constructor lookup
    public LispVector(LispObject[] elements)
    {
        _elements = elements;
        _fillPointer = elements.Length;
        _declaredSize = elements.Length;
        _hasFillPointer = false;
        ElementTypeName = "T";
        DotCL.Diagnostics.AllocCounter.Inc("LispVector");
    }

    public LispVector(LispObject[] elements, string elementType)
    {
        ElementTypeName = elementType;
        if (elementType == "BIT")
        {
            _bitData = PackBits(elements);
            _elements = Array.Empty<LispObject>();
        }
        else if ((_numKind = NumKindForElementType(elementType)) != 0)
        {
            PackNum(elements);
            _elements = Array.Empty<LispObject>();
        }
        else
        {
            _elements = elements;
        }
        _fillPointer = elements.Length;
        _declaredSize = elements.Length;
        _hasFillPointer = false;
        DotCL.Diagnostics.AllocCounter.Inc("LispVector");
    }

    public LispVector(LispObject[] elements, int[] dimensions, string elementType)
    {
        ElementTypeName = elementType;
        _dimensions = dimensions;
        if (elementType == "BIT")
        {
            _bitData = PackBits(elements);
            _elements = Array.Empty<LispObject>();
        }
        else if ((_numKind = NumKindForElementType(elementType)) != 0)
        {
            PackNum(elements);
            _elements = Array.Empty<LispObject>();
        }
        else
        {
            _elements = elements;
        }
        _fillPointer = elements.Length;
        _declaredSize = elements.Length;
        _hasFillPointer = false;
        DotCL.Diagnostics.AllocCounter.Inc("LispVector");
    }

    /// <summary>The TYPE-ERROR a value that is not 0 or 1 gets from a bit array.</summary>
    private static Exception BitTypeError(LispObject val) =>
        new LispErrorException(new LispTypeError(
            "array of element-type BIT: value is not a bit", val, Startup.Sym("BIT")));

    /// <summary>The bits ADJUST-ARRAY gives the elements it adds: 0 when no
    /// :INITIAL-ELEMENT was supplied, otherwise 0 or 1.</summary>
    private static long BitFill(LispObject? initialElement)
    {
        if (initialElement is null) return 0;
        if (initialElement is not Fixnum f || (ulong)f.Value > 1UL)
            throw BitTypeError(initialElement);
        return f.Value;
    }

    /// <summary>The first Length elements of a BIT or (unsigned-byte 8) vector as
    /// bytes, bit I of a bit vector in bit I%8 of byte I/8. False for any other
    /// storage.</summary>
    internal bool TryPackLiteral(out byte[] bytes)
    {
        int len = Length;
        if (_displacedTo == null && _bitData != null && ElementTypeName == "BIT")
        {
            bytes = new byte[(len + 7) / 8];
            for (int i = 0; i < len; i++)
                if (((_bitData[i >> 6] >> (i & 63)) & 1) != 0)
                    bytes[i >> 3] |= (byte)(1 << (i & 7));
            return true;
        }
        if (_displacedTo == null && _numKind == 1 && ArrayAs<byte>(_numData) is { } raw && len <= raw.Length)
        {
            bytes = new byte[len];
            Buffer.BlockCopy(raw, 0, bytes, 0, len);
            return true;
        }
        bytes = Array.Empty<byte>();
        return false;
    }

    public static LispVector FromPackedLiteral(string base64, int length, string elementType) =>
        FromPackedBytes(Convert.FromBase64String(base64), length, elementType);

    internal static LispVector FromPackedBytes(byte[] bytes, int length, string elementType)
    {
        var v = new LispVector(length, Nil.Instance, elementType);
        if (v._bitData != null)
        {
            for (int i = 0; i < bytes.Length; i++)
                v._bitData[i >> 3] |= (ulong)bytes[i] << ((i & 7) * 8);
        }
        else
            Buffer.BlockCopy(bytes, 0, (byte[])v._numData!, 0, bytes.Length);
        return v;
    }

    private static ulong[] PackBits(LispObject[] elements)
    {
        int size = elements.Length;
        var data = new ulong[(size + 63) / 64];
        for (int i = 0; i < size; i++)
        {
            // A slot the caller never filled is C# null (MAKE-ARRAY with neither
            // :initial-element nor :initial-contents) and means 0; anything else has
            // to be a bit.
            var e = elements[i];
            if (e is null) continue;
            if (e is not Fixnum f || (ulong)f.Value > 1UL)
                throw BitTypeError(e);
            if (f.Value != 0)
                data[i >> 6] |= 1UL << (i & 63);
        }
        return data;
    }

    /// <summary>A CHARACTER vector whose storage IS CHARS (no copy). This is how a
    /// LispString becomes the target of a displaced array: the string's own char[]
    /// backing is shared, so a write through either object is seen by the other.
    /// The view itself is never handed to user code (ARRAY-DISPLACEMENT maps it
    /// back to the string).</summary>
    internal static LispVector CharView(char[] chars)
    {
        var v = new LispVector(0, Nil.Instance, "CHARACTER");
        v._numData = chars;
        v._numLen = chars.Length;
        v._fillPointer = chars.Length;
        v._declaredSize = chars.Length;
        return v;
    }

    // Constructor for displaced arrays (no local element storage)
    public LispVector(int size, LispVector displacedTo, int displacedOffset, string elementType, int[]? dimensions = null)
    {
        _elements = Array.Empty<LispObject>();
        _declaredSize = size;
        _fillPointer = size;
        _hasFillPointer = false;
        _displacedTo = displacedTo;
        _displacedOffset = displacedOffset;
        ElementTypeName = elementType;
        // For rank-0 or multi-dim displaced arrays, set explicit dimensions
        if (dimensions != null && dimensions.Length != 1)
            _dimensions = dimensions;
        DotCL.Diagnostics.AllocCounter.Inc("LispVector+Displaced");
    }

    // Returns true if this vector has a character element type (is a string)
    public bool IsCharVector => ElementTypeName is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR" or "NIL";
    public bool IsBaseCharVector => ElementTypeName is "BASE-CHAR" or "STANDARD-CHAR";

    // Returns true if this is a bit vector (element type BIT)
    public bool IsBitVector => ElementTypeName == "BIT";

    // Returns true if this vector has a fill pointer (not a simple array)
    public bool HasFillPointer => _hasFillPointer;

    // Returns true if this is a displaced array
    public bool IsDisplaced => _displacedTo != null;

    // Returns the displaced-to vector (or null if not displaced)
    public LispVector? DisplacedTo => _displacedTo;

    // Returns the displaced offset
    public int DisplacedOffset => _displacedOffset;

    // ADJUSTABLE-ARRAY-P. Like SBCL, every array that is not simple answers
    // true: one made with :adjustable t, one with a fill pointer, and a
    // displaced one. ADJUST-ARRAY changes all of them in place (same object),
    // so the answer is never a promise ADJUST-ARRAY cannot keep.
    public bool IsAdjustable { get => _isAdjustable || _hasFillPointer || _displacedTo != null; set => _isAdjustable = value; }

    // A simple array in the CLHS sense (1.4.4, glossary): not displaced, not
    // actually adjustable, and no fill pointer. Every SIMPLE-* type test
    // (SIMPLE-STRING-P, SIMPLE-VECTOR-P, TYPEP, TYPE-OF, COERCE) reads this one
    // definition; if two of them disagree, TYPEP rejects what TYPE-OF returned.
    public bool IsSimple => !_hasFillPointer && !_isAdjustable && _displacedTo == null;

    // Raw element get/set: handles displacement transparently
    private LispObject RawGet(int index)
    {
        if (_displacedTo != null) return _displacedTo.RawGet(_displacedOffset + index);
        if (_bitData != null)
            return Fixnum.Make((long)((_bitData[index >> 6] >> (index & 63)) & 1));
        if (_numData != null)
            return NumBox(index);
        return _elements[index] ?? Nil.Instance;
    }

    /// <summary>A numeric array of integers kept in a typed CLR array (not displaced; not a float,
    /// character or (unsigned-byte 64) array).</summary>
    internal bool IsPlainIntegerNum =>
        _displacedTo == null && _numData != null
        && _numKind != 0 && _numKind != 5 && _numKind != 6 && _numKind != 7 && _numKind != 11;

    private static int IdxOf<T>(T[] a, T v, int start, int n, bool fromEnd) where T : IEquatable<T>
        => fromEnd ? Array.LastIndexOf(a, v, start + n - 1, n) : Array.IndexOf(a, v, start, n);

    /// <summary>Index of the first (the last when FROMEND) element of [START, END) equal to the
    /// integer VALUE; -1 when there is none; -2 when this is not a plain integer array.</summary>
    internal int NumIndexOf(long value, int start, int end, bool fromEnd)
    {
        if (!IsPlainIntegerNum) return -2;
        int n = end - start;
        if (n <= 0) return -1;
        switch (_numKind)
        {
            case 1: return (ulong)value > byte.MaxValue ? -1 : IdxOf(ArrayCast<byte>(_numData), (byte)value, start, n, fromEnd);
            case 2: return (ulong)value > ushort.MaxValue ? -1 : IdxOf(ArrayCast<ushort>(_numData), (ushort)value, start, n, fromEnd);
            case 3: return value < int.MinValue || value > int.MaxValue ? -1 : IdxOf(ArrayCast<int>(_numData), (int)value, start, n, fromEnd);
            case 4: return ArrayAs<long>(_numData) is { } la ? IdxOf(la, value, start, n, fromEnd) : -2;
        }
        if (fromEnd) { for (int i = end - 1; i >= start; i--) if (NumGet(i) == value) return i; }
        else { for (int i = start; i < end; i++) if (NumGet(i) == value) return i; }
        return -1;
    }

    /// <summary>How many elements of [START, END) equal the integer VALUE; -1 when this is not a plain integer array.</summary>
    internal int NumCountOf(long value, int start, int end)
    {
        if (!IsPlainIntegerNum) return -1;
        int count = 0;
        switch (_numKind)
        {
            case 1: { if ((ulong)value > byte.MaxValue) return 0; var a = ArrayCast<byte>(_numData); byte v = (byte)value; for (int i = start; i < end; i++) if (a[i] == v) count++; return count; }
            case 2: { if ((ulong)value > ushort.MaxValue) return 0; var a = ArrayCast<ushort>(_numData); ushort v = (ushort)value; for (int i = start; i < end; i++) if (a[i] == v) count++; return count; }
            case 3: { if (value < int.MinValue || value > int.MaxValue) return 0; var a = ArrayCast<int>(_numData); int v = (int)value; for (int i = start; i < end; i++) if (a[i] == v) count++; return count; }
            case 4: { if (ArrayAs<long>(_numData) is not { } a) return -1; for (int i = start; i < end; i++) if (a[i] == value) count++; return count; }
        }
        for (int i = start; i < end; i++) if (NumGet(i) == value) count++;
        return count;
    }

    /// <summary>Index of the first (the last when FROMEND) element of [START, END) that is minus (OP 1),
    /// zero (OP 0) or plus (OP 2); -1 when none; -2 when this is not a plain integer array.</summary>
    internal int NumIndexWhereSign(int op, int start, int end, bool fromEnd)
    {
        if (!IsPlainIntegerNum) return -2;
        if (end <= start) return -1;
        // An unsigned array has no minus element.
        if (op == 1 && (_numKind == 1 || _numKind == 2 || _numKind == 8)) return -1;
        if (fromEnd)
        {
            for (int i = end - 1; i >= start; i--) { long x = NumGet(i); if (op == 1 ? x < 0 : op == 0 ? x == 0 : x > 0) return i; }
        }
        else
        {
            for (int i = start; i < end; i++) { long x = NumGet(i); if (op == 1 ? x < 0 : op == 0 ? x == 0 : x > 0) return i; }
        }
        return -1;
    }

    /// <summary>Store ITEM into elements [START, END) of a vector that keeps its elements as
    /// objects (not displaced, not a bit or numeric array); false for any other vector.</summary>
    internal bool TryFillObjects(int start, int end, LispObject item)
    {
        if (_displacedTo != null || _bitData != null || _numData != null || _elements == null) return false;
#if NETSTANDARD2_0
        var elements = _elements;
        for (int i = start; i < end; i++) elements[i] = item;
#else
        System.Array.Fill(_elements, item, start, end - start);
#endif
        return true;
    }

    private void RawSet(int index, LispObject val)
    {
        if (_displacedTo != null) { _displacedTo.RawSet(_displacedOffset + index, val); return; }
        if (_bitData != null)
        {
            // Only 0 and 1 are of type BIT. Folding everything non-zero to 1 (and
            // everything else to 0) stored a value the array cannot hold and said
            // nothing: (setf (aref bv 0) 7) left #*100 and (aref bv 0) then read 1.
            if (val is not Fixnum f || (ulong)f.Value > 1UL)
                throw BitTypeError(val);
            if (f.Value != 0)
                _bitData[index >> 6] |= 1UL << (index & 63);
            else
                _bitData[index >> 6] &= ~(1UL << (index & 63));
            return;
        }
        if (_numData != null)
        {
            if (!TryNumStore(index, val))
                throw new LispErrorException(new LispTypeError(
                    $"array of element-type {ElementTypeName}: cannot store", val));
            return;
        }
        _elements[index] = val;
    }

    // Extract string value from a character vector
    public string ToCharString()
    {
        var len = Length;
        // Packed character storage is already the string's contents.
        if (_numKind == 7 && _displacedTo == null && len <= _numLen)
            return new string((char[])_numData!, 0, len);
        var chars = new char[len];
        for (int i = 0; i < len; i++)
        {
            if (RawGet(i) is LispChar c)
                chars[i] = c.Value;
            // nil-element-type arrays have no actual char elements, return empty chars
        }
        return new string(chars);
    }

    // In-place char modification (for NSTRING-* on char vectors)
    public void ToUpperInPlace(int start, int end)
    {
        for (int i = start; i < end; i++)
        {
            var elem = RawGet(i);
            if (elem is LispChar c) RawSet(i, LispChar.Make(CharCase.Upcase(c.Value)));
        }
    }

    public void ToLowerInPlace(int start, int end)
    {
        for (int i = start; i < end; i++)
        {
            var elem = RawGet(i);
            if (elem is LispChar c) RawSet(i, LispChar.Make(CharCase.Downcase(c.Value)));
        }
    }

    public void ToCapitalizeInPlace(int start, int end)
    {
        bool wordBoundary = true;
        for (int i = start; i < end; i++)
        {
            if (RawGet(i) is not LispChar lc) continue;
            char c = lc.Value;
            if (char.IsLetter(c))
            {
                RawSet(i, LispChar.Make(wordBoundary ? CharCase.Upcase(c) : CharCase.Downcase(c)));
                wordBoundary = false;
            }
            else if (char.IsDigit(c))
                wordBoundary = false;
            else
                wordBoundary = true;
        }
    }

    // Access element regardless of fill pointer (for displaced arrays)
    // Null-safe: returns Nil.Instance if element is C# null (e.g., uninitialized slots)
    public LispObject ElementAt(int index) => RawGet(index);

    // Raw element access ignoring fill pointer (for CHAR/AREF which don't respect fill pointer)
    public LispObject GetElement(int index) => RawGet(index);
    public void SetElement(int index, LispObject val) => RawSet(index, val);

    public int Length => _hasFillPointer ? _fillPointer : _declaredSize;
    public int Rank => _dimensions?.Length ?? 1;
    // Dimensions returns actual declared size (not fill-pointer), per CL ARRAY-DIMENSIONS spec
    public int[] Dimensions => _dimensions ?? new[] { _declaredSize };
    public int Capacity => _displacedTo != null ? _declaredSize
        : _bitData != null ? _declaredSize
        : _numData != null ? _numLen
        : _elements.Length;

    public LispObject this[int index]
    {
        get
        {
            if (index < 0 || index >= Length)
                throw new IndexOutOfRangeException($"Index {index} out of bounds for vector of length {Length}");
            return RawGet(index);
        }
        set
        {
            if (index < 0 || index >= Length)
                throw new IndexOutOfRangeException($"Index {index} out of bounds for vector of length {Length}");
            RawSet(index, value);
        }
    }

    public void SetFillPointer(int fp)
    {
        _hasFillPointer = true;
        _fillPointer = fp;
    }

    // Adjust the array in-place (for adjustable arrays).
    // Resizes to newSize, copies existing elements, fills new slots with initialElement.
    // Optionally sets a new fill pointer.
    public void Adjust(int newSize, LispObject? initialElement, int[]? newDimensions, int? newFillPointer)
    {
        if (_displacedTo != null)
        {
            // Converting from displaced to non-displaced. A displaced array has
            // no own storage (_numKind stays 0), so derive the numeric kind from
            // the element type when acquiring storage here.
            byte kind = NumKindForElementType(ElementTypeName);
            if (ElementTypeName != "BIT" && kind != 0)
            {
                byte oldKind = _numKind; var oldNum = _numData; int oldLen = _numLen;
                _numKind = kind;
                _numData = AllocNum(newSize);
                var oldSizeN = _declaredSize;
                for (int i = 0; i < Math.Min(oldSizeN, newSize); i++)
                {
                    // Old storage is the displaced target: read through it.
                    var e = _displacedTo.RawGet(_displacedOffset + i);
                    TryNumStore(i, e); // int and float kinds; incompatible leaves 0
                }
                // A rejected :INITIAL-ELEMENT leaves the array as it was.
                try
                {
                    if (initialElement is not null && initialElement is not Nil)
                        for (int i = oldSizeN; i < newSize; i++) NumStoreOrThrow(i, initialElement);
                }
                catch { _numKind = oldKind; _numData = oldNum; _numLen = oldLen; throw; }
                _elements = Array.Empty<LispObject>();
            }
            else if (ElementTypeName == "BIT")
            {
                var newBits = new ulong[(newSize + 63) / 64];
                var oldSize = _declaredSize;
                for (int i = 0; i < Math.Min(oldSize, newSize); i++)
                {
                    var e = RawGet(i);
                    if (e is not Fixnum bf || (ulong)bf.Value > 1UL)
                        throw BitTypeError(e);
                    if (bf.Value != 0)
                        newBits[i >> 6] |= 1UL << (i & 63);
                }
                long fillBit = BitFill(initialElement);
                if (fillBit != 0)
                    for (int i = oldSize; i < newSize; i++)
                        newBits[i >> 6] |= 1UL << (i & 63);
                _bitData = newBits;
                _elements = Array.Empty<LispObject>();
                _numData = null;
                _numLen = 0;
                _numKind = 0;
            }
            else
            {
                var newElems = new LispObject[newSize];
                var oldSize = _declaredSize;
                LispObject fill = initialElement ?? DefaultElement(ElementTypeName);
                for (int i = 0; i < newSize; i++)
                    newElems[i] = i < oldSize ? RawGet(i) : fill;
                _elements = newElems;
            }
            _displacedTo = null;
            _displacedOffset = 0;
        }
        else if (_bitData != null)
        {
            var oldSize = _declaredSize;
            if (newSize != oldSize)
            {
                var newBits = new ulong[(newSize + 63) / 64];
                int copyWords = Math.Min(_bitData.Length, newBits.Length);
                Array.Copy(_bitData, newBits, copyWords);
                // Clear excess bits in last copied word if shrinking
                if (newSize < oldSize && newSize % 64 != 0)
                    newBits[newSize / 64] &= (1UL << (newSize % 64)) - 1;
                // New bits take the initial element. The last old word can hold
                // set bits past the old size (a constructor that fills whole
                // words), so each new bit is written, not only the 1s.
                long fillBit = BitFill(initialElement);
                for (int i = oldSize; i < newSize; i++)
                    if (fillBit != 0) newBits[i >> 6] |= 1UL << (i & 63);
                    else newBits[i >> 6] &= ~(1UL << (i & 63));
                _bitData = newBits;
            }
        }
        else if (_numData != null)
        {
            var oldSize = _numData.Length;
            if (newSize != oldSize)
            {
                var oldNum = _numData;
                int oldLen = _numLen;
                _numData = AllocNum(newSize);
                Array.Copy(oldNum, _numData, Math.Min(oldSize, newSize));
                // A rejected :INITIAL-ELEMENT leaves the array as it was.
                try
                {
                    if (initialElement is not null && initialElement is not Nil)
                        for (int i = oldSize; i < newSize; i++) NumStoreOrThrow(i, initialElement);
                }
                catch { _numData = oldNum; _numLen = oldLen; throw; }
            }
        }
        else
        {
            var oldSize = _elements.Length;
            if (newSize != oldSize)
            {
                var newElems = new LispObject[newSize];
                Array.Copy(_elements, newElems, Math.Min(oldSize, newSize));
                LispObject fill = initialElement ?? DefaultElement(ElementTypeName);
                for (int i = oldSize; i < newSize; i++) newElems[i] = fill;
                _elements = newElems;
            }
        }
        _declaredSize = newSize;
        if (newDimensions != null) _dimensions = newDimensions;
        if (newFillPointer.HasValue)
        {
            _hasFillPointer = true;
            _fillPointer = newFillPointer.Value;
        }
        else if (!_hasFillPointer)
        {
            _fillPointer = newSize;
        }
    }

    // Adjust with explicit elements array (for :initial-contents case)
    public void Adjust(int newSize, LispObject? initialElement, int[]? newDimensions, int? newFillPointer, LispObject[] newContents)
    {
        if (ElementTypeName == "BIT")
        {
            _bitData = PackBits(newContents);
            _elements = Array.Empty<LispObject>();
        }
        else if ((_numKind = NumKindForElementType(ElementTypeName)) != 0)
        {
            PackNum(newContents);
            _elements = Array.Empty<LispObject>();
        }
        else
        {
            _elements = newContents;
        }
        _displacedTo = null;
        _displacedOffset = 0;
        _declaredSize = newSize;
        if (newDimensions != null) _dimensions = newDimensions;
        if (newFillPointer.HasValue) { _hasFillPointer = true; _fillPointer = newFillPointer.Value; }
        else if (!_hasFillPointer) _fillPointer = newSize;
    }

    // Adjust to become a displaced array pointing at another vector
    public void AdjustToDisplaced(int newSize, LispVector displacedTo, int offset, string elementType, int[]? newDimensions, int? newFillPointer)
    {
        _elements = Array.Empty<LispObject>();
        _numData = null;
        _numLen = 0;
        _numKind = 0;
        _displacedTo = displacedTo;
        _displacedOffset = offset;
        _declaredSize = newSize;
        ElementTypeName = elementType;
        if (newDimensions != null) _dimensions = newDimensions;
        if (newFillPointer.HasValue) { _hasFillPointer = true; _fillPointer = newFillPointer.Value; }
        else if (!_hasFillPointer) _fillPointer = newSize;
    }

    // VECTOR-PUSH: push element, return fill-pointer before push (or NIL if no room)
    public LispObject VectorPushCL(LispObject element)
    {
        if (!_hasFillPointer)
            throw new LispErrorException(new LispError("VECTOR-PUSH: no fill pointer"));
        if (_fillPointer >= _declaredSize)
            return Nil.Instance; // no room
        int old = _fillPointer;
        RawSet(_fillPointer, element);
        _fillPointer++;
        return Fixnum.Make(old);
    }

    // VECTOR-PUSH-EXTEND: push element, extend if needed, return fill-pointer before push
    public int VectorPushExtend(LispObject element, int extension)
    {
        if (!_hasFillPointer)
            throw new LispErrorException(new LispError("VECTOR-PUSH-EXTEND: no fill pointer"));
        int fp = _fillPointer;
        if (fp >= _declaredSize)
        {
            // Extend
            int growth = Math.Max(extension, _declaredSize);
            if (growth < 1) growth = 1;
            int newSize = _declaredSize + growth;
            if (_displacedTo != null)
            {
                // Convert from displaced to own storage
                var newElems = new LispObject[newSize];
                for (int i = 0; i < _declaredSize; i++) newElems[i] = RawGet(i);
                _elements = newElems;
                _displacedTo = null;
                _displacedOffset = 0;
            }
            else if (_bitData != null)
            {
                var newBits = new ulong[(newSize + 63) / 64];
                int copyWords = Math.Min(_bitData.Length, newBits.Length);
                Array.Copy(_bitData, newBits, copyWords);
                _bitData = newBits;
            }
            else if (_numData != null)
            {
                var oldNum = _numData;
                _numData = AllocNum(newSize);
                Array.Copy(oldNum, _numData, oldNum.Length);
            }
            else
            {
                var newElems = new LispObject[newSize];
                Array.Copy(_elements, newElems, _elements.Length);
                _elements = newElems;
            }
            _declaredSize = newSize;
        }
        // Fast path: direct element write for non-displaced, non-bit, non-numeric arrays
        if (_displacedTo == null && _bitData == null && _numData == null)
            _elements[fp] = element;
        else
            RawSet(fp, element);
        _fillPointer = fp + 1;
        return fp;
    }

    public void VectorPush(LispObject element)
    {
        if (_displacedTo != null)
            throw new InvalidOperationException("VectorPush not supported on displaced arrays");
        if (_numData != null)
        {
            if (_fillPointer >= _numData.Length)
            {
                var oldNum = _numData;
                _numData = AllocNum(Math.Max(1, oldNum.Length * 2));
                Array.Copy(oldNum, _numData, oldNum.Length);
            }
            RawSet(_fillPointer++, element);
            _hasFillPointer = true;
            return;
        }
        if (_fillPointer >= _elements.Length)
        {
            var newElements = new LispObject[_elements.Length * 2];
            Array.Copy(_elements, newElements, _elements.Length);
            _elements = newElements;
        }
        _elements[_fillPointer++] = element;
        _hasFillPointer = true;
    }

    public override string ToString()
    {
        int rank = Rank;
        // Rank-0 arrays
        if (rank == 0)
        {
            var elem = _declaredSize > 0 ? RawGet(0) : Nil.Instance;
            return $"#0A{elem}";
        }
        if (IsBitVector)
        {
            if (rank == 1)
            {
                var sb = new System.Text.StringBuilder("#*");
                for (int i = 0; i < _declaredSize; i++)
                {
                    var elem = ElementAt(i);
                    sb.Append(elem is Fixnum f ? f.Value.ToString() : "0");
                }
                return sb.ToString();
            }
            // Multi-dimensional bit array
            return $"#A{FormatArrayContents(_dimensions!, 0, new int[rank], 0)}";
        }
        if (rank == 1)
        {
            var parts = new string[_declaredSize];
            for (int i = 0; i < _declaredSize; i++)
                parts[i] = RawGet(i).ToString();
            return $"#({string.Join(" ", parts)})";
        }
        // Multi-dimensional general array
        return $"#A{FormatArrayContents(_dimensions!, 0, new int[rank], 0)}";
    }

    private string FormatArrayContents(int[] dims, int dim, int[] indices, int baseOffset)
    {
        if (dim == dims.Length)
        {
            // Calculate linear index
            int idx = 0;
            int stride = 1;
            for (int d = dims.Length - 1; d >= 0; d--)
            {
                idx += indices[d] * stride;
                stride *= dims[d];
            }
            return RawGet(idx).ToString();
        }
        var parts = new string[dims[dim]];
        for (int i = 0; i < dims[dim]; i++)
        {
            indices[dim] = i;
            parts[i] = FormatArrayContents(dims, dim + 1, indices, baseOffset);
        }
        return $"({string.Join(" ", parts)})";
    }
}

public sealed class LispHashTable : LispObject
{
    // Storage. The dictionary KEY is:
    //   - strong modes (Weakness null or :VALUE): the LispObject key directly.
    //   - key-weak modes (:KEY, :KEY-AND-VALUE, :KEY-OR-VALUE): a WeakKeyBox that
    //     holds a WeakReference to the key plus a frozen hash code, so the dict
    //     does not strong-root the key (letting GC reclaim it).
    // The dictionary VALUE is the LispObject value directly, or a
    // WeakReference<LispObject> when the value side is weak (:VALUE,
    // :KEY-AND-VALUE, :KEY-OR-VALUE). Storing as `object` keeps all shapes in one
    // dictionary instance.
    private readonly Dictionary<object, object> _dict;
    private readonly bool _weakKey;    // key side is weak
    private readonly bool _weakValue;  // value side is weak
    // :KEY-OR-VALUE keeps an entry alive while EITHER side is live; all other
    // weak modes require ALL weak sides to be live (the default "and" semantics).
    private readonly bool _keyOrValue;
    private readonly Func<LispObject, LispObject, bool> _test;
    private readonly string _testName;
    // A table made with :HASH-FUNCTION and a test other than the four standard
    // ones: what HASH-TABLE-TEST answers, and the hash function.
    private sealed record UserTest(LispFunction? TestFn, LispObject? TestName, LispFunction Hash);
    private readonly UserTest? _user;

    private static string UserTestName(LispObject? name)
        => name is Symbol sym ? sym.Name : "USER-DEFINED";
    // Not inline in the constructor: a captured parameter there allocates its
    // closure object on every construction.
    private static Func<LispObject, LispObject, bool> UserTestPredicate(LispFunction test)
        => (a, b) =>
        {
            var r = Runtime.IsTruthy(test.Invoke2(a, b));
            MultipleValues.Reset();
            return r;
        };
    // When Synchronized, all mutating/reading operations take _lock.
    // Concurrent access without Synchronized is undefined per CLHS (mirrors
    // SBCL: make-hash-table :synchronized t opts in to thread-safety).
    private readonly object _lock = new();
    public bool Synchronized { get; }

    /// <summary>The :rehash-size and :rehash-threshold the table was made with, as
    /// given (HASH-TABLE-REHASH-SIZE and HASH-TABLE-REHASH-THRESHOLD return them).
    /// The table does not use them to size itself.</summary>
    public LispObject RehashSize { get; set; } = new SingleFloat(1.5f);
    /// <summary>The :size the table was made with, which HASH-TABLE-SIZE reports until the
    /// table holds more entries than that; -1 when none was given.</summary>
    public int DeclaredSize { get; set; } = -1;
    public LispObject RehashThreshold { get; set; } = new SingleFloat(1.0f);

    // Bumped after every store, removal and clear. Something that remembers what
    // it read from a table (the tree-walk evaluator's expansion cache reads the
    // compiler's *MACROS*) compares this instead of reading the table again.
    private int _version;
    public int Version => System.Threading.Volatile.Read(ref _version);
    /// <summary>
    /// Weakness mode (SBCL extension), one of NULL, ":VALUE", ":KEY",
    /// ":KEY-AND-VALUE", ":KEY-OR-VALUE". A weak side is stored behind a
    /// WeakReference (value) / WeakKeyBox (key) so the table does not keep the
    /// object alive; the entry surfaces as absent once the required side(s) die.
    /// </summary>
    public string? Weakness { get; }

    // Default-parameter constructor IS the unique declaration at the IL
    // level (3 params, with default values as metadata). CilAssembler emits
    // reflection lookup `GetConstructor(new[] { typeof(string) })` to find a
    // *1-arg* constructor, so we add the 1-arg and 2-arg overloads explicitly
    // so existing callers / emitted CIL continue to bind to a real ctor.
    public LispHashTable(string test) : this(test, false, null) { }
    public LispHashTable(string test, bool synchronized) : this(test, synchronized, null) { }
    public LispHashTable() : this("EQL", false, null) { }

    public LispHashTable(string test, bool synchronized, string? weakness)
        : this(test, synchronized, weakness, null, null, null) { }

    /// <summary>A table with a :HASH-FUNCTION (SBCL extension). TEST is one of
    /// the four standard tests by name, or null when TESTFN is any other
    /// two-argument predicate; TESTDESIGNATOR is what HASH-TABLE-TEST answers
    /// for it. HASHFN maps a key to an integer, and keys the test calls equal
    /// must get the same one.</summary>
    public LispHashTable(string? test, LispFunction? testFn, LispObject? testDesignator,
                         LispFunction hashFn, bool synchronized, string? weakness)
        : this(test ?? "", synchronized, weakness, testFn, testDesignator, hashFn) { }

    private LispHashTable(string test, bool synchronized, string? weakness,
                          LispFunction? userTest, LispObject? userTestName, LispFunction? userHash)
    {
        _testName = userTest != null ? UserTestName(userTestName) : test.ToUpperInvariant();
        if (userHash != null) _user = new UserTest(userTest, userTestName, userHash);
        _test = userTest != null
            ? UserTestPredicate(userTest)
            : _testName switch
        {
            "EQ" => (a, b) => Runtime.IsEqRef(a, b),
            "EQL" => Eql,
            "EQUAL" => LispEqual,
            "EQUALP" => Equalp,
            // (make-hash-table :test 'string=) is a common slip, so it has to
            // read as a bad argument (TYPE-ERROR naming the test) rather than
            // the PROGRAM-ERROR a raw ArgumentException maps to.
            _ => throw new LispErrorException(new LispTypeError(
                $"MAKE-HASH-TABLE: unknown :test {test} (expected EQ, EQL, EQUAL or EQUALP)",
                Startup.Sym(_testName),
                Runtime.List(Startup.Sym("MEMBER"), Startup.Sym("EQ"), Startup.Sym("EQL"),
                             Startup.Sym("EQUAL"), Startup.Sym("EQUALP"))))
        };
        Synchronized = synchronized;
        if (weakness != null)
        {
            switch (weakness.ToUpperInvariant())
            {
                case "VALUE":         Weakness = ":VALUE";         _weakValue = true; break;
                case "KEY":           Weakness = ":KEY";           _weakKey = true;   break;
                case "KEY-AND-VALUE": Weakness = ":KEY-AND-VALUE"; _weakKey = true; _weakValue = true; break;
                case "KEY-OR-VALUE":  Weakness = ":KEY-OR-VALUE";  _weakKey = true; _weakValue = true; _keyOrValue = true; break;
                default:
                    throw new LispErrorException(new LispTypeError(
                        $"MAKE-HASH-TABLE: :weakness {weakness.ToLowerInvariant()} invalid " +
                        "(expected :key, :value, :key-and-value, or :key-or-value)",
                        Startup.Keyword(weakness.ToUpperInvariant()),
                        Runtime.List(Startup.Sym("MEMBER"), Startup.Keyword("KEY"),
                                     Startup.Keyword("VALUE"), Startup.Keyword("KEY-AND-VALUE"),
                                     Startup.Keyword("KEY-OR-VALUE"))));
            }
        }
        // Key-weak tables box the key in a WeakKeyBox so the dict doesn't root it;
        // the comparer resolves boxes (and bare live keys used for lookup) through
        // the test. Strong/value-only tables key on the LispObject directly.
        var inner = new LispEqualityComparer(_test, _testName, userHash);
        _dict = new Dictionary<object, object>(
            _weakKey ? new WeakKeyComparer(inner) : (IEqualityComparer<object>)new BoxedObjectComparer(inner));
        DotCL.Diagnostics.AllocCounter.Inc("LispHashTable");
    }

    // --- weak storage helpers ---------------------------------------------

    // A weak dictionary key: holds the key weakly + a frozen hash so the entry's
    // bucket stays stable after the key is collected (until pruned).
    private sealed class WeakKeyBox
    {
        public readonly WeakReference<LispObject> Ref;
        public readonly int Hash;
        public WeakKeyBox(LispObject key, int hash) { Ref = new WeakReference<LispObject>(key); Hash = hash; }
        public LispObject? Live => Ref.TryGetTarget(out var v) ? v : null;
    }

    // Comparer for strong/value-only tables: dict key is a bare LispObject.
    private sealed class BoxedObjectComparer : IEqualityComparer<object>
    {
        private readonly LispEqualityComparer _inner;
        public BoxedObjectComparer(LispEqualityComparer inner) => _inner = inner;
        public new bool Equals(object? x, object? y) => _inner.Equals((LispObject?)x, (LispObject?)y);
        public int GetHashCode(object obj) => _inner.GetHashCode((LispObject)obj);
    }

    // Comparer for key-weak tables: stored keys are WeakKeyBox, lookup keys are
    // bare LispObject. Resolves both sides to the live key and compares via test;
    // hash comes from the box (frozen) or is computed for a bare lookup key.
    private sealed class WeakKeyComparer : IEqualityComparer<object>
    {
        private readonly LispEqualityComparer _inner;
        public WeakKeyComparer(LispEqualityComparer inner) => _inner = inner;
        private LispObject? Resolve(object o) => o is WeakKeyBox b ? b.Live : (LispObject)o;
        public new bool Equals(object? x, object? y)
        {
            var lx = x == null ? null : Resolve(x);
            var ly = y == null ? null : Resolve(y);
            // A dead box never equals anything (so pruning can find/remove it by ==).
            if (lx == null || ly == null) return ReferenceEquals(x, y);
            return _inner.Equals(lx, ly);
        }
        public int GetHashCode(object obj)
        {
            if (obj is WeakKeyBox b) return b.Hash;
            return _inner.GetHashCode((LispObject)obj);
        }
        public int HashOf(LispObject key) => _inner.GetHashCode(key);
    }

    // Box a value for storage (weak when the value side is weak).
    private object WrapValue(LispObject value)
        => _weakValue ? new WeakReference<LispObject>(value) : value;

    private LispObject? ResolveValue(object stored)
        => stored is WeakReference<LispObject> wr ? (wr.TryGetTarget(out var v) ? v : null) : (LispObject)stored;

    private LispObject? ResolveKey(object storedKey)
        => storedKey is WeakKeyBox b ? b.Live : (LispObject)storedKey;

    // Is the entry (storedKey -> storedVal) still live under this table's mode?
    // Returns the (key,value) when live, else null. KEY-OR-VALUE survives while
    // either weak side is live; all other modes need every weak side live.
    private (LispObject k, LispObject v)? LiveEntry(object storedKey, object storedVal)
    {
        var k = ResolveKey(storedKey);
        var v = ResolveValue(storedVal);
        bool keyOk = !_weakKey || k != null;
        bool valOk = !_weakValue || v != null;
        bool alive = _keyOrValue ? (keyOk || valOk) : (keyOk && valOk);
        if (!alive) return null;
        // For OR-mode a side may be dead but the other alive; surface NIL? No;
        // CL semantics keep the pair; if one side is collected the entry is
        // logically gone for use. We require both resolvable to hand back a pair;
        // a half-dead OR entry is treated as live-but-unusable -> prune lazily.
        if (k == null || v == null) return null;
        return (k, v);
    }

    public LispObject Get(LispObject key, LispObject defaultValue)
    {
        if (Synchronized) { lock (_lock) return GetImpl(key, defaultValue); }
        return GetImpl(key, defaultValue);
    }
    private LispObject GetImpl(LispObject key, LispObject defaultValue)
    {
        if (_dict.TryGetValue(key, out var stored))
        {
            var live = ResolveValue(stored);
            // entry-level liveness also depends on key side for key-weak modes,
            // but if we found it by live key, the key is necessarily alive.
            if (live != null) return live;
            RemoveByLookup(key);
        }
        return defaultValue;
    }

    public bool TryGet(LispObject key, out LispObject value)
    {
        if (Synchronized) { lock (_lock) return TryGetImpl(key, out value); }
        return TryGetImpl(key, out value);
    }
    private bool TryGetImpl(LispObject key, out LispObject value)
    {
        if (_dict.TryGetValue(key, out var s))
        {
            var live = ResolveValue(s);
            if (live != null) { value = live; return true; }
            RemoveByLookup(key);
        }
        value = null!; return false;
    }

    public void Set(LispObject key, LispObject value)
    {
        if (Synchronized) { lock (_lock) SetImpl(key, value); }
        else SetImpl(key, value);
    }
    private void SetImpl(LispObject key, LispObject value)
    {
        // For key-weak modes, an existing live entry must be updated in place
        // (its box is the dict key). TryGetValue by the bare key finds it via the
        // resolving comparer; if present we keep the existing box key.
        if (_weakKey)
        {
            if (_dict.ContainsKey(key)) { _dict[key] = WrapValue(value); _version++; return; }
            PruneBeforeInsert();
            _dict[new WeakKeyBox(key, ((WeakKeyComparer)_dict.Comparer).HashOf(key))] = WrapValue(value);
            _version++;
            return;
        }
        if (_weakValue && !_dict.ContainsKey(key)) PruneBeforeInsert();
        _dict[key] = WrapValue(value);
        _version++;
    }

    // A weak entry whose key (or value) has been collected is only removed when
    // something walks the table (COUNT, MAPHASH) or looks that key up, and a dead
    // key is never looked up again. A table that only ever has new keys added
    // (a cache keyed by objects that come and go) therefore kept every dead
    // entry: its box, its weak handle and its value. Sweep before an insert once
    // the table has doubled since the last sweep, so the work stays amortized
    // constant per insert and the table holds at most about twice its live
    // entries.
    private int _pruneAt = 64;
    private void PruneBeforeInsert()
    {
        if (_dict.Count < _pruneAt) return;
        int alive = CountAlivePruning();
        _pruneAt = Math.Max(64, alive * 2);
    }

    /// <summary>Entries TABLE is holding, dead weak ones included (COUNT
    /// sweeps them first and answers only the live ones). Static so Lisp can
    /// ask through DOTNET:STATIC.</summary>
    public static int StoredCount(LispHashTable table)
    {
        if (table.Synchronized) lock (table._lock) return table._dict.Count;
        return table._dict.Count;
    }

    public bool Remove(LispObject key)
    {
        if (Synchronized) { lock (_lock) return RemoveByLookup(key); }
        return RemoveByLookup(key);
    }
    // Remove by live key. The resolving comparer matches the stored box, so a
    // bare key removes the corresponding weak entry.
    private bool RemoveByLookup(LispObject key)
    {
        bool removed = _dict.Remove(key);
        if (removed) _version++;
        return removed;
    }

    /// <summary>A fresh table with the test and the entries of PROTO. A fasl
    /// builds a large hash table literal once as a prototype it keeps to itself
    /// and hands out copies, so literals that share most of their entries
    /// (environment tables copied from a parent) share one builder.</summary>
    public static LispHashTable CopyLiteral(LispHashTable proto)
    {
        var h = proto._user is { } u
            ? new LispHashTable(proto._testName, false, null, u.TestFn, u.TestName, u.Hash)
            : new LispHashTable(proto._testName);
        foreach (var kv in proto.Entries) h.Set(kv.Key, kv.Value);
        return h;
    }

    /// <summary>MAKE-HASH-TABLE's :SIZE: room for N entries up front, so a
    /// table filled to a known size (COPY-HASH-TABLE passes the source's size)
    /// is not rehashed on the way. Capped so a huge :SIZE cannot allocate at
    /// once what it may never use.</summary>
    public void EnsureCapacity(int n)
    {
        if (n <= 0) return;
        if (n > 1 << 16) n = 1 << 16;
#if !NETSTANDARD2_0
        if (Synchronized) { lock (_lock) _dict.EnsureCapacity(n); }
        else _dict.EnsureCapacity(n);
#endif
    }

    public void Clear()
    {
        if (Synchronized) lock (_lock) { _dict.Clear(); _version++; }
        else { _dict.Clear(); _version++; }
    }

    public int Count
    {
        get
        {
            // Not under the lock even when Synchronized: the count is a field
            // read, which a concurrent store cannot make fail, and a count read
            // while another thread stores is out of date the moment it returns
            // either way. Taking the lock made every thread that asks (the
            // tree-walk evaluator asks on every form it evaluates) wait on every
            // other.
            if (Weakness == null)
                return _dict.Count;
            if (Synchronized) lock (_lock) return CountAlivePruning();
            return CountAlivePruning();
        }
    }

    private int CountAlivePruning()
    {
        var dead = new List<object>();
        int alive = 0;
        foreach (var kv in _dict)
        {
            if (LiveEntry(kv.Key, kv.Value) != null) alive++;
            else dead.Add(kv.Key);
        }
        foreach (var k in dead) _dict.Remove(k);
        return alive;
    }
    public string TestName => _testName;

    /// <summary>What HASH-TABLE-TEST answers: the standard test's symbol, or
    /// for a table made with :HASH-FUNCTION and another test, that test's name
    /// (or the function itself when it has none).</summary>
    public LispObject TestDesignator => _user?.TestFn is { } userTestFn
        ? _user.TestName ?? userTestFn
        : Startup.Sym(_testName);

    // Enumeration returns a snapshot under lock when Synchronized so the
    // iteration itself cannot race with concurrent mutation. Dead weak
    // entries are filtered (and pruned) during snapshot construction.
    public IEnumerable<KeyValuePair<LispObject, LispObject>> Entries
    {
        get
        {
            if (Synchronized)
            {
                KeyValuePair<LispObject, LispObject>[] snapshot;
                lock (_lock) snapshot = SnapshotAlive();
                return snapshot;
            }
            return SnapshotAlive();
        }
    }

    /// <summary>The live entries as one array, key then value, in the order of Entries
    /// (the order MAPHASH walks), for the hash-table iterators: no pair objects.</summary>
    public LispObject[] FlatSnapshot()
    {
        if (Weakness == null && !Synchronized)
        {
            var arr = new LispObject[_dict.Count * 2];
            int i = 0;
            foreach (var kv in _dict)
            {
                arr[i++] = (LispObject)kv.Key;
                arr[i++] = (LispObject)kv.Value;
            }
            return arr;
        }
        var snap = Entries is KeyValuePair<LispObject, LispObject>[] a ? a : new List<KeyValuePair<LispObject, LispObject>>(Entries).ToArray();
        var flat = new LispObject[snap.Length * 2];
        for (int i = 0; i < snap.Length; i++) { flat[2 * i] = snap[i].Key; flat[2 * i + 1] = snap[i].Value; }
        return flat;
    }

    private KeyValuePair<LispObject, LispObject>[] SnapshotAlive()
    {
        if (Weakness == null)
        {
            var arr = new KeyValuePair<LispObject, LispObject>[_dict.Count];
            int i = 0;
            foreach (var kv in _dict)
                arr[i++] = new KeyValuePair<LispObject, LispObject>((LispObject)kv.Key, (LispObject)kv.Value);
            return arr;
        }
        var dead = new List<object>();
        var alive = new List<KeyValuePair<LispObject, LispObject>>();
        foreach (var kv in _dict)
        {
            var live = LiveEntry(kv.Key, kv.Value);
            if (live != null) alive.Add(new KeyValuePair<LispObject, LispObject>(live.Value.k, live.Value.v));
            else dead.Add(kv.Key);
        }
        foreach (var k in dead) _dict.Remove(k);
        return alive.ToArray();
    }

    public void ForEach(Action<LispObject, LispObject> action)
        => ForEachCore(action, static (a, k, v) => a(k, v));

    /// <summary>MAPHASH's entry: takes the Lisp function itself, so walking a
    /// table does not build a closure over it on every call.</summary>
    public void ForEachLisp(LispFunction fn)
        => ForEachCore(fn, static (f, k, v) => f.Invoke2(k, v));

    private void ForEachCore<TState>(TState state, Action<TState, LispObject, LispObject> action)
    {
        if (Synchronized)
        {
            KeyValuePair<LispObject, LispObject>[] snapshot;
            lock (_lock) snapshot = SnapshotAlive();
            foreach (var pair in snapshot)
                action(state, pair.Key, pair.Value);
            return;
        }
        if (Weakness != null)
        {
            // Take a snapshot for weak tables so the action's side effects
            // (which may keep dead values alive momentarily) don't confuse
            // the prune walk.
            var snap = SnapshotAlive();
            foreach (var pair in snap)
                action(state, pair.Key, pair.Value);
            return;
        }
        // Snapshot before iterating so ACTION may add/remove entries during the
        // walk. CLHS leaves mid-MAPHASH modification undefined, but SBCL/CCL/ECL
        // tolerate it and real code relies on it (e.g. lem's
        // add-lisp-color-aliases inserts aliases while mapping the color table).
        // Iterating Dictionary live would throw InvalidOperationException
        // ("Collection was modified"); snapshotting matches the other impls.
        var buf = RentEntries(out int n, out bool pooled);
        try
        {
            for (int i = 0; i < n; i++)
                action(state, (LispObject)buf[i].Key, (LispObject)buf[i].Value);
        }
        finally
        {
            ReturnEntries(buf, n, pooled);
        }
    }

    /// <summary>A snapshot of the entries, for a walk that may change the table.
    /// The buffer is reused per thread: MAPHASH is an iteration, and allocating
    /// an array of every entry on each call made walking a table cost more than
    /// the walk. A nested walk (the action maps another table, or the same one)
    /// finds the buffer in use and gets its own. Hand it back with ReturnEntries.</summary>
    internal KeyValuePair<object, object>[] RentEntries(out int n, out bool pooled)
    {
        if (Synchronized || Weakness != null)
        {
            KeyValuePair<LispObject, LispObject>[] snap;
            if (Synchronized) { lock (_lock) snap = SnapshotAlive(); }
            else snap = SnapshotAlive();
            var copy = new KeyValuePair<object, object>[snap.Length];
            for (int i = 0; i < snap.Length; i++)
                copy[i] = new KeyValuePair<object, object>(snap[i].Key, snap[i].Value);
            n = copy.Length;
            pooled = false;
            return copy;
        }
        n = _dict.Count;
        KeyValuePair<object, object>[] buf;
        pooled = false;
        if (!_forEachBufInUse)
        {
            if (_forEachBuf == null || _forEachBuf.Length < n)
                _forEachBuf = new KeyValuePair<object, object>[n < 16 ? 16 : n];
            buf = _forEachBuf;
            _forEachBufInUse = true;
            pooled = true;
        }
        else
        {
            buf = new KeyValuePair<object, object>[n];
        }
        ((System.Collections.Generic.ICollection<KeyValuePair<object, object>>)_dict)
            .CopyTo(buf, 0);
        return buf;
    }

    internal static void ReturnEntries(KeyValuePair<object, object>[] buf, int n, bool pooled)
    {
        // Do not keep the entries alive through the pooled buffer.
        if (pooled) { Array.Clear(buf, 0, n); _forEachBufInUse = false; }
    }

    [ThreadStatic] private static KeyValuePair<object, object>[]? _forEachBuf;
    [ThreadStatic] private static bool _forEachBufInUse;

    // The one EQL. This used to be a partial copy that knew only fixnums,
    // characters and floats, so a hash table could not find a bignum, ratio or
    // complex key even though CL:EQL reported the two keys EQL. Delegating
    // keeps the table's notion of key identity from drifting from the language's.
    private static bool Eql(LispObject a, LispObject b) => Runtime.IsTrueEql(a, b);

    private static bool LispEqual(LispObject a, LispObject b)
    {
        if (Eql(a, b)) return true;
        // String comparison: LispString and char-vector are interchangeable per EQUAL
        bool aStr = a is LispString || (a is LispVector av && av.IsCharVector);
        bool bStr = b is LispString || (b is LispVector bv && bv.IsCharVector);
        if (aStr && bStr)
        {
            string sa = a is LispString ls1 ? ls1.Value : ((LispVector)a).ToCharString();
            string sb = b is LispString ls2 ? ls2.Value : ((LispVector)b).ToCharString();
            return sa == sb;
        }
        if (a is Cons ca && b is Cons cb)
            return LispEqual(ca.Car, cb.Car) && LispEqual(ca.Cdr, cb.Cdr);
        // Pathnames are EQUAL when their components are (CLHS EQUAL), as
        // Runtime.Equal answers; an EQUAL table has to agree with it.
        if (a is LispPathname pa && b is LispPathname pb)
            return LispEqual(pa.Host ?? Nil.Instance, pb.Host ?? Nil.Instance)
                && LispEqual(pa.Device ?? Nil.Instance, pb.Device ?? Nil.Instance)
                && LispEqual(pa.DirectoryComponent ?? Nil.Instance, pb.DirectoryComponent ?? Nil.Instance)
                && LispEqual(pa.NameComponent ?? Nil.Instance, pb.NameComponent ?? Nil.Instance)
                && LispEqual(pa.TypeComponent ?? Nil.Instance, pb.TypeComponent ?? Nil.Instance)
                && LispEqual(Runtime.NormalPathnameVersion(pa.Version), Runtime.NormalPathnameVersion(pb.Version));
        // Bit-vector comparison
        if (a is LispVector bva && bva.IsBitVector && b is LispVector bvb && bvb.IsBitVector)
        {
            if (bva.Length != bvb.Length) return false;
            for (int i = 0; i < bva.Length; i++)
                if (!Eql(bva.GetElement(i), bvb.GetElement(i))) return false;
            return true;
        }
        return false;
    }

    public static bool Equalp(LispObject a, LispObject b)
    {
        a = Runtime.Primary(a); b = Runtime.Primary(b);
        while (true)
        {
            if (ReferenceEquals(a, b)) return true;
            if (a is Fixnum fa) return b is Fixnum fb ? fa.Value == fb.Value : b is Number nb0 && NumbersEqualp(fa, nb0);
            if (a is Cons ca2)
            {
                Runtime.StackGuard("EQUALP");
                if (b is not Cons cb2 || !Equalp(ca2.Car, cb2.Car)) return false;
                a = ca2.Cdr;
                b = cb2.Cdr;
                continue;
            }
            if (a is LispChar ca)
                return b is LispChar cb && CharEqualp(ca.Value, cb.Value);
            if (a is Number na) return b is Number nb && NumbersEqualp(na, nb);
            if (a is Symbol || a is Nil || a is T) return Runtime.IsTrueEq(a, b);
            if (a is LispSimdPack simdA) return b is LispSimdPack simdB && simdA.SameBits(simdB);
            if (LispStruct.TryAs(a, out var sa))
            {
                if (!LispStruct.TryAs(b, out var sb)) return false;
                if (!SameStructType(sa.TypeName, sb.TypeName)) return false;
                if (sa.SlotCount != sb.SlotCount) return false;
                Runtime.StackGuard("EQUALP");
                for (int i = 0; i < sa.SlotCount; i++)
                    if (!Equalp(sa.GetSlot(i), sb.GetSlot(i))) return false;
                return true;
            }
            if (a is LispString las0 && b is LispString lbs0)
            {
                var xs = las0.Chars; var ys = lbs0.Chars;
                if (xs.Length != ys.Length) return false;
                for (int i = 0; i < xs.Length; i++)
                    if (!CharEqualp(xs[i], ys[i])) return false;
                return true;
            }
            if ((a is LispString || a is LispVector) && (b is LispString || b is LispVector))
                return ArraysEqualp(a, b);
            // Hash table comparison: same test, same count, same key->value pairs
            if (a is LispHashTable ha && b is LispHashTable hb)
            {
                if (ha.TestName != hb.TestName) return false;
                if (ha.Count != hb.Count) return false;
                foreach (var (key, val) in ha.Entries)
                {
                    if (!hb.TryGet(key, out var bVal)) return false;
                    if (!Equalp(val, bVal)) return false;
                }
                return true;
            }
            // Pathname comparison: CLHS says equalp on pathnames is same as equal
            if (a is LispPathname pa && b is LispPathname pb)
            {
                return Runtime.IsTruthy(Runtime.Equal(pa.Host ?? Nil.Instance, pb.Host ?? Nil.Instance))
                    && Runtime.IsTruthy(Runtime.Equal(pa.Device ?? Nil.Instance, pb.Device ?? Nil.Instance))
                    && Runtime.IsTruthy(Runtime.Equal(pa.DirectoryComponent ?? Nil.Instance, pb.DirectoryComponent ?? Nil.Instance))
                    && Runtime.IsTruthy(Runtime.Equal(pa.NameComponent ?? Nil.Instance, pb.NameComponent ?? Nil.Instance))
                    && Runtime.IsTruthy(Runtime.Equal(pa.TypeComponent ?? Nil.Instance, pb.TypeComponent ?? Nil.Instance))
                    && Runtime.PathnameVersionsEqual(pa.Version, pb.Version);
            }
            return false;
        }
    }

    // Same structure type means the same class, not just the same type name.
    private static bool SameStructType(Symbol a, Symbol b)
    {
        if (ReferenceEquals(a, b)) return true;
        if (a.Name != b.Name) return false;
        return Runtime.FindClassOrNil(a) is LispClass ac
            && Runtime.FindClassOrNil(b) is LispClass bc
            && ReferenceEquals(ac, bc);
    }

    private static bool ArraysEqualp(LispObject a, LispObject b)
    {
        var va = a as LispVector; var vb = b as LispVector;
        int ra = va?.Rank ?? 1, rb = vb?.Rank ?? 1;
        if (ra != rb) return false;
        int len;
        if (ra == 1)
        {
            len = va?.Length ?? ((LispString)a).Length;
            if (len != (vb?.Length ?? ((LispString)b).Length)) return false;
        }
        else
        {
            var da = va!.Dimensions; var db = vb!.Dimensions;
            for (int i = 0; i < ra; i++) if (da[i] != db[i]) return false;
            len = va.Length;
        }
        for (int i = 0; i < len; i++)
        {
            var x = va != null ? va.ElementAt(i) : LispChar.Make(((LispString)a)[i]);
            var y = vb != null ? vb.ElementAt(i) : LispChar.Make(((LispString)b)[i]);
            if (x is LispChar cx && y is LispChar cy)
            {
                if (!CharEqualp(cx.Value, cy.Value)) return false;
            }
            else if (!Equalp(x, y)) return false;
        }
        return true;
    }

    private static bool CharEqualp(char x, char y)
        => x == y || CharCase.Fold(x) == CharCase.Fold(y);

    // = plus EQL, so a NaN is still EQUALP to a bit-identical NaN.
    private static bool NumbersEqualp(Number a, Number b)
    {
        if (a is DoubleFloat da && b is DoubleFloat db)
            return da.Value == db.Value
                || BitConverter.DoubleToInt64Bits(da.Value) == BitConverter.DoubleToInt64Bits(db.Value);
        return Arithmetic.IsNumericEqual(a, b) || Runtime.IsTrueEql(a, b);
    }

    public override string ToString() => $"#<HASH-TABLE :{_testName}{(Synchronized ? " :SYNCHRONIZED" : "")} {_dict.Count}/{_dict.Count}>";

    private class LispEqualityComparer : IEqualityComparer<LispObject>
    {
        private readonly Func<LispObject, LispObject, bool> _test;
        private readonly LispFunction? _hash;
        // The test as a small integer, so hashing a key does not compare the
        // test's name as a string on every lookup.
        private const int KindEq = 0, KindEql = 1, KindEqual = 2, KindEqualp = 3, KindOther = 4;
        private readonly int _kind;

        public LispEqualityComparer(Func<LispObject, LispObject, bool> test, string testName,
                                    LispFunction? hash = null)
        {
            _test = test;
            _hash = hash;
            _kind = testName switch
            {
                "EQ" => KindEq,
                "EQL" => KindEql,
                "EQUAL" => KindEqual,
                "EQUALP" => KindEqualp,
                _ => KindOther
            };
        }

        private int UserHash(LispObject obj)
        {
            var h = _hash!.Invoke1(obj);
            if (h is MvReturn mv) h = mv.PrimaryValue;
            MultipleValues.Reset();
            return h switch
            {
                Fixnum f => f.Value.GetHashCode(),
                Bignum b => b.Value.GetHashCode(),
                _ => throw new LispErrorException(new LispTypeError(
                    "hash table :HASH-FUNCTION returned a non-integer", h, Startup.Sym("INTEGER")))
            };
        }

        public bool Equals(LispObject? x, LispObject? y)
        {
            if (x == null && y == null) return true;
            if (x == null || y == null) return false;
            // All four standard tests hold for an object and itself.
            if (_kind != KindOther && ReferenceEquals(x, y)) return true;
            return _test(x, y);
        }

        public int GetHashCode(LispObject obj)
        {
            if (_hash != null) return UserHash(obj);
            switch (_kind)
            {
                case KindEq:
                    return System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj);
                case KindEql:
                    // Only numbers and characters hash by value; test for them
                    // with two type checks before trying each number type.
                    if (obj is Number || obj is LispChar || obj is LispSimdPack)
                        return NumericOrCharHash(obj)
                               ?? System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj);
                    return System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj);
                case KindEqual: return GetEqualHash(obj);
                case KindEqualp: return GetEqualpHash(obj);
                default: return obj.GetHashCode();
            }
        }

        // EQL compares numbers by type and value, so every number must hash by
        // value. Falling through to the identity hash (as bignums, ratios and
        // complexes used to) puts two EQL keys in different buckets, and the
        // table can never find them again: EQUAL inherits this through its
        // number case, which is how a float-keyed EQUAL table silently
        // accumulated one entry per lookup. Returns null for non-numbers.
        // Collisions across types are fine (1 vs 1.0d0 hash alike); the test
        // function still separates them.
        private static int? NumericOrCharHash(LispObject obj) => obj switch
        {
            Fixnum f => f.Value.GetHashCode(),
            LispChar c => c.Value.GetHashCode(),
            LispSimdPack sp => HashCode.Combine((int)sp.Kind, sp.Lo, sp.Hi),
            SingleFloat sf => sf.Value.GetHashCode(),
            DoubleFloat df => df.Value.GetHashCode(),
            Bignum b => b.Value.GetHashCode(),
            Ratio r => HashCode.Combine(r.Numerator.GetHashCode(), r.Denominator.GetHashCode()),
            LispComplex lc => HashCode.Combine(NumericOrCharHash(lc.Real) ?? 0,
                                               NumericOrCharHash(lc.Imaginary) ?? 0),
            _ => null
        };

        // Depth limit for structural key hashing. Like SXHASH (SxhashCompute),
        // EQUAL/EQUALP hashing descends a bounded number of levels so a circular
        // key (e.g. #1=(1 2 3 . #1#)) truncates instead of recursing until the
        // native stack overflows and takes the whole process down.
        private const int EqualHashMaxDepth = 8;

        private static int GetEqualHash(LispObject obj) => GetEqualHash(obj, EqualHashMaxDepth);

        // A list hashes by its first elements, walked along the spine in a loop (a long or circular
        // list stops after EqualHashMaxDepth conses); the elements themselves descend one level less.
        private static int ConsEqualHash(Cons c, int depth)
        {
            int h = 17;
            LispObject cur = c;
            for (int i = 0; i < EqualHashMaxDepth && cur is Cons cc; i++, cur = cc.Cdr)
                h = unchecked(h * 31 + GetEqualHash(cc.Car, depth - 1));
            return h;
        }

        private static int ConsEqualpHash(Cons c, int depth)
        {
            int h = 17;
            LispObject cur = c;
            for (int i = 0; i < EqualHashMaxDepth && cur is Cons cc; i++, cur = cc.Cdr)
                h = unchecked(h * 31 + GetEqualpHash(cc.Car, depth - 1));
            return h;
        }

        private static int GetEqualHash(LispObject obj, int depth)
        {
            if (depth == 0) return 0;
            return obj switch
            {
                // From the characters: a string written into (SETF CHAR) is
                // char-array backed, and VALUE would build a System.String of
                // it on every lookup.
                LispString s => Compat.StringHash(s.Chars),
                LispVector v when v.IsCharVector => Compat.StringHash(v.ToCharString()),
                Cons c => ConsEqualHash(c, depth),
                LispVector bv when bv.IsBitVector => HashBitVector(bv),
                LispPathname pn => PathnameHash(pn, depth),
                // EQUAL falls back to EQL for numbers and characters, so they
                // must hash by value here too: including inside a cons, which
                // is how SBCL's inline-constant table keys its float constants.
                _ => NumericOrCharHash(obj)
                     ?? System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj)
            };
        }

        // A few slots are enough to spread the keys; the depth limit keeps a
        // structure that refers to itself from recursing.
        // A structure's type contributes the identity of its class's name symbol:
        // stable across redefinition, and distinct for same-named classes.
        private static int StructTypeHash(Symbol typeName)
            => Runtime.FindClassOrNil(typeName) is LispClass cls
                ? RuntimeHelpers.GetHashCode(cls.Name)
                : RuntimeHelpers.GetHashCode(typeName);

        private static int StructEqualpHash(LispStruct st, int depth)
        {
            var h = new HashCode();
            h.Add(StructTypeHash(st.TypeName));
            int n = Math.Min(st.SlotCount, 4);
            for (int i = 0; i < n; i++) h.Add(GetEqualpHash(st.GetSlot(i), depth - 1));
            return h.ToHashCode();
        }

        // By the components EQUAL and EQUALP compare.
        private static int PathnameHash(LispPathname pn, int depth)
            => HashCode.Combine(
                GetEqualHash(pn.Host ?? Nil.Instance, depth - 1),
                GetEqualHash(pn.Device ?? Nil.Instance, depth - 1),
                GetEqualHash(pn.DirectoryComponent ?? Nil.Instance, depth - 1),
                GetEqualHash(pn.NameComponent ?? Nil.Instance, depth - 1),
                GetEqualHash(pn.TypeComponent ?? Nil.Instance, depth - 1),
                GetEqualHash(Runtime.NormalPathnameVersion(pn.Version), depth - 1));

        private static int HashBitVector(LispVector bv)
        {
            var h = new HashCode();
            for (int i = 0; i < bv.Length; i++)
                h.Add(bv.GetElement(i) is Fixnum f ? f.Value : 0);
            return h.ToHashCode();
        }

        private static int GetEqualpHash(LispObject obj) => GetEqualpHash(obj, EqualHashMaxDepth);

        private static int UpcaseHash(ReadOnlySpan<char> cs)
        {
            int h = cs.Length;
            foreach (var c in cs) h = h * 31 + CharCase.Fold(c);
            return h;
        }

        // Must agree with UpcaseHash when V holds only characters.
        private static int ArrayEqualpHash(LispVector v, int depth)
        {
            int n = v.Length;
            int h = n;
            for (int i = 0; i < n; i++)
            {
                var e = v.ElementAt(i);
                h = h * 31 + (e is LispChar c ? CharCase.Fold(c.Value) : GetEqualpHash(e, depth - 1));
            }
            return h;
        }

        private static int GetEqualpHash(LispObject obj, int depth)
        {
            if (depth == 0) return 0;
            return obj switch
            {
                LispString s => UpcaseHash(s.Chars),
                LispVector v => ArrayEqualpHash(v, depth),
                LispChar c => CharCase.Fold(c.Value),
                // A complex has to hash by its parts: EQUALP compares numbers with
                // =, so #C(1 2) and #C(1.0d0 2.0d0) are EQUALP and must land in
                // the same bucket. It also has to agree with a REAL when the
                // imaginary part is zero, since (= #C(1.0d0 0.0d0) 1.0d0) is true.
                // Falling into the Number case below asked ToDouble for the value
                // of a complex, which has none -- so a complex key threw instead
                // of hashing.
                LispComplex cx => Arithmetic.ToDouble(cx.Imaginary) == 0.0
                    ? Arithmetic.ToDouble(cx.Real).GetHashCode()
                    : HashCode.Combine(Arithmetic.ToDouble(cx.Real).GetHashCode(),
                                       Arithmetic.ToDouble(cx.Imaginary).GetHashCode()),
                Number n => Arithmetic.ToDouble(n).GetHashCode(),
                Cons c => ConsEqualpHash(c, depth),
                // EQUALP compares structures slot by slot, hash tables by test,
                // count and entries, and pathnames as EQUAL does, so none of them
                // may hash by identity: two EQUALP keys landed in different
                // buckets and GETHASH missed a key that was there.
                LispStruct st => StructEqualpHash(st, depth),
                LispHashTable ht => HashCode.Combine(ht.TestName, ht.Count),
                LispPathname pn => PathnameHash(pn, depth),
                LispSimdPack sp => HashCode.Combine((int)sp.Kind, sp.Lo, sp.Hi),
                _ => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj)
            };
        }
    }
}

/// <summary>MT19937 with SBCL's state layout and its RANDOM algorithms, so a state
/// SBCL printed reads back here and draws the same numbers. The state is 627
/// words: 0-1 the constants 0 and MATRIX_A (indexed by y &amp; 1 in the update),
/// 2 the index k, 3-626 the 624 words of state.</summary>
public class LispRandomState : LispObject
{
    public const int StateLength = 627;
    const int N = 624, M = 397;
    const uint UpperMask = 0x80000000, LowerMask = 0x7FFFFFFF;
    const uint MatrixA = 0x9908B0DF, TemperB = 0x9D2C5680, TemperC = 0xEFC60000;

    private readonly uint[] _s = new uint[StateLength];

    public LispRandomState() => SeedByArray(OsSeed());

    public LispRandomState(uint seed) => InitRandomState(seed);

    public LispRandomState(LispRandomState other) => Array.Copy(other._s, _s, StateLength);

    private LispRandomState(uint[] state, bool raw) => Array.Copy(state, _s, StateLength);

    public static LispRandomState FromState(uint[] state) => new LispRandomState(state, true);

    public uint[] StateWords() => (uint[])_s.Clone();

    static uint[] OsSeed()
    {
        var bytes = new byte[32];
        using (var rng = System.Security.Cryptography.RandomNumberGenerator.Create())
            rng.GetBytes(bytes);
        var key = new uint[8];
        for (int i = 0; i < 8; i++) key[i] = BitConverter.ToUInt32(bytes, i * 4);
        return key;
    }

    void InitRandomState(uint seed)
    {
        _s[0] = 0;
        _s[1] = MatrixA;
        _s[2] = N;
        uint s = seed;
        for (int i = 0; i < N; i++)
        {
            if (i > 0) s = unchecked(1812433253u * (s ^ (s >> 30)) + (uint)i);
            _s[3 + i] = s;
        }
    }

    void SeedByArray(uint[] key)
    {
        InitRandomState(19650218);
        int i = 1, j = 0, l = key.Length;
        for (int k = Math.Max(N, l); k > 0; k--)
        {
            _s[i + 3] = unchecked((_s[i + 3] ^ ((_s[i + 2] ^ (_s[i + 2] >> 30)) * 1664525u)) + key[j] + (uint)j);
            i++; if (i >= N) { _s[3] = _s[2 + N]; i = 1; }
            j++; if (j >= l) j = 0;
        }
        for (int k = N - 1; k > 0; k--)
        {
            _s[i + 3] = unchecked((_s[i + 3] ^ ((_s[i + 2] ^ (_s[i + 2] >> 30)) * 1566083941u)) - (uint)i);
            i++; if (i >= N) { _s[3] = _s[2 + N]; i = 1; }
        }
        _s[3] = 0x80000000;
    }

    void Update()
    {
        uint y;
        int kk;
        for (kk = 3; kk < 3 + (N - M); kk++)
        {
            y = (_s[kk] & UpperMask) | (_s[kk + 1] & LowerMask);
            _s[kk] = _s[kk + M] ^ (y >> 1) ^ _s[y & 1];
        }
        for (; kk < 3 + (N - 1); kk++)
        {
            y = (_s[kk] & UpperMask) | (_s[kk + 1] & LowerMask);
            _s[kk] = _s[kk + (M - N)] ^ (y >> 1) ^ _s[y & 1];
        }
        y = (_s[3 + (N - 1)] & UpperMask) | (_s[3] & LowerMask);
        _s[3 + (N - 1)] = _s[3 + (M - 1)] ^ (y >> 1) ^ _s[y & 1];
    }

    public uint Chunk()
    {
        uint k = _s[2];
        if (k >= N) { Update(); k = 0; }
        _s[2] = k + 1;
        uint y = _s[3 + k];
        y ^= y >> 11;
        y ^= (y << 7) & TemperB;
        y ^= (y << 15) & TemperC;
        y ^= y >> 18;
        return y;
    }

    ulong BigChunk()
    {
        ulong hi = Chunk();
        return (hi << 32) | Chunk();
    }

    static int IntegerLength(ulong x)
    {
        int n = 0;
        while (x != 0) { x >>= 1; n++; }
        return n;
    }

    const long SbclMostPositiveFixnum = (1L << 62) - 1;

    /// <summary>A random integer in [0, limit), limit positive.</summary>
    public System.Numerics.BigInteger NextBelow(System.Numerics.BigInteger limit)
    {
        if (limit <= 0) throw new ArgumentException("limit must be positive");
        if (limit <= SbclMostPositiveFixnum) return NextBelowFixnum((long)limit);
        return NextBelowBignum(limit);
    }

    public long NextBelowFixnum(long limit)
    {
        if (limit > SbclMostPositiveFixnum) return (long)NextBelowBignum(limit);
        if (limit == 1) return 0;
        int nBits = IntegerLength((ulong)(limit - 1));
        ulong mask = (1UL << nBits) - 1;
        while (true)
        {
            ulong bits = mask & (nBits <= 32 ? Chunk() : BigChunk());
            if (bits < (ulong)limit) return (long)bits;
        }
    }

    ulong BignumDigit() => BigChunk();

    ulong BignumPartialDigit(int nBits)
        => ((1UL << nBits) - 1) & (nBits <= 32 ? Chunk() : BigChunk());

    System.Numerics.BigInteger MakeRandomBignum(long nBits)
    {
        long nDigits = nBits / 64;
        int partial = (int)(nBits % 64);
        System.Numerics.BigInteger r = 0;
        for (long i = 0; i < nDigits; i++)
            r |= (System.Numerics.BigInteger)BignumDigit() << (int)(64 * i);
        if (partial != 0)
            r |= (System.Numerics.BigInteger)BignumPartialDigit(partial) << (int)(64 * nDigits);
        return r;
    }

    System.Numerics.BigInteger NextBelowBignum(System.Numerics.BigInteger limit)
    {
        long nBits = limit.GetBitLength();
        if (limit.IsPowerOfTwo)
        {
            long n = nBits - 1;
            if (n <= 32) return Chunk() >> (int)(32 - n);
            return MakeRandomBignum(n);
        }
        if (nBits <= 32)
        {
            int shift = (int)(32 - nBits);
            while (true)
            {
                uint bits = Chunk() >> shift;
                if (bits < limit) return bits;
            }
        }
        int sh = (int)(nBits - 32);
        uint firstChunk = (uint)((limit >> sh) & 0xFFFFFFFF);
        while (true)
        {
            uint rc = Chunk();
            if (rc > firstChunk) continue;
            var bits = ((System.Numerics.BigInteger)rc << sh) | MakeRandomBignum(sh);
            if (rc < firstChunk || bits < limit) return bits;
        }
    }

    public double NextDouble(double limit)
    {
        while (true)
        {
            ulong hi = 0x3FF00000UL | ((Chunk() >> 11) & 0xFFFFF);
            ulong lo = Chunk();
            double c = limit * (BitConverter.Int64BitsToDouble((long)((hi << 32) | lo)) - 1d);
            if (c != limit) return c;
        }
    }

    public float NextSingle(float limit)
    {
        while (true)
        {
            int bits = 0x3F800000 | (int)((Chunk() >> 8) & 0x7FFFFF);
            float c = limit * (Compat.Int32BitsToSingle(bits) - 1f);
            if (c != limit) return c;
        }
    }

    public override string ToString() => "#<RANDOM-STATE>";
}

/// <summary>Pprint dispatch table: the entries set by SET-PPRINT-DISPATCH, and
/// whether the standard entries (CLHS 22.2.1.4, printed by Runtime.PrettyStd)
/// apply under them. The standard entries rank below every user entry.</summary>
public class LispPprintDispatchTable : LispObject
{
    /// <summary>Entries: type-specifier-key -> (type-specifier, function, priority)</summary>
    public Dictionary<string, (LispObject TypeSpec, LispObject Function, double Priority)> Entries { get; }

    /// <summary>True for the initial table and copies of it.</summary>
    public bool Standard { get; }

    public LispPprintDispatchTable(bool standard = false)
    {
        Entries = new Dictionary<string, (LispObject, LispObject, double)>();
        Standard = standard;
    }

    public LispPprintDispatchTable(LispPprintDispatchTable other)
    {
        Entries = new Dictionary<string, (LispObject, LispObject, double)>(other.Entries);
        Standard = other.Standard;
    }

    public override string ToString() => "#<PPRINT-DISPATCH-TABLE>";
}

/// <summary>
/// A weak pointer to a single Lisp object, backed by System.WeakReference.
/// weak-pointer-value returns the object while it is still reachable elsewhere,
/// and NIL once the GC has reclaimed it. Used by trivial-garbage's
/// make-weak-pointer / weak-pointer-value.
/// </summary>
public class LispWeakPointer : LispObject
{
    private readonly WeakReference<LispObject> _ref;

    public LispWeakPointer(LispObject target)
    {
        // trackResurrection: false: value becomes unreachable once collected.
        _ref = new WeakReference<LispObject>(target);
    }

    /// <summary>The referenced object, or NIL if it has been collected.</summary>
    public LispObject Value => _ref.TryGetTarget(out var t) ? t : Nil.Instance;

    public override string ToString() => "#<WEAK-POINTER>";
}

/// <summary>A vector's storage as the .NET array it is. A cast or `is` test to
/// an array of a primitive type is a runtime helper call, because the CLR lets
/// arrays of the same element size pass for each other (a ulong[] is a long[],
/// an sbyte[] a byte[]). An exact type match is one compare inline, so these
/// try that first and fall back to the ordinary test, with the same answer.</summary>
internal static class NumStorage
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static T[]? ArrayAs<T>(Array? a) =>
        a != null && a.GetType() == typeof(T[]) ? Unsafe.As<T[]>(a) : a as T[];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static T[] ArrayCast<T>(Array? a) =>
        a != null && a.GetType() == typeof(T[]) ? Unsafe.As<T[]>(a) : (T[])a!;
}

/// <summary>The state of an opened MAPHASH (Runtime.HashWalkBegin): a snapshot
/// of the entries and the position in it.</summary>
public sealed class HashTableWalk : LispObject
{
    private KeyValuePair<object, object>[]? _buf;
    private readonly int _n;
    private readonly bool _pooled;
    private readonly int _thread;
    private int _i = -1;

    public HashTableWalk(LispHashTable table)
    {
        _buf = table.RentEntries(out _n, out _pooled);
        _thread = Environment.CurrentManagedThreadId;
    }

    public bool MoveNext() => _buf != null && ++_i < _n;

    public LispObject Key => (LispObject)_buf![_i].Key;

    public LispObject Value => (LispObject)_buf![_i].Value;

    public void End()
    {
        if (_buf == null) return;
        // The pool is per thread; a walk resumed elsewhere (an await in the
        // body) leaves the owner's buffer marked in use rather than freeing
        // another thread's.
        if (_thread == Environment.CurrentManagedThreadId)
            LispHashTable.ReturnEntries(_buf, _n, _pooled);
        else
            Array.Clear(_buf, 0, _n);
        _buf = null;
    }

    public override string ToString() => "#<HASH-TABLE-WALK>";
}
