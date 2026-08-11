using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public class LispStruct : LispObject
{
    public Symbol TypeName { get; }
    public LispObject[] Slots { get; }

    // Intern cache for EQ-preserving FASL deserialization.
    // Uses WeakReference values so GC can collect structs no longer referenced elsewhere.
    private static readonly ConcurrentDictionary<string, WeakReference<LispStruct>> _internCache = new();

    public LispStruct(Symbol typeName, LispObject[] slots)
    {
        TypeName = typeName;
        Slots = slots;
        DotCL.Diagnostics.AllocCounter.Inc("LispStruct");
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
        var result = new LispStruct(typeSym, slots);
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
        var obj = Runtime.Eval(form);
        if (obj is LispStruct result)
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
            var parts = new string[Slots.Length];
            for (int i = 0; i < Slots.Length; i++)
                parts[i] = Slots[i].ToString();
            if (parts.Length == 0)
                return $"#S({TypeName.Name})";
            return $"#S({TypeName.Name} {string.Join(" ", parts)})";
        }
        finally { _printing.Remove(this); }
    }
}

public class LispVector : LispObject
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
    internal Array? _numData;   // byte[] | ushort[] | int[] | long[] | float[] | double[] per _numKind
    internal int _numLen;       // _numData.Length (Array.Length on the abstract
                                // static type is a runtime call, not ldlen — hot
                                // aref paths bounds-check against this instead)
    internal byte _numKind;    // 0=none 1=u8 2=u16 3=i32 4=i64 5=f4(float[]) 6=f8(double[])

    // Element type: "T" (general), "CHARACTER"/"BASE-CHAR"/"STANDARD-CHAR" (string-like), "NIL" (bit vector of nil), etc.
    public string ElementTypeName { get; private set; } = "T";

    /// <summary>Storage kind for a bounded-integer element type name, or 0 for
    /// none. Names arrive normalized from ParseElementTypeName ("UNSIGNED-BYTE-8",
    /// "SIGNED-BYTE-32", "FIXNUM", ...). (unsigned-byte 64) does not fit a long
    /// (and can legally hold dotcl bignums), so it stays on boxed storage.</summary>
    internal static byte NumKindForElementType(string et)
    {
        if (et == "FIXNUM") return 4;
        if (et == "SINGLE-FLOAT") return 5;
        if (et == "DOUBLE-FLOAT") return 6;
        const string ub = "UNSIGNED-BYTE-";
        const string sb = "SIGNED-BYTE-";
        if (et.StartsWith(ub, StringComparison.Ordinal)
            && int.TryParse(et.Substring(ub.Length), out int un) && un >= 1)
            return un <= 8 ? (byte)1 : un <= 16 ? (byte)2 : un <= 31 ? (byte)3
                 : un <= 63 ? (byte)4 : (byte)0;
        if (et.StartsWith(sb, StringComparison.Ordinal)
            && int.TryParse(et.Substring(sb.Length), out int sn) && sn >= 1)
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
            _ => new double[size],
        };
    }

    // True when the numeric backing holds floats (kind 5=float[], 6=double[])
    // rather than integers. Integer kinds cross the compiler boundary as a raw
    // long (NumGet/NumSet); float kinds as a raw double (NumGetF/NumSetF).
    internal bool IsFloatNumKind => _numKind >= 5;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal long NumGet(int i) => _numKind switch
    {
        1 => ((byte[])_numData!)[i],
        2 => ((ushort[])_numData!)[i],
        3 => ((int[])_numData!)[i],
        _ => ((long[])_numData!)[i],
    };

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void NumSet(int i, long v)
    {
        // Range-checked store: silently wrapping a value that exceeds the
        // storage width would corrupt data, so an element-type violation
        // (undefined behavior per CLHS) signals loudly instead.
        switch (_numKind)
        {
            case 1:
                if ((ulong)v > byte.MaxValue) throw NumRangeError(v);
                ((byte[])_numData!)[i] = (byte)v; return;
            case 2:
                if ((ulong)v > ushort.MaxValue) throw NumRangeError(v);
                ((ushort[])_numData!)[i] = (ushort)v; return;
            case 3:
                if (v < int.MinValue || v > int.MaxValue) throw NumRangeError(v);
                ((int[])_numData!)[i] = (int)v; return;
            default:
                ((long[])_numData!)[i] = v; return;
        }
    }

    // Raw float element access (kind 5=float[] / 6=double[]). single-float
    // storage narrows the double to float on store; NaN/inf are legal values
    // so there is no range check (unlike the integer NumSet).
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal double NumGetF(int i) => _numKind == 5
        ? ((float[])_numData!)[i]
        : ((double[])_numData!)[i];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void NumSetF(int i, double v)
    {
        if (_numKind == 5) ((float[])_numData!)[i] = (float)v;
        else ((double[])_numData!)[i] = v;
    }

    // Box the element at index i per the backing kind: Fixnum for integer
    // kinds, SingleFloat/DoubleFloat for float kinds.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal LispObject NumBox(int i) => _numKind >= 5
        ? (_numKind == 5 ? new SingleFloat((float)NumGetF(i)) : (LispObject)new DoubleFloat(NumGetF(i)))
        : Fixnum.Make(NumGet(i));

    // Store a boxed value into numeric backing at (already bounds-checked)
    // index i, dispatching on the backing kind. Returns false when val's type
    // is incompatible with the backing so the caller can fall to the general
    // (boxed) slow path. Integer range violations still throw loudly via NumSet.
    internal bool TryNumStore(int i, LispObject val)
    {
        if (_numKind >= 5)
        {
            switch (val)
            {
                case DoubleFloat df: NumSetF(i, df.Value); return true;
                case SingleFloat sf: NumSetF(i, sf.Value); return true;
                case Fixnum fx: NumSetF(i, fx.Value); return true;
                default: return false;
            }
        }
        if (val is Fixnum nf) { NumSet(i, nf.Value); return true; }
        return false;
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

    public LispVector(int size, LispObject initialElement, string elementType)
    {
        ElementTypeName = elementType;
        if (elementType == "BIT")
        {
            _bitData = new ulong[(size + 63) / 64];
            _elements = Array.Empty<LispObject>();
            // If initial element is 1, fill all bits
            if (initialElement is Fixnum f && f.Value == 1)
                Compat.Fill(_bitData, ulong.MaxValue);
        }
        else if ((_numKind = NumKindForElementType(elementType)) != 0)
        {
            _numData = AllocNum(size);
            _elements = Array.Empty<LispObject>();
            if (_numKind >= 5)
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

    private static ulong[] PackBits(LispObject[] elements)
    {
        int size = elements.Length;
        var data = new ulong[(size + 63) / 64];
        for (int i = 0; i < size; i++)
        {
            if (elements[i] is Fixnum f && f.Value != 0)
                data[i >> 6] |= 1UL << (i & 63);
        }
        return data;
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

    // Returns true if array was created with :adjustable t
    public bool IsAdjustable { get => _isAdjustable; set => _isAdjustable = value; }

    // Raw element get/set — handles displacement transparently
    private LispObject RawGet(int index)
    {
        if (_displacedTo != null) return _displacedTo.RawGet(_displacedOffset + index);
        if (_bitData != null)
            return Fixnum.Make((long)((_bitData[index >> 6] >> (index & 63)) & 1));
        if (_numData != null)
            return NumBox(index);
        return _elements[index] ?? Nil.Instance;
    }

    private void RawSet(int index, LispObject val)
    {
        if (_displacedTo != null) { _displacedTo.RawSet(_displacedOffset + index, val); return; }
        if (_bitData != null)
        {
            long bit = val is Fixnum f ? f.Value : 0;
            if (bit != 0)
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
            if (elem is LispChar c) RawSet(i, LispChar.Make(char.ToUpperInvariant(c.Value)));
        }
    }

    public void ToLowerInPlace(int start, int end)
    {
        for (int i = start; i < end; i++)
        {
            var elem = RawGet(i);
            if (elem is LispChar c) RawSet(i, LispChar.Make(char.ToLowerInvariant(c.Value)));
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
                RawSet(i, LispChar.Make(wordBoundary ? char.ToUpperInvariant(c) : char.ToLowerInvariant(c)));
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
                _numKind = kind;
                _numData = AllocNum(newSize);
                var oldSizeN = _declaredSize;
                for (int i = 0; i < Math.Min(oldSizeN, newSize); i++)
                {
                    // Old storage is the displaced target: read through it.
                    var e = _displacedTo.RawGet(_displacedOffset + i);
                    TryNumStore(i, e); // int and float kinds; incompatible leaves 0
                }
                if (initialElement is not null && initialElement is not Nil)
                    for (int i = oldSizeN; i < newSize; i++) TryNumStore(i, initialElement);
                _elements = Array.Empty<LispObject>();
            }
            else if (_bitData != null)
            {
                var newBits = new ulong[(newSize + 63) / 64];
                var oldSize = _declaredSize;
                for (int i = 0; i < Math.Min(oldSize, newSize); i++)
                {
                    if (((_displacedTo._bitData != null
                          ? (_displacedTo._bitData[(i + _displacedOffset) >> 6] >> ((i + _displacedOffset) & 63)) & 1
                          : (RawGet(i) is Fixnum f && f.Value != 0 ? 1UL : 0UL)) != 0))
                        newBits[i >> 6] |= 1UL << (i & 63);
                }
                long fillBit = initialElement is Fixnum fi ? fi.Value : 0;
                if (fillBit != 0)
                    for (int i = oldSize; i < newSize; i++)
                        newBits[i >> 6] |= 1UL << (i & 63);
                _bitData = newBits;
                _elements = Array.Empty<LispObject>();
            }
            else
            {
                var newElems = new LispObject[newSize];
                var oldSize = _declaredSize;
                LispObject fill = initialElement ?? Nil.Instance;
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
                // Fill new bits if expanding with 1
                long fillBit = initialElement is Fixnum fi ? fi.Value : 0;
                if (fillBit != 0)
                    for (int i = oldSize; i < newSize; i++)
                        newBits[i >> 6] |= 1UL << (i & 63);
                _bitData = newBits;
            }
        }
        else if (_numData != null)
        {
            var oldSize = _numData.Length;
            if (newSize != oldSize)
            {
                var oldNum = _numData;
                _numData = AllocNum(newSize);
                Array.Copy(oldNum, _numData, Math.Min(oldSize, newSize));
                if (initialElement is not null && initialElement is not Nil)
                    for (int i = oldSize; i < newSize; i++) TryNumStore(i, initialElement);
            }
        }
        else
        {
            var oldSize = _elements.Length;
            if (newSize != oldSize)
            {
                var newElems = new LispObject[newSize];
                Array.Copy(_elements, newElems, Math.Min(oldSize, newSize));
                LispObject fill = initialElement ?? Nil.Instance;
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
        else _fillPointer = newSize;
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
        else _fillPointer = newSize;
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

public class LispHashTable : LispObject
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
    // When Synchronized, all mutating/reading operations take _lock.
    // Concurrent access without Synchronized is undefined per CLHS (mirrors
    // SBCL: make-hash-table :synchronized t opts in to thread-safety).
    private readonly object _lock = new();
    public bool Synchronized { get; }
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
    {
        _testName = test.ToUpperInvariant();
        _test = _testName switch
        {
            "EQ" => (a, b) => Runtime.IsEqRef(a, b),
            "EQL" => Eql,
            "EQUAL" => LispEqual,
            "EQUALP" => Equalp,
            _ => throw new ArgumentException($"Unknown hash table test: {test}")
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
                    throw new ArgumentException(
                        $":weakness {weakness.ToLowerInvariant()} invalid " +
                        "(expected :key, :value, :key-and-value, or :key-or-value)");
            }
        }
        // Key-weak tables box the key in a WeakKeyBox so the dict doesn't root it;
        // the comparer resolves boxes (and bare live keys used for lookup) through
        // the test. Strong/value-only tables key on the LispObject directly.
        var inner = new LispEqualityComparer(_test, _testName);
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
        // For OR-mode a side may be dead but the other alive; surface NIL? No —
        // CL semantics keep the pair; if one side is collected the entry is
        // logically gone for use. We require both resolvable to hand back a pair;
        // a half-dead OR entry is treated as live-but-unusable → prune lazily.
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
            if (_dict.ContainsKey(key)) { _dict[key] = WrapValue(value); return; }
            _dict[new WeakKeyBox(key, ((WeakKeyComparer)_dict.Comparer).HashOf(key))] = WrapValue(value);
            return;
        }
        _dict[key] = WrapValue(value);
    }

    public bool Remove(LispObject key)
    {
        if (Synchronized) { lock (_lock) return RemoveByLookup(key); }
        return RemoveByLookup(key);
    }
    // Remove by live key. The resolving comparer matches the stored box, so a
    // bare key removes the corresponding weak entry.
    private bool RemoveByLookup(LispObject key) => _dict.Remove(key);

    public void Clear()
    {
        if (Synchronized) lock (_lock) _dict.Clear();
        else _dict.Clear();
    }

    public int Count
    {
        get
        {
            if (Weakness == null)
            {
                if (Synchronized) lock (_lock) return _dict.Count;
                return _dict.Count;
            }
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
    {
        if (Synchronized)
        {
            KeyValuePair<LispObject, LispObject>[] snapshot;
            lock (_lock) snapshot = SnapshotAlive();
            foreach (var pair in snapshot)
                action(pair.Key, pair.Value);
            return;
        }
        if (Weakness != null)
        {
            // Take a snapshot for weak tables so the action's side effects
            // (which may keep dead values alive momentarily) don't confuse
            // the prune walk.
            var snap = SnapshotAlive();
            foreach (var pair in snap)
                action(pair.Key, pair.Value);
            return;
        }
        // Snapshot before iterating so ACTION may add/remove entries during the
        // walk. CLHS leaves mid-MAPHASH modification undefined, but SBCL/CCL/ECL
        // tolerate it and real code relies on it (e.g. lem's
        // add-lisp-color-aliases inserts aliases while mapping the color table).
        // Iterating Dictionary live would throw InvalidOperationException
        // ("Collection was modified"); snapshotting matches the other impls.
        var snapshot2 = new KeyValuePair<object, object>[_dict.Count];
        ((System.Collections.Generic.ICollection<KeyValuePair<object, object>>)_dict)
            .CopyTo(snapshot2, 0);
        foreach (var pair in snapshot2)
            action((LispObject)pair.Key, (LispObject)pair.Value);
    }

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
        // Iterative loop to handle conses without stack overflow
        while (true)
        {
            if (LispEqual(a, b)) return true;
            if (a is Number na && b is Number nb)
                return Arithmetic.IsNumericEqual(na, nb);
            if (a is LispChar ca && b is LispChar cb)
                return char.ToUpperInvariant(ca.Value) == char.ToUpperInvariant(cb.Value);
            // Cons (list) comparison: recurse on car, iterate on cdr
            if (a is Cons ca2 && b is Cons cb2)
            {
                if (!Equalp(ca2.Car, cb2.Car)) return false;
                a = ca2.Cdr;
                b = cb2.Cdr;
                continue;
            }
            // String comparisons: case-insensitive, handle LispString <-> char-vector
            bool aIsStr = a is LispString || (a is LispVector av && av.IsCharVector);
            bool bIsStr = b is LispString || (b is LispVector bv && bv.IsCharVector);
            if (aIsStr && bIsStr)
            {
                string sa2 = a is LispString ls1 ? ls1.Value : ((LispVector)a).ToCharString();
                string sb2 = b is LispString ls2 ? ls2.Value : ((LispVector)b).ToCharString();
                return string.Equals(sa2, sb2, StringComparison.OrdinalIgnoreCase);
            }
            // CL spec: arrays are equalp if same dimensions and elements are pairwise equalp.
            int aLen = a is LispString las ? las.Length : (a is LispVector lav ? lav.Length : -1);
            int bLen = b is LispString lbs ? lbs.Length : (b is LispVector lbv ? lbv.Length : -1);
            if (aLen >= 0 && bLen >= 0 && aLen == bLen)
            {
                LispObject GetAt(LispObject seq, int i) => seq is LispString s
                    ? LispChar.Make(s[i])
                    : ((LispVector)seq).ElementAt(i);
                for (int i = 0; i < aLen; i++)
                    if (!Equalp(GetAt(a, i), GetAt(b, i))) return false;
                return true;
            }
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
                    && Runtime.IsTruthy(Runtime.Equal(pa.Version ?? Nil.Instance, pb.Version ?? Nil.Instance));
            }
            // Struct comparison: same type, all slots equalp
            if (a is LispStruct sa && b is LispStruct sb)
            {
                if (sa.TypeName.Name != sb.TypeName.Name) return false;
                if (sa.Slots.Length != sb.Slots.Length) return false;
                for (int i = 0; i < sa.Slots.Length; i++)
                    if (!Equalp(sa.Slots[i], sb.Slots[i])) return false;
                return true;
            }
            return false;
        }
    }

    public override string ToString() => $"#<HASH-TABLE :{_testName}{(Synchronized ? " :SYNCHRONIZED" : "")} {_dict.Count}/{_dict.Count}>";

    private class LispEqualityComparer : IEqualityComparer<LispObject>
    {
        private readonly Func<LispObject, LispObject, bool> _test;
        private readonly string _testName;

        public LispEqualityComparer(Func<LispObject, LispObject, bool> test, string testName)
        {
            _test = test;
            _testName = testName;
        }

        public bool Equals(LispObject? x, LispObject? y)
        {
            if (x == null && y == null) return true;
            if (x == null || y == null) return false;
            return _test(x, y);
        }

        // Normalize T.Instance/Nil.Instance to their symbol forms so EQ/EQL
        // hash tables treat them as the same key.
        private static LispObject Canonical(LispObject obj) =>
            obj is T ? (LispObject)(Startup.T_SYM ?? obj) :
            obj is Nil ? (Startup.NIL_SYM ?? (LispObject)obj) : obj;

        public int GetHashCode(LispObject obj)
        {
            return _testName switch
            {
                "EQ" => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(Canonical(obj)),
                "EQL" => NumericOrCharHash(obj)
                         ?? System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(Canonical(obj)),
                "EQUAL" => GetEqualHash(obj),
                "EQUALP" => GetEqualpHash(obj),
                _ => obj.GetHashCode()
            };
        }

        // EQL compares numbers by type and value, so every number must hash by
        // value. Falling through to the identity hash (as bignums, ratios and
        // complexes used to) puts two EQL keys in different buckets, and the
        // table can never find them again — EQUAL inherits this through its
        // number case, which is how a float-keyed EQUAL table silently
        // accumulated one entry per lookup. Returns null for non-numbers.
        // Collisions across types are fine (1 vs 1.0d0 hash alike); the test
        // function still separates them.
        private static int? NumericOrCharHash(LispObject obj) => obj switch
        {
            Fixnum f => f.Value.GetHashCode(),
            LispChar c => c.Value.GetHashCode(),
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

        private static int GetEqualHash(LispObject obj, int depth)
        {
            if (depth == 0) return 0;
            return obj switch
            {
                LispString s => s.Value.GetHashCode(),
                LispVector v when v.IsCharVector => v.ToCharString().GetHashCode(),
                Cons c => HashCode.Combine(GetEqualHash(c.Car, depth - 1), GetEqualHash(c.Cdr, depth - 1)),
                LispVector bv when bv.IsBitVector => HashBitVector(bv),
                // EQUAL falls back to EQL for numbers and characters, so they
                // must hash by value here too — including inside a cons, which
                // is how SBCL's inline-constant table keys its float constants.
                _ => NumericOrCharHash(obj)
                     ?? System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj)
            };
        }

        private static int HashBitVector(LispVector bv)
        {
            var h = new HashCode();
            for (int i = 0; i < bv.Length; i++)
                h.Add(bv.GetElement(i) is Fixnum f ? f.Value : 0);
            return h.ToHashCode();
        }

        private static int GetEqualpHash(LispObject obj) => GetEqualpHash(obj, EqualHashMaxDepth);

        private static int GetEqualpHash(LispObject obj, int depth)
        {
            if (depth == 0) return 0;
            return obj switch
            {
                LispString s => s.Value.ToUpperInvariant().GetHashCode(),
                LispVector v when v.IsCharVector => v.ToCharString().ToUpperInvariant().GetHashCode(),
                LispChar c => char.ToUpperInvariant(c.Value).GetHashCode(),
                Number n => Arithmetic.ToDouble(n).GetHashCode(),
                Cons c => HashCode.Combine(GetEqualpHash(c.Car, depth - 1), GetEqualpHash(c.Cdr, depth - 1)),
                LispVector v => v.Length == 0 ? 0 : GetEqualpHash(v.ElementAt(0), depth - 1),
                _ => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj)
            };
        }
    }
}

public class LispRandomState : LispObject
{
    private ulong _s0, _s1;

    public LispRandomState() {
        var rng = new Random();
        _s0 = (ulong)rng.NextInt64();
        _s1 = (ulong)rng.NextInt64();
        if (_s0 == 0 && _s1 == 0) _s1 = 1;
    }

    public LispRandomState(int seed) {
        var rng = new Random(seed);
        _s0 = (ulong)rng.NextInt64();
        _s1 = (ulong)rng.NextInt64();
        if (_s0 == 0 && _s1 == 0) _s1 = 1;
    }

    // Copy constructor - key for make-random-state
    public LispRandomState(LispRandomState other) {
        _s0 = other._s0;
        _s1 = other._s1;
    }

    // xorshift128+ PRNG
    public ulong Next() {
        ulong s1 = _s0;
        ulong s0 = _s1;
        _s0 = s0;
        s1 ^= s1 << 23;
        _s1 = s1 ^ s0 ^ (s1 >> 17) ^ (s0 >> 26);
        return _s1 + s0;
    }

    // Random integer in [0, limit) for BigInteger limit
    public System.Numerics.BigInteger NextBelow(System.Numerics.BigInteger limit) {
        if (limit <= 0) throw new ArgumentException("limit must be positive");
        if (limit <= long.MaxValue) {
            long lim = (long)limit;
            return (long)(Next() % (ulong)lim);
        }
        int byteCount = limit.GetByteCount(isUnsigned: true) + 1;
        byte[] bytes = new byte[byteCount];
        for (int i = 0; i < byteCount; i++)
            bytes[i] = (byte)(Next() & 0xFF);
        bytes[byteCount - 1] = 0; // ensure positive
        var result = Compat.MakeBigInteger(bytes, isUnsigned: true);
        return result % limit;
    }

    // Random double in [0.0, 1.0)
    public double NextDouble() {
        return (double)(Next() >> 11) / (1UL << 53);
    }

    // Random float in [0.0f, 1.0f)
    public float NextSingle() {
        return (float)(Next() >> 40) / (1UL << 24);
    }

    public override string ToString() => "#<RANDOM-STATE>";

    /// <summary>Readable form that can be read back via #. eval.</summary>
    /// The constructor is a dotcl extension living in DOTCL-INTERNAL, so that is the
    /// package it must be printed in. It used to be spelled COMMON-LISP::, which is
    /// a symbol that has no function: the compiled path still worked because the
    /// compiler resolves a call by NAME through CilAssembler (which bridges bare
    /// names across dotcl's own packages), but the tree-walk evaluator resolves the
    /// operator through SYMBOL-FUNCTION on that exact symbol and got
    /// "Undefined function: MAKE-RANDOM-STATE-FROM-SEEDS" (ansi-test
    /// PRINT.RANDOM-STATE.1). Reading the old spelling also interned a
    /// non-standard name into COMMON-LISP as a side effect.
    public string ToReadableString() =>
        $"#.(DOTCL-INTERNAL::MAKE-RANDOM-STATE-FROM-SEEDS {(System.Numerics.BigInteger)_s0} {(System.Numerics.BigInteger)_s1})";

    /// <summary>Restore a random state from its two seed values.</summary>
    public static LispRandomState FromSeeds(ulong s0, ulong s1)
    {
        var rs = new LispRandomState();
        rs._s0 = s0;
        rs._s1 = s1;
        return rs;
    }
}

/// <summary>Pprint dispatch table (stub for ANSI compliance).</summary>
public class LispPprintDispatchTable : LispObject
{
    /// <summary>Entries: type-specifier-key → (type-specifier, function, priority)</summary>
    public Dictionary<string, (LispObject TypeSpec, LispObject Function, double Priority)> Entries { get; }

    public LispPprintDispatchTable()
    {
        Entries = new Dictionary<string, (LispObject, LispObject, double)>();
    }

    public LispPprintDispatchTable(LispPprintDispatchTable other)
    {
        Entries = new Dictionary<string, (LispObject, LispObject, double)>(other.Entries);
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
        // trackResurrection: false — value becomes unreachable once collected.
        _ref = new WeakReference<LispObject>(target);
    }

    /// <summary>The referenced object, or NIL if it has been collected.</summary>
    public LispObject Value => _ref.TryGetTarget(out var t) ? t : Nil.Instance;

    public override string ToString() => "#<WEAK-POINTER>";
}
