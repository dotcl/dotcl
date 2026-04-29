using System.Collections.Concurrent;

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

    // Element type: "T" (general), "CHARACTER"/"BASE-CHAR"/"STANDARD-CHAR" (string-like), "NIL" (bit vector of nil), etc.
    public string ElementTypeName { get; private set; } = "T";

    public LispVector(int size, LispObject initialElement, string elementType)
    {
        ElementTypeName = elementType;
        if (elementType == "BIT")
        {
            _bitData = new ulong[(size + 63) / 64];
            _elements = Array.Empty<LispObject>();
            // If initial element is 1, fill all bits
            if (initialElement is Fixnum f && f.Value == 1)
                Array.Fill(_bitData, ulong.MaxValue);
        }
        else
        {
            _elements = new LispObject[size];
            Array.Fill(_elements, initialElement);
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
            // Converting from displaced to non-displaced
            if (_bitData != null)
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
            else
            {
                var newElems = new LispObject[newSize];
                Array.Copy(_elements, newElems, _elements.Length);
                _elements = newElems;
            }
            _declaredSize = newSize;
        }
        // Fast path: direct element write for non-displaced, non-bit arrays
        if (_displacedTo == null && _bitData == null)
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
    // For :weakness :value, _dict's value is a System.WeakReference<LispObject>;
    // for :weakness nil (default), it's the LispObject directly. Storing as
    // `object` keeps both shapes in one dictionary instance without doubling
    // the field surface (#147).
    private readonly Dictionary<LispObject, object> _dict;
    private readonly Func<LispObject, LispObject, bool> _test;
    private readonly string _testName;
    // When Synchronized, all mutating/reading operations take _lock.
    // Concurrent access without Synchronized is undefined per CLHS (mirrors
    // SBCL: make-hash-table :synchronized t opts in to thread-safety).
    private readonly object _lock = new();
    public bool Synchronized { get; }
    /// <summary>
    /// Weakness mode (SBCL extension). Currently :value is the only supported
    /// non-nil mode. Values are stored as WeakReference&lt;LispObject&gt; so that
    /// when no strong reference outside the table holds the value, it becomes
    /// eligible for GC and the entry surfaces as if absent. Required by
    /// sheeple. Other modes (:key / :key-or-value / :key-and-value) are not
    /// yet implemented and will throw at construction time.
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
        _dict = new Dictionary<LispObject, object>(
            new LispEqualityComparer(_test, _testName));
        Synchronized = synchronized;
        if (weakness != null)
        {
            var w = weakness.ToUpperInvariant();
            if (w != "VALUE")
                throw new ArgumentException(
                    $":weakness {weakness.ToLowerInvariant()} not yet supported (only :value)");
            Weakness = ":VALUE";
        }
        DotCL.Diagnostics.AllocCounter.Inc("LispHashTable");
    }

    /// <summary>
    /// Resolve a stored value to its live LispObject, or return null if the
    /// slot is a dead WeakReference. Caller decides what "dead" means
    /// (default value vs. absent).
    /// </summary>
    private LispObject? Resolve(object stored)
    {
        if (stored is WeakReference<LispObject> wr)
            return wr.TryGetTarget(out var v) ? v : null;
        return (LispObject)stored;
    }

    private object Wrap(LispObject value)
        => Weakness == ":VALUE" ? new WeakReference<LispObject>(value) : value;

    public LispObject Get(LispObject key, LispObject defaultValue)
    {
        if (Synchronized)
        {
            lock (_lock)
            {
                if (_dict.TryGetValue(key, out var stored))
                {
                    var live = Resolve(stored);
                    if (live != null) return live;
                    _dict.Remove(key);
                }
                return defaultValue;
            }
        }
        if (_dict.TryGetValue(key, out var s))
        {
            var live = Resolve(s);
            if (live != null) return live;
            _dict.Remove(key);
        }
        return defaultValue;
    }

    public bool TryGet(LispObject key, out LispObject value)
    {
        if (Synchronized)
        {
            lock (_lock)
            {
                if (_dict.TryGetValue(key, out var stored))
                {
                    var live = Resolve(stored);
                    if (live != null) { value = live; return true; }
                    _dict.Remove(key);
                }
                value = null!; return false;
            }
        }
        if (_dict.TryGetValue(key, out var s))
        {
            var live = Resolve(s);
            if (live != null) { value = live; return true; }
            _dict.Remove(key);
        }
        value = null!; return false;
    }

    public void Set(LispObject key, LispObject value)
    {
        if (Synchronized) lock (_lock) _dict[key] = Wrap(value);
        else _dict[key] = Wrap(value);
    }

    public bool Remove(LispObject key)
    {
        if (Synchronized) lock (_lock) return _dict.Remove(key);
        return _dict.Remove(key);
    }

    public void Clear()
    {
        if (Synchronized) lock (_lock) _dict.Clear();
        else _dict.Clear();
    }

    public int Count
    {
        get
        {
            // For weak tables, Count must skip dead entries; we lazily prune
            // them while counting so subsequent enumeration is consistent.
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
        var dead = new List<LispObject>();
        int alive = 0;
        foreach (var kv in _dict)
        {
            if (Resolve(kv.Value) != null) alive++;
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
                arr[i++] = new KeyValuePair<LispObject, LispObject>(kv.Key, (LispObject)kv.Value);
            return arr;
        }
        var dead = new List<LispObject>();
        var alive = new List<KeyValuePair<LispObject, LispObject>>();
        foreach (var kv in _dict)
        {
            var live = Resolve(kv.Value);
            if (live != null) alive.Add(new KeyValuePair<LispObject, LispObject>(kv.Key, live));
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
        foreach (var pair in _dict)
            action(pair.Key, (LispObject)pair.Value);
    }

    private static bool Eql(LispObject a, LispObject b)
    {
        if (Runtime.IsEqRef(a, b)) return true;
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value == fb.Value;
        if (a is LispChar ca && b is LispChar cb) return ca.Value == cb.Value;
        if (a is SingleFloat sa && b is SingleFloat sb) return sa.Value == sb.Value;
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value == db.Value;
        return false;
    }

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
        // hash tables treat them as the same key (issue #22).
        private static LispObject Canonical(LispObject obj) =>
            obj is T ? (LispObject)(Startup.T_SYM ?? obj) :
            obj is Nil ? (Startup.NIL_SYM ?? (LispObject)obj) : obj;

        public int GetHashCode(LispObject obj)
        {
            return _testName switch
            {
                "EQ" => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(Canonical(obj)),
                "EQL" => obj switch
                {
                    Fixnum f => f.Value.GetHashCode(),
                    LispChar c => c.Value.GetHashCode(),
                    SingleFloat sf => sf.Value.GetHashCode(),
                    DoubleFloat df => df.Value.GetHashCode(),
                    _ => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(Canonical(obj))
                },
                "EQUAL" => GetEqualHash(obj),
                "EQUALP" => GetEqualpHash(obj),
                _ => obj.GetHashCode()
            };
        }

        private static int GetEqualHash(LispObject obj) => obj switch
        {
            LispString s => s.Value.GetHashCode(),
            LispVector v when v.IsCharVector => v.ToCharString().GetHashCode(),
            Cons c => HashCode.Combine(GetEqualHash(c.Car), GetEqualHash(c.Cdr)),
            Fixnum f => f.Value.GetHashCode(),
            LispChar ch => ch.Value.GetHashCode(),
            LispVector bv when bv.IsBitVector => HashBitVector(bv),
            _ => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj)
        };

        private static int HashBitVector(LispVector bv)
        {
            var h = new HashCode();
            for (int i = 0; i < bv.Length; i++)
                h.Add(bv.GetElement(i) is Fixnum f ? f.Value : 0);
            return h.ToHashCode();
        }

        private static int GetEqualpHash(LispObject obj) => obj switch
        {
            LispString s => s.Value.ToUpperInvariant().GetHashCode(),
            LispVector v when v.IsCharVector => v.ToCharString().ToUpperInvariant().GetHashCode(),
            LispChar c => char.ToUpperInvariant(c.Value).GetHashCode(),
            Number n => Arithmetic.ToDouble(n).GetHashCode(),
            Cons c => HashCode.Combine(GetEqualpHash(c.Car), GetEqualpHash(c.Cdr)),
            LispVector v => v.Length == 0 ? 0 : GetEqualpHash(v.ElementAt(0)),
            _ => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj)
        };
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
        var result = new System.Numerics.BigInteger(bytes, isUnsigned: true);
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
    public string ToReadableString() =>
        $"#.(COMMON-LISP::MAKE-RANDOM-STATE-FROM-SEEDS {(System.Numerics.BigInteger)_s0} {(System.Numerics.BigInteger)_s1})";

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
