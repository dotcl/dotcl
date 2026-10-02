using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace DotCL;

/// <summary>
/// Load-time construction of fasl literals from data instead of from IL.
///
/// Code in a fasl that runs exactly once per load (building the objects that
/// MAKE-LOAD-FORM-SAVING-SLOTS describes, filling a literal hash table) costs
/// more to JIT-compile than to run. The compiler writes such payloads as a byte
/// string in the PE data section, and the loader rebuilds the objects here with
/// code that is already compiled. Only objects whose reconstruction is plain
/// data go this way (no sharing inside one value, nothing that needs a user
/// MAKE-LOAD-FORM); anything else keeps the IL route.
///
/// A payload ("part") starts with its own symbol table, so each interned symbol
/// is looked up once per part. All parts of a fasl are stored as one blob and
/// split into a byte[][] by the fasl's type initializer.
/// </summary>
public static class FaslData
{
    internal const byte TNil = 0, TT = 1, TFixnum = 2, TString = 3, TSymbol = 4, TUninterned = 5,
        TChar = 6, TSingle = 7, TDouble = 8, TBignum = 9, TRatio = 10, TList = 11, TVector = 12,
        TInstance = 13, THashTable = 14, TComplex = 15, THtPrototype = 16,
        TShared = 17, TDef = 18, TRef = 19;

    /// <summary>Split the joined parts (each prefixed by its length) back into
    /// the parts. The blob may come in several pieces: one data field can hold
    /// only so much.</summary>
    public static byte[][] SplitParts(byte[][] pieces)
    {
        int total = 0;
        foreach (var p in pieces) total += p.Length;
        var all = new byte[total];
        int at = 0;
        foreach (var p in pieces) { Buffer.BlockCopy(p, 0, all, at, p.Length); at += p.Length; }
        return SplitParts(all);
    }

    public static byte[][] SplitParts(byte[] all)
    {
        var parts = new List<byte[]>();
        int pos = 0;
        while (pos < all.Length)
        {
            int len = (int)ReadVarint(all, ref pos);
            var p = new byte[len];
            Buffer.BlockCopy(all, pos, p, 0, len);
            pos += len;
            parts.Add(p);
        }
        return parts.ToArray();
    }

    /// <summary>Run a batch of MAKE-LOAD-FORM-SAVING-SLOTS creations: for each
    /// object, nothing when its key is already interned, else allocate it,
    /// intern it under the key and set the saved slots.</summary>
    public static LispObject RunMlfBatch(byte[][] parts, int index, LispObject?[] cache, Symbol[]? gsyms,
        LispObject?[]? shared)
    {
        var data = parts[index];
        var r = new Reader(data, gsyms, parts, cache, shared);
        while (r.Pos < data.Length)
        {
            string key = r.Str();
            int skipTo = (int)r.Varint();
            skipTo += r.Pos;
            if (LispInstance.IsInterned(key)) { r.Pos = skipTo; continue; }
            var className = r.Value();
            var obj = LispInstance.InternAllocateInstance(key, className);
            int slots = (int)r.Varint();
            for (int s = 0; s < slots; s++)
            {
                var slot = r.Value();
                var value = r.Value();
                Runtime.SetSlotValue(obj, slot, value);
            }
        }
        return Nil.Instance;
    }

    /// <summary>Run a batch of MAKE-LOAD-FORM creation forms: for each object,
    /// nothing when its key is already interned, else evaluate the form and
    /// intern the result (LispInstance.InternViaEval).</summary>
    public static LispObject RunMlfEvalBatch(byte[][] parts, int index, LispObject?[] cache, Symbol[]? gsyms,
        LispObject?[]? shared)
    {
        var data = parts[index];
        var r = new Reader(data, gsyms, parts, cache, shared);
        while (r.Pos < data.Length)
        {
            string key = r.Str();
            var form = r.Value();
            LispInstance.InternViaEval(key, form);
        }
        return Nil.Instance;
    }

    /// <summary>The prototype of a literal hash table, built on first use and
    /// kept in CACHE[INDEX]. The part names the table's test, the prototype it
    /// starts from (a copy of it; -1 for none), the keys to remove from that
    /// copy and the entries to set.</summary>
    public static LispHashTable HtPrototype(byte[][] parts, int index, LispObject?[] cache, Symbol[]? gsyms)
    {
        if (cache[index] is LispHashTable done) return done;
        var r = new Reader(parts[index], gsyms, parts, cache);
        string test = r.Str();
        int baseIndex = (int)r.Varint() - 1;
        var ht = baseIndex >= 0
            ? LispHashTable.CopyLiteral(HtPrototype(parts, baseIndex, cache, gsyms))
            : new LispHashTable(test);
        int removed = (int)r.Varint();
        for (int i = 0; i < removed; i++) ht.Remove(r.Value());
        int added = (int)r.Varint();
        for (int i = 0; i < added; i++)
        {
            var k = r.Value();
            var v = r.Value();
            ht.Set(k, v);
        }
        cache[index] = ht;
        return ht;
    }

    /// <summary>The literal held in data part INDEX.</summary>
    public static LispObject ReadValue(byte[][] parts, int index, LispObject?[] cache, Symbol[]? gsyms,
        LispObject?[]? shared) =>
        new Reader(parts[index], gsyms, parts, cache, shared).Value();

    /// <summary>Keep V in SLOTS[SLOT], the place every literal of a top level
    /// form that contains it looks first; returns V.</summary>
    public static LispObject Share(LispObject v, LispObject?[] slots, int slot)
    {
        slots[slot] = v;
        return v;
    }

    /// <summary>Intern the symbols a fasl names (see Startup.PreinternSymbol),
    /// listed as (package, name) pairs.</summary>
    public static void Preintern(byte[] data)
    {
        int pos = 0;
        int n = (int)ReadVarint(data, ref pos);
        for (int i = 0; i < n; i++)
        {
            string pkg = ReadStr(data, ref pos);
            string name = ReadStr(data, ref pos);
            Startup.PreinternSymbol(name, pkg);
        }
    }

    internal static ulong ReadVarint(byte[] b, ref int pos)
    {
        ulong v = 0;
        int shift = 0;
        while (true)
        {
            byte x = b[pos++];
            v |= (ulong)(x & 0x7f) << shift;
            if ((x & 0x80) == 0) return v;
            shift += 7;
        }
    }

    internal static string ReadStr(byte[] b, ref int pos)
    {
        int len = (int)ReadVarint(b, ref pos);
        var s = Encoding.UTF8.GetString(b, pos, len);
        pos += len;
        return s;
    }

    private sealed class Reader
    {
        private readonly byte[] _b;
        private readonly Symbol[]? _gsyms;
        private readonly Symbol[] _syms;
        private readonly byte[][]? _parts;
        private readonly LispObject?[]? _cache;
        private readonly LispObject?[]? _shared;
        private List<LispObject?>? _locals;
        public int Pos;

        public Reader(byte[] b, Symbol[]? gsyms, byte[][]? parts = null, LispObject?[]? cache = null,
            LispObject?[]? shared = null)
        {
            _b = b;
            _gsyms = gsyms;
            _parts = parts;
            _cache = cache;
            _shared = shared;
            int n = (int)Varint();
            _syms = new Symbol[n];
            for (int i = 0; i < n; i++)
            {
                string pkg = Str();
                string name = Str();
                _syms[i] = pkg == "KEYWORD" ? Startup.Keyword(name) : Startup.SymInPkg(name, pkg);
            }
        }

        public ulong Varint() => ReadVarint(_b, ref Pos);
        public string Str() => ReadStr(_b, ref Pos);

        public LispObject Value()
        {
            byte tag = _b[Pos++];
            switch (tag)
            {
                case TNil: return Nil.Instance;
                case TT: return T.Instance;
                case TFixnum:
                {
                    ulong z = Varint();
                    return Fixnum.Make((long)(z >> 1) ^ -(long)(z & 1));
                }
                case TString: return new LispString(Str());
                case TSymbol: return _syms[(int)Varint()];
                case TUninterned: return _gsyms![(int)Varint()];
                case TChar: return LispChar.Make((char)Varint());
                case TSingle:
                {
                    int bits = BitConverter.ToInt32(_b, Pos);
                    Pos += 4;
                    return new SingleFloat(Compat.Int32BitsToSingle(bits));
                }
                case TDouble:
                {
                    long bits = BitConverter.ToInt64(_b, Pos);
                    Pos += 8;
                    return new DoubleFloat(BitConverter.Int64BitsToDouble(bits));
                }
                case TBignum: return new Bignum(BigInteger.Parse(Str()));
                case TRatio:
                {
                    var num = BigInteger.Parse(Str());
                    var den = BigInteger.Parse(Str());
                    return Ratio.Make(num, den);
                }
                case TComplex:
                {
                    var re = (Number)Value();
                    var im = (Number)Value();
                    return LispComplex.Of(re, im);
                }
                case TList:
                {
                    int n = (int)Varint();
                    var items = new LispObject[n];
                    for (int i = 0; i < n; i++) items[i] = Value();
                    LispObject list = Value();
                    for (int i = n - 1; i >= 0; i--) list = new Cons(items[i], list);
                    return list;
                }
                case TVector:
                {
                    int n = (int)Varint();
                    var items = new LispObject[n];
                    for (int i = 0; i < n; i++) items[i] = Value();
                    return new LispVector(items);
                }
                case TInstance: return LispInstance.InternViaEval(Str(), Nil.Instance);
                case THashTable:
                {
                    var ht = new LispHashTable(Str());
                    int n = (int)Varint();
                    for (int i = 0; i < n; i++)
                    {
                        var k = Value();
                        var v = Value();
                        ht.Set(k, v);
                    }
                    return ht;
                }
                case TShared:
                {
                    int slot = (int)Varint();
                    int len = (int)Varint();
                    if (_shared![slot] is LispObject known) { Pos += len; return known; }
                    var v = Value();
                    _shared[slot] = v;
                    return v;
                }
                case TDef:
                {
                    int idx = (int)Varint();
                    _locals ??= new List<LispObject?>();
                    while (_locals.Count <= idx) _locals.Add(null);
                    var v = Value();
                    _locals[idx] = v;
                    return v;
                }
                case TRef: return _locals![(int)Varint()]!;
                case THtPrototype:
                    return LispHashTable.CopyLiteral(HtPrototype(_parts!, (int)Varint(), _cache!, _gsyms));
                default:
                    throw new InvalidOperationException($"fasl data: unknown tag {tag}");
            }
        }
    }
}

/// <summary>Compile-time side of <see cref="FaslData"/>: writes one part.
/// <see cref="TryValue"/> refuses (and the caller rolls back to
/// <see cref="Mark"/>) anything the reader could not rebuild as the IL route
/// would.</summary>
internal sealed class FaslDataWriter
{
    private readonly List<byte> _body = new();
    private readonly Dictionary<(string, string), int> _symIndex = new();
    private readonly List<(string Pkg, string Name)> _syms = new();

    /// <summary>Index of an uninterned symbol in the fasl's table, or -1 when
    /// there is no table.</summary>
    public Func<Symbol, int>? UninternedIndex;
    /// <summary>Key of an instance already registered for creation in this
    /// fasl, or null.</summary>
    public Func<LispInstance, string?>? InstanceKey;
    /// <summary>Called with (name, package) for every interned symbol written.</summary>
    public Action<string, string>? OnSymbol;
    /// <summary>For an object several literals of the current top level form
    /// contain: its slot in the fasl's shared array, else -1. The first
    /// literal built keeps it there and the others take it from there.</summary>
    public Func<LispObject, int>? SharedSlot;
    /// <summary>Objects that occur more than once in the value being written:
    /// written once, then referred to.</summary>
    public HashSet<LispObject>? LocalShared;
    private readonly Dictionary<LispObject, int> _localIdx = new(ReferenceEqualityComparer.Instance);
    /// <summary>For a hash table: the data part of its prototype (written as a
    /// copy of it), -2 to write it in full here, -1 to refuse.</summary>
    public Func<LispHashTable, int>? HtPrototypePart;

    private const int MaxDepth = 1000;

    public int Mark => _body.Count;

    public void Reset(int mark) => _body.RemoveRange(mark, _body.Count - mark);

    public void Varint(ulong v)
    {
        while (v >= 0x80) { _body.Add((byte)(v | 0x80)); v >>= 7; }
        _body.Add((byte)v);
    }

    public void Str(string s)
    {
        var b = Encoding.UTF8.GetBytes(s);
        Varint((ulong)b.Length);
        _body.AddRange(b);
    }

    /// <summary>Reserve room for a length that is known only after the bytes it
    /// measures are written; <see cref="PatchLength"/> fills it in.</summary>
    public int ReserveLength()
    {
        int at = _body.Count;
        for (int i = 0; i < 5; i++) _body.Add(0x80);
        _body[at + 4] = 0;
        return at;
    }

    public void PatchLength(int at)
    {
        uint len = (uint)(_body.Count - at - 5);
        for (int i = 0; i < 5; i++)
        {
            byte x = (byte)(len & 0x7f);
            len >>= 7;
            _body[at + i] = i < 4 ? (byte)(x | 0x80) : x;
        }
    }

    public bool TryValue(LispObject v)
    {
        var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        int mark = Mark;
        int locals = _localIdx.Count;
        if (Write(v, seen, 0)) return true;
        Reset(mark);
        if (_localIdx.Count != locals)
            foreach (var k in new List<LispObject>(_localIdx.Keys))
                if (_localIdx[k] >= locals) _localIdx.Remove(k);
        return false;
    }

    private bool IsSharedNode(LispObject v) =>
        (SharedSlot?.Invoke(v) ?? -1) >= 0 || (LocalShared?.Contains(v) ?? false);

    private int SymbolIndex(string pkg, string name)
    {
        if (!_symIndex.TryGetValue((pkg, name), out int idx))
        {
            idx = _syms.Count;
            _syms.Add((pkg, name));
            _symIndex[(pkg, name)] = idx;
        }
        return idx;
    }

    /// <summary>Objects written so far (each cons cell counts).</summary>
    public int Nodes;

    private bool Write(LispObject v, HashSet<LispObject> seen, int depth, bool here = false)
    {
        if (depth > MaxDepth) return false;
        Nodes++;
        if (!here && v is Cons or LispVector or LispHashTable)
        {
            int slot = SharedSlot?.Invoke(v) ?? -1;
            if (slot >= 0)
            {
                _body.Add(FaslData.TShared);
                Varint((ulong)slot);
                int len = ReserveLength();
                if (!Write(v, seen, depth, here: true)) return false;
                PatchLength(len);
                return true;
            }
            if (LocalShared != null && LocalShared.Contains(v))
            {
                if (_localIdx.TryGetValue(v, out int known))
                {
                    _body.Add(FaslData.TRef);
                    Varint((ulong)known);
                    return true;
                }
                int idx = _localIdx.Count;
                _localIdx[v] = idx;
                _body.Add(FaslData.TDef);
                Varint((ulong)idx);
                return Write(v, seen, depth, here: true);
            }
        }
        switch (v)
        {
            case Nil: _body.Add(FaslData.TNil); return true;
            case T: _body.Add(FaslData.TT); return true;
            case Fixnum f:
            {
                long x = f.Value;
                _body.Add(FaslData.TFixnum);
                Varint((ulong)((x << 1) ^ (x >> 63)));
                return true;
            }
            case LispString s:
                // As the IL route: a fresh simple string with the same characters.
                _body.Add(FaslData.TString);
                Str(s.Value);
                return true;
            case Symbol sym:
                if (sym.HomePackage != null)
                {
                    string pkg = sym.HomePackage.Name;
                    if (pkg != "KEYWORD") OnSymbol?.Invoke(sym.Name, pkg);
                    _body.Add(FaslData.TSymbol);
                    Varint((ulong)SymbolIndex(pkg, sym.Name));
                    return true;
                }
                if (UninternedIndex == null) return false;
                int ui = UninternedIndex(sym);
                if (ui < 0) return false;
                _body.Add(FaslData.TUninterned);
                Varint((ulong)ui);
                return true;
            case LispChar c:
                _body.Add(FaslData.TChar);
                Varint(c.Value);
                return true;
            case SingleFloat sf:
            {
                _body.Add(FaslData.TSingle);
                _body.AddRange(BitConverter.GetBytes(Compat.SingleToInt32Bits(sf.Value)));
                return true;
            }
            case DoubleFloat df:
                _body.Add(FaslData.TDouble);
                _body.AddRange(BitConverter.GetBytes(BitConverter.DoubleToInt64Bits(df.Value)));
                return true;
            case Bignum bn:
                _body.Add(FaslData.TBignum);
                Str(bn.Value.ToString());
                return true;
            case Ratio rat:
                _body.Add(FaslData.TRatio);
                Str(rat.Numerator.ToString());
                Str(rat.Denominator.ToString());
                return true;
            case LispComplex cx:
                _body.Add(FaslData.TComplex);
                return Write(cx.Real, seen, depth + 1) && Write(cx.Imaginary, seen, depth + 1);
            case Cons cons:
            {
                var items = new List<LispObject>();
                LispObject tail = cons;
                while (tail is Cons c)
                {
                    // A shared cell further down is written as the tail, so
                    // that it stays one object.
                    if (!ReferenceEquals(c, cons) && IsSharedNode(c)) break;
                    if (!seen.Add(c)) return false;
                    items.Add(c.Car);
                    tail = c.Cdr;
                    Nodes++;
                }
                _body.Add(FaslData.TList);
                Varint((ulong)items.Count);
                foreach (var it in items)
                    if (!Write(it, seen, depth + 1)) return false;
                return Write(tail, seen, depth + 1);
            }
            case LispVector vec:
            {
                if (vec.ElementTypeName != "T" || vec._dimensions != null) return false;
                if (!seen.Add(vec)) return false;
                _body.Add(FaslData.TVector);
                Varint((ulong)vec.Length);
                for (int i = 0; i < vec.Length; i++)
                    if (!Write(vec.ElementAt(i), seen, depth + 1)) return false;
                return true;
            }
            case LispInstance li:
            {
                var key = InstanceKey?.Invoke(li);
                if (key == null) return false;
                _body.Add(FaslData.TInstance);
                Str(key);
                return true;
            }
            case LispHashTable ht:
            {
                if (!seen.Add(ht)) return false;
                int proto = HtPrototypePart?.Invoke(ht) ?? -2;
                if (proto == -1) return false;
                if (proto >= 0)
                {
                    _body.Add(FaslData.THtPrototype);
                    Varint((ulong)proto);
                    return true;
                }
                _body.Add(FaslData.THashTable);
                Str(ht.TestName);
                var entries = new List<KeyValuePair<LispObject, LispObject>>(ht.Entries);
                Varint((ulong)entries.Count);
                foreach (var kv in entries)
                    if (!Write(kv.Key, seen, depth + 1) || !Write(kv.Value, seen, depth + 1)) return false;
                return true;
            }
            default:
                return false;
        }
    }

    /// <summary>The body alone, for a part with no symbol table.</summary>
    public byte[] BodyToArray() => _body.ToArray();

    /// <summary>The part: its symbol table, then the body.</summary>
    public byte[] ToArray()
    {
        var head = new FaslDataWriter();
        head.Varint((ulong)_syms.Count);
        foreach (var (pkg, name) in _syms) { head.Str(pkg); head.Str(name); }
        var result = new byte[head._body.Count + _body.Count];
        head._body.CopyTo(result, 0);
        _body.CopyTo(result, head._body.Count);
        return result;
    }

    /// <summary>Join parts into one blob, each prefixed by its length (the
    /// format <see cref="FaslData.SplitParts(byte[])"/> reads).</summary>
    public static byte[] JoinParts(IReadOnlyList<byte[]> parts)
    {
        var w = new FaslDataWriter();
        foreach (var p in parts)
        {
            w.Varint((ulong)p.Length);
            w._body.AddRange(p);
        }
        return w._body.ToArray();
    }
}
