using System.Collections;

namespace DotCL;

/// <summary>
/// Character-keyed tables for readtables. Not Dictionary&lt;char, T&gt; / HashSet&lt;char&gt;: a
/// framework generic over a value type (char, or an enum) is not precompiled in a
/// ReadyToRun image, so creating the first readtable would wait for the JIT. These
/// hold the Latin-1 range in an array and anything above it in a non-generic Hashtable.
/// </summary>
internal sealed class CharMap<T> where T : class
{
    private T?[]? _low;
    private Hashtable? _high;

    public bool TryGetValue(char ch, out T value)
    {
        T? found = ch < 256 ? _low?[ch] : _high?[ch] as T;
        value = found!;
        return found != null;
    }

    public bool ContainsKey(char ch) => TryGetValue(ch, out _);

    public T? this[char ch]
    {
        set
        {
            if (value == null) { Remove(ch); return; }
            if (ch < 256) (_low ??= new T?[256])[ch] = value;
            else (_high ??= new Hashtable())[ch] = value;
        }
    }

    public bool Remove(char ch)
    {
        if (ch < 256)
        {
            if (_low == null || _low[ch] == null) return false;
            _low[ch] = null;
            return true;
        }
        if (_high == null || !_high.ContainsKey(ch)) return false;
        _high.Remove(ch);
        return true;
    }

    public void Clear() { _low = null; _high = null; }

    /// <summary>Calls <paramref name="visit"/> for every entry.</summary>
    public void ForEach(Action<char, T> visit)
    {
        if (_low != null)
            for (int i = 0; i < 256; i++)
                if (_low[i] is { } v) visit((char)i, v);
        if (_high != null)
            foreach (DictionaryEntry e in _high)
                visit((char)e.Key, (T)e.Value!);
    }

    /// <summary>Copies the entries of <paramref name="other"/> into this table.</summary>
    public void CopyFrom(CharMap<T> other) => other.ForEach((k, v) => this[k] = v);
}

/// <summary>The syntax type of each character that has one set (see CharMap).</summary>
internal sealed class CharSyntaxMap
{
    // 0 = not set, otherwise the SyntaxType + 1.
    private byte[]? _low;
    private Hashtable? _high;

    public bool TryGetValue(char ch, out SyntaxType type)
    {
        int v = 0;
        if (ch < 256) { if (_low != null) v = _low[ch]; }
        else if (_high?[ch] is byte b) v = b;
        type = (SyntaxType)(v - 1);
        return v != 0;
    }

    public void Set(char ch, SyntaxType type)
    {
        byte v = (byte)((int)type + 1);
        if (ch < 256) (_low ??= new byte[256])[ch] = v;
        else (_high ??= new Hashtable())[ch] = v;
    }

    public void Clear() { _low = null; _high = null; }

    public void CopyFrom(CharSyntaxMap other)
    {
        if (other._low != null) _low = (byte[])other._low.Clone();
        else _low = null;
        _high = other._high == null ? null : new Hashtable(other._high);
    }
}

/// <summary>A set of characters (see CharMap).</summary>
internal sealed class CharBitSet
{
    private bool[]? _low;
    private Hashtable? _high;

    public bool Contains(char ch)
    {
        if (ch < 256) return _low != null && _low[ch];
        return _high != null && _high.ContainsKey(ch);
    }

    public void Add(char ch)
    {
        if (ch < 256) (_low ??= new bool[256])[ch] = true;
        else (_high ??= new Hashtable())[ch] = true;
    }

    public void Remove(char ch)
    {
        if (ch < 256) { if (_low != null) _low[ch] = false; }
        else _high?.Remove(ch);
    }

    public void Clear() { _low = null; _high = null; }

    public void CopyFrom(CharBitSet other)
    {
        _low = other._low == null ? null : (bool[])other._low.Clone();
        _high = other._high == null ? null : new Hashtable(other._high);
    }
}
