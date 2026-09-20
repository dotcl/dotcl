namespace IlParity;

/// <summary>An open-addressed hash table with fixnum keys and linear probing.
///
/// Two parallel arrays and a state array rather than an array of entries: the
/// point of comparison is the probe loop, and an entry struct would drag the
/// struct-representation question (how a value type is stored inside an array)
/// into a case that is supposed to be about indexing and arithmetic.
///
/// Key 0 is a legal key; occupancy is carried by _state, so the table never has
/// to reserve a sentinel value out of the key space.</summary>
public sealed class HashTable
{
    private const int Empty = 0;
    private const int Full = 1;

    private readonly long[] _keys;
    private readonly long[] _values;
    private readonly byte[] _state;
    private readonly int _mask;
    private int _count;

    /// <summary>CAPACITY must be a power of two; the mask is what makes the
    /// wrap-around a single AND instead of a division.</summary>
    public HashTable(int capacity)
    {
        _keys = new long[capacity];
        _values = new long[capacity];
        _state = new byte[capacity];
        _mask = capacity - 1;
        _count = 0;
    }

    public long Hash(long key)
    {
        long h = key * 2654435761L;
        return (h ^ (h >> 15)) & _mask;
    }

    public long Put(long key, long value)
    {
        long[] keys = _keys;
        byte[] state = _state;
        long i = Hash(key);
        while (state[(int)i] == Full)
        {
            if (keys[(int)i] == key) { _values[(int)i] = value; return value; }
            i = (i + 1) & _mask;
        }
        keys[(int)i] = key;
        _values[(int)i] = value;
        state[(int)i] = Full;
        _count++;
        return value;
    }

    /// <summary>The value for KEY, or NOTFOUND when it is absent -- returned
    /// rather than signalled, so the probe loop is the only control flow in
    /// the method on both sides.</summary>
    public long Get(long key, long notFound)
    {
        long[] keys = _keys;
        byte[] state = _state;
        long i = Hash(key);
        while (state[(int)i] == Full)
        {
            if (keys[(int)i] == key) return _values[(int)i];
            i = (i + 1) & _mask;
        }
        return notFound;
    }

    public long Count => _count;

    public static long SelfCheck(long n)
    {
        var h = new HashTable(1024);
        for (long i = 0; i < n; i++) h.Put(i * 7, i * 11);
        long acc = 0;
        for (long i = 0; i < n; i++) acc += h.Get(i * 7, -1);
        acc += h.Get(999999, -1);
        acc += h.Count;
        return acc;
    }
}
