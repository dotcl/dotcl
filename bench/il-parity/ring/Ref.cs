namespace IlParity;

/// <summary>A fixed-capacity ring buffer of fixnums: the queue of the set.
///
/// Head, tail and a count, with the wrap done by AND against a power-of-two
/// mask. Every operation is two field reads, one element access and two field
/// writes -- the densest field traffic of the five, which is what makes it the
/// case where a struct representation difference shows up first.</summary>
public sealed class Ring
{
    private readonly long[] _items;
    private readonly int _mask;
    private int _head;
    private int _tail;
    private int _count;

    /// <summary>CAPACITY must be a power of two.</summary>
    public Ring(int capacity)
    {
        _items = new long[capacity];
        _mask = capacity - 1;
        _head = 0;
        _tail = 0;
        _count = 0;
    }

    public long Enqueue(long v)
    {
        int t = _tail;
        _items[t] = v;
        _tail = (t + 1) & _mask;
        _count++;
        return v;
    }

    public long Dequeue()
    {
        int h = _head;
        long v = _items[h];
        _head = (h + 1) & _mask;
        _count--;
        return v;
    }

    public long Peek() => _items[_head];

    public long Count => _count;

    public static long SelfCheck(long n)
    {
        var r = new Ring(64);
        long acc = 0;
        for (long i = 0; i < n; i++)
        {
            r.Enqueue(i * 5);
            // Keep it under capacity: enqueue two, drain one, so the indices
            // wrap many times over the run.
            if ((i & 1) == 1) acc += r.Dequeue();
        }
        while (r.Count > 0) acc += r.Dequeue();
        return acc;
    }
}
