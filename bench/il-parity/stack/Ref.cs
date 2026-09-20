namespace IlParity;

/// <summary>A fixnum stack over a flat array: push, pop, peek, count.
///
/// The simplest shape in the set, and the one that isolates the cost of the
/// container itself. Every method is a field read, an element access and a
/// field write -- if anything boxes here it boxes everywhere.</summary>
public sealed class Stack
{
    private readonly long[] _items;
    private int _count;

    public Stack(int capacity)
    {
        _items = new long[capacity];
        _count = 0;
    }

    public long Push(long v)
    {
        long[] items = _items;
        int n = _count;
        items[n] = v;
        _count = n + 1;
        return v;
    }

    public long Pop()
    {
        int n = _count - 1;
        _count = n;
        return _items[n];
    }

    public long Peek() => _items[_count - 1];

    public long Count => _count;

    /// <summary>Push 1..n, pop half of them back, and return a number that
    /// depends on every step. The Lisp side computes the same number, so the
    /// two implementations are known to agree before their IL is compared.</summary>
    public static long SelfCheck(long n)
    {
        var s = new Stack((int)n);
        long acc = 0;
        for (long i = 0; i < n; i++) s.Push(i * 3);
        for (long i = 0; i < n / 2; i++) acc += s.Pop();
        acc += s.Peek();
        acc += s.Count;
        return acc;
    }
}
