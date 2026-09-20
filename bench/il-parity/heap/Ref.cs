namespace IlParity;

/// <summary>A binary min-heap of fixnums.
///
/// The sift loops are the interesting part: index arithmetic that a compiler
/// either keeps in registers or spills through an object per step. Both sides
/// write the loops the same way -- a while with an explicit break -- so a
/// difference in the IL is a difference in lowering, not in phrasing.</summary>
public sealed class Heap
{
    private readonly long[] _items;
    private int _count;

    public Heap(int capacity)
    {
        _items = new long[capacity];
        _count = 0;
    }

    public long Push(long v)
    {
        long[] items = _items;
        int i = _count;
        items[i] = v;
        _count = i + 1;
        while (i > 0)
        {
            int parent = (i - 1) >> 1;
            if (items[parent] <= items[i]) break;
            long t = items[parent];
            items[parent] = items[i];
            items[i] = t;
            i = parent;
        }
        return v;
    }

    public long Pop()
    {
        long[] items = _items;
        long top = items[0];
        int n = _count - 1;
        _count = n;
        items[0] = items[n];
        int i = 0;
        while (true)
        {
            int left = 2 * i + 1;
            if (left >= n) break;
            int small = left;
            int right = left + 1;
            if (right < n && items[right] < items[left]) small = right;
            if (items[i] <= items[small]) break;
            long t = items[i];
            items[i] = items[small];
            items[small] = t;
            i = small;
        }
        return top;
    }

    public long Count => _count;

    public static long SelfCheck(long n)
    {
        var h = new Heap((int)n + 1);
        for (long i = 0; i < n; i++) h.Push((i * 37) % 101);
        long acc = 0;
        long prev = -1;
        for (long i = 0; i < n; i++)
        {
            long v = h.Pop();
            // Pops come out non-decreasing; fold that into the answer so a
            // broken sift shows up as a different number, not just a slow one.
            if (v < prev) acc -= 1000;
            prev = v;
            acc += v * (i + 1);
        }
        return acc;
    }
}
