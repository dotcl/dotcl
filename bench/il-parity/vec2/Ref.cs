namespace IlParity;

/// <summary>A 2D vector with double fields: the float half of the structure
/// question.
///
/// The integer cases ask what a declared FIXNUM slot costs; this asks the same
/// of a DOUBLE-FLOAT one, where the boxed representation costs an object per
/// value rather than per value outside a small cache. Normalising is the shape
/// that reads both fields, does real arithmetic and writes both back.</summary>
public sealed class Vec2
{
    private double _x;
    private double _y;

    public Vec2(double x, double y) { _x = x; _y = y; }

    public double X => _x;
    public double Y => _y;

    public double Length() => System.Math.Sqrt(_x * _x + _y * _y);

    /// <summary>Scale to unit length in place, returning the old length.</summary>
    public double Normalize()
    {
        double len = System.Math.Sqrt(_x * _x + _y * _y);
        if (len == 0.0) return 0.0;
        _x = _x / len;
        _y = _y / len;
        return len;
    }

    public double Dot(Vec2 other) => _x * other._x + _y * other._y;

    public static long SelfCheck(long n)
    {
        double acc = 0.0;
        for (long i = 1; i <= n; i++)
        {
            var v = new Vec2(i, i * 2.0);
            acc += v.Normalize();
            acc += v.Dot(v) * 1000.0;
            acc += v.Length();
        }
        // Fold to an integer so the two halves can be compared exactly rather
        // than within a tolerance: the same operations in the same order give
        // the same doubles on both sides.
        return (long)(acc * 1000.0);
    }
}
