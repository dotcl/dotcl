namespace DotCL;

/// <summary>
/// Main runtime for the dotcl Common Lisp implementation.
/// Split into partial class files by functional area.
/// </summary>
public static partial class Runtime
{
    /// <summary>Convert a Lisp integer (Fixnum or Bignum) to ulong.</summary>
    internal static ulong ToUlong(LispObject obj, string context)
    {
        if (obj is Fixnum f) return (ulong)(long)f.Value;
        if (obj is Bignum b) return (ulong)(System.Numerics.BigInteger)b.Value;
        throw new LispErrorException(new LispTypeError($"{context}: not an integer", obj));
    }
}
