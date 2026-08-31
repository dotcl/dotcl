#if DEBUG
// Test-only interop helpers. Compiled into DEBUG builds only (the regression suite runs
// on the Debug build via `dotnet run`), so these types are ABSENT from shipped Release
// artifacts. They exercise interop surface that BCL types and runtime-emitted
// (dotnet:%define-class) classes cannot express — e.g. C# optional parameters with
// default values (dotcl/dotcl#24).
namespace DotCL.TestSupport
{
    public class OptionalArgs
    {
        // All-optional and partially-optional instance methods.
        public string Greet(string name = "world", int times = 1)
        {
            var sb = new System.Text.StringBuilder();
            for (int i = 0; i < times; i++) sb.Append($"hi {name};");
            return sb.ToString();
        }

        public int Add(int a, int b = 10, int c = 100) => a + b + c;

        public static int StaticAdd(int a, int b = 5) => a + b;
    }

    // A value-type (struct) used to model Avalonia.Media.Color: ctor overload
    // resolution must prefer ColorBox(ColorVal) over ColorBox(uint) for a wrapped
    // ColorVal arg, never Convert.ChangeType(struct, typeof(uint)).
    public struct ColorVal
    {
        public byte R, G, B;
        public ColorVal(byte r, byte g, byte b) { R = r; G = g; B = b; }
        public static ColorVal Parse(string _) => new ColorVal(128, 128, 128);
    }

    // Ctors whose bodies throw. Reflection wraps the throw in
    // TargetInvocationException; dotnet:new must surface the inner exception's
    // CLR type (for dotnet:exception-typep) and message, not the wrapper's
    // "Exception has been thrown by the target of an invocation."
    public class ThrowingCtor
    {
        public ThrowingCtor() => throw new System.InvalidOperationException("ctor boom");
        public ThrowingCtor(int n) => throw new System.ArgumentOutOfRangeException(nameof(n), "ctor boom " + n);
    }

    // Throws an IOException wrapping a SocketException — the shape a stream
    // read/write timeout produces. dotnet:exception-object must expose the
    // instance so a handler can walk InnerException and read SocketErrorCode /
    // ErrorCode instead of matching localized message text.

    /// <summary>
    /// Demo target for the advice contrib and its manual E2E: a plain, jitted
    /// static method in a normally loaded assembly. Harmony patches those
    /// reliably, while a method in a Reflection.Emit dynamic assembly (what
    /// dotnet:define-class produces) is not always patchable on CoreCLR -- so
    /// the target of the demo cannot itself be defined from Lisp.
    /// </summary>
    public static class AdviceDemo
    {
        public static int DemoAdd(int a, int b) => a + b;
    }

    public static class Throwers
    {
        public static void ThrowWrapped() =>
            throw new System.IO.IOException("outer io",
                new System.Net.Sockets.SocketException(10060));
    }

    // Plain `ref` parameters (in-out) for dotnet:call-out: the caller supplies
    // the initial value and receives the updated one as an extra return value.
    // StringBuilder is a non-IConvertible reference type, so a misclassified
    // ref slot (arg shifted into the wrong parameter) fails loudly — the same
    // shape as Socket.ReceiveFrom(byte[], ref EndPoint).
    public class RefParams
    {
        public static int AppendBang(ref System.Text.StringBuilder sb)
        {
            sb.Append("!");
            return sb.Length;
        }

        // Overload pair separated only by in-arg count once ref counts as in.
        public static string Describe(ref System.Text.StringBuilder sb) => "one:" + sb;
        public static string Describe(ref System.Text.StringBuilder sb, int n) => "two:" + sb + n;

        // A ref that REPLACES its referent — the extra value must be the new object.
        public static bool Replace(ref string s) { s = s + "-replaced"; return true; }
    }

    // Ctor set mirrors Avalonia.Media.SolidColorBrush exactly: (),
    // (ColorVal, double opacity = 1), (uint). There is NO 1-arg (ColorVal) ctor —
    // a single ColorVal must select the 2-param ctor with its opacity defaulted,
    // not the fixed-arity (uint) ctor (which would Convert.ChangeType the struct →
    // IConvertible). Exercises the optional-tail ctor path.
    public class ColorBox
    {
        public string Tag = "";
        public ColorBox() { Tag = "empty"; }
        public ColorBox(ColorVal c, double opacity = 1.0) { Tag = $"color {c.R} op {opacity}"; }
        public ColorBox(uint argb) { Tag = $"uint {argb}"; }
    }

    // Explicit interface implementations: private on the concrete type, so
    // reflection over its public members cannot see them. No BCL type expresses
    // the generic-method case conveniently, and dotnet:%define-class cannot emit
    // an explicit implementation.
    public interface IEcho
    {
        T Echo<T>(T value);
        string Where { get; }
    }

    public class ExplicitEcho : IEcho
    {
        public string Where => "class";                 // shadowed by the explicit one
        T IEcho.Echo<T>(T value) => value;
        string IEcho.Where => "interface";
    }

    // A struct reachable from a string only through an implicit operator, the
    // shape ASP.NET uses everywhere (StringValues, PathString, HostString). The
    // setter below takes it, so assigning a string has to apply the operator.
    public struct Tag
    {
        public string Text;
        public Tag(string text) { Text = text; }
        public static implicit operator Tag(string s) => new Tag(s);
        public override string ToString() => Text ?? "";
    }

    public class TagBox
    {
        public Tag Slot { get; set; }
        public string Read() => Slot.ToString();
    }

    // An interface whose members are implemented explicitly, one of them inherited
    // from a base interface: a cast to IDerived has to reach BaseValue as well.
    public interface IBase { int BaseValue { get; } }
    public interface IDerived : IBase { int OwnValue { get; } }

    public class Layered : IDerived
    {
        int IBase.BaseValue => 1;
        int IDerived.OwnValue => 2;
    }

}
#endif
