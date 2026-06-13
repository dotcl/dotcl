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
}
#endif
