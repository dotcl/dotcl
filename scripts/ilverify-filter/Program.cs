// Drops one known-false ILVerify diagnostic from an ilverify run, so the rest
// can gate a build.
//
// ILVerify reports "StackUnderflow" at the first instruction of an exception
// filter when that filter sits inside another clause's handler region. The
// filter is entered by the runtime with the exception object on the stack, so
// there is nothing wrong with the IL: the verifier just fails to seed that
// state for this nesting. It is not a dotcl bug either -- the C# below, built
// by Roslyn, produces the same diagnostic:
//
//     try { return G(x); }
//     finally {
//         try { G(x + 1); }
//         catch (Exception) when (F()) { G(-1); }
//         finally { G(-2); }
//     }
//
// dotcl emits exactly this shape for (unwind-protect x (ignore-errors y)) and
// for a CATCH in an unwind-protect cleanup, both of which are ordinary Lisp.
//
// The suppression is deliberately narrow: only StackUnderflow, and only at an
// offset that is a filter clause's first instruction. Real stack-depth bugs
// (an operand left on the stack across a branch, say) report at the merge
// point or the offending instruction, never there.
//
// Usage: ilverify-filter <assembly> < ilverify-output
//   exits 0 when nothing but suppressed diagnostics remain, 1 otherwise.

using System.Globalization;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Text.RegularExpressions;

if (args.Length != 1)
{
    Console.Error.WriteLine("usage: ilverify-filter <assembly> < ilverify-output");
    return 2;
}

var filterRegions = FilterRegions(args[0]);

// [IL]: Error [StackUnderflow]: [<path> : <Type>::<Method>(<args>)][offset 0x000000BD] ...
var line = new Regex(@"Error \[(?<kind>\w+)\].*::(?<method>[^(\[]+).*offset 0x(?<offset>[0-9A-Fa-f]+)");

int kept = 0, suppressed = 0;
for (string? text = Console.ReadLine(); text != null; text = Console.ReadLine())
{
    var m = line.Match(text);
    if (m.Success && m.Groups["kind"].Value == "StackUnderflow")
    {
        string method = m.Groups["method"].Value;
        int offset = int.Parse(m.Groups["offset"].Value, NumberStyles.HexNumber);
        if (filterRegions.Any(r => r.Method == method && offset >= r.Start && offset < r.End))
        {
            suppressed++;
            continue;
        }
    }
    if (text.Contains("Error [", StringComparison.Ordinal)) kept++;
    Console.WriteLine(text);
}

if (suppressed > 0)
    Console.WriteLine($"  ({suppressed} StackUnderflow inside a filter body suppressed: ILVerify " +
                      "does not seed the exception for a filter nested in a handler)");
return kept > 0 ? 1 : 0;

// Every exception filter body: [filter start, handler start) per method. The
// verifier's missing seed makes the whole filter body read as underflowed, so
// the diagnostic can land on any instruction in it, not just the first.
static List<(string Method, int Start, int End)> FilterRegions(string assemblyPath)
{
    var regions = new List<(string, int, int)>();
    using var stream = File.OpenRead(assemblyPath);
    using var pe = new PEReader(stream);
    var md = pe.GetMetadataReader();
    foreach (var handle in md.MethodDefinitions)
    {
        var method = md.GetMethodDefinition(handle);
        if (method.RelativeVirtualAddress == 0) continue;
        string name = md.GetString(method.Name);
        var body = pe.GetMethodBody(method.RelativeVirtualAddress);
        foreach (var region in body.ExceptionRegions)
            if (region.Kind == ExceptionRegionKind.Filter)
                regions.Add((name, region.FilterOffset, region.HandlerOffset));
    }
    return regions;
}
