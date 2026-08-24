// bench/fasl-il.cs -- what is actually inside a .fasl (a .NET PE assembly)
//
// Usage (from the project root; .NET 10 file-based app, no csproj needed):
//   dotnet run bench/fasl-il.cs -- <file.fasl> [mode...]
//
// Modes (default: summary methods):
//   summary    totals: types / methods / fields / total IL / largest method
//   methods    the largest method bodies, with locals and maxstack
//   prefixes   IL grouped by method-name prefix — _const_ (literal construction)
//              vs _toplevel_ vs closure_ vs function bodies
//   fields     field count grouped by name prefix — _gsym_ (uninterned symbols)
//              vs _symfn_ (call-site caches) vs _str_ (string literals)
//   types      per-type field and method counts
//   limits     assert the shape stays inside safe bounds; exits 1 on a violation
//              (thresholds are overridable: limits il=262144 fields=8192
//               methods=32768 us=8388608)
//   all        every mode except limits
//
// Why this exists: loading a fasl means JITting its IL, so what matters for load
// time and RSS is the SHAPE of the emitted assembly — how many bytes sit in one
// method, how many fields sit on one type — and none of that is visible from the
// Lisp side or from the fasl's size on disk. Measurements taken with earlier
// throwaway versions of this tool drove four separate fixes to how constants and
// top-level forms are split across methods; the tool itself was never committed,
// so those numbers could not be reproduced.
//
// Reference points, so a reading can be judged:
//   - a method over ~2.5 MB of IL will not JIT at all (measured)
//   - load RSS is superlinear in per-method IL: a 5 MB method cost +107 MB,
//     a 17 MB method over +3.6 GB, while total IL and compile time were unchanged
//   - 65,535 fields per type is a hard CoreCLR limit; the failure is
//     "Internal limitation: too many fields." at load, naming neither file nor cause
//   - all ldstr strings in a module share a 16 MB #US heap

using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335; // HeapIndex, for the #US heap size
using System.Reflection.PortableExecutable;

if (args.Length == 0)
{
    Console.Error.WriteLine("usage: dotnet run bench/fasl-il.cs -- <file.fasl> [summary|methods|prefixes|fields|types|all]");
    return 1;
}

string path = args[0];
// Threshold arguments (name=value) are pulled out before the mode set is built.
var thresholds = new Dictionary<string, long>();
var modeArgs = new List<string>();
foreach (var a in args.Skip(1))
{
    int eq = a.IndexOf('=');
    if (eq > 0 && long.TryParse(a.AsSpan(eq + 1), out long v))
        thresholds[a.Substring(0, eq).ToLowerInvariant()] = v;
    else
        modeArgs.Add(a);
}
var modes = new HashSet<string>(modeArgs, StringComparer.OrdinalIgnoreCase);
if (modes.Count == 0) { modes.Add("summary"); modes.Add("methods"); }
if (modes.Contains("all")) { modes.UnionWith(new[] { "summary", "methods", "prefixes", "fields", "types" }); }

using var fs = File.OpenRead(path);
using var pe = new PEReader(fs);
var md = pe.GetMetadataReader();

var methods = new List<(string Name, string Type, int Il, int Locals, int MaxStack)>();
foreach (var h in md.MethodDefinitions)
{
    var m = md.GetMethodDefinition(h);
    string name = md.GetString(m.Name);
    var declaring = m.GetDeclaringType();
    string typeName = declaring.IsNil ? "?" : md.GetString(md.GetTypeDefinition(declaring).Name);
    if (m.RelativeVirtualAddress == 0) { methods.Add((name, typeName, 0, 0, 0)); continue; }
    var body = pe.GetMethodBody(m.RelativeVirtualAddress);
    int locals = 0;
    if (!body.LocalSignature.IsNil)
    {
        var blob = md.GetBlobReader(md.GetStandaloneSignature(body.LocalSignature).Signature);
        blob.ReadSignatureHeader();
        locals = blob.ReadCompressedInteger();
    }
    methods.Add((name, typeName, body.GetILBytes()?.Length ?? 0, locals, body.MaxStack));
}

long totalIl = methods.Sum(m => (long)m.Il);
var biggest = methods.OrderByDescending(m => m.Il).FirstOrDefault();

// Name prefix: everything up to the last '_' that is followed by digits, so
// _const_12 and _ht_3_chunk_7 both collapse to a family rather than a serial.
static string Family(string name)
{
    var parts = name.Split('_');
    var keep = parts.Where(p => p.Length > 0 && !p.All(char.IsDigit)).ToArray();
    string joined = string.Join("_", keep);
    return name.StartsWith("_") ? "_" + joined : joined;
}

if (modes.Contains("summary"))
{
    Console.WriteLine($"file={path}");
    Console.WriteLine($"  bytes={fs.Length:N0}  types={md.TypeDefinitions.Count}  " +
                      $"methods={md.MethodDefinitions.Count}  fields={md.FieldDefinitions.Count}");
    Console.WriteLine($"  total_il={totalIl:N0}  largest={biggest.Il:N0} ({biggest.Type}.{biggest.Name})");
    int over64k = methods.Count(m => m.Il > 65536);
    int over2m = methods.Count(m => m.Il > 2_500_000);
    Console.WriteLine($"  methods over 64KB={over64k}  over 2.5MB (will not JIT)={over2m}");
    Console.WriteLine();
}

if (modes.Contains("methods"))
{
    Console.WriteLine($"{"il_bytes",14} {"locals",8} {"maxstack",9}  name");
    foreach (var m in methods.OrderByDescending(m => m.Il).Take(15))
        Console.WriteLine($"{m.Il,14:N0} {m.Locals,8} {m.MaxStack,9}  {m.Type}.{m.Name}");
    Console.WriteLine();
}

if (modes.Contains("prefixes"))
{
    Console.WriteLine("IL by method family (a fasl that is mostly _const is mostly literal construction):");
    var byFamily = methods.GroupBy(m => Family(m.Name))
                          .Select(g => (Family: g.Key, Il: g.Sum(x => (long)x.Il), Count: g.Count()))
                          .OrderByDescending(g => g.Il);
    foreach (var g in byFamily.Take(12))
        Console.WriteLine($"{g.Il,14:N0}  {100.0 * g.Il / Math.Max(1, totalIl),5:F1}%  n={g.Count,6:N0}  {g.Family}");
    Console.WriteLine();
}

if (modes.Contains("fields"))
{
    Console.WriteLine("Fields by family (65,535 per type is a hard limit):");
    var byField = md.FieldDefinitions
                    .Select(h => Family(md.GetString(md.GetFieldDefinition(h).Name)))
                    .GroupBy(x => x)
                    .Select(g => (Family: g.Key, Count: g.Count()))
                    .OrderByDescending(g => g.Count);
    foreach (var g in byField.Take(12))
        Console.WriteLine($"{g.Count,14:N0}  {g.Family}");
    Console.WriteLine();
}

if (modes.Contains("types"))
{
    Console.WriteLine($"{"fields",10} {"methods",10}  type");
    foreach (var th in md.TypeDefinitions)
    {
        var t = md.GetTypeDefinition(th);
        int nf = t.GetFields().Count, nm = t.GetMethods().Count;
        if (nf + nm == 0) continue;
        Console.WriteLine($"{nf,10:N0} {nm,10:N0}  {md.GetString(t.Name)}");
    }
    Console.WriteLine();
}

if (modes.Contains("limits"))
{
    // A guard, not a report: the failures these bounds stand for are invisible on
    // the compile side (total IL, fasl bytes and compile time all stay flat) and
    // surface at LOAD as a diagnostic that names neither the file nor the cause —
    // "Internal limitation: too many fields.", a bare InvalidProgramException, or
    // the process being killed while JITting one huge method. Every past instance
    // was found by a user's out-of-memory, never by CI.
    //
    // The defaults sit far below the hard limits and far above what dotcl emits
    // today, so they fire on a shape REGRESSION (an unsplit per-form or per-symbol
    // growth path) rather than on ordinary growth of a big library.
    long limIl = thresholds.GetValueOrDefault("il", 262_144);
    long limFields = thresholds.GetValueOrDefault("fields", 8_192);
    long limMethods = thresholds.GetValueOrDefault("methods", 32_768);
    long limUs = thresholds.GetValueOrDefault("us", 8_388_608);

    var worstFields = ("", 0);
    var worstMethods = ("", 0);
    foreach (var th in md.TypeDefinitions)
    {
        var t = md.GetTypeDefinition(th);
        string tn = md.GetString(t.Name);
        int nf = t.GetFields().Count, nm = t.GetMethods().Count;
        if (nf > worstFields.Item2) worstFields = (tn, nf);
        if (nm > worstMethods.Item2) worstMethods = (tn, nm);
    }
    int usHeap = md.GetHeapSize(HeapIndex.UserString);

    int failed = 0;
    void Check(string what, long value, long limit, string where)
    {
        bool ok = value <= limit;
        if (!ok) failed++;
        Console.WriteLine($"  {(ok ? "ok  " : "FAIL")} {what,-18} {value,12:N0} / {limit,12:N0}  {where}");
    }
    Console.WriteLine($"limits {path}");
    Check("max_method_il", biggest.Il, limIl, $"{biggest.Type}.{biggest.Name}");
    Check("fields_per_type", worstFields.Item2, limFields, worstFields.Item1);
    Check("methods_per_type", worstMethods.Item2, limMethods, worstMethods.Item1);
    Check("us_heap_bytes", usHeap, limUs, "module-wide (ldstr strings)");
    Console.WriteLine();
    if (failed > 0)
    {
        Console.Error.WriteLine(
            $"{path}: {failed} shape limit(s) exceeded. This is a load-time hazard, " +
            "not a style rule: see the reference points at the top of this file.");
        return 1;
    }
}

return 0;
