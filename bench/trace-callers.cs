// bench/trace-callers.cs -- who calls a hot method, from a speedscope profile
//
// Usage (from the project root; .NET 10 file-based app, no csproj needed):
//   dotnet run bench/trace-callers.cs -- <trace.speedscope.json> <pattern> [depth] [top]
//
//   pattern  substring matched against frame names, case-insensitive
//   depth    how many frames above the match to attribute to (default 1)
//   top      how many callers to print (default 15)
//
// Why this exists: `dotnet-trace report topN` answers WHICH method is hot and
// nothing else. Every perf decision after that needs the caller — "castclass is
// 10% of the run" is not actionable until you know whether the calls come from
// the IL dotcl emits or from type tests inside the runtime's own C#, because
// those are different repairs. That attribution was done by hand out of the
// speedscope JSON at least twice in this repo, and both times the tool was left
// in a session scratchpad, so the numbers in the decision records cannot be
// reproduced. This is that tool, committed.
//
// The profile must be collected with --format speedscope:
//   dotnet-trace collect --format speedscope -o out.nettrace -- <program>
//
// Measure execution, not compilation: compile the workload to a .fasl first and
// let the profiled run only LOAD and run it. A profile that includes the
// compiler puts Reflection.Emit and the assembler in the denominator and every
// runtime share reads low: measured on one workload, the same method reads
// 2.5% with the compiler in the profile and 9.8% without it.

using System.Text.Json;

if (args.Length < 2)
{
    Console.Error.WriteLine(
        "usage: dotnet run bench/trace-callers.cs -- <trace.speedscope.json> <pattern> [depth] [top]");
    return 1;
}

string path = args[0];
string pattern = args[1];
int depth = args.Length > 2 && int.TryParse(args[2], out var d) ? d : 1;
int top = args.Length > 3 && int.TryParse(args[3], out var t) ? t : 15;

using var doc = JsonDocument.Parse(File.ReadAllText(path));
var root = doc.RootElement;

// shared.frames[i].name — the frame table every sample indexes into.
var frames = root.GetProperty("shared").GetProperty("frames")
    .EnumerateArray()
    .Select(f => f.TryGetProperty("name", out var n) ? (n.GetString() ?? "") : "")
    .ToArray();

var matching = new HashSet<int>();
for (int i = 0; i < frames.Length; i++)
    if (frames[i].Contains(pattern, StringComparison.OrdinalIgnoreCase))
        matching.Add(i);
if (matching.Count == 0)
{
    Console.Error.WriteLine($"no frame name contains \"{pattern}\"");
    return 1;
}

// One entry per sample: the stack (leaf last, as speedscope stores it) and its
// weight. Sum weights rather than counting samples: a sampled profile's weights
// are the time each sample stands for, and they are not all equal.
double total = 0, matchedWeight = 0;
var byCaller = new Dictionary<string, double>();
var selfOnly = new Dictionary<string, double>();   // matched frame is the leaf

// Attribute one interval of time spent on STACK (leaf last).
void Attribute(List<int> stack, double w)
{
    total += w;
    for (int k = stack.Count - 1; k >= 0; k--)
    {
        if (!matching.Contains(stack[k])) continue;
        matchedWeight += w;
        if (k == stack.Count - 1)
        {
            string leaf = frames[stack[k]];
            selfOnly[leaf] = selfOnly.GetValueOrDefault(leaf) + w;
        }
        int up = k - depth;
        string caller = up >= 0 ? frames[stack[up]] : "(stack bottom)";
        byCaller[caller] = byCaller.GetValueOrDefault(caller) + w;
        break;   // first match from the leaf: a recursive frame counts once
    }
}

foreach (var profile in root.GetProperty("profiles").EnumerateArray())
{
    string kind = profile.GetProperty("type").GetString() ?? "";
    if (kind == "sampled")
    {
        var samples = profile.GetProperty("samples");
        var weights = profile.GetProperty("weights");
        int n = samples.GetArrayLength();
        for (int s = 0; s < n; s++)
        {
            var stack = new List<int>();
            foreach (var f in samples[s].EnumerateArray()) stack.Add(f.GetInt32());
            Attribute(stack, weights[s].GetDouble());
        }
    }
    else if (kind == "evented")
    {
        // What dotnet-trace's exporter (TraceEvent) writes: a stream of open and
        // close events. The time between two consecutive events belongs to
        // whatever stack is open at that moment, so rebuild the stack as we go.
        var stack = new List<int>();
        double last = 0;
        bool started = false;
        foreach (var ev in profile.GetProperty("events").EnumerateArray())
        {
            double at = ev.GetProperty("at").GetDouble();
            if (started && stack.Count > 0) Attribute(stack, at - last);
            else if (started) total += at - last;
            last = at; started = true;
            string type = ev.GetProperty("type").GetString() ?? "";
            int frame = ev.GetProperty("frame").GetInt32();
            if (type == "O") stack.Add(frame);
            else if (stack.Count > 0) stack.RemoveAt(stack.Count - 1);
        }
    }
}

Console.WriteLine($"file={path}");
Console.WriteLine($"pattern=\"{pattern}\"  frames matched={matching.Count}  depth={depth}");
Console.WriteLine($"weight on stacks containing a match: {matchedWeight:N0} / {total:N0} " +
                  $"({(total > 0 ? 100 * matchedWeight / total : 0):F2}% of the profile)");
Console.WriteLine();
Console.WriteLine($"callers {depth} frame(s) above the match:");
foreach (var (name, w) in byCaller.OrderByDescending(kv => kv.Value).Take(top))
    Console.WriteLine($"  {100 * w / Math.Max(1e-9, matchedWeight),6:F2}%  {w,12:N0}  {name}");

if (selfOnly.Count > 0)
{
    Console.WriteLine();
    Console.WriteLine("matched frames that were the leaf (the method itself running):");
    foreach (var (name, w) in selfOnly.OrderByDescending(kv => kv.Value).Take(top))
        Console.WriteLine($"  {100 * w / Math.Max(1e-9, total),6:F2}%  {w,12:N0}  {name}");
}
return 0;
