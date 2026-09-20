using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace IlParity.Tool;

/// <summary>Pairs a C# method with the dotcl function that answers it and
/// reports what the two bodies cost in instructions.</summary>
public static class Compare
{
    /// <summary>The categories the report counts. What a parity question is
    /// actually about: does the Lisp side reach for an object where the C#
    /// side has a value, and does it call where the C# side does not.</summary>
    private static readonly string[] Categories =
        { "box", "unbox", "castclass", "isinst", "newobj", "call", "callvirt",
          "field", "element", "total" };

    private static string Category(Instr ins) => ins.Op switch
    {
        "box" => "box",
        "unbox" or "unbox.any" => "unbox",
        "castclass" => "castclass",
        "isinst" => "isinst",
        "newobj" => "newobj",
        "call" => "call",
        "callvirt" => "callvirt",
        "ldfld" or "stfld" or "ldsfld" or "stsfld" => "field",
        _ => ins.Op.StartsWith("ldelem", StringComparison.Ordinal)
             || ins.Op.StartsWith("stelem", StringComparison.Ordinal) ? "element" : null,
    };

    /// <summary>True for the multiple-values protocol, which the comparison
    /// excludes: it is a calling convention, not part of what the body
    /// computes, and it is its own subject elsewhere.</summary>
    private static bool IsMvProtocol(Instr ins) =>
        ins.Arg is "MultipleValues.Reset" or "Runtime.UnwrapMv";

    /// <summary>The body proper, with the entry sequence removed.
    ///
    /// A dotcl function body opens by copying each incoming argument from its
    /// parameter into a local and resetting the values protocol. That is the
    /// calling convention -- boxed arguments are the subject of their own
    /// issue -- so the maximal leading run of (ldarg, stloc) pairs, plus a
    /// values reset, is dropped before counting. The pattern is written as a
    /// PAIR on purpose: a C# method that opens by reading a field through
    /// ldarg.0 does not match it and keeps every instruction it has.</summary>
    public static List<Instr> Body(List<Instr> instrs)
    {
        int i = 0;
        while (i + 1 < instrs.Count
               && instrs[i].Op.StartsWith("ldarg", StringComparison.Ordinal)
               && instrs[i + 1].Op.StartsWith("stloc", StringComparison.Ordinal))
            i += 2;
        if (i < instrs.Count && instrs[i].Arg == "MultipleValues.Reset") i++;
        var body = instrs.Skip(i).Where(x => !IsMvProtocol(x)).ToList();
        if (body.Count > 0 && body[^1].Op == "ret") body.RemoveAt(body.Count - 1);
        return body;
    }

    private static Dictionary<string, int> Count(List<Instr> body)
    {
        var counts = Categories.ToDictionary(c => c, _ => 0);
        foreach (var ins in body)
        {
            var c = Category(ins);
            if (c != null) counts[c]++;
            counts["total"]++;
        }
        return counts;
    }

    private static List<Instr> FindLisp(Dictionary<string, List<Instr>> asm, string lispName)
    {
        // A dotcl function compiles to an entry thunk and a body method named
        // <NAME>_body_<n>. The body is what has the code in it.
        string prefix = "CompiledModule." + lispName.Replace('-', '_') + "_body_";
        var hit = asm.Where(kv => kv.Key.StartsWith(prefix, StringComparison.Ordinal))
                     .OrderByDescending(kv => kv.Value.Count)
                     .Select(kv => kv.Value)
                     .FirstOrDefault();
        return hit;
    }

    /// <summary>One tab-separated line per pair, for the gate. The report is
    /// for reading; this is for diffing against a recorded baseline, so it
    /// carries the dotcl numbers and nothing else.</summary>
    public static int Counts(string caseDir, string refDll, string fasl, TextWriter w)
    {
        string name = Path.GetFileName(caseDir.TrimEnd('/', '\\'));
        var cs = Il.ReadAssembly(refDll);
        var lisp = Il.ReadAssembly(fasl);
        int missing = 0;
        foreach (var line in File.ReadAllLines(Path.Combine(caseDir, "methods.txt")))
        {
            var t = line.Trim();
            if (t.Length == 0 || t.StartsWith("#", StringComparison.Ordinal)) continue;
            var parts = t.Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length < 2) continue;
            if (!cs.TryGetValue(parts[0], out _)) { missing++; continue; }
            var lispAll = FindLisp(lisp, parts[1]);
            if (lispAll == null) { missing++; continue; }
            var c = Count(Body(lispAll));
            w.WriteLine(name + "\t" + parts[1] + "\t"
                        + string.Join("\t", Categories.Select(k => c[k].ToString())));
        }
        return missing;
    }

    public static int Run(string caseDir, string refDll, string fasl, TextWriter w)
    {
        string name = Path.GetFileName(caseDir.TrimEnd('/', '\\'));
        var cs = Il.ReadAssembly(refDll);
        var lisp = Il.ReadAssembly(fasl);
        int missing = 0;

        foreach (var line in File.ReadAllLines(Path.Combine(caseDir, "methods.txt")))
        {
            var t = line.Trim();
            if (t.Length == 0 || t.StartsWith("#", StringComparison.Ordinal)) continue;
            var parts = t.Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length < 2) continue;
            string csName = parts[0], lispName = parts[1];

            if (!cs.TryGetValue(csName, out var csAll))
            {
                w.WriteLine($"{name}/{csName}: NOT FOUND in the reference assembly");
                missing++;
                continue;
            }
            var lispAll = FindLisp(lisp, lispName);
            if (lispAll == null)
            {
                w.WriteLine($"{name}/{lispName}: NOT FOUND in the fasl");
                missing++;
                continue;
            }

            var csBody = Body(csAll);
            var lispBody = Body(lispAll);
            var csCount = Count(csBody);
            var lispCount = Count(lispBody);

            w.WriteLine($"## {name}: {csName} vs {lispName}");
            w.WriteLine();
            w.WriteLine("| category | C# | dotcl | delta |");
            w.WriteLine("| --- | ---: | ---: | ---: |");
            foreach (var c in Categories)
            {
                int a = csCount[c], b = lispCount[c];
                w.WriteLine($"| {c} | {a} | {b} | {(b - a > 0 ? "+" : "")}{b - a} |");
            }
            w.WriteLine();

            // The surplus, as a multiset difference rather than a positional
            // diff: the question is which instructions the Lisp side pays for
            // that the C# side does not, and a positional diff of two
            // independently scheduled bodies answers a noisier question.
            var surplus = Multiset(lispBody);
            foreach (var kv in Multiset(csBody))
                if (surplus.TryGetValue(kv.Key, out int have))
                    surplus[kv.Key] = have - kv.Value;
            var extra = surplus.Where(kv => kv.Value > 0)
                               .OrderByDescending(kv => kv.Value)
                               .ThenBy(kv => kv.Key, StringComparer.Ordinal)
                               .ToList();
            if (extra.Count == 0)
            {
                w.WriteLine("surplus: none");
            }
            else
            {
                w.WriteLine("surplus (dotcl instructions with no counterpart in the C# body):");
                foreach (var kv in extra) w.WriteLine($"  {kv.Value,3}x {kv.Key}");
            }
            w.WriteLine();
        }
        return missing;
    }

    private static Dictionary<string, int> Multiset(List<Instr> body)
    {
        var m = new Dictionary<string, int>(StringComparer.Ordinal);
        foreach (var ins in body)
        {
            string k = ins.ToString();
            m[k] = m.TryGetValue(k, out int n) ? n + 1 : 1;
        }
        return m;
    }
}
