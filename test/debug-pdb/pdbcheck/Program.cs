using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

// Validate a Portable PDB emitted by dotcl's debug build: it must read back with
// the standard MetadataReader (a malformed PDB throws here), carry at least one
// document with a source hash, and have well-formed sequence points / local
// scopes. Exit 0 = OK, 1 = malformed/unexpected. Used by `make test-debug-pdb`.
if (args.Length < 1) { Console.Error.WriteLine("usage: pdbcheck <file.pdb>"); return 2; }
string path = args[0];

try
{
    using var stream = File.OpenRead(path);
    using var provider = MetadataReaderProvider.FromPortablePdbStream(stream);
    var r = provider.GetMetadataReader();

    if (r.Documents.Count == 0) { Console.Error.WriteLine("FAIL: no documents"); return 1; }

    foreach (var dh in r.Documents)
    {
        var d = r.GetDocument(dh);
        string name = r.GetString(d.Name);
        if (d.Hash.IsNil || r.GetBlobBytes(d.Hash).Length == 0)
        { Console.Error.WriteLine($"FAIL: document {name} has no source hash"); return 1; }
    }

    bool dump = args.Length > 1 && args[1] == "--dump";
    int methodsWithSeq = 0, scopeCount = 0, localCount = 0;
    foreach (var mdih in r.MethodDebugInformation)
    {
        var mdi = r.GetMethodDebugInformation(mdih);
        if (mdi.SequencePointsBlob.IsNil) continue;
        methodsWithSeq++;
        int prev = -1, minLine = int.MaxValue, maxLine = 0;
        string docName = "";
        var spans = new List<string>();
        foreach (var sp in mdi.GetSequencePoints())
        {
            // Offsets must be non-decreasing; a decode error would throw above.
            if (!sp.IsHidden && sp.Offset < prev)
            { Console.Error.WriteLine("FAIL: sequence points out of order"); return 1; }
            prev = sp.Offset;
            if (!sp.IsHidden)
            {
                docName = r.GetString(r.GetDocument(sp.Document).Name);
                if (sp.StartLine < minLine) minLine = sp.StartLine;
                if (sp.StartLine > maxLine) maxLine = sp.StartLine;
                spans.Add($"{sp.StartLine}:{sp.StartColumn}-{sp.EndLine}:{sp.EndColumn}");
            }
        }
        if (dump)
            Console.WriteLine($"  method#{MetadataTokens.GetRowNumber(mdih)}: {Path.GetFileName(docName)} lines {minLine}..{maxLine}  spans[{string.Join(" ", spans)}]");
    }
    foreach (var lsh in r.LocalScopes)
    {
        var ls = r.GetLocalScope(lsh);
        scopeCount++;
        var names = new List<string>();
        foreach (var lvh in ls.GetLocalVariables())
        {
            var lv = r.GetLocalVariable(lvh);
            localCount++;
            names.Add($"{r.GetString(lv.Name)}@{lv.Index}");
        }
        if (dump && names.Count > 0)
            Console.WriteLine($"  scope method#{MetadataTokens.GetRowNumber(ls.Method)}: [{string.Join(", ", names)}]");
    }

    Console.WriteLine($"OK: {r.Documents.Count} doc(s), {methodsWithSeq} method(s) with sequence points, "
                    + $"{scopeCount} local scope(s), {localCount} named local(s)");
    if (methodsWithSeq == 0) { Console.Error.WriteLine("FAIL: no methods with sequence points"); return 1; }
    return 0;
}
catch (Exception ex)
{
    Console.Error.WriteLine($"FAIL: PDB did not read back: {ex.GetType().Name}: {ex.Message}");
    return 1;
}
