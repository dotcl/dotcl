// fasl-digest.cs -- a text form of a dotcl fasl that two builds of the same
// source can be compared by.
//
// Usage (.NET 10 file-based app):
//   dotnet run bench/parallel-compile/fasl-digest.cs -- [--gensyms] <file.fasl>...
//   dotnet run bench/parallel-compile/fasl-digest.cs -- [--gensyms] --compare <dir-a> <dir-b>
//
// --compare digests every .fasl under DIR-A and the file at the same relative
// path under DIR-B, and prints the ones that differ (with their first differing
// line), the ones missing from DIR-B, and a SAME / DIFFER count.
//
// A fasl is a .NET assembly, and two compiles of one file never give the same
// bytes: the module version id is random and the module name carries a random
// suffix. This prints, per file, everything else that decides what the fasl
// does: each type's fields and methods with their signatures and IL, the IL's
// tokens replaced by what they name (a string, a field, a method), so that a
// string of another length earlier in the heap does not shift every token after
// it. The module name's random suffix is dropped, and so is the random part
// of the temporary file name ASDF compiles to.
//
// Fields and methods whose names contain numbers are renumbered in the order
// they are defined: the numbers come from counters of the whole process, so the
// same file gets other numbers depending on what the process compiled before it.
//
// Branch targets and exception regions are printed as instruction numbers, not
// byte offsets: a constant from a process-wide counter can be a byte longer in
// another build, which moves every offset after it. A call that passes the
// caller's value mode (ldc.i4 MODE; call LispFunction.CallNM) is printed as the
// plain CallN: the compiler passes the mode only to a callee it already knows
// takes one (a DEFMETHOD on a standard function such as DOCUMENTATION, or a
// DEFUN compiled earlier), so the same source gives either form depending on
// what the process compiled or loaded before it. The two calls do the same.
//
// --gensyms also writes as '#' every run of digits in a string literal, in the
// text of a custom attribute and in a literal data blob, the length of such a
// blob, and the size in the name of the type that holds it. Uninterned symbols
// are named from one process-wide counter, so files compiled at the same time
// on several threads give their gensyms other numbers than a serial build
// does; this tells that apart from any other difference.

using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Text;
using System.Text.RegularExpressions;

static bool IsSimpleLoad(string name) => name.StartsWith("ldc.i4") || name.StartsWith("ldarg") || name.StartsWith("ldloc");

// A symbol-function call site method (sff#N) that takes the caller's value mode as an
// extra last Int32 argument is written as the one without it (see the mode calls below).
static string MethodSig(string name, string sig)
{
    var m = Regex.Match(sig, @"^00([0-9A-F]{2})(.*)08$");
    return name.StartsWith("sff#") && m.Success
        ? "00" + (Convert.ToInt32(m.Groups[1].Value, 16) - 1).ToString("X2") + m.Groups[2].Value
        : sig;
}

bool gensyms = args.Contains("--gensyms");
bool compare = args.Contains("--compare");
var files = args.Where(a => a != "--gensyms" && a != "--compare").ToList();
if (files.Count == 0 || (compare && files.Count != 2))
{
    Console.Error.WriteLine("usage: dotnet run fasl-digest.cs -- [--gensyms] <file.fasl>... | [--gensyms] --compare <dir-a> <dir-b>");
    return 1;
}

var oneByte = new OpCode?[256];
var twoByte = new OpCode?[256];
foreach (var f in typeof(OpCodes).GetFields(BindingFlags.Public | BindingFlags.Static))
{
    var op = (OpCode)f.GetValue(null)!;
    if (op.Size == 1) oneByte[(byte)op.Value] = op;
    else twoByte[(byte)(op.Value & 0xff)] = op;
}

var suffix = new Regex("_[0-9a-f]{8,32}$");
var tmpName = new Regex("-tmp[0-9A-Z]{4,8}\\b");
var anyDigits = new Regex("[0-9]+");

if (!compare)
{
    foreach (var path in files) Console.Write(Digest(path));
    return 0;
}
int same = 0, differ = 0, missing = 0;
foreach (var a in Directory.EnumerateFiles(files[0], "*.fasl", SearchOption.AllDirectories).OrderBy(x => x, StringComparer.Ordinal))
{
    var rel = Path.GetRelativePath(files[0], a);
    var b = Path.Combine(files[1], rel);
    if (!File.Exists(b)) { missing++; Console.WriteLine("MISSING " + rel); continue; }
    var da = Digest(a).Split('\n');
    var db = Digest(b).Split('\n');
    int i = 0;
    while (i < da.Length && i < db.Length && da[i] == db[i]) i++;
    if (i == da.Length && i == db.Length) { same++; continue; }
    differ++;
    Console.WriteLine("DIFFER " + rel + " line " + (i + 1));
    Console.WriteLine("  a: " + (i < da.Length ? da[i] : "<end>"));
    Console.WriteLine("  b: " + (i < db.Length ? db[i] : "<end>"));
}
Console.WriteLine($"SAME {same} DIFFER {differ} MISSING {missing}");
return differ + missing == 0 ? 0 : 2;

string Digest(string path)
{
    using var fs = File.OpenRead(path);
    using var pe = new PEReader(fs);
    var md = pe.GetMetadataReader();
    var sb = new StringBuilder();
    string moduleName = md.GetString(md.GetModuleDefinition().Name);
    string moduleBase = suffix.Replace(moduleName, "");

    string Str(StringHandle h) => tmpName.Replace(md.GetString(h).Replace(moduleName, moduleBase), "-tmp");
    string Lit(string s)
    {
        s = tmpName.Replace(s.Replace(moduleName, moduleBase), "-tmp");
        if (gensyms) s = anyDigits.Replace(s, "#");
        return s;
    }
    var memberName = new Dictionary<EntityHandle, string>();
    foreach (var th0 in md.TypeDefinitions)
    {
        var td0 = md.GetTypeDefinition(th0);
        var seq = new Dictionary<string, int>();
        string Canon(string n)
        {
            var c = anyDigits.Replace(n, "#");
            if (c == n) return n;
            seq.TryGetValue(c, out int k);
            seq[c] = k + 1;
            return c + "~" + k;
        }
        foreach (var fh in td0.GetFields()) memberName[fh] = Canon(md.GetString(md.GetFieldDefinition(fh).Name));
        foreach (var mh in td0.GetMethods()) memberName[mh] = Canon(md.GetString(md.GetMethodDefinition(mh).Name));
    }
    string Blob(BlobHandle h) => h.IsNil ? "-" : Convert.ToHexString(md.GetBlobBytes(h));
    string TypeName(EntityHandle h) => h.Kind switch
    {
        HandleKind.TypeDefinition => Str(md.GetTypeDefinition((TypeDefinitionHandle)h).Name),
        HandleKind.TypeReference => Str(md.GetTypeReference((TypeReferenceHandle)h).Namespace) + "." + Str(md.GetTypeReference((TypeReferenceHandle)h).Name),
        HandleKind.TypeSpecification => "spec:" + Blob(md.GetTypeSpecification((TypeSpecificationHandle)h).Signature),
        _ => h.Kind.ToString(),
    };
    string Token(int token)
    {
        var h = MetadataTokens.EntityHandle(token);
        switch (h.Kind)
        {
            case HandleKind.FieldDefinition:
            {
                var fd = md.GetFieldDefinition((FieldDefinitionHandle)h);
                return "field " + TypeName(fd.GetDeclaringType()) + "::" + memberName[h];
            }
            case HandleKind.MethodDefinition:
            {
                var m = md.GetMethodDefinition((MethodDefinitionHandle)h);
                return "method " + TypeName(m.GetDeclaringType()) + "::" + memberName[h] + " " + Blob(m.Signature);
            }
            case HandleKind.MemberReference:
            {
                var r = md.GetMemberReference((MemberReferenceHandle)h);
                return "ref " + TypeName(r.Parent) + "::" + Str(r.Name) + " " + Blob(r.Signature);
            }
            case HandleKind.MethodSpecification:
            {
                var s = md.GetMethodSpecification((MethodSpecificationHandle)h);
                return "spec " + Token(MetadataTokens.GetToken(s.Method)) + " " + Blob(s.Signature);
            }
            case HandleKind.StandaloneSignature:
                return "sig " + Blob(md.GetStandaloneSignature((StandaloneSignatureHandle)h).Signature);
            default:
                return "type " + TypeName(h);
        }
    }
    void Il(byte[] il, System.Collections.Immutable.ImmutableArray<ExceptionRegion> regions)
    {
        // A branch is printed as the number of the instruction it goes to, not as
        // a byte offset: a constant that comes from a counter of the whole process
        // (an id, a number in a name) can be one byte longer or shorter in another
        // build, which moves every offset after it and says nothing about the code.
        var lines = new List<(int start, string name, string operand, int[]? targets)>();
        int i = 0;
        while (i < il.Length)
        {
            int start = i;
            OpCode op;
            if (il[i] == 0xfe) { op = twoByte[il[i + 1]]!.Value; i += 2; }
            else { op = oneByte[il[i]]!.Value; i += 1; }
            string operand = "";
            int[]? targets = null;
            switch (op.OperandType)
            {
                case OperandType.InlineNone: break;
                case OperandType.ShortInlineBrTarget:
                    targets = new[] { i + 1 + (sbyte)il[i] }; i += 1; break;
                case OperandType.ShortInlineI:
                case OperandType.ShortInlineVar:
                    operand = il[i].ToString(); i += 1; break;
                case OperandType.InlineVar:
                    operand = BitConverter.ToUInt16(il, i).ToString(); i += 2; break;
                case OperandType.InlineBrTarget:
                    targets = new[] { i + 4 + BitConverter.ToInt32(il, i) }; i += 4; break;
                case OperandType.InlineI:
                case OperandType.ShortInlineR:
                    operand = BitConverter.ToInt32(il, i).ToString(); i += 4; break;
                case OperandType.InlineI8:
                case OperandType.InlineR:
                    operand = BitConverter.ToInt64(il, i).ToString(); i += 8; break;
                case OperandType.InlineString:
                {
                    int tok = BitConverter.ToInt32(il, i); i += 4;
                    operand = "\"" + Lit(md.GetUserString(MetadataTokens.UserStringHandle(tok & 0xffffff))) + "\"";
                    break;
                }
                case OperandType.InlineSwitch:
                {
                    int n = BitConverter.ToInt32(il, i); i += 4;
                    int baseOffset = i + 4 * n;
                    var t = new int[n];
                    for (int k = 0; k < n; k++) { t[k] = baseOffset + BitConverter.ToInt32(il, i); i += 4; }
                    targets = t;
                    break;
                }
                default:
                {
                    int tok = BitConverter.ToInt32(il, i); i += 4;
                    operand = Token(tok);
                    break;
                }
            }
            lines.Add((start, op.Name!, operand, targets));
        }
        // A call of LispFunction.CallN that passes the caller's value mode
        // (ldc.i4 MODE; call CallNM) is the same call as CallN with no mode:
        // whether the compiler passes the mode depends on whether it already knew
        // the callee takes one, which depends on what the process compiled or
        // loaded before (a DEFMETHOD on a standard function, a DEFUN compiled
        // earlier). It changes how the call is made, not what the code does, so
        // it is written as the plain call, and the load of the mode is dropped.
        var modeCall = new Regex(@"^ref DotCL\.LispFunction::Call(\d+)M 00([0-9A-F]{2})(.*)08$");
        // The same for a call of a symbol-function call site (CompiledModule::sff#N), which
        // takes the mode as its last argument when the callee was known to take one.
        var siteCall = new Regex(@"^method CompiledModule::(sff#~\d+) 00([0-9A-F]{2})(.*)08$");
        var modeStarts = new Dictionary<int, int>();
        var shown = new List<(int start, string name, string operand, int[]? targets)>();
        for (int k = 0; k < lines.Count; k++)
        {
            var line = lines[k];
            if (line.name == "call" && siteCall.Match(line.operand) is { Success: true } sm
                && shown.Count > 0 && IsSimpleLoad(shown[^1].name))
            {
                var dropped = shown[^1];
                shown.RemoveAt(shown.Count - 1);
                shown.Add((dropped.start, "call", "method CompiledModule::" + sm.Groups[1].Value + " 00"
                    + (Convert.ToInt32(sm.Groups[2].Value, 16) - 1).ToString("X2") + sm.Groups[3].Value, null));
                modeStarts[line.start] = dropped.start;
                continue;
            }
            if (line.name == "call" && modeCall.Match(line.operand) is { Success: true } m
                && shown.Count > 0 && IsSimpleLoad(shown[^1].name))
            {
                // the instruction after the dropped load keeps its place for branch targets
                var dropped = shown[^1];
                shown.RemoveAt(shown.Count - 1);
                shown.Add((dropped.start, "call", "ref DotCL.LispFunction::Call" + m.Groups[1].Value + " 00"
                    + (Convert.ToInt32(m.Groups[2].Value, 16) - 1).ToString("X2") + m.Groups[3].Value, null));
                modeStarts[line.start] = dropped.start;
                continue;
            }
            shown.Add(line);
        }
        var index = new Dictionary<int, int>();
        for (int k = 0; k < shown.Count; k++) index[shown[k].start] = k;
        foreach (var (os, ds) in modeStarts) index[os] = index[ds];
        // A region is printed with instruction numbers too, as a start and a length
        // counted in instructions.
        int Ix(int offset) => offset == il.Length ? shown.Count : index.TryGetValue(offset, out var k) ? k : -offset;
        foreach (var r in regions)
            sb.Append("   region ").Append(r.Kind).Append(' ').Append(Ix(r.TryOffset)).Append(' ')
              .Append(Ix(r.TryOffset + r.TryLength) - Ix(r.TryOffset))
              .Append(' ').Append(Ix(r.HandlerOffset)).Append(' ')
              .Append(Ix(r.HandlerOffset + r.HandlerLength) - Ix(r.HandlerOffset))
              .Append(r.CatchType.IsNil ? "" : " " + TypeName(r.CatchType)).Append('\n');
        // The slot of a LOAD-TIME-VALUE is a number from a counter of the whole process (the
        // fasl keeps its slots under its own module name, so only the number differs between
        // builds): in a method that reads or writes such a slot, the integer literals made
        // into Fixnums are written as '#'.
        bool ltvMethod = shown.Any(l => l.operand.Contains("LTV-SLOT"));
        for (int k = 0; ltvMethod && k + 1 < shown.Count; k++)
            if (shown[k].name == "ldc.i8" && shown[k + 1].operand.Contains("DotCL.Fixnum::Make"))
                shown[k] = (shown[k].start, shown[k].name, "#", shown[k].targets);
        foreach (var (start, name, operand, targets) in shown)
        {
            string text = operand;
            if (targets != null)
                text = string.Join(",", targets.Select(t => index.TryGetValue(t, out var k) ? "@" + k : "?" + t));
            sb.Append("    ").Append(name).Append(' ').Append(text).Append('\n');
        }
    }
    sb.Append("assembly ").Append(Str(md.GetAssemblyDefinition().Name)).Append('\n');
    foreach (var th in md.TypeDefinitions)
    {
        var td = md.GetTypeDefinition(th);
        sb.Append("type ").Append(Str(td.Namespace)).Append('.').Append(Str(td.Name)).Append(' ').Append(td.Attributes).Append('\n');
        foreach (var fh in td.GetFields())
        {
            var fd = md.GetFieldDefinition(fh);
            sb.Append("  field ").Append(memberName[fh]).Append(' ').Append(Blob(fd.Signature));
            int rva = fd.GetRelativeVirtualAddress();
            if (rva != 0)
            {
                var br = md.GetBlobReader(fd.Signature);
                br.ReadSignatureHeader();
                int size = 0;
                if (br.ReadSignatureTypeCode() == SignatureTypeCode.TypeHandle
                    && br.ReadTypeHandle() is var th2 && th2.Kind == HandleKind.TypeDefinition)
                    size = md.GetTypeDefinition((TypeDefinitionHandle)th2).GetLayout().Size;
                byte[] data = pe.GetSectionData(rva).GetContent(0, size).ToArray();
                // The module name the literal data names (a symbol's home module, a source
                // file) carries ASDF's random temporary-file suffix, as the assembly's name does.
                data = Encoding.Latin1.GetBytes(Regex.Replace(Encoding.Latin1.GetString(data),
                    @"-tmp[A-Z0-9]{8}_[0-9a-f]{8}", "-tmp"));
                if (gensyms)
                    data = Encoding.Latin1.GetBytes(anyDigits.Replace(Encoding.Latin1.GetString(data), "#"));
                sb.Append(" data ").Append(Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(data)));
            }
            sb.Append('\n');
        }
        foreach (var mh in td.GetMethods())
        {
            var m = md.GetMethodDefinition(mh);
            sb.Append("  method ").Append(memberName[mh]).Append(' ').Append(MethodSig(memberName[mh], Blob(m.Signature))).Append('\n');
            if (m.RelativeVirtualAddress == 0) continue;
            var body = pe.GetMethodBody(m.RelativeVirtualAddress);
            sb.Append("   maxstack ").Append(body.MaxStack).Append(" locals ")
              .Append(body.LocalSignature.IsNil ? "-" : Blob(md.GetStandaloneSignature(body.LocalSignature).Signature)).Append('\n');
            Il(body.GetILBytes()!, body.ExceptionRegions);
        }
    }
    foreach (var ch in md.CustomAttributes)
    {
        var ca = md.GetCustomAttribute(ch);
        string value = Blob(ca.Value);
        if (gensyms)
            value = anyDigits.Replace(new string(md.GetBlobBytes(ca.Value).Select(x => x >= 0x20 && x < 0x7f ? (char)x : '.').ToArray()), "#");
        sb.Append("attribute ").Append(Token(MetadataTokens.GetToken(ca.Constructor))).Append(' ').Append(value).Append('\n');
    }
    foreach (var rh in md.ManifestResources)
    {
        var r = md.GetManifestResource(rh);
        sb.Append("resource ").Append(Str(r.Name)).Append('\n');
        if (r.Implementation.IsNil)
        {
            var dir = pe.PEHeaders.CorHeader!.ResourcesDirectory;
            var block = pe.GetSectionData(dir.RelativeVirtualAddress);
            var reader = block.GetReader((int)r.Offset, block.Length - (int)r.Offset);
            int len = reader.ReadInt32();
            var bytes = reader.ReadBytes(len);
            sb.Append("  data ").Append(Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(bytes))).Append(' ').Append(len).Append('\n');
        }
    }
    var text = sb.ToString();
    if (gensyms)
        text = Regex.Replace(text, @"(ldc\.i4(?:\.s)? )-?[0-9]+(\n    newarr type System\.Byte\n    dup \n    ldtoken field CompiledModule::_litdata)", "$1#$2");
    if (gensyms)
        text = Regex.Replace(text, @"\$ArrayType\$[0-9]+", "$$ArrayType$$#");
    return text;
}
