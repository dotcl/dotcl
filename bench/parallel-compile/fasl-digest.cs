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
    void Il(byte[] il)
    {
        int i = 0;
        while (i < il.Length)
        {
            OpCode op;
            if (il[i] == 0xfe) { op = twoByte[il[i + 1]]!.Value; i += 2; }
            else { op = oneByte[il[i]]!.Value; i += 1; }
            string operand = "";
            switch (op.OperandType)
            {
                case OperandType.InlineNone: break;
                case OperandType.ShortInlineBrTarget:
                case OperandType.ShortInlineI:
                case OperandType.ShortInlineVar:
                    operand = il[i].ToString(); i += 1; break;
                case OperandType.InlineVar:
                    operand = BitConverter.ToUInt16(il, i).ToString(); i += 2; break;
                case OperandType.InlineBrTarget:
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
                    var t = new List<string>();
                    for (int k = 0; k < n; k++) { t.Add(BitConverter.ToInt32(il, i).ToString()); i += 4; }
                    operand = string.Join(",", t);
                    break;
                }
                default:
                {
                    int tok = BitConverter.ToInt32(il, i); i += 4;
                    operand = Token(tok);
                    break;
                }
            }
            sb.Append("    ").Append(op.Name).Append(' ').Append(operand).Append('\n');
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
                if (gensyms)
                    data = Encoding.Latin1.GetBytes(anyDigits.Replace(Encoding.Latin1.GetString(data), "#"));
                sb.Append(" data ").Append(Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(data)));
            }
            sb.Append('\n');
        }
        foreach (var mh in td.GetMethods())
        {
            var m = md.GetMethodDefinition(mh);
            sb.Append("  method ").Append(memberName[mh]).Append(' ').Append(Blob(m.Signature)).Append('\n');
            if (m.RelativeVirtualAddress == 0) continue;
            var body = pe.GetMethodBody(m.RelativeVirtualAddress);
            sb.Append("   maxstack ").Append(body.MaxStack).Append(" locals ")
              .Append(body.LocalSignature.IsNil ? "-" : Blob(md.GetStandaloneSignature(body.LocalSignature).Signature)).Append('\n');
            foreach (var r in body.ExceptionRegions)
                sb.Append("   region ").Append(r.Kind).Append(' ').Append(r.TryOffset).Append(' ').Append(r.TryLength)
                  .Append(' ').Append(r.HandlerOffset).Append(' ').Append(r.HandlerLength)
                  .Append(r.CatchType.IsNil ? "" : " " + TypeName(r.CatchType)).Append('\n');
            Il(body.GetILBytes()!);
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
