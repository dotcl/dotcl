#if NET9_0_OR_GREATER
using System;
using System.IO;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace DotCL.Emitter;

/// <summary>
/// Makes the helper types of a library assembly (the module type, the literal
/// holder types and the data-blob types) non-public, in place, before the image
/// is written. They are implementation detail of the baked module; left public
/// they show up in <c>GetExportedTypes</c>, in a C# consumer's IntelliSense, and
/// make the type name <c>CompiledModule</c> ambiguous when two such libraries are
/// referenced together. The runtime finds them by name with
/// <c>Assembly.GetType</c>, which does not care about visibility.
/// </summary>
internal static class FaslTypeVisibility
{
    private const uint VisibilityMask = 0x7;   // TypeAttributes.VisibilityMask
    private const uint Public = 0x1;           // TypeAttributes.Public (NotPublic = 0)

    internal static bool IsHelperType(string ns, string name) =>
        ns.Length == 0 &&
        (name == "CompiledModule"
         || name.StartsWith("CompiledModuleLiterals", StringComparison.Ordinal)
         || name.StartsWith("$ArrayType$", StringComparison.Ordinal));

    /// <summary>Returns the number of types changed.</summary>
    public static int HideHelperTypes(byte[] bytes)
    {
        int n = 0;
        using var ms = new MemoryStream(bytes, writable: false);
        using var pe = new PEReader(ms);
        var md = pe.GetMetadataReader();
        int start = pe.PEHeaders.MetadataStartOffset + md.GetTableMetadataOffset(TableIndex.TypeDef);
        int rowSize = md.GetTableRowSize(TableIndex.TypeDef);
        int row = 0;
        foreach (var h in md.TypeDefinitions)
        {
            row++;
            var td = md.GetTypeDefinition(h);
            if (!td.GetDeclaringType().IsNil) continue;
            if (!IsHelperType(md.GetString(td.Namespace), md.GetString(td.Name))) continue;
            int off = start + (row - 1) * rowSize;
            uint flags = (uint)(bytes[off] | (bytes[off + 1] << 8) | (bytes[off + 2] << 16) | (bytes[off + 3] << 24));
            if ((flags & VisibilityMask) != Public) continue;
            flags &= ~VisibilityMask;
            bytes[off] = (byte)flags; bytes[off + 1] = (byte)(flags >> 8);
            bytes[off + 2] = (byte)(flags >> 16); bytes[off + 3] = (byte)(flags >> 24);
            n++;
        }
        return n;
    }
}
#endif
