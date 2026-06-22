#if NET9_0_OR_GREATER
using System;
using System.IO;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace DotCL.Emitter;

/// <summary>
/// Post-processes a freshly-saved .fasl so its corlib assembly reference points at
/// the portable <c>netstandard</c> facade instead of <c>System.Private.CoreLib</c>.
///
/// Why: the dev compiler runs on CoreCLR, so <see cref="System.Reflection.Emit.PersistedAssemblyBuilder"/>
/// stamps every base-type reference (System.Object/Int64/Func&lt;&gt;/…) as living in
/// <c>System.Private.CoreLib</c>. NativeAOT shares that corlib so the fasl links there,
/// but Unity's IL2CPP/Mono BCL exposes only <c>netstandard</c>/<c>mscorlib</c> and cannot
/// resolve <c>System.Private.CoreLib</c>. A netstandard-referencing fasl loads under
/// IL2CPP exactly as the netstandard2.0 <c>DotCL.Runtime.dll</c> itself does (netstandard's
/// type-forwarders redirect the base types to the real corlib).
///
/// The rewrite is a surgical in-place byte patch of the single AssemblyRef row:
/// name string (<c>System.Private.CoreLib</c> → <c>netstandard</c>, shorter so it fits),
/// version (→ 2.1.0.0) and public-key token (→ the netstandard facade token). Offsets are
/// located with the public <see cref="MetadataReader"/> table/heap-offset APIs, so no
/// hand-rolled metadata-header parsing. The patched file is re-read and verified.
/// </summary>
internal static class FaslCorlibRetarget
{
    private const string SpclName = "System.Private.CoreLib";
    private const string NetstandardName = "netstandard";

    // Well-known netstandard facade identity — matches Unity IL2CPP's netstandard.dll
    // (netstandard, 2.1.0.0, PublicKeyToken=cc7b13ffcd2ddd51).
    private const ushort NsMajor = 2, NsMinor = 1, NsBuild = 0, NsRevision = 0;
    private static readonly byte[] NetstandardToken =
        { 0xcc, 0x7b, 0x13, 0xff, 0xcd, 0x2d, 0xdd, 0x51 };

    // ECMA-335 II.23.1.2 AssemblyFlags: PublicKey = 0x0001 (the PublicKeyOrToken
    // blob holds a full public key rather than its 8-byte token).
    private const uint AssemblyRefPublicKeyFlag = 0x0001;

    /// <summary>
    /// Retarget the corlib reference of the fasl at <paramref name="faslPath"/>.
    /// Only "netstandard" is supported. A no-op (returns false) if the fasl has no
    /// System.Private.CoreLib reference.
    /// </summary>
    public static bool RetargetCorlib(string faslPath, string corlibName)
    {
        if (!string.Equals(corlibName, NetstandardName, StringComparison.OrdinalIgnoreCase))
            throw new NotSupportedException(
                $"corlib retarget target '{corlibName}' not supported (only \"netstandard\")");

        byte[] bytes = File.ReadAllBytes(faslPath);

        int mdStart, nameFileOffset = -1, rowFileOffset = -1, blobFileOffset = -1, oldBlobCapacity = 0;
        bool found = false;

        using (var ms = new MemoryStream(bytes, writable: false))
        using (var pe = new PEReader(ms))
        {
            mdStart = pe.PEHeaders.MetadataStartOffset;
            var md = pe.GetMetadataReader();
            int arTableOffset = md.GetTableMetadataOffset(TableIndex.AssemblyRef);
            int arRowSize = md.GetTableRowSize(TableIndex.AssemblyRef);
            int stringHeapStart = md.GetHeapMetadataOffset(HeapIndex.String);
            int blobHeapStart = md.GetHeapMetadataOffset(HeapIndex.Blob);

            int row = 0;
            foreach (var h in md.AssemblyReferences)
            {
                row++;
                var ar = md.GetAssemblyReference(h);
                if (md.GetString(ar.Name) != SpclName) continue;

                found = true;
                rowFileOffset = mdStart + arTableOffset + (row - 1) * arRowSize;
                nameFileOffset = mdStart + stringHeapStart + md.GetHeapOffset(ar.Name);

                if (ar.PublicKeyOrToken.IsNil)
                    throw new InvalidOperationException(
                        "corlib retarget: System.Private.CoreLib reference has no public key / token");
                // The ref may carry an 8-byte TOKEN or a full PUBLIC KEY (~160 bytes,
                // AssemblyFlags.PublicKey set). We normalize either to an 8-byte
                // netstandard token written in place; the blob (length-prefix + content)
                // must be at least 9 bytes to hold [0x08][8-byte token].
                blobFileOffset = mdStart + blobHeapStart + md.GetHeapOffset(ar.PublicKeyOrToken);
                oldBlobCapacity = CompressedIntSize(bytes[blobFileOffset]) + md.GetBlobReader(ar.PublicKeyOrToken).Length;
                if (oldBlobCapacity < 1 + NetstandardToken.Length)
                    throw new InvalidOperationException(
                        $"corlib retarget: public-key/token blob too small ({oldBlobCapacity} bytes)");
                break;
            }
        }

        if (!found) return false;

        // 1) Version: first 8 bytes of the AssemblyRef row are 4 little-endian u16s.
        WriteU16(bytes, rowFileOffset + 0, NsMajor);
        WriteU16(bytes, rowFileOffset + 2, NsMinor);
        WriteU16(bytes, rowFileOffset + 4, NsBuild);
        WriteU16(bytes, rowFileOffset + 6, NsRevision);

        // 2) Flags (u32 at row+8): clear the PublicKey bit so PublicKeyOrToken is read
        //    as an 8-byte token (what we write below), not a full public key.
        uint flags = ReadU32(bytes, rowFileOffset + 8);
        WriteU32(bytes, rowFileOffset + 8, flags & ~AssemblyRefPublicKeyFlag);

        // 3) Name string in #Strings, in place. "System.Private.CoreLib" (22) is longer
        //    than "netstandard" (11), so it fits; the trailing bytes become an
        //    unreferenced orphan string (harmless — nothing indexes into them).
        var newName = System.Text.Encoding.UTF8.GetBytes(NetstandardName);
        Array.Copy(newName, 0, bytes, nameFileOffset, newName.Length);
        bytes[nameFileOffset + newName.Length] = 0; // null terminator

        // 4) PublicKeyOrToken blob → [0x08][netstandard 8-byte token], in place. Any
        //    leftover bytes of the old (larger) blob become unreferenced heap data.
        bytes[blobFileOffset] = (byte)NetstandardToken.Length; // compressed length 8 = one byte
        Array.Copy(NetstandardToken, 0, bytes, blobFileOffset + 1, NetstandardToken.Length);

        File.WriteAllBytes(faslPath, bytes);
        Verify(faslPath);
        return true;
    }

    private static void Verify(string faslPath)
    {
        using var fs = File.OpenRead(faslPath);
        using var pe = new PEReader(fs);
        var md = pe.GetMetadataReader();
        foreach (var h in md.AssemblyReferences)
        {
            var ar = md.GetAssemblyReference(h);
            var name = md.GetString(ar.Name);
            if (name == SpclName)
                throw new InvalidOperationException(
                    "corlib retarget verify: System.Private.CoreLib reference still present");
            if (name == NetstandardName)
            {
                if (ar.Version.Major != NsMajor || ar.Version.Minor != NsMinor)
                    throw new InvalidOperationException(
                        $"corlib retarget verify: netstandard version is {ar.Version}, expected 2.1.0.0");
                var tok = md.GetBlobBytes(ar.PublicKeyOrToken);
                for (int i = 0; i < NetstandardToken.Length; i++)
                    if (i >= tok.Length || tok[i] != NetstandardToken[i])
                        throw new InvalidOperationException(
                            "corlib retarget verify: netstandard public-key token mismatch");
            }
        }
    }

    private static void WriteU16(byte[] b, int off, ushort v)
    {
        b[off] = (byte)(v & 0xFF);
        b[off + 1] = (byte)(v >> 8);
    }

    private static uint ReadU32(byte[] b, int off) =>
        (uint)(b[off] | (b[off + 1] << 8) | (b[off + 2] << 16) | (b[off + 3] << 24));

    private static void WriteU32(byte[] b, int off, uint v)
    {
        b[off] = (byte)(v & 0xFF);
        b[off + 1] = (byte)((v >> 8) & 0xFF);
        b[off + 2] = (byte)((v >> 16) & 0xFF);
        b[off + 3] = (byte)((v >> 24) & 0xFF);
    }

    // Size in bytes of an ECMA-335 compressed unsigned integer given its first byte.
    private static int CompressedIntSize(byte first)
    {
        if ((first & 0x80) == 0) return 1;
        if ((first & 0xC0) == 0x80) return 2;
        return 4;
    }
}
#endif
