#if !NETSTANDARD2_0
using System.Reflection.PortableExecutable;

namespace DotCL;

/// <summary>
/// Whether a ReadyToRun image was compiled in a version bubble with a build of
/// DotCL.Runtime other than the one running. crossgen2 --inputbubble inlines
/// Runtime code into the image, so the image is bound to that exact build: the
/// .NET loader stops the process ("MVID mismatch") when the build differs. The
/// image lists the MVIDs it was bubbled with in its ManifestAssemblyMvids
/// section; this reads that list so the caller can load the image without its
/// precompiled code instead of dying.
/// </summary>
internal static class R2rBubble
{
    const int ManifestAssemblyMvids = 118;

    /// <summary>True when PATH is a ReadyToRun image with a bubble MVID list that
    /// does not contain this runtime's MVID. False for anything else, including a
    /// file that cannot be read as a PE image.</summary>
    internal static bool RuntimeMismatch(string path)
    {
        try
        {
            using var fs = File.OpenRead(path);
            using var pe = new PEReader(fs);
            var cor = pe.PEHeaders.CorHeader;
            if (cor == null || cor.ManagedNativeHeaderDirectory.Size == 0) return false;
            if (!pe.PEHeaders.TryGetDirectoryOffset(cor.ManagedNativeHeaderDirectory, out int off)) return false;
            fs.Position = off;
            var br = new BinaryReader(fs);
            if (br.ReadUInt32() != 0x00525452) return false;   // "RTR\0"
            br.ReadUInt16(); br.ReadUInt16();                    // version
            br.ReadUInt32();                                     // flags
            uint n = br.ReadUInt32();
            if (n > 256) return false;
            for (uint i = 0; i < n; i++)
            {
                uint type = br.ReadUInt32();
                int rva = br.ReadInt32();
                int size = br.ReadInt32();
                if (type != ManifestAssemblyMvids) continue;
                if (size <= 0 || size % 16 != 0) return false;
                var block = pe.GetSectionData(rva);
                var bytes = block.GetContent(0, size);
                var mine = typeof(R2rBubble).Module.ModuleVersionId;
                for (int k = 0; k < size; k += 16)
                    if (new Guid(bytes.AsSpan(k, 16)) == mine) return false;
                return true;
            }
            return false;
        }
        catch { return false; }
    }
}
#endif
