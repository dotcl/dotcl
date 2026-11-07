#if NET9_0_OR_GREATER
using System;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Security.Cryptography;

namespace DotCL.Emitter;

/// <summary>
/// Makes a finished .fasl image a function of its content. The two places a build puts something
/// that is not: the module version id (a random GUID) and the PE header's time stamp. Both are
/// replaced by a hash of the image with those fields zeroed, as Roslyn's deterministic mode
/// does. A reader sees an ordinary MVID and time stamp; nothing keys on their values.
/// </summary>
internal static class FaslDeterministic
{
    internal static void Stamp(byte[] image)
    {
        int mvidOffset, stampOffset;
        try
        {
            using var pe = new PEReader(new System.IO.MemoryStream(image, writable: false));
            var headers = pe.PEHeaders;
            stampOffset = headers.CoffHeaderStartOffset + 4;
            var reader = pe.GetMetadataReader();
            var mvid = reader.GetModuleDefinition().Mvid;
            if (mvid.IsNil) return;
            mvidOffset = headers.MetadataStartOffset
                         + reader.GetHeapMetadataOffset(HeapIndex.Guid)
                         + (MetadataTokens.GetHeapOffset(mvid) - 1) * 16;
        }
        catch { return; }   // not an image this knows: leave it as it is

        Array.Clear(image, stampOffset, 4);
        Array.Clear(image, mvidOffset, 16);
        var hash = SHA256.HashData(image);

        var guid = new byte[16];
        Array.Copy(hash, 0, guid, 0, 16);
        guid[7] = (byte)((guid[7] & 0x0F) | 0x40);   // version 4 layout, as a GUID reads
        guid[8] = (byte)((guid[8] & 0x3F) | 0x80);
        Array.Copy(guid, 0, image, mvidOffset, 16);

        // The time stamp: 31 bits of the hash with the top bit set, so it is not a plausible date
        // and nothing mistakes it for the time of the build.
        image[stampOffset + 0] = hash[16];
        image[stampOffset + 1] = hash[17];
        image[stampOffset + 2] = hash[18];
        image[stampOffset + 3] = (byte)(hash[19] | 0x80);
    }
}
#endif
