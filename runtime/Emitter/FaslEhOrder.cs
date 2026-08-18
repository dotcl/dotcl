#if NET9_0_OR_GREATER
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

namespace DotCL.Emitter;

/// <summary>
/// Post-processes a freshly-saved .fasl so its exception-handling clauses are
/// ordered the way the CLR requires: a clause nested inside another clause's
/// protected region — its try, its filter, or its handler — must be listed
/// before the clause that encloses it.
///
/// Why: <see cref="System.Reflection.Emit.PersistedAssemblyBuilder"/> orders the
/// clauses of a method by where their try blocks end. That is correct while
/// nesting happens through try blocks, but a try nested inside an enclosing
/// clause's HANDLER starts after that handler's try has already ended, so the
/// enclosing clause is written first. The CLR rejects the method with
/// InvalidProgramException the moment it is JITted.
///
/// The shape that hits this is ordinary Lisp: any construct that needs an
/// exception block inside an unwind-protect cleanup — (unwind-protect x
/// (ignore-errors ...)), a CATCH in a cleanup, and so on. The same code emitted
/// through the runtime ILGenerator (a DynamicMethod, i.e. everything that is not
/// compile-file) is ordered correctly, so this only ever showed up in .fasl.
///
/// The rewrite is in place and size-preserving: clause records are fixed width
/// and only their order changes, so every offset in the file stays valid.
/// </summary>
internal static class FaslEhOrder
{
    // ECMA-335 II.25.4.5 method data section kinds.
    private const byte SectEHTable = 0x01;
    private const byte SectFatFormat = 0x40;
    private const byte SectMoreSects = 0x80;

    // ECMA-335 II.25.4.4 method header flags (low byte).
    private const byte HeaderFatFormat = 0x03;
    private const byte HeaderMoreSects = 0x08;

    // ECMA-335 II.25.4.6 clause flags: 0 = typed catch, 1 = filter.
    private const int ClauseIsFilter = 0x0001;

    private const int FatClauseSize = 24;
    private const int SmallClauseSize = 12;

    /// <summary>
    /// Reorder the exception clauses of every method in the fasl that needs it.
    /// Returns the number of methods rewritten (0 leaves the file untouched).
    /// </summary>
    public static int Fix(string faslPath)
    {
        byte[] bytes = File.ReadAllBytes(faslPath);
        int fixedMethods = 0;

        using (var ms = new MemoryStream(bytes, writable: false))
        using (var pe = new PEReader(ms))
        {
            var md = pe.GetMetadataReader();
            foreach (var handle in md.MethodDefinitions)
            {
                var method = md.GetMethodDefinition(handle);
                int rva = method.RelativeVirtualAddress;
                if (rva == 0) continue;                    // abstract / extern

                int section = pe.PEHeaders.GetContainingSectionIndex(rva);
                if (section < 0) continue;
                var header = pe.PEHeaders.SectionHeaders[section];
                int bodyOffset = header.PointerToRawData + (rva - header.VirtualAddress);

                if (FixMethod(bytes, bodyOffset)) fixedMethods++;
            }
        }

        if (fixedMethods > 0)
            File.WriteAllBytes(faslPath, bytes);
        return fixedMethods;
    }

    /// <summary>Reorder one method body's clauses in <paramref name="bytes"/>.</summary>
    private static bool FixMethod(byte[] bytes, int bodyOffset)
    {
        byte flags = bytes[bodyOffset];
        if ((flags & HeaderFatFormat) != HeaderFatFormat) return false;  // tiny header: no EH
        if ((flags & HeaderMoreSects) == 0) return false;

        int headerSize = (bytes[bodyOffset + 1] >> 4) * 4;
        int codeSize = BitConverter.ToInt32(bytes, bodyOffset + 4);
        int sectionOffset = Align4(bodyOffset + headerSize + codeSize);

        bool changed = false;
        while (true)
        {
            byte kind = bytes[sectionOffset];
            bool fat = (kind & SectFatFormat) != 0;
            int dataSize = fat
                ? bytes[sectionOffset + 1] | (bytes[sectionOffset + 2] << 8) | (bytes[sectionOffset + 3] << 16)
                : bytes[sectionOffset + 1];

            if ((kind & SectEHTable) != 0)
                changed |= ReorderClauses(bytes, sectionOffset + 4, dataSize, fat);

            if ((kind & SectMoreSects) == 0) break;
            sectionOffset = Align4(sectionOffset + dataSize);
        }
        return changed;
    }

    private static bool ReorderClauses(byte[] bytes, int firstClause, int dataSize, bool fat)
    {
        int clauseSize = fat ? FatClauseSize : SmallClauseSize;
        int count = (dataSize - 4) / clauseSize;
        if (count < 2) return false;

        var clauses = new Clause[count];
        for (int i = 0; i < count; i++)
            clauses[i] = ReadClause(bytes, firstClause + i * clauseSize, fat);

        // Depth = how many other clauses enclose this one. Deepest first; ties keep
        // the order the emitter chose (a filter and the finally wrapping it).
        var depth = new int[count];
        for (int i = 0; i < count; i++)
            for (int j = 0; j < count; j++)
                if (i != j && Encloses(clauses[j], clauses[i]))
                    depth[i]++;

        var order = new List<int>(count);
        for (int i = 0; i < count; i++) order.Add(i);
        order.Sort((a, b) => depth[a] != depth[b] ? depth[b] - depth[a] : a - b);

        bool sameOrder = true;
        for (int i = 0; i < count; i++)
            if (order[i] != i) { sameOrder = false; break; }
        if (sameOrder) return false;

        var original = new byte[count][];
        for (int i = 0; i < count; i++)
        {
            original[i] = new byte[clauseSize];
            Array.Copy(bytes, firstClause + i * clauseSize, original[i], 0, clauseSize);
        }
        for (int i = 0; i < count; i++)
            Array.Copy(original[order[i]], 0, bytes, firstClause + i * clauseSize, clauseSize);
        return true;
    }

    private readonly struct Clause
    {
        public readonly int TryStart, TryEnd, ProtectedStart, HandlerEnd;

        public Clause(int flags, int tryOffset, int tryLength,
                      int handlerOffset, int handlerLength, int extra)
        {
            TryStart = tryOffset;
            TryEnd = tryOffset + tryLength;
            // A filter's code sits between its own start and the handler, and is
            // protected by this clause just as the handler is.
            ProtectedStart = (flags & ClauseIsFilter) != 0 ? extra : handlerOffset;
            HandlerEnd = handlerOffset + handlerLength;
        }
    }

    private static Clause ReadClause(byte[] b, int at, bool fat) => fat
        ? new Clause(BitConverter.ToInt32(b, at),
                     BitConverter.ToInt32(b, at + 4), BitConverter.ToInt32(b, at + 8),
                     BitConverter.ToInt32(b, at + 12), BitConverter.ToInt32(b, at + 16),
                     BitConverter.ToInt32(b, at + 20))
        : new Clause(BitConverter.ToUInt16(b, at),
                     BitConverter.ToUInt16(b, at + 2), b[at + 4],
                     BitConverter.ToUInt16(b, at + 5), b[at + 7],
                     BitConverter.ToInt32(b, at + 8));

    /// <summary>True when <paramref name="inner"/>'s try block sits inside
    /// <paramref name="outer"/>'s try, or inside its filter/handler.</summary>
    private static bool Encloses(in Clause outer, in Clause inner) =>
        (inner.TryStart >= outer.TryStart && inner.TryEnd <= outer.TryEnd)
        || (inner.TryStart >= outer.ProtectedStart && inner.TryEnd <= outer.HandlerEnd);

    private static int Align4(int offset) => (offset + 3) & ~3;
}
#endif
