using System;
using System.Collections.Generic;
using System.Text;

namespace DotCL;

/// <summary>
/// Records the host-dependent features a source file's #+ / #- read, so a fasl can
/// say which machine-specific choices were baked into it, and checks them against
/// the host the fasl is loaded on.
/// </summary>
internal static class HostFeatures
{
    /// <summary>Assembly-metadata key holding the record. A fasl without it is read as
    /// having no record.</summary>
    internal const string MetadataKey = "dotcl-host-features";

    /// <summary>Feature names (as read in KEYWORD) whose truth depends on the machine
    /// or OS the code is read on. Implementation names (SBCL, DOTCL, ...) and
    /// capability names are not here: they do not change from host to host.</summary>
    private static readonly HashSet<string> Table = new(StringComparer.Ordinal)
    {
        // OS
        "WINDOWS", "WIN32", "WIN64", "MSWINDOWS", "CYGWIN", "MINGW32", "MINGW64", "UNIX", "LINUX",
        "DARWIN", "MACOS", "MACOSX", "BSD", "FREEBSD", "NETBSD", "OPENBSD", "DRAGONFLY",
        "ANDROID", "IOS", "SOLARIS", "SUNOS", "HAIKU", "BROWSER",
        // CPU
        "X86", "X86-64", "X86_64", "X8664", "AMD64", "I386", "I486", "I586", "I686", "PC386",
        "ARM", "ARM64", "AARCH64", "ARMV6", "ARMV7", "PPC", "PPC64", "POWERPC", "SPARC", "SPARC64",
        "MIPS", "RISCV", "RISCV64", "LOONGARCH64", "S390X",
        // word size and byte order
        "64-BIT", "32-BIT", "LITTLE-ENDIAN", "BIG-ENDIAN",
    };

    internal static bool IsHostDependent(string name) => Table.Contains(name);

    [ThreadStatic] private static SortedDictionary<string, bool>? s_sink;

    /// <summary>Start recording into SINK; returns what to hand to <see cref="End"/>.</summary>
    internal static SortedDictionary<string, bool>? Begin(SortedDictionary<string, bool> sink)
    {
        var prev = s_sink;
        s_sink = sink;
        return prev;
    }

    internal static void End(SortedDictionary<string, bool>? prev) => s_sink = prev;

    /// <summary>Record that the feature atom NAME was examined and had truth TRUTH.
    /// Called for each atom a feature expression's evaluation actually reaches, so
    /// the record holds exactly what the result depended on.</summary>
    internal static void NoteAtom(string name, bool truth)
    {
        var sink = s_sink;
        if (sink == null || !IsHostDependent(name) || IsIgnored(name)) return;
        sink[name] = truth;
    }

    private static bool IsIgnored(string name)
    {
        var sym = Startup.DotclPkg?.FindSymbol("*HOST-FEATURE-IGNORE*").Item1;
        if (sym == null) return false;
        for (var cur = DynamicBindings.Get(sym); cur is Cons c; cur = c.Cdr)
            if (c.Car is Symbol fs && string.Equals(fs.Name, name, StringComparison.Ordinal))
                return true;
        return false;
    }

    internal static string Encode(IReadOnlyDictionary<string, bool> record)
    {
        var sb = new StringBuilder();
        foreach (var kv in record)
        {
            if (sb.Length > 0) sb.Append(' ');
            sb.Append(kv.Value ? '+' : '-').Append(kv.Key);
        }
        return sb.ToString();
    }

    internal static List<(string Name, bool Compiled)> Decode(string value)
    {
        var list = new List<(string, bool)>();
        foreach (var tok in value.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries))
            if (tok.Length > 1 && (tok[0] == '+' || tok[0] == '-'))
                list.Add((tok.Substring(1), tok[0] == '+'));
        return list;
    }

    /// <summary>Whether NAME is a member of dotcl:*host-features* now.</summary>
    internal static bool HostHas(string name)
    {
        var sym = Startup.DotclPkg?.FindSymbol("*HOST-FEATURES*").Item1;
        if (sym == null) return Startup.HasFeature(name);
        for (var cur = DynamicBindings.Get(sym); cur is Cons c; cur = c.Cdr)
            if (c.Car is Symbol fs && string.Equals(fs.Name, name, StringComparison.Ordinal))
                return true;
        return false;
    }

    /// <summary>The features of VALUE (an encoded record) whose truth here differs
    /// from the truth recorded.</summary>
    internal static List<(string Name, bool Compiled)> Differences(string value)
    {
        var diff = new List<(string, bool)>();
        foreach (var (name, compiled) in Decode(value))
            if (HostHas(name) != compiled) diff.Add((name, compiled));
        return diff;
    }
}
