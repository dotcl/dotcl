namespace DotCL;

/// <summary>
/// The build-tool half of the .NET surface: resolving an ASDF system's
/// dependencies, compiling a project to a fasl, packing one for distribution,
/// and reading a system's metadata for a nuspec. These are what the
/// <c>dotcl</c> CLI and the MSBuild tasks call.
///
/// Separate from <see cref="DotclHost"/> because the two have different
/// audiences and different lifetimes. DotclHost is what an application embeds:
/// it is about running Lisp inside a process that is doing something else, and
/// its contract is about lifecycle, conditions, values and streams. What is
/// here runs at BUILD time, in a tool, and its contract is about files on disk.
/// A host that embeds Lisp in a game never calls any of it, and having both on
/// one type made the embedding surface look larger and less decided than it is.
///
/// The implementations still live in DotclHost.cs for now, reached through
/// internal entry points; this type is where the public names are. Moving the
/// bodies across is a separate, purely mechanical step.
/// </summary>
public static class DotclBuild
{
    /// <summary>
    /// Metadata read off an ASDF system definition, used to fill in nuspec
    /// fields for `dotcl pack`. Every field is null when the .asd omits it.
    /// </summary>
    public sealed class SystemMeta
    {
        public string? Description;
        public string? Homepage;
        public string? SourceControlUrl;
        public string? Author;
        public string? License;
        public string? AsdDirectory;   // where to look for a sibling README
        public string? Version;        // null unless the .asd states a string version
        public string? EntryPoint;     // :entry-point, as a "pkg:name" string; null if none
    }

    /// <summary>
    /// Resolve an ASDF system's dependencies and write the manifest and root
    /// source list the build then consumes.
    /// </summary>
    public static void ResolveDeps(string asdPath, string? manifestOut, string? rootSourcesOut,
                                   string? targetRid = null, string[]? buildInit = null,
                                   string[]? searchPaths = null)
        => DotclHost.ResolveDepsCore(asdPath, manifestOut, rootSourcesOut, targetRid, buildInit, searchPaths);

    /// <summary>
    /// Compile an ASDF system's own sources to <paramref name="outputPath"/>,
    /// with its dependencies staying as pre-built fasls resolved by
    /// <see cref="ResolveDeps"/>.
    /// </summary>
    public static void CompileProject(string asdPath, string outputPath, string[]? buildInit = null,
                                      string[]? searchPaths = null, bool debugInfo = false)
        => DotclHost.CompileProjectCore(asdPath, outputPath, buildInit, searchPaths, debugInfo);

    /// <summary>
    /// Build a system and everything it depends on into one fasl at
    /// <paramref name="outputFasl"/>, for shipping.
    /// </summary>
    public static void PackFasl(string system, string outputFasl, string? toplevel = null,
                                string[]? buildInit = null, string[]? searchPaths = null)
        => DotclHost.PackFaslCore(system, outputFasl, toplevel, buildInit, searchPaths);

    /// <summary>
    /// As <see cref="PackFasl(string, string, string?, string[]?, string[]?)"/>,
    /// with PRELUDE sources compiled ahead of the system's closure.
    /// </summary>
    public static void PackFasl(string system, string outputFasl, string? toplevel,
                                string[]? buildInit, string[]? searchPaths,
                                string[]? prelude)
        => DotclHost.PackFaslCore(system, outputFasl, toplevel, buildInit, searchPaths, prelude);

    /// <summary>
    /// Read the standard metadata slots off an ASDF system, as nuspec defaults.
    /// Fields are null where the .asd is silent; the result is null when the
    /// system cannot be found at all.
    /// </summary>
    public static SystemMeta? ReadSystemMeta(string system, string[]? searchPaths = null)
        => DotclHost.ReadSystemMetaCore(system, searchPaths);

    /// <summary>
    /// As <see cref="ReadSystemMeta(string, string[]?)"/>, and when the result is
    /// null <paramref name="error"/> says why: the system is not visible to ASDF,
    /// or loading its .asd signalled. Null error means the system was read.
    /// </summary>
    public static SystemMeta? ReadSystemMeta(string system, string[]? searchPaths, out string? error)
        => DotclHost.ReadSystemMetaCore(system, searchPaths, out error);
}
