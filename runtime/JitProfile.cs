namespace DotCL;

using System.IO;
using System.Reflection;
using System.Text;

/// <summary>
/// Where the .NET multi-core JIT profile goes.
///
/// The runtime records which methods a run compiles and, on the next run,
/// compiles them again on a background thread while the foreground starts.
/// Measured on a cold start, that is roughly 15-35% when dotcl.core is plain IL
/// (a development tree) and about 2-5% in the shipped ReadyToRun layout, where
/// little is left to JIT. The profile is a cache:
/// disposable, machine local, and rebuilt from nothing whenever it is missing.
/// The runtime invalidates it by itself when the assembly changes.
///
/// It used to be written beside the executing assembly, which is the one place
/// it must not go. An executable produced by DOTCL:SAVE-APPLICATION keeps
/// dotcl's entry point, so a shipped application wrote a dotcl-named file into
/// its own install directory on every run, and under a read-only install the
/// write failed and the speedup was zero. An installed tool directory is meant
/// to be immutable. A profile left behind in a publish directory is picked up
/// by `dotnet pack` and ships inside the package.
///
/// So it lives in the user's cache home instead, found by the same rule the
/// fasl cache uses (see FaslCache.CacheHome), and is named after the program
/// that writes it. The naming is not decoration: the runtime overwrites the
/// profile with the current run's trace, so two programs sharing one file
/// would each leave the other a list of methods it never calls, and the
/// background thread would spend the startup window compiling them. The dotcl
/// CLI and an application built from it are two such programs.
/// </summary>
public static class JitProfile
{
    /// <summary>The directory the profile file is written to.</summary>
    public static string Root() => Path.Combine(FaslCache.CacheHome(), "dotcl", "jit");

    /// <summary>
    /// Whether this process records a profile. The startup path asks before
    /// switching recording on, and `dotcl clean` asks because the answer says
    /// whether there is a file of this process's own in the directory it is
    /// about to empty. One spelling of the switch, two readers.
    /// </summary>
    public static bool Recording()
        => Environment.GetEnvironmentVariable("DOTCL_NO_JIT_PROFILE") != "1";

    /// <summary>
    /// The profile files directly under ROOT, oldest write first: what
    /// `dotcl clean` removes. Only names ending in ".profile" count, and only
    /// files, so a subdirectory or anything else a user parked there is left
    /// alone. The extension is matched here rather than handed to the
    /// enumerator because a Windows short name lets "*.profile" match longer
    /// extensions as well.
    /// </summary>
    public static List<FileInfo> Entries(string root)
    {
        var result = new List<FileInfo>();
        DirectoryInfo dir;
        try { dir = new DirectoryInfo(root); } catch { return result; }
        if (!dir.Exists) return result;
        try
        {
            foreach (var f in dir.EnumerateFiles())
            {
                if (f.Name.EndsWith(".profile", StringComparison.OrdinalIgnoreCase))
                    result.Add(f);
            }
        }
        catch { return result; }   // unreadable directory: nothing to offer
        result.Sort((a, b) => a.LastWriteTimeUtc.CompareTo(b.LastWriteTimeUtc));
        return result;
    }

    /// <summary>Whether NAME is the profile this process is writing. Case is
    /// folded on Windows, where two spellings name one file.</summary>
    public static bool IsCurrent(string name)
        => string.Equals(name, Name(),
                         Compat.IsWindows() ? StringComparison.OrdinalIgnoreCase
                                            : StringComparison.Ordinal);

    /// <summary>
    /// The profile file name for a program whose executable is EXEPATH and
    /// whose assemblies sit in BASEDIR.
    ///
    /// BASEDIR is what identifies the program, and EXEPATH cannot do the job
    /// alone: run as `dotnet whatever.dll` the executable is the shared dotnet
    /// host, the same path for every build on the machine. EXEPATH still
    /// matters for the opposite case, an application launched through its own
    /// apphost: SAVE-APPLICATION copies one and the same runtime.exe to
    /// whatever the user called it, without renaming the assembly, so two
    /// applications installed side by side in one directory are told apart by
    /// their file name and nothing else.
    ///
    /// The name carries a readable stem so the directory can be made sense of
    /// by eye, and a hash of the pair so the parts that are not readable still
    /// count. Case is folded on Windows, where two spellings name one file.
    /// </summary>
    public static string NameFor(string? exePath, string? baseDir)
    {
        var exe = (exePath ?? "").Trim();
        var home = Norm(baseDir);
        // The apphost sits beside the assemblies it loads; the shared dotnet
        // host does not. Only in the first case does the executable's name say
        // anything about which program this is.
        var ownHost = exe.Length > 0
                      && home.Length > 0
                      && Norm(Path.GetDirectoryName(exe)) == home;
        var stem = (ownHost ? Path.GetFileNameWithoutExtension(exe) : EntryName()) ?? "";
        if (stem.Length == 0) stem = "dotcl";
        if (stem.Length > 32) stem = stem.Substring(0, 32);
        if (home.Length == 0 && exe.Length == 0) return "dotcl-unknown.profile";
        return stem + "-" + Hash(home + "|" + stem) + ".profile";
    }

    /// <summary>The profile file name for this process.</summary>
    public static string Name() => NameFor(Compat.ProcessPath(), AppContext.BaseDirectory);

    /// <summary>Root and name together: what the profile is called on disk.</summary>
    public static string FilePath() => Path.Combine(Root(), Name());

    private static string? EntryName()
    {
        try { return Assembly.GetEntryAssembly()?.GetName().Name; }
        catch { return null; }
    }

    /// <summary>A directory path in one spelling, so that two ways of writing
    /// the same directory compare equal.</summary>
    private static string Norm(string? dir)
    {
        var d = (dir ?? "").Trim().Replace('\\', '/').TrimEnd('/');
        return Compat.IsWindows() ? d.ToLowerInvariant() : d;
    }

    /// <summary>
    /// FNV-1a, 64 bit. A non-cryptographic hash is all this needs, but it does
    /// have to be the same number on every run: string.GetHashCode is seeded
    /// per process and would hand out a new profile each start.
    /// </summary>
    private static string Hash(string s)
    {
        ulong h = 14695981039346656037UL;
        foreach (var b in Encoding.UTF8.GetBytes(s))
        {
            h ^= b;
            h *= 1099511628211UL;
        }
        return h.ToString("x16");
    }

    /// <summary>
    /// Lisp entry points for the regression suite: (dotcl::%jit-profile-path),
    /// (dotcl::%jit-profile-name-for exe-path base-dir) and
    /// (dotcl::%jit-profile-entries root). The second takes its inputs so the
    /// naming rule can be checked against the launch shapes that matter,
    /// rather than only against wherever this one build sits; the third takes
    /// a root so a test can point the selection rule at a directory it built
    /// itself instead of the user's cache.
    /// </summary>
    public static LispObject JitProfilePath(LispObject[] args)
        => new LispString(FilePath().Replace("\\", "/"));

    public static LispObject JitProfileNameFor(LispObject[] args)
        => new LispString(NameFor(Arg(args, 0), Arg(args, 1)));

    public static LispObject JitProfileEntries(LispObject[] args)
    {
        var root = args.Length > 0 && args[0] is LispString s ? s.Value : Root();
        LispObject result = Nil.Instance;
        var names = Entries(root);
        for (int i = names.Count - 1; i >= 0; i--)
            result = new Cons(new LispString(names[i].Name), result);
        return result;
    }

    private static string? Arg(LispObject[] args, int i)
        => args.Length > i && args[i] is LispString s ? s.Value : null;
}
