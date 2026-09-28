namespace DotCL;

using System.IO;

/// <summary>
/// The shared compile cache ASDF writes to, and the `dotcl clean` subcommand
/// that empties it. Clean also removes the JIT profiles (see JitProfile): the
/// two caches are written by different parts of the system, but a user asking
/// for dotcl's caches to go means both, and a cache nothing can remove is the
/// question that put the profile in a cache directory in the first place.
///
/// ASDF sends every fasl it compiles for a system to
/// {cache-home}/common-lisp/{implementation-identifier}/{mirrored-source-path},
/// which lives outside any project and survives `dotnet clean`. The identifier
/// carries the exact build (version + commit + dirty flag), on purpose: a fasl
/// must never be reused by a build whose code generation differs. The cost is one
/// directory per build, and nothing ever removed them.
///
/// The location is uiop's XDG-CACHE-HOME rule, recomputed here rather than asked
/// of a running Lisp: cleaning is what a user reaches for when loading is broken,
/// so it must not need the core or ASDF. A regression test compares this against
/// uiop's own answer so the two cannot drift apart silently.
/// </summary>
public static class FaslCache
{
    private const string DirPrefix = "dotcl-";

    private static string? AbsoluteEnv(string name)
    {
        var v = Environment.GetEnvironmentVariable(name);
        if (string.IsNullOrEmpty(v)) return null;
        return Path.IsPathRooted(v) ? v : null;   // uiop ignores a relative setting
    }

    /// <summary>
    /// The user's cache directory, by uiop's XDG-CACHE-HOME rule. The fasl
    /// cache hangs below it, and so does the JIT profile (see JitProfile):
    /// both are disposable, machine local and rebuilt on demand, and there is
    /// one rule for finding that kind of thing rather than one per writer.
    /// </summary>
    public static string CacheHome()
    {
        var cacheHome = AbsoluteEnv("XDG_CACHE_HOME");
        if (cacheHome == null)
        {
            if (Compat.IsWindows())
            {
                // uiop: (xdg-data-home "cache/") on Windows, and xdg-data-home is
                // XDG_DATA_HOME or LOCALAPPDATA.
                var dataHome = AbsoluteEnv("XDG_DATA_HOME")
                    ?? AbsoluteEnv("LOCALAPPDATA")
                    ?? Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
                cacheHome = Path.Combine(dataHome, "cache");
            }
            else
            {
                var home = Environment.GetEnvironmentVariable("HOME")
                    ?? Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
                cacheHome = Path.Combine(home, ".cache");
            }
        }
        return cacheHome;
    }

    /// <summary>The common-lisp cache directory: where the per-build directories live.</summary>
    public static string Root() => Path.Combine(CacheHome(), "common-lisp");

    /// <summary>
    /// The per-build directories under ROOT, oldest write first. Only real
    /// directories named "dotcl-*" count: a file with that name, a directory
    /// belonging to another implementation, and anything reached through a
    /// symlink or junction are all left alone (deleting through a link would
    /// delete whatever it points at).
    /// </summary>
    public static List<DirectoryInfo> Entries(string root)
    {
        var result = new List<DirectoryInfo>();
        DirectoryInfo dir;
        try { dir = new DirectoryInfo(root); } catch { return result; }
        if (!dir.Exists) return result;
        foreach (var sub in dir.EnumerateDirectories(DirPrefix + "*"))
        {
            if (Compat.IsDirectoryLink(sub)) continue;
            result.Add(sub);
        }
        result.Sort((a, b) => a.LastWriteTimeUtc.CompareTo(b.LastWriteTimeUtc));
        return result;
    }

    private static long DirectorySize(DirectoryInfo dir)
    {
        long total = 0;
        try
        {
            foreach (var f in dir.EnumerateFiles("*", SearchOption.AllDirectories))
            {
                try { total += f.Length; } catch { }
            }
        }
        catch { }   // unreadable subtree: report what was countable
        return total;
    }

    private static string HumanSize(long bytes)
    {
        string[] units = { "B", "KB", "MB", "GB", "TB" };
        double v = bytes;
        int u = 0;
        while (v >= 1024 && u < units.Length - 1) { v /= 1024; u++; }
        return u == 0 ? $"{bytes} {units[u]}" : $"{v:0.#} {units[u]}";
    }

    /// <summary>
    /// The `dotcl clean` subcommand: every cache dotcl writes. That is two
    /// directories, the compile cache here and the JIT profiles under
    /// JitProfile.Root, and they get one report line each. One sentence over
    /// both would have to name two paths and add two counts together, and the
    /// path is the part a user who is asking "what is this file" came for.
    /// Returns the process exit code.
    /// </summary>
    public static int Run(bool dryRun, string? keepPrefix, bool verbose, TextWriter o)
    {
        var failed = CleanCacheDirs(dryRun, keepPrefix, verbose, o);
        failed += CleanJitProfiles(dryRun, verbose, o);
        return failed == 0 ? 0 : 1;
    }

    /// <summary>
    /// The per-build compile cache directories. KEEPPREFIX, when given, spares the
    /// directories whose name starts with it: "dotcl-{this build's version}-", so
    /// --keep-current does not force the next start to recompile. A prefix rather
    /// than an exact name: the OS/architecture suffix is ASDF's to spell, and the
    /// version alone (which carries the commit) already identifies the build.
    /// Returns the number of entries that could not be removed.
    /// </summary>
    private static int CleanCacheDirs(bool dryRun, string? keepPrefix, bool verbose, TextWriter o)
    {
        var root = Root();
        var entries = Entries(root);
        if (keepPrefix != null)
            entries.RemoveAll(e => e.Name.StartsWith(keepPrefix, StringComparison.Ordinal));

        if (entries.Count == 0)
        {
            o.WriteLine($"dotcl clean: nothing to remove in {root}");
            return 0;
        }

        long freed = 0;
        int removed = 0;
        var failures = new List<string>();
        foreach (var e in entries)
        {
            var size = DirectorySize(e);
            if (dryRun)
            {
                if (verbose) o.WriteLine($"  would remove {e.Name} ({HumanSize(size)})");
                freed += size;
                removed++;
                continue;
            }
            try
            {
                e.Delete(recursive: true);
                if (verbose) o.WriteLine($"  removed {e.Name} ({HumanSize(size)})");
                freed += size;
                removed++;
            }
            catch (Exception ex)
            {
                failures.Add($"{e.Name}: {ex.Message}");
            }
        }

        var verb = dryRun ? "would remove" : "removed";
        o.WriteLine($"dotcl clean: {verb} {removed} cache director{(removed == 1 ? "y" : "ies")}"
                  + $" ({HumanSize(freed)}) under {root}");
        if (keepPrefix != null) o.WriteLine($"  kept the cache for this build ({keepPrefix}*)");
        foreach (var f in failures) o.WriteLine($"  could not remove {f}");
        return failures.Count;
    }

    /// <summary>
    /// The JIT profiles. There is no --keep-current here and the flag does not
    /// reach this far: its prefix spells ASDF's implementation-identifier
    /// naming, which these files do not use, and what "current" would name is
    /// the single file this very process is writing.
    ///
    /// That file is skipped, and is not counted as removed. Removing it works
    /// -- the runtime keeps no lock on it -- but the runtime writes it again as
    /// this run ends, so it would free nothing, would leave the directory
    /// unable to ever report itself empty, and would throw away the profile
    /// that shortens the next start. With recording switched off there is no
    /// file of ours in use and nothing is spared.
    /// Returns the number of entries that could not be removed.
    /// </summary>
    private static int CleanJitProfiles(bool dryRun, bool verbose, TextWriter o)
    {
        var root = JitProfile.Root();
        var entries = JitProfile.Entries(root);
        bool recording = JitProfile.Recording();

        long freed = 0;
        int removed = 0;
        var failures = new List<string>();
        foreach (var e in entries)
        {
            if (recording && JitProfile.IsCurrent(e.Name))
            {
                if (verbose) o.WriteLine($"  kept {e.Name}, the profile this run is writing");
                continue;
            }
            long size;
            try { size = e.Length; } catch { size = 0; }
            if (dryRun)
            {
                if (verbose) o.WriteLine($"  would remove {e.Name} ({HumanSize(size)})");
                freed += size;
                removed++;
                continue;
            }
            try
            {
                e.Delete();
                if (verbose) o.WriteLine($"  removed {e.Name} ({HumanSize(size)})");
                freed += size;
                removed++;
            }
            catch (Exception ex)
            {
                failures.Add($"{e.Name}: {ex.Message}");
            }
        }

        if (removed == 0 && failures.Count == 0)
        {
            o.WriteLine($"dotcl clean: nothing to remove in {root}");
            return 0;
        }

        var verb = dryRun ? "would remove" : "removed";
        o.WriteLine($"dotcl clean: {verb} {removed} JIT profile{(removed == 1 ? "" : "s")}"
                  + $" ({HumanSize(freed)}) under {root}");
        foreach (var f in failures) o.WriteLine($"  could not remove {f}");
        return failures.Count;
    }

    /// <summary>
    /// Lisp entry points, for the regression tests: (dotcl::%fasl-cache-root) and
    /// (dotcl::%fasl-cache-entries root): the second so a test can point the
    /// selection rule at a directory it built itself instead of the user's cache.
    /// </summary>
    public static LispObject FaslCacheRoot(LispObject[] args)
        => new LispString(Root().Replace("\\", "/"));

    public static LispObject FaslCacheEntries(LispObject[] args)
    {
        var root = args.Length > 0 && args[0] is LispString s ? s.Value : Root();
        LispObject result = Nil.Instance;
        var names = Entries(root);
        for (int i = names.Count - 1; i >= 0; i--)
            result = new Cons(new LispString(names[i].Name), result);
        return result;
    }
}
