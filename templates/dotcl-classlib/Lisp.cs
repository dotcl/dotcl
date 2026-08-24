using DotCL;

namespace DotclLibrary;

/// <summary>
/// The .NET face of this library. The Lisp side lives in library.lisp and is
/// compiled to DotclLibrary.fasl at build time; the methods here boot the dotcl
/// runtime once, load that fasl, and call in.
///
/// Consumers reference this project and call <c>Lisp.Greet("world")</c> — they
/// never see the runtime or the fasl.
/// </summary>
public static class Lisp
{
    private static readonly object Gate = new();
    private static bool _loaded;

    /// <summary>
    /// Boot the runtime and load this library's fasl. Idempotent and safe to
    /// call from several threads; every entry point below calls it first, so a
    /// consumer does not have to.
    /// </summary>
    public static void EnsureLoaded()
    {
        if (_loaded) return;
        lock (Gate)
        {
            if (_loaded) return;
            DotclHost.Initialize();
            // Boot the base image unless the host already did — a consuming app
            // may be a dotcl project itself and have loaded it.
            DotclHost.EnsureCore();
            // This library's own manifest. It is named after the project rather
            // than dotcl-deps.txt because a consuming dotcl app owns that name
            // in the shared output directory. Entries already loaded (the core,
            // shared dependencies) are skipped.
            var manifest = Path.Combine(
                AppContext.BaseDirectory, "dotcl-fasl", "DotclLibrary.deps.txt");
            DotclHost.LoadFromManifest(manifest);
            _loaded = true;
        }
    }

    /// <summary>Calls LIB:GREET.</summary>
    public static string Greet(string name)
    {
        EnsureLoaded();
        return DotclHost.ToClr<string>(DotclHost.Call("LIB:GREET", name));
    }

    /// <summary>
    /// Calls LIB:SUM-OF-SQUARES with a sequence. A .NET collection passed
    /// directly would reach Lisp as a foreign object (a byte[] stays the same
    /// buffer, by design), so hand it over as a Lisp list explicitly.
    /// </summary>
    public static long SumOfSquares(IEnumerable<int> numbers)
    {
        EnsureLoaded();
        return DotclHost.ToClr<long>(
            DotclHost.Call("LIB:SUM-OF-SQUARES", DotclHost.ToLispList(numbers)));
    }
}
