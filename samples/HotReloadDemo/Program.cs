using DotCL;

// HotReloadDemo — a plain .NET console host that embeds dotcl and hot-reloads
// its Lisp "handler layer" while running. Lisp has redefinition semantics
// built into the language, so unlike .NET Hot Reload (MetadataUpdateHandler)
// there is no restriction on what an edit may change: signatures, new
// functions, new macros, new classes — a re-load just installs the new
// definitions and the next call uses them.
//
// Run with `dotnet run` from this directory, then edit handlers.lisp and save.

string script = Path.Combine(Environment.CurrentDirectory, "handlers.lisp");
if (!File.Exists(script))
{
    Console.Error.WriteLine($"handlers.lisp not found at {script} — run `dotnet run` from the HotReloadDemo directory.");
    return 1;
}

DotclHost.Initialize();
var core = DotclHost.FindCore() ?? FindRepoCore()
    ?? throw new InvalidOperationException("dotcl core not found (build the repo first: make cross-compile)");
DotclHost.LoadCore(core);

// Running from the repo checkout: walk up from the sample directory to the
// repo root and use the freshly cross-compiled core there.
static string? FindRepoCore()
{
    for (var dir = new DirectoryInfo(Environment.CurrentDirectory); dir != null; dir = dir.Parent)
    {
        var candidate = Path.Combine(dir.FullName, "compiler", "cil-out.sil");
        if (File.Exists(candidate)) return candidate;
    }
    return null;
}

// A failed load (typo mid-edit) must not kill the host: report it and keep
// serving with the previous definitions. That is the whole point of a
// hot-reload loop — the image only moves forward on a load that succeeds.
void LoadHandlers(string reason)
{
    try
    {
        DotclHost.LoadLispFile(script);
        Console.WriteLine($"[reload] {reason}: handlers.lisp loaded");
    }
    catch (Exception ex)
    {
        Console.WriteLine($"[reload] {reason}: FAILED, keeping old definitions — {ex.Message}");
    }
}

LoadHandlers("startup");

// Watch the source file. Editors fire several change events per save and the
// file can still be locked by the editor on the first one, so debounce a
// little and retry through the load's own error handling.
using var watcher = new FileSystemWatcher(Environment.CurrentDirectory, "handlers.lisp")
{
    NotifyFilter = NotifyFilters.LastWrite | NotifyFilters.FileName | NotifyFilters.Size,
    EnableRaisingEvents = true,
};
DateTime lastReload = DateTime.MinValue;
watcher.Changed += (_, _) =>
{
    var now = DateTime.UtcNow;
    if ((now - lastReload).TotalMilliseconds < 200) return; // debounce editor double-fires
    lastReload = now;
    Thread.Sleep(50); // let the editor finish writing
    LoadHandlers("file changed");
};

Console.WriteLine("host running — edit handlers.lisp and save to hot-reload (Ctrl+C to quit)");
for (long n = 1; ; n++)
{
    try
    {
        var reply = DotclHost.ToClr<string>(DotclHost.Call("HANDLE-REQUEST", n));
        Console.WriteLine($"  request {n} -> {reply}");
    }
    catch (Exception ex)
    {
        Console.WriteLine($"  request {n} -> handler error: {ex.Message}");
    }
    Thread.Sleep(1000);
}
