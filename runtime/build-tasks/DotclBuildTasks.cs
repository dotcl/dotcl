using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.ExceptionServices;
using System.Runtime.Loader;
using System.Threading;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Task = Microsoft.Build.Utilities.Task;

namespace DotCL.Build.Tasks;

/// <summary>
/// One-time, per-MSBuild-process boot of the in-process dotcl compiler.
/// Methods here that DON'T touch DotCL.* types (InstallResolver) are kept
/// separate from those that do (Boot) so the assembly-resolve handler is
/// registered before the JIT first pulls in DotCL.Runtime.
/// </summary>
internal static class DotclBoot
{
    private static readonly object _gate = new();
    private static bool _resolverInstalled;
    private static bool _coreLoaded;
    private static string? _runtimeAssemblyPath;

    /// <summary>
    /// Register an assembly-resolve handler that loads DotCL.Runtime from
    /// <paramref name="runtimeAssemblyPath"/> when it is not already adjacent to
    /// this task assembly. In the packaged layout the task lives under tasks/
    /// while the runtime ships under lib/, so the package targets pass the lib
    /// path here to avoid shipping (and loading) a second copy.
    /// </summary>
    public static void InstallResolver(string? runtimeAssemblyPath)
    {
        lock (_gate)
        {
            if (!string.IsNullOrEmpty(runtimeAssemblyPath))
                _runtimeAssemblyPath = Path.GetFullPath(runtimeAssemblyPath!);
            if (_resolverInstalled) return;
            _resolverInstalled = true;

            // Load DotCL.Runtime into the DEFAULT load context, not the task's
            // MSBuildLoadContext. The compiler loads FASLs (the base core and the
            // compiled outputs) via Assembly.LoadFrom, which always resolves into
            // the Default ALC; those FASLs reference DotCL.Runtime, so the runtime
            // must live in Default for them — and for the task's own DotclHost
            // calls — to bind to the *same* instance. (A non-default ALC falls
            // back to Default when its own Load returns null, so the task resolves
            // there too.) Binding is by simple name, so a version skew between a
            // FASL's recorded reference and the loaded runtime is tolerated.
            var alc = AssemblyLoadContext.Default;

            // Backup resolver, in case probing asks before the eager load lands.
            alc.Resolving += (ctx, name) =>
            {
                if (name.Name != "DotCL.Runtime") return null;
                if (_runtimeAssemblyPath != null && File.Exists(_runtimeAssemblyPath))
                    return ctx.LoadFromAssemblyPath(_runtimeAssemblyPath);
                return null;
            };

            // Primary: eagerly load the packaged runtime into Default so that
            // resolving DotCL.Runtime by simple name returns this instance.
            if (_runtimeAssemblyPath != null && File.Exists(_runtimeAssemblyPath))
                alc.LoadFromAssemblyPath(_runtimeAssemblyPath);
        }
    }

    /// <summary>
    /// Initialize the runtime, point contrib discovery at the packaged contrib
    /// tree, and load the base core once. Idempotent across task invocations in
    /// the same MSBuild process. Touches DotCL.* — only call after InstallResolver.
    /// </summary>
    public static void Boot(string baseCore, string? contribDir,
                            IEnumerable<string>? referenceDirs = null)
    {
        lock (_gate)
        {
            DotclHost.Initialize();
            if (!string.IsNullOrEmpty(contribDir))
            {
                var dir = Path.GetFullPath(contribDir);
                if (Directory.Exists(dir) && !Runtime.ContribExtraSearchPaths.Contains(dir))
                    Runtime.ContribExtraSearchPaths.Add(dir);
            }
            // Consumer reference assemblies live in the project's output, not the
            // MSBuild/SDK base dir, so resolve-type cannot see them without this.
            if (referenceDirs != null)
                foreach (var d in referenceDirs)
                {
                    if (string.IsNullOrEmpty(d)) continue;
                    var dir = Path.GetFullPath(d);
                    if (Directory.Exists(dir) && !Runtime.AssemblyProbeSearchPaths.Contains(dir))
                        Runtime.AssemblyProbeSearchPaths.Add(dir);
                }
            if (!_coreLoaded)
            {
                DotclHost.LoadCore(baseCore);
                // No console here — surface Lisp errors as exceptions (→ MSBuild
                // errors) rather than stalling in the interactive debugger.
                DotclHost.SetThrowingDebuggerHook();
                _coreLoaded = true;
            }
        }
    }

    /// <summary>
    /// Run <paramref name="body"/> on a dedicated thread with a large stack.
    /// The recursive-descent compiler can recurse deeply on large/deep Lisp
    /// sources; the MSBuild worker thread's default (~1 MB) stack overflows
    /// intermittently there. The CLI avoids this by running on a 256 MB
    /// stack thread — mirror that here. Exceptions propagate to the caller with
    /// their original stack trace.
    /// </summary>
    /// <summary>Distinct directories holding the given reference assembly files
    /// (@(ReferenceCopyLocalPaths) items), for AssemblyProbeSearchPaths. Kept
    /// here (not touching DotCL.* types) so callers can compute it before Boot.</summary>
    public static IEnumerable<string> ReferenceDirs(ITaskItem[]? referencePaths)
    {
        if (referencePaths == null) return System.Array.Empty<string>();
        return referencePaths
            .Select(i => i.GetMetadata("FullPath"))
            .Where(p => !string.IsNullOrEmpty(p))
            .Select(p => Path.GetDirectoryName(p) ?? "")
            .Where(d => d.Length > 0)
            .Distinct();
    }

    public static void RunOnLargeStack(Action body)
    {
        const int stackSize = 256 * 1024 * 1024;
        ExceptionDispatchInfo? edi = null;
        var t = new Thread(() =>
        {
            try { body(); }
            catch (Exception ex) { edi = ExceptionDispatchInfo.Capture(ex); }
        }, stackSize);
        t.Start();
        t.Join();
        edi?.Throw();
    }

    /// <summary>
    /// What the build prints when the Lisp side refuses. The raw exception is
    /// missing two things the reader needs. It names an internal step
    /// ("resolve-deps", "compile-project") that nobody typed -- the command was
    /// `dotnet build` -- and for the common failure, a dependency that cannot be
    /// found, it stops at the diagnosis without naming the one build property
    /// that fixes it. The CL_SOURCE_REGISTRY line is there because that is the
    /// mechanism people reach for first: the dotcl CLI honours it and the build
    /// does not, so "it works when I run dotcl but not when I build" is the
    /// natural next confusion.
    /// </summary>
    internal static string FailureMessage(System.Exception ex)
    {
        // A SIMPLE-ERROR's type name tells the reader nothing they can act on,
        // and it sits between two prefixes that do ("dotcl:" and the step). Other
        // condition types stay: those name a real category.
        var raw = ex.Message;
        const string simplePrefix = "SIMPLE-ERROR: ";
        if (raw.StartsWith(simplePrefix, System.StringComparison.Ordinal))
            raw = raw.Substring(simplePrefix.Length);
        var msg = "dotcl: " + raw;
        if (raw.Contains("cannot be found") || raw.Contains("not found"))
            msg += "\n  If that system lives outside the project, add its directory:"
                 + "\n    <ItemGroup><DotclAsdSearchPath Include=\"path/to/dir\" /></ItemGroup>"
                 + "\n  CL_SOURCE_REGISTRY is honoured by the dotcl CLI but is not read during the build.";
        return msg;
    }
}

/// <summary>
/// Walk an ASDF system's :depends-on graph and write the dependency fasl
/// manifest (and optionally the root component source list). In-process
/// equivalent of `dotcl build &lt;asd&gt; --resolve-deps ...`.
/// </summary>
public sealed class DotclResolveDeps : Task
{
    [Required] public string Asd { get; set; } = "";
    [Required] public string BaseCore { get; set; } = "";
    public string? ContribDir { get; set; }
    public string? ManifestOut { get; set; }
    public string? RootSourcesOut { get; set; }
    public string? TargetRid { get; set; }
    /// <summary>Path to DotCL.Runtime.dll in the package lib/ (packaged layout).</summary>
    public string? RuntimeAssemblyPath { get; set; }
    /// <summary>@(DotclBuildInit): Lisp scripts loaded before dependency
    /// resolution so the project can make external systems discoverable
    /// (e.g. (pushnew … asdf:*central-registry*) / boot quicklisp).</summary>
    public ITaskItem[]? BuildInit { get; set; }
    /// <summary>@(DotclAsdSearchPath): external system directories pushed onto
    /// asdf:*central-registry* before resolution — the declarative form of the
    /// build-init pushnew.</summary>
    public ITaskItem[]? AsdSearchPath { get; set; }
    /// <summary>@(ReferenceCopyLocalPaths): the consumer's referenced .NET
    /// assemblies. Their directories are registered as assembly probe paths so
    /// compile-time (dotnet:resolve-type ...) can see PackageReference /
    /// ProjectReference types.</summary>
    public ITaskItem[]? ReferencePath { get; set; }

    public override bool Execute()
    {
        DotclBoot.InstallResolver(RuntimeAssemblyPath);
        try { DotclBoot.RunOnLargeStack(Run); return true; }
        catch (Exception ex)
        {
            Log.LogError(DotclBoot.FailureMessage(ex));
            return false;
        }
    }

    private void Run()
    {
        DotclBoot.Boot(BaseCore, ContribDir, DotclBoot.ReferenceDirs(ReferencePath));
        DotclHost.ResolveDeps(Asd, ManifestOut, RootSourcesOut,
                              string.IsNullOrEmpty(TargetRid) ? null : TargetRid,
                              BuildInit?.Select(i => i.GetMetadata("FullPath")).ToArray(),
                              AsdSearchPath?.Select(i => i.GetMetadata("FullPath")).ToArray());
    }
}

/// <summary>
/// Concatenate-and-compile an ASDF system's root components into one fasl.
/// In-process equivalent of `dotcl build &lt;asd&gt; --output &lt;fasl&gt;`.
/// </summary>
public sealed class DotclCompileProject : Task
{
    [Required] public string Asd { get; set; } = "";
    [Required] public string Output { get; set; } = "";
    [Required] public string BaseCore { get; set; } = "";
    public string? ContribDir { get; set; }
    public string? RuntimeAssemblyPath { get; set; }
    /// <summary>@(DotclBuildInit): Lisp scripts loaded before compilation
    /// (same as DotclResolveDeps; the root .asd may reference external systems).</summary>
    public ITaskItem[]? BuildInit { get; set; }
    /// <summary>@(DotclAsdSearchPath): external system directories pushed onto
    /// asdf:*central-registry* before compilation (same as DotclResolveDeps).</summary>
    public ITaskItem[]? AsdSearchPath { get; set; }
    /// <summary>@(ReferenceCopyLocalPaths): the consumer's referenced .NET
    /// assemblies, so compile-time (dotnet:resolve-type ...) can see
    /// PackageReference / ProjectReference types (same as DotclResolveDeps).</summary>
    public ITaskItem[]? ReferencePath { get; set; }
    /// <summary>Emit a Portable PDB alongside the fasl (Debug build) so the
    /// project is source-debuggable in a .NET debugger.</summary>
    public bool DebugInfo { get; set; }

    public override bool Execute()
    {
        DotclBoot.InstallResolver(RuntimeAssemblyPath);
        try { DotclBoot.RunOnLargeStack(Run); return true; }
        catch (Exception ex)
        {
            Log.LogError(DotclBoot.FailureMessage(ex));
            return false;
        }
    }

    private void Run()
    {
        DotclBoot.Boot(BaseCore, ContribDir, DotclBoot.ReferenceDirs(ReferencePath));
        DotclHost.CompileProject(Asd, Output, BuildInit?.Select(i => i.GetMetadata("FullPath")).ToArray(),
                                 AsdSearchPath?.Select(i => i.GetMetadata("FullPath")).ToArray(),
                                 DebugInfo);
    }
}
