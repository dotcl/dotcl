using System;
using System.IO;
using System.Runtime.Loader;
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
    public static void Boot(string baseCore, string? contribDir)
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

    public override bool Execute()
    {
        DotclBoot.InstallResolver(RuntimeAssemblyPath);
        try { Run(); return true; }
        catch (Exception ex)
        {
            Log.LogError($"dotcl resolve-deps failed for {Asd}: {ex.Message}");
            return false;
        }
    }

    private void Run()
    {
        DotclBoot.Boot(BaseCore, ContribDir);
        DotclHost.ResolveDeps(Asd, ManifestOut, RootSourcesOut,
                              string.IsNullOrEmpty(TargetRid) ? null : TargetRid);
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

    public override bool Execute()
    {
        DotclBoot.InstallResolver(RuntimeAssemblyPath);
        try { Run(); return true; }
        catch (Exception ex)
        {
            Log.LogError($"dotcl compile-project failed for {Asd}: {ex.Message}");
            return false;
        }
    }

    private void Run()
    {
        DotclBoot.Boot(BaseCore, ContribDir);
        DotclHost.CompileProject(Asd, Output);
    }
}
