using System.Diagnostics;
using System.IO.Compression;

namespace DotCL;

/// <summary>
/// Compile a packed application's fasl to its ReadyToRun sibling, for a RID that
/// need not be the host's.
///
/// A packed tool loads its own dotcl.user.fasl from beside the executable, and
/// takes the ahead-of-time sibling dotcl.user.fasl.r2r-&lt;rid&gt; when there is
/// one that is not older than the fasl. The runtime and the core underneath the
/// tool have always been native; without this the application itself, which is
/// the whole program and starts once per invocation, was compiled by the JIT at
/// every start. It is worth roughly 2x on a short run.
///
/// Two things decide whether this produces working native code, and getting
/// either wrong fails quietly rather than loudly:
///
///   * crossgen2 has to see the very runtime images the fasl will run against:
///     the runtime.dll, DotCL.Runtime.dll and dotcl.core out of the dotcl
///     package this pack is restamping, not the ones the packing host happens to
///     be running. Given a different set it resolves nothing and emits an
///     assembly with no native code in it.
///
///   * the host crossgen2 pack and the target runtime reference pack are
///     restored separately. A ReadyToRun publish whose target OS is not the
///     host's does not reliably restore the host crossgen2 pack, so priming only
///     for the target leaves the compiler itself missing on a cross-OS pack.
///
/// Every failure returns null with a reason rather than throwing: a package
/// without the sibling is complete and correct, only slower to start.
/// </summary>
static class PackR2r
{
    /// <summary>
    /// Build the ReadyToRun sibling of FASLPATH for TARGETRID, against the
    /// runtime images inside RIDNUPKG (the dotcl.&lt;rid&gt; package being
    /// restamped). Returns the path of the produced file under WORKROOT, or null
    /// with WHY saying what was missing.
    /// </summary>
    public static string? Compile(string faslPath, string targetRid, string ridNupkg,
                                  string workRoot, out string why)
    {
        why = "";
        var hostRid = HostRid();
        if (hostRid == null) { why = "unrecognized host platform"; return null; }
        if (!TargetParts(targetRid, out var targetOs, out var targetArch))
        {
            why = $"unsupported target rid {targetRid}";
            return null;
        }

        var crossgen2 = FindCrossgen2(hostRid);
        if (crossgen2 == null)
        {
            Prime(hostRid);
            crossgen2 = FindCrossgen2(hostRid);
        }
        if (crossgen2 == null)
        {
            why = $"crossgen2 for the packing host ({hostRid}) is not restored, and "
                + $"`dotnet publish -r {hostRid} -p:PublishReadyToRun=true` did not bring it in";
            return null;
        }

        var refDir = FindRuntimeRefDir(targetRid);
        if (refDir == null)
        {
            Prime(targetRid);
            refDir = FindRuntimeRefDir(targetRid);
        }
        if (refDir == null)
        {
            why = $"the .NET runtime reference pack for {targetRid} is not restored, and "
                + $"`dotnet publish -r {targetRid} -p:PublishReadyToRun=true` did not bring it in";
            return null;
        }

        var work = Path.Combine(workRoot, "r2r-" + targetRid);
        Directory.CreateDirectory(work);

        // crossgen2 wants a .dll extension on its input, and dotcl.core is a .NET
        // assembly under a name crossgen2 will not read as one either.
        var input = Path.Combine(work, "in.dll");
        File.Copy(faslPath, input, true);
        var payload = Path.Combine(work, "payload");
        Directory.CreateDirectory(payload);
        var wanted = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            ["runtime.dll"] = "runtime.dll",
            ["DotCL.Runtime.dll"] = "DotCL.Runtime.dll",
            ["dotcl.core"] = "dotcl.core.dll",
        };
        // Extracted every time, overwriting: a second pack into the same output
        // directory finds the previous one's copies still there, and those may
        // have come from a different donor. Deciding by what is on disk
        // afterwards rather than by what this pass happened to write is also the
        // only way the answer stays right when nothing needed extracting.
        using (var zip = ZipFile.OpenRead(ridNupkg))
        {
            foreach (var entry in zip.Entries)
            {
                if (!wanted.TryGetValue(Path.GetFileName(entry.FullName), out var asName))
                    continue;
                entry.ExtractToFile(Path.Combine(payload, asName), true);
            }
        }
        var absent = wanted.Values.Where(v => !File.Exists(Path.Combine(payload, v))).ToList();
        if (absent.Count > 0)
        {
            why = $"{Path.GetFileName(ridNupkg)} does not carry " + string.Join(", ", absent);
            return null;
        }

        // Written under a temporary name: an output truncated by a crossgen2 that
        // died midway would be NEWER than the fasl, so the loader would take it.
        var temp = Path.Combine(work, "out.tmp");
        var psi = new ProcessStartInfo(crossgen2)
        {
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
        };
        psi.ArgumentList.Add(input);
        psi.ArgumentList.Add("-r");
        psi.ArgumentList.Add(Path.Combine(refDir, "*.dll"));
        psi.ArgumentList.Add("-r");
        psi.ArgumentList.Add(Path.Combine(payload, "*.dll"));
        psi.ArgumentList.Add("--targetos");
        psi.ArgumentList.Add(targetOs);
        psi.ArgumentList.Add("--targetarch");
        psi.ArgumentList.Add(targetArch);
        psi.ArgumentList.Add("-O");
        psi.ArgumentList.Add("-o");
        psi.ArgumentList.Add(temp);

        string stderr;
        int exit;
        try
        {
            using var proc = Process.Start(psi);
            if (proc == null) { why = "crossgen2 did not start"; return null; }
            proc.StandardOutput.ReadToEnd();
            stderr = proc.StandardError.ReadToEnd();
            proc.WaitForExit();
            exit = proc.ExitCode;
        }
        catch (Exception ex)
        {
            why = "crossgen2 did not run: " + ex.Message;
            return null;
        }
        if (exit != 0 || !File.Exists(temp))
        {
            var tail = stderr.Trim();
            if (tail.Length > 300) tail = tail.Substring(0, 300) + " ...";
            why = $"crossgen2 exited {exit}" + (tail.Length > 0 ? ": " + tail : "");
            return null;
        }

        var final = Path.Combine(work, $"dotcl.user.fasl.r2r-{targetRid}");
        if (File.Exists(final)) File.Delete(final);
        File.Move(temp, final);
        return final;
    }

    /// <summary>
    /// Restore the .NET packs for RID by doing the one thing that asks for them:
    /// a ReadyToRun publish. Neither the crossgen2 pack nor the target runtime
    /// pack is a dependency of anything else here, so there is no other way to
    /// get them short of telling the caller to go and find them. Best-effort; the
    /// caller checks again afterwards.
    /// </summary>
    static void Prime(string rid)
    {
        var dir = Path.Combine(Path.GetTempPath(), "dotcl-r2r-prime-" + Guid.NewGuid().ToString("N"));
        try
        {
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, "prime.csproj"),
                "<Project Sdk=\"Microsoft.NET.Sdk\">\n"
                + "  <PropertyGroup>\n"
                + "    <TargetFramework>net10.0</TargetFramework>\n"
                + "    <OutputType>Exe</OutputType>\n"
                + "    <AssemblyName>prime</AssemblyName>\n"
                + "  </PropertyGroup>\n"
                + "</Project>\n");
            File.WriteAllText(Path.Combine(dir, "Program.cs"),
                "class P { static void Main() {} }\n");
            var psi = new ProcessStartInfo("dotnet")
            {
                WorkingDirectory = dir,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            };
            foreach (var a in new[]
                     { "publish", "prime.csproj", "-c", "Release", "-r", rid,
                       "--self-contained", "false", "-p:PublishReadyToRun=true" })
                psi.ArgumentList.Add(a);
            using var proc = Process.Start(psi);
            if (proc == null) return;
            proc.StandardOutput.ReadToEnd();
            proc.StandardError.ReadToEnd();
            proc.WaitForExit();
        }
        catch { /* best-effort: the caller looks for the packs again */ }
        finally
        {
            try { Directory.Delete(dir, true); } catch { }
        }
    }

    /// <summary>The crossgen2 for this machine, restoring it first when it is
    /// missing, or null when that did not bring it in. `pack --library --r2r`
    /// asks before packing: the siblings are written inside the Lisp image, which
    /// only notes a missing crossgen2 and goes on, and a package asked to carry
    /// ReadyToRun images should not quietly come out without them.</summary>
    internal static string? EnsureHostCrossgen2()
    {
        var host = HostRid();
        if (host == null) return null;
        var cg = FindCrossgen2(host);
        if (cg != null) return cg;
        Prime(host);
        return FindCrossgen2(host);
    }

    static string NuGetPackagesDir() =>
        Environment.GetEnvironmentVariable("NUGET_PACKAGES")
        ?? Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                        ".nuget", "packages");

    static string? FindCrossgen2(string hostRid)
    {
        var exe = hostRid.StartsWith("win", StringComparison.Ordinal) ? "crossgen2.exe" : "crossgen2";
        return NewestUnder(Path.Combine(NuGetPackagesDir(), $"microsoft.netcore.app.crossgen2.{hostRid}"),
                           ver => Path.Combine(ver, "tools", exe), File.Exists);
    }

    static string? FindRuntimeRefDir(string targetRid)
    {
        var pkg = Path.Combine(NuGetPackagesDir(), $"microsoft.netcore.app.runtime.{targetRid}");
        return NewestUnder(pkg, ver =>
        {
            var lib = Path.Combine(ver, "runtimes", targetRid, "lib");
            if (!Directory.Exists(lib)) return "";
            // Highest target framework the pack carries; a pack holds exactly one
            // in practice, but ordering it costs nothing and does not guess.
            return Directory.EnumerateDirectories(lib).OrderBy(Path.GetFileName,
                       StringComparer.OrdinalIgnoreCase).LastOrDefault() ?? "";
        }, p => p.Length > 0 && Directory.Exists(p));
    }

    /// <summary>The path built from the highest-versioned directory under PKGDIR
    /// that satisfies EXISTS, or null when there is none.</summary>
    static string? NewestUnder(string pkgDir, Func<string, string> build, Func<string, bool> exists)
    {
        if (!Directory.Exists(pkgDir)) return null;
        string? best = null;
        Version? bestV = null;
        foreach (var verDir in Directory.EnumerateDirectories(pkgDir))
        {
            var candidate = build(verDir);
            if (!exists(candidate)) continue;
            // Package version directories look like 10.0.0 or 10.0.0-rc.1.25451.1;
            // compare on the numeric part, and take any parse failure as oldest.
            Version.TryParse(Path.GetFileName(verDir).Split('-')[0], out var v);
            if (best == null || (v != null && (bestV == null || v > bestV))) { best = candidate; bestV = v; }
        }
        return best;
    }

    internal static string? HostRid()
    {
        var os = OperatingSystem.IsWindows() ? "win"
               : OperatingSystem.IsMacOS() ? "osx"
               : OperatingSystem.IsLinux() ? "linux"
               : null;
        if (os == null) return null;
        var arch = System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture switch
        {
            System.Runtime.InteropServices.Architecture.X64 => "x64",
            System.Runtime.InteropServices.Architecture.Arm64 => "arm64",
            System.Runtime.InteropServices.Architecture.X86 => "x86",
            _ => null,
        };
        return arch == null ? null : $"{os}-{arch}";
    }

    static bool TargetParts(string rid, out string os, out string arch)
    {
        os = rid.StartsWith("win", StringComparison.Ordinal) ? "windows"
           : rid.StartsWith("linux", StringComparison.Ordinal) ? "linux"
           : rid.StartsWith("osx", StringComparison.Ordinal) ? "osx"
           : "";
        var dash = rid.LastIndexOf('-');
        arch = dash < 0 ? "" : rid.Substring(dash + 1);
        return os.Length > 0 && (arch == "x64" || arch == "arm64" || arch == "x86");
    }
}
