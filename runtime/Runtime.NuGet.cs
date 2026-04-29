using System.IO.Compression;
using System.Net.Http;
using System.Runtime.InteropServices;
using System.Text.Json;

namespace DotCL;

internal static class DotNetNuGet
{
    private static readonly HttpClient _http = new();

    // (dotnet:require "System.Management")
    // (dotnet:require "System.Management" "8.0.0")
    public static LispObject Require(LispObject[] args)
    {
        if (args.Length < 1 || args.Length > 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:REQUIRE: expected 1 or 2 arguments"));

        string packageId = args[0] is LispString s0 ? s0.Value : args[0].ToString()!;
        string? version = args.Length > 1
            ? (args[1] is LispString s1 ? s1.Value : args[1].ToString())
            : null;

        return RequireAsync(packageId, version).GetAwaiter().GetResult();
    }

    private static async Task<LispObject> RequireAsync(string packageId, string? version)
    {
        string idLower = packageId.ToLowerInvariant();
        string globalDir = GetGlobalPackagesFolder();
        string packageDir = Path.Combine(globalDir, idLower);

        string resolvedVersion = version
            ?? await ResolveLatestVersionAsync(idLower, packageDir);
        string versionLower = resolvedVersion.ToLowerInvariant();
        string versionDir = Path.Combine(packageDir, versionLower);

        if (!Directory.Exists(versionDir))
            await DownloadAndExtractAsync(idLower, versionLower, versionDir);

        int loaded = 0;
        foreach (var dll in FindDlls(versionDir))
        {
            System.Reflection.Assembly.LoadFrom(dll);
            loaded++;
        }

        if (loaded == 0)
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:REQUIRE: no compatible DLL found in {packageId}/{resolvedVersion}"));

        return new LispString($"{packageId}/{resolvedVersion}");
    }

    private static string GetGlobalPackagesFolder()
    {
        var env = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
        if (!string.IsNullOrEmpty(env)) return env;
        return Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
            ".nuget", "packages");
    }

    private static async Task<string> ResolveLatestVersionAsync(string idLower, string packageDir)
    {
        // Use cached version if available
        if (Directory.Exists(packageDir))
        {
            var cached = Directory.GetDirectories(packageDir)
                .Select(Path.GetFileName)
                .OfType<string>()
                .OrderByDescending(v => v)
                .FirstOrDefault();
            if (cached != null) return cached;
        }

        var url = $"https://api.nuget.org/v3-flatcontainer/{idLower}/index.json";
        string json;
        try { json = await _http.GetStringAsync(url); }
        catch (Exception ex)
        {
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:REQUIRE: failed to fetch versions for {idLower}: {ex.Message}"));
        }

        using var doc = JsonDocument.Parse(json);
        var versions = doc.RootElement.GetProperty("versions")
            .EnumerateArray()
            .Select(v => v.GetString()!)
            .ToList();

        // Prefer latest stable (no pre-release suffix)
        return versions.LastOrDefault(v => !v.Contains('-')) ?? versions.Last();
    }

    private static async Task DownloadAndExtractAsync(string idLower, string version, string targetDir)
    {
        var url = $"https://api.nuget.org/v3-flatcontainer/{idLower}/{version}/{idLower}.{version}.nupkg";
        byte[] nupkg;
        try { nupkg = await _http.GetByteArrayAsync(url); }
        catch (Exception ex)
        {
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:REQUIRE: failed to download {idLower}/{version}: {ex.Message}"));
        }

        Directory.CreateDirectory(targetDir);

        using var zip = new ZipArchive(new MemoryStream(nupkg), ZipArchiveMode.Read);
        foreach (var entry in zip.Entries)
        {
            // Extract lib/ and runtimes/ subtrees only
            if (!entry.FullName.StartsWith("lib/") && !entry.FullName.StartsWith("runtimes/"))
                continue;
            if (entry.Name == "") continue; // directory entry

            var dest = Path.Combine(targetDir,
                entry.FullName.Replace('/', Path.DirectorySeparatorChar));
            Directory.CreateDirectory(Path.GetDirectoryName(dest)!);
            entry.ExtractToFile(dest, overwrite: true);
        }
    }

    // TFM preference: newest .NET first, then netstandard fallbacks.
    private static readonly string[] TfmChain =
    [
        "net10.0", "net9.0", "net8.0", "net7.0", "net6.0", "net5.0",
        "netstandard2.1", "netstandard2.0",
        "netstandard1.6", "netstandard1.5", "netstandard1.4",
        "netstandard1.3", "netstandard1.2", "netstandard1.1", "netstandard1.0",
    ];

    private static IEnumerable<string> FindDlls(string versionDir)
    {
        var libDir = Path.Combine(versionDir, "lib");

        // On Windows, RID-specific lib takes precedence
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            var winDir = Path.Combine(versionDir, "runtimes", "win", "lib");
            foreach (var tfm in TfmChain)
            {
                var d = Path.Combine(winDir, tfm);
                if (!Directory.Exists(d)) continue;
                foreach (var dll in Directory.GetFiles(d, "*.dll"))
                    yield return dll;
                yield break;
            }
        }

        if (!Directory.Exists(libDir)) yield break;

        foreach (var tfm in TfmChain)
        {
            var d = Path.Combine(libDir, tfm);
            if (!Directory.Exists(d)) continue;
            foreach (var dll in Directory.GetFiles(d, "*.dll"))
                yield return dll;
            yield break;
        }
    }
}
