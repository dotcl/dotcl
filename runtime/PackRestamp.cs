using System.IO.Compression;
using System.Text;
using System.Xml;
using System.Xml.Linq;

namespace DotCL;

/// <summary>
/// Rebuilds an application's dotnet-tool packages out of the published dotcl
/// tool packages: unzip, rewrite the package id / version / command name, drop
/// the app's fasl next to the runtime, rezip. The runtime images in the
/// per-RID packages are reused byte-for-byte, so packing needs no csproj, no
/// `dotnet pack` child process, and no local crossgen — which matters because
/// crossgen2 cannot produce R2R images for a RID other than the host's, so a
/// from-source pack can only ever cover the machine it runs on.
///
/// Three package shapes take part, all of them restamped:
///   dotcl              — the pointer package (DotnetTool). Carries no runtime;
///                        its DotnetToolSettings.xml maps each RID to the
///                        dotcl.&lt;rid&gt; package that does.
///   dotcl.&lt;rid&gt;  — self-contained payload (DotnetToolRidPackage), an
///                        apphost plus R2R images.
///   dotcl.any          — the framework-dependent payload, launched via
///                        `dotnet runtime.dll` rather than an apphost.
/// </summary>
static class PackRestamp
{
    /// <summary>
    /// Restamp the dotcl packages in <paramref name="sourceDir"/> into
    /// <paramref name="outputDir"/>. Returns the produced nupkg paths.
    /// </summary>
    public static List<string> Run(
        string sourceDir, string? dotclVersion, string newId, string command,
        string version, string faslPath, string? bundleDir,
        IReadOnlyList<string> rids, string outputDir, bool dryRun)
    {
        dotclVersion ??= InferDotclVersion(sourceDir);

        // Resolve every input up front so a missing RID package is reported as
        // one list rather than discovered halfway through writing the output.
        var basePkg = Path.Combine(sourceDir, $"dotcl.{dotclVersion}.nupkg");
        var ridPkgs = new List<(string Rid, string Path)>();
        foreach (var rid in rids)
            ridPkgs.Add((rid, Path.Combine(sourceDir, $"dotcl.{rid}.{dotclVersion}.nupkg")));

        var missing = new List<string>();
        if (!File.Exists(basePkg)) missing.Add(Path.GetFileName(basePkg));
        foreach (var (_, path) in ridPkgs)
            if (!File.Exists(path)) missing.Add(Path.GetFileName(path));
        if (missing.Count > 0)
            throw new InvalidOperationException(
                $"missing source package(s) in {sourceDir}: {string.Join(", ", missing)}");

        var produced = new List<string>();
        if (dryRun)
        {
            produced.Add(Path.Combine(outputDir, $"{newId}.{version}.nupkg"));
            foreach (var (rid, _) in ridPkgs)
                produced.Add(Path.Combine(outputDir, $"{newId}.{rid}.{version}.nupkg"));
            return produced;
        }

        // The pointer package gets the rewritten RID map but no fasl: it holds
        // no runtime, so nothing there ever executes.
        produced.Add(RestampOne(basePkg, "dotcl", newId, version, command,
                                ridMap: rids, faslPath: null, bundleDir: null, outputDir));
        foreach (var (rid, path) in ridPkgs)
            produced.Add(RestampOne(path, $"dotcl.{rid}", $"{newId}.{rid}", version, command,
                                    ridMap: null, faslPath: faslPath, bundleDir: bundleDir, outputDir));
        return produced;
    }

    /// <summary>
    /// Pick the dotcl version out of a directory of published packages. Only
    /// the pointer package is named dotcl.&lt;version&gt;.nupkg — the RID ones
    /// have a RID between the id and the version, so a digit after "dotcl."
    /// identifies the base.
    /// </summary>
    static string InferDotclVersion(string dir)
    {
        if (!Directory.Exists(dir))
            throw new InvalidOperationException($"source package directory not found: {dir}");
        var found = new List<string>();
        foreach (var f in Directory.GetFiles(dir, "dotcl.*.nupkg"))
        {
            var name = Path.GetFileName(f);
            var mid = name.Substring("dotcl.".Length,
                                     name.Length - "dotcl.".Length - ".nupkg".Length);
            if (mid.Length > 0 && char.IsDigit(mid[0])) found.Add(mid);
        }
        if (found.Count == 0)
            throw new InvalidOperationException(
                $"no dotcl.<version>.nupkg found in {dir}");
        if (found.Count > 1)
            throw new InvalidOperationException(
                $"{dir} holds several dotcl versions ({string.Join(", ", found)}); "
                + "pass --dotcl-version to pick one");
        return found[0];
    }

    static string RestampOne(
        string srcPath, string oldId, string newId, string version, string command,
        IReadOnlyList<string>? ridMap, string? faslPath, string? bundleDir, string outputDir)
    {
        Directory.CreateDirectory(outputDir);
        var dest = Path.Combine(outputDir, $"{newId}.{version}.nupkg");
        if (File.Exists(dest)) File.Delete(dest);

        using var src = ZipFile.OpenRead(srcPath);
        using var outFs = new FileStream(dest, FileMode.CreateNew);
        using var outZip = new ZipArchive(outFs, ZipArchiveMode.Create);

        // Set when the package turns out to carry a runtime: the directory
        // holding the apphost is where the fasl has to land to be found.
        string? payloadDir = null;

        foreach (var entry in src.Entries)
        {
            if (entry.Name.Length == 0) continue; // directory marker
            var name = entry.FullName;

            // An author signature covers the package hash, so it cannot survive
            // a rewrite — packages pulled from nuget.org carry one.
            if (name.Equals(".signature.p7s", StringComparison.OrdinalIgnoreCase))
                continue;

            var bytes = ReadAll(entry);
            var outName = name;

            if (name.Equals($"{oldId}.nuspec", StringComparison.OrdinalIgnoreCase))
            {
                bytes = RewriteNuspec(bytes, newId, version);
                outName = $"{newId}.nuspec";
            }
            else if (name.Equals("_rels/.rels", StringComparison.OrdinalIgnoreCase))
            {
                bytes = RewriteRels(bytes, oldId, newId);
            }
            else if (name.EndsWith(".psmdcp", StringComparison.OrdinalIgnoreCase))
            {
                bytes = RewritePsmdcp(bytes, newId, version);
            }
            else if (Path.GetFileName(name).Equals("DotnetToolSettings.xml",
                                                   StringComparison.OrdinalIgnoreCase))
            {
                bytes = RewriteToolSettings(bytes, command, newId, ridMap, out var isPayload);
                if (isPayload) payloadDir = name.Substring(0, name.LastIndexOf('/') + 1);
            }

            // Carry the source package's stamp over verbatim: whatever NuGet
            // wrote when it built the dotcl package round-trips unchanged.
            WriteEntry(outZip, outName, bytes, entry.LastWriteTime);
        }

        if (faslPath != null)
        {
            if (payloadDir == null)
                throw new InvalidOperationException(
                    $"{Path.GetFileName(srcPath)}: no DotnetToolSettings.xml declaring an "
                    + "EntryPoint, so there is no runtime to place the fasl beside");
            WriteEntry(outZip, payloadDir + "dotcl.user.fasl",
                       File.ReadAllBytes(faslPath), SourceTimestamp(faslPath));

            if (bundleDir != null)
            {
                var root = Path.GetFullPath(bundleDir);
                foreach (var f in Directory.GetFiles(root, "*", SearchOption.AllDirectories))
                {
                    var rel = Path.GetRelativePath(root, f).Replace('\\', '/');
                    WriteEntry(outZip, payloadDir + rel, File.ReadAllBytes(f),
                               SourceTimestamp(f));
                }
            }
        }

        return dest;
    }

    static byte[] RewriteNuspec(byte[] bytes, string newId, string version)
    {
        var doc = LoadXml(bytes);
        var md = doc.Root?.Elements().FirstOrDefault(e => e.Name.LocalName == "metadata")
            ?? throw new InvalidOperationException("nuspec has no <metadata>");
        SetChildValue(md, "id", newId);
        SetChildValue(md, "version", version);
        return SaveXml(doc);
    }

    static void SetChildValue(XElement parent, string localName, string value)
    {
        var e = parent.Elements().FirstOrDefault(x => x.Name.LocalName == localName)
            ?? throw new InvalidOperationException($"nuspec metadata has no <{localName}>");
        e.Value = value;
    }

    static byte[] RewriteRels(byte[] bytes, string oldId, string newId)
    {
        var doc = LoadXml(bytes);
        foreach (var r in doc.Root!.Elements().Where(e => e.Name.LocalName == "Relationship"))
        {
            var target = (string?)r.Attribute("Target");
            if (target != null && target.Equals($"/{oldId}.nuspec", StringComparison.OrdinalIgnoreCase))
                r.SetAttributeValue("Target", $"/{newId}.nuspec");
        }
        return SaveXml(doc);
    }

    static byte[] RewritePsmdcp(byte[] bytes, string newId, string version)
    {
        var doc = LoadXml(bytes);
        foreach (var e in doc.Root!.Elements())
        {
            if (e.Name.LocalName == "identifier") e.Value = newId;
            else if (e.Name.LocalName == "version") e.Value = version;
        }
        return SaveXml(doc);
    }

    /// <summary>
    /// Rename the installed command, and in the pointer package replace the RID
    /// map wholesale — it must name the app's own RID packages, and only the
    /// ones actually being produced.
    /// </summary>
    static byte[] RewriteToolSettings(
        byte[] bytes, string command, string newId, IReadOnlyList<string>? ridMap,
        out bool isPayload)
    {
        var doc = LoadXml(bytes);
        isPayload = false;

        foreach (var c in doc.Descendants().Where(e => e.Name.LocalName == "Command"))
        {
            c.SetAttributeValue("Name", command);
            // EntryPoint/Runner name the executable; the pointer package's
            // settings carry neither. The apphost keeps its own name across a
            // rename, so EntryPoint is left alone.
            if (c.Attribute("EntryPoint") != null) isPayload = true;
        }

        var pkgs = doc.Descendants()
                      .FirstOrDefault(e => e.Name.LocalName == "RuntimeIdentifierPackages");
        if (pkgs != null && ridMap != null)
        {
            var ns = pkgs.Name.Namespace;
            pkgs.RemoveNodes();
            foreach (var rid in ridMap)
                pkgs.Add(new XElement(ns + "RuntimeIdentifierPackage",
                                      new XAttribute("RuntimeIdentifier", rid),
                                      new XAttribute("Id", $"{newId}.{rid}")));
        }
        return SaveXml(doc);
    }

    static XDocument LoadXml(byte[] bytes)
    {
        using var ms = new MemoryStream(bytes);
        return XDocument.Load(ms, LoadOptions.PreserveWhitespace);
    }

    static byte[] SaveXml(XDocument doc)
    {
        var settings = new XmlWriterSettings
        {
            // Match what NuGet itself writes: UTF-8 with a BOM, and the
            // original layout (PreserveWhitespace carries it over).
            Encoding = new UTF8Encoding(encoderShouldEmitUTF8Identifier: true),
            Indent = false,
        };
        using var ms = new MemoryStream();
        using (var w = XmlWriter.Create(ms, settings)) doc.Save(w);
        return ms.ToArray();
    }

    static byte[] ReadAll(ZipArchiveEntry entry)
    {
        using var s = entry.Open();
        using var ms = new MemoryStream();
        s.CopyTo(ms);
        return ms.ToArray();
    }

    /// <summary>
    /// Zip stores a DOS timestamp, which carries no timezone. NuGet writes the
    /// UTC wall clock into that field and reads it back the same way, so an
    /// entry stamped with local time comes out of `dotnet tool install` shifted
    /// into the future by the UTC offset. Following NuGet's convention keeps an
    /// extracted file's mtime equal to the original's.
    /// </summary>
    static DateTimeOffset SourceTimestamp(string path)
    {
        var utc = new DateTimeOffset(File.GetLastWriteTimeUtc(path), TimeSpan.Zero);
        // DOS timestamps cannot express anything before 1980; the setter throws.
        return utc < DosEpoch ? DosEpoch : utc;
    }

    static readonly DateTimeOffset DosEpoch =
        new DateTimeOffset(1980, 1, 1, 0, 0, 0, TimeSpan.Zero);

    /// <param name="lastWrite">
    /// Timestamp for the entry. Never leave this to CreateEntry's default: it
    /// stamps every entry with the current time, which both dates the package
    /// into the future (see SourceTimestamp) and flattens the relative order of
    /// the files — ASDF then sees bundled sources as no older than the fasls
    /// built from them and recompiles them on every run.
    /// </param>
    static void WriteEntry(ZipArchive zip, string name, byte[] bytes,
                           DateTimeOffset lastWrite)
    {
        var e = zip.CreateEntry(name, CompressionLevel.Optimal);
        e.LastWriteTime = lastWrite;
        using var s = e.Open();
        s.Write(bytes, 0, bytes.Length);
    }
}
