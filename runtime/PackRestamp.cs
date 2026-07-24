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
    /// Optional nuspec metadata overrides for a pack. Each null field leaves
    /// the source dotcl package's value in place; a non-null one replaces it in
    /// every produced package. Without these a tool packed under a different id
    /// keeps dotcl's description, project URL, repository, embedded README and
    /// tags, so its nuget.org page misrepresents what it actually is.
    /// </summary>
    public sealed class Meta
    {
        public string? Description;
        public string? ProjectUrl;
        public string? RepositoryUrl;
        public string? RepositoryCommit;
        public string? ReadmePath;   // file whose bytes replace the packaged README
        public string? Tags;         // free-form; normalized to space-separated
        public string? Authors;
        public string? Copyright;
        public string? License;
    }

    /// <summary>
    /// Provenance fields whose donor value describes dotcl, not the app being
    /// packed. Once the package id is no longer the donor's, keeping them is
    /// worse than dropping them: a repository url and commit belonging to
    /// another project is not stale metadata, it is wrong attribution. Any of
    /// these that the caller did not supply is removed from the nuspec.
    /// </summary>
    static readonly string[] DonorProvenanceFields =
        { "description", "projectUrl", "repository", "readme", "tags", "authors", "copyright",
          "license", "licenseUrl", "icon", "iconUrl" };

    /// <summary>The package id the payloads are restamped from.</summary>
    const string DonorId = "dotcl";

    /// <summary>True once the package is no longer dotcl itself.</summary>
    static bool IsRebrand(string newId) =>
        !newId.Equals(DonorId, StringComparison.OrdinalIgnoreCase);

    /// <summary>
    /// NuGet requires &lt;description&gt; and &lt;authors&gt;. When the package
    /// is rebranded away from dotcl we will not inherit dotcl's values for them,
    /// so they have to come from the .asd or the command line. Checked before
    /// any work happens rather than surfacing as a misdescribed package.
    /// </summary>
    public static void EnsureRequiredMetadata(string newId, Meta? meta)
    {
        if (!IsRebrand(newId)) return;
        var missing = new List<string>();
        if (string.IsNullOrWhiteSpace(meta?.Description))
            missing.Add("a description (--description, or :description in the .asd)");
        if (string.IsNullOrWhiteSpace(meta?.Authors))
            missing.Add("an author (--authors, or :author in the .asd)");
        if (missing.Count > 0)
            throw new InvalidOperationException(
                $"packing as '{newId}' needs {string.Join(" and ", missing)}. NuGet requires "
                + "these fields, and dotcl's own values would misdescribe your package.");
    }

    /// <summary>
    /// Restamp the dotcl packages in <paramref name="sourceDir"/> into
    /// <paramref name="outputDir"/>. Returns the produced nupkg paths.
    /// </summary>
    public static List<string> Run(
        string sourceDir, string? dotclVersion, string newId, string command,
        string version, string faslPath, string? bundleDir,
        IReadOnlyList<string> rids, string outputDir, Meta? meta, bool dryRun)
    {
        dotclVersion ??= InferDotclVersion(sourceDir);

        // A payload runtime older than the loose-fasl loader would restamp into a
        // tool that starts a REPL instead of running the app — fail loudly first.
        EnsureLoaderCapablePayload(dotclVersion);

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
                                ridMap: rids, faslPath: null, bundleDir: null, meta, outputDir));
        foreach (var (rid, path) in ridPkgs)
            produced.Add(RestampOne(path, $"dotcl.{rid}", $"{newId}.{rid}", version, command,
                                    ridMap: null, faslPath: faslPath, bundleDir: bundleDir, meta, outputDir));
        return produced;
    }

    // The first released dotcl whose payload runtime loads a loose dotcl.user.fasl
    // at startup. A --from payload older than this restamps into an installable
    // tool that silently drops to a REPL instead of running the app (it never
    // loads the user fasl), so refuse to build one from it.
    static readonly Version LoaderFloor = new Version(0, 1, 19);

    internal static void EnsureLoaderCapablePayload(string dotclVersion)
    {
        // Compare the leading numeric X.Y.Z, ignoring any -prerelease / +build
        // suffix a local dev payload may carry. Only block versions we can prove
        // are too old; let an unparseable version through.
        var numeric = dotclVersion;
        int cut = numeric.IndexOfAny(new[] { '-', '+' });
        if (cut >= 0) numeric = numeric.Substring(0, cut);
        if (Version.TryParse(numeric, out var v) && v < LoaderFloor)
            throw new InvalidOperationException(
                $"--from payload dotcl {dotclVersion} is older than {LoaderFloor}, the "
                + "first version whose runtime loads a loose user fasl. A tool "
                + "restamped from an older payload would silently start a REPL instead "
                + $"of running your app; use a dotcl {LoaderFloor} or newer payload.");
    }

    /// <summary>
    /// Pick the dotcl version out of a directory of published packages. Only
    /// the pointer package is named dotcl.&lt;version&gt;.nupkg — the RID ones
    /// have a RID between the id and the version, so a digit after "dotcl."
    /// identifies the base.
    /// </summary>
    internal static string InferDotclVersion(string dir)
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
        IReadOnlyList<string>? ridMap, string? faslPath, string? bundleDir,
        Meta? meta, string outputDir)
    {
        Directory.CreateDirectory(outputDir);
        var dest = Path.Combine(outputDir, $"{newId}.{version}.nupkg");
        if (File.Exists(dest)) File.Delete(dest);

        using var src = ZipFile.OpenRead(srcPath);
        using var outFs = new FileStream(dest, FileMode.CreateNew);
        using var outZip = new ZipArchive(outFs, ZipArchiveMode.Create);

        // The nuspec's <readme> names a file packaged alongside it. Read that
        // name before the copy loop, since the README entry can precede the
        // nuspec in the archive. A --readme override replaces its bytes; with no
        // override on a rebranded package the file is dropped entirely, because
        // shipping dotcl's README as the app's own is what nuget.org would show.
        string? readmeName = null;
        byte[]? readmeBytes = null;
        bool dropReadme = false;
        if (meta?.ReadmePath != null || IsRebrand(newId))
        {
            var nuspecEntry = src.Entries.FirstOrDefault(e =>
                e.FullName.Equals($"{oldId}.nuspec", StringComparison.OrdinalIgnoreCase));
            if (nuspecEntry != null)
                readmeName = LoadXml(ReadAll(nuspecEntry))
                    .Descendants().FirstOrDefault(e => e.Name.LocalName == "readme")
                    ?.Value?.Trim();
            if (readmeName != null)
            {
                if (meta?.ReadmePath != null) readmeBytes = File.ReadAllBytes(meta.ReadmePath);
                else dropReadme = true;
            }
        }

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

            // Dropped <readme> target: leave the donor's README out of the zip.
            if (dropReadme && readmeName != null
                && name.Equals(readmeName, StringComparison.OrdinalIgnoreCase))
                continue;

            // Debug symbols: a distributed tool never loads its own .pdb at run
            // time, so the donor packages' runtime.pdb / DotCL.Runtime.pdb are
            // pure weight in every restamped package. Drop them.
            if (name.EndsWith(".pdb", StringComparison.OrdinalIgnoreCase))
                continue;

            var bytes = ReadAll(entry);
            var outName = name;

            if (name.Equals($"{oldId}.nuspec", StringComparison.OrdinalIgnoreCase))
            {
                bytes = RewriteNuspec(bytes, newId, version, meta);
                outName = $"{newId}.nuspec";
            }
            else if (readmeName != null
                     && name.Equals(readmeName, StringComparison.OrdinalIgnoreCase))
            {
                bytes = readmeBytes!;
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

    static byte[] RewriteNuspec(byte[] bytes, string newId, string version, Meta? meta)
    {
        var doc = LoadXml(bytes);
        var md = doc.Root?.Elements().FirstOrDefault(e => e.Name.LocalName == "metadata")
            ?? throw new InvalidOperationException("nuspec has no <metadata>");
        SetChildValue(md, "id", newId);
        SetChildValue(md, "version", version);
        if (meta != null)
        {
            if (meta.Description != null) SetOrCreateChild(md, "description", meta.Description);
            if (meta.ProjectUrl != null) SetOrCreateChild(md, "projectUrl", meta.ProjectUrl);
            if (meta.Authors != null) SetOrCreateChild(md, "authors", meta.Authors);
            if (meta.Copyright != null) SetOrCreateChild(md, "copyright", meta.Copyright);
            if (meta.Tags != null) SetOrCreateChild(md, "tags", NormalizeTags(meta.Tags));
            if (meta.License != null) SetLicenseExpression(md, meta.License);
            if (meta.RepositoryUrl != null)
                SetRepository(md, meta.RepositoryUrl, meta.RepositoryCommit);
        }
        // Anything the caller did not supply still holds dotcl's value. Under a
        // different id that is wrong attribution, not merely stale, so drop it.
        if (IsRebrand(newId)) DropUnsuppliedDonorFields(md, meta);
        return SaveXml(doc);
    }

    /// <summary>
    /// Remove the donor's provenance fields that neither the .asd nor the
    /// command line replaced. Emitting nothing is strictly better than pointing
    /// at another project's repository, README or copyright holder.
    /// </summary>
    static void DropUnsuppliedDonorFields(XElement md, Meta? meta)
    {
        var supplied = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        if (meta?.Description != null) supplied.Add("description");
        if (meta?.ProjectUrl != null) supplied.Add("projectUrl");
        if (meta?.RepositoryUrl != null) supplied.Add("repository");
        if (meta?.ReadmePath != null) supplied.Add("readme");
        if (meta?.Tags != null) supplied.Add("tags");
        if (meta?.Authors != null) supplied.Add("authors");
        if (meta?.Copyright != null) supplied.Add("copyright");
        if (meta?.License != null) { supplied.Add("license"); supplied.Add("licenseUrl"); }

        foreach (var field in DonorProvenanceFields)
        {
            if (supplied.Contains(field)) continue;
            md.Elements().Where(e => e.Name.LocalName == field).Remove();
        }
    }

    /// <summary>
    /// Write &lt;license type="expression"&gt;. A donor licenseUrl (deprecated by
    /// NuGet, and pointing at dotcl's) is removed so the two cannot disagree.
    /// </summary>
    static void SetLicenseExpression(XElement md, string expression)
    {
        md.Elements().Where(e => e.Name.LocalName == "licenseUrl").Remove();
        var lic = md.Elements().FirstOrDefault(e => e.Name.LocalName == "license");
        if (lic == null)
        {
            lic = new XElement(md.Name.Namespace + "license");
            md.Add(lic);
        }
        lic.SetAttributeValue("type", "expression");
        lic.Value = expression;
    }

    static void SetChildValue(XElement parent, string localName, string value)
    {
        var e = parent.Elements().FirstOrDefault(x => x.Name.LocalName == localName)
            ?? throw new InvalidOperationException($"nuspec metadata has no <{localName}>");
        e.Value = value;
    }

    /// <summary>Set an existing metadata child, or append one if absent.</summary>
    static void SetOrCreateChild(XElement parent, string localName, string value)
    {
        var e = parent.Elements().FirstOrDefault(x => x.Name.LocalName == localName);
        if (e != null) e.Value = value;
        else parent.Add(new XElement(parent.Name.Namespace + localName, value));
    }

    /// <summary>
    /// Point &lt;repository&gt; at the app's own repo. When the override omits a
    /// commit, drop the stale one carried over from dotcl rather than keep it.
    /// </summary>
    static void SetRepository(XElement md, string url, string? commit)
    {
        var repo = md.Elements().FirstOrDefault(x => x.Name.LocalName == "repository");
        if (repo == null)
        {
            repo = new XElement(md.Name.Namespace + "repository", new XAttribute("type", "git"));
            md.Add(repo);
        }
        repo.SetAttributeValue("url", url);
        repo.SetAttributeValue("commit", commit); // null removes the attribute
    }

    /// <summary>
    /// nuspec stores tags space-separated; accept comma / semicolon / whitespace
    /// input so `--tags "a,b"` and `--tags "a b"` both work.
    /// </summary>
    static string NormalizeTags(string raw)
    {
        var parts = raw.Split(new[] { ',', ';', ' ', '\t', '\n', '\r' },
            StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        return string.Join(' ', parts);
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
