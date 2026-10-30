using System;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Security;
using System.Text;

namespace DotCL;

public static partial class Runtime
{
    /// <summary>(dotnet:%pack-library dll-path id version authors description output-dir)
    /// Write a NuGet package (a zip) holding a library .dll saved by dotnet:library,
    /// its sidecar XML doc when present, and a dependency on the DotCL.Runtime this
    /// process runs. A C# project then needs only a PackageReference. Returns the
    /// nupkg path.</summary>
    public static LispObject DotNetPackLibrary(LispObject[] args)
    {
        if (args.Length != 6)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:%PACK-LIBRARY: requires 6 arguments (dll-path id version authors description output-dir)"));

        static string? Opt(LispObject o) => o == Nil.Instance ? null : NameArg(o);

        string dll = NameArg(args[0]);
        if (!File.Exists(dll))
            throw new LispErrorException(new LispError($"DOTNET:%PACK-LIBRARY: {dll} does not exist"));

        string id = Opt(args[1]) ?? Path.GetFileNameWithoutExtension(dll);
        string? version = Opt(args[2]);
        if (version == null)
        {
            var v = AssemblyName.GetAssemblyName(dll).Version ?? new Version(1, 0, 0, 0);
            version = v.Revision > 0 ? v.ToString(4) : v.ToString(3);
        }
        string authors = Opt(args[3]) ?? id;
        string description = Opt(args[4]) ?? id;
        string outDir = Opt(args[5]) ?? (Path.GetDirectoryName(Path.GetFullPath(dll)) ?? ".");

        // The DotCL.Runtime package has the version of its assembly. The informational
        // version names the last git tag, which is an older release on any commit after it.
        var rv = typeof(Runtime).Assembly.GetName().Version
            ?? throw new LispErrorException(new LispError("DOTNET:%PACK-LIBRARY: cannot tell the DotCL.Runtime version"));
        string rtVersion = rv.Revision > 0 ? rv.ToString(4) : rv.ToString(3);

        static string X(string s) => SecurityElement.Escape(s) ?? "";
        string nuspec =
            "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
            "<package xmlns=\"http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd\">\n" +
            "  <metadata>\n" +
            $"    <id>{X(id)}</id>\n" +
            $"    <version>{X(version)}</version>\n" +
            $"    <authors>{X(authors)}</authors>\n" +
            $"    <description>{X(description)}</description>\n" +
            "    <dependencies>\n" +
            "      <group targetFramework=\".NETStandard2.0\">\n" +
            $"        <dependency id=\"DotCL.Runtime\" version=\"[{X(rtVersion)}]\" />\n" +
            "      </group>\n" +
            "    </dependencies>\n" +
            "  </metadata>\n" +
            "</package>\n";

        Directory.CreateDirectory(outDir);
        string nupkg = Path.Combine(outDir, $"{id}.{version}.nupkg");
        if (File.Exists(nupkg)) File.Delete(nupkg);

        string fileName = Path.GetFileName(dll);
        string xml = Path.ChangeExtension(dll, ".xml");
        using (var zip = ZipFile.Open(nupkg, ZipArchiveMode.Create))
        {
            void Text(string name, string content)
            {
                var e = zip.CreateEntry(name);
                using var s = e.Open();
                var b = new UTF8Encoding(false).GetBytes(content);
                s.Write(b, 0, b.Length);
            }
            Text("[Content_Types].xml",
                "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
                "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">" +
                "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\" />" +
                "<Default Extension=\"nuspec\" ContentType=\"application/octet\" />" +
                "<Default Extension=\"dll\" ContentType=\"application/octet\" />" +
                "<Default Extension=\"xml\" ContentType=\"application/octet\" />" +
                "</Types>");
            Text("_rels/.rels",
                "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
                "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">" +
                $"<Relationship Type=\"http://schemas.microsoft.com/packaging/2010/07/manifest\" Target=\"/{id}.nuspec\" Id=\"R1\" />" +
                "</Relationships>");
            Text($"{id}.nuspec", nuspec);
            zip.CreateEntryFromFile(dll, $"lib/netstandard2.0/{fileName}");
            if (File.Exists(xml))
                zip.CreateEntryFromFile(xml, $"lib/netstandard2.0/{Path.GetFileName(xml)}");
        }
        return new LispString(nupkg);
    }
}
