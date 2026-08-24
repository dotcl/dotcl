using System.Reflection;
using System.Collections.Generic;
using System.IO;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace DotCL.Emitter;

/// <summary>
/// Writes a monolithic FASL bundle: one file that carries several .fasl inputs
/// and loads all of them.
///
/// Most implementations combine FASLs by concatenating object files. A dotcl
/// FASL is a .NET PE assembly, and two of those concatenated are not an
/// assembly, so the bundle is a container instead: an assembly whose only
/// contents are the inputs, embedded as managed resources in order. Loading it
/// loads each part (see Runtime.LoadFaslBundleParts), which is the contract
/// uiop's bundle-op wants -- ship one file, load it, get everything.
///
/// The container carries no code of its own, so nothing here needs
/// Reflection.Emit; it is metadata plus a resource blob. It does need to be a
/// valid assembly, which means an assembly row, a module row, and the
/// &lt;Module&gt; pseudo-type that every .NET module begins with.
/// </summary>
internal static class FaslBundle
{
    /// <summary>Resource-name prefix marking a bundled part. The index keeps
    /// load order, which matters: a later part may depend on an earlier one.</summary>
    internal const string PartPrefix = Runtime.FaslBundlePartPrefix;

    internal static string PartName(int index, string fileName)
        => $"{PartPrefix}{index:D4}/{fileName}";

    /// <summary>
    /// Write OUTPUTPATH as a bundle of INPUTPATHS, in order.
    /// </summary>
    internal static void Write(IReadOnlyList<string> inputPaths, string outputPath)
    {
        var md = new MetadataBuilder();
        var name = Path.GetFileNameWithoutExtension(outputPath);
        if (string.IsNullOrEmpty(name)) name = "dotcl-bundle";

        md.AddAssembly(
            md.GetOrAddString(name),
            new System.Version(1, 0, 0, 0),
            culture: default,
            publicKey: default,
            flags: 0,
            hashAlgorithm: AssemblyHashAlgorithm.Sha1);

        md.AddModule(
            generation: 0,
            moduleName: md.GetOrAddString(Path.GetFileName(outputPath)),
            mvid: md.GetOrAddGuid(System.Guid.NewGuid()),
            encId: default,
            encBaseId: default);

        // Row 1 of the TypeDef table is the <Module> pseudo-type; a module
        // without it does not load.
        md.AddTypeDefinition(
            attributes: default,
            @namespace: default,
            name: md.GetOrAddString("<Module>"),
            baseType: default,
            fieldList: MetadataTokens.FieldDefinitionHandle(1),
            methodList: MetadataTokens.MethodDefinitionHandle(1));

        var resources = new BlobBuilder();
        for (int i = 0; i < inputPaths.Count; i++)
        {
            var bytes = File.ReadAllBytes(inputPaths[i]);
            // Managed resources are length-prefixed and 8-byte aligned in the
            // section the ManifestResource offsets point into.
            int offset = resources.Count;
            resources.WriteInt32(bytes.Length);
            resources.WriteBytes(bytes);
            resources.Align(8);

            md.AddManifestResource(
                attributes: ManifestResourceAttributes.Public,
                name: md.GetOrAddString(PartName(i, Path.GetFileName(inputPaths[i]))),
                implementation: default,
                offset: (uint)offset);
        }

        var pe = new ManagedPEBuilder(
            header: new PEHeaderBuilder(
                imageCharacteristics: Characteristics.ExecutableImage | Characteristics.Dll),
            metadataRootBuilder: new MetadataRootBuilder(md),
            ilStream: new BlobBuilder(),
            managedResources: resources);

        var peBlob = new BlobBuilder();
        pe.Serialize(peBlob);

        var dir = Path.GetDirectoryName(Path.GetFullPath(outputPath));
        if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir);
        using var stream = new FileStream(outputPath, FileMode.Create, FileAccess.Write);
        peBlob.WriteContentTo(stream);
    }
}
