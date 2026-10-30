using System;
using System.Reflection;
#if NET9_0_OR_GREATER
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Security.Cryptography;
using System.Text;
#endif

namespace DotCL;

/// <summary>
/// The part of this runtime a compiled-in library (dotnet:library) calls: every
/// type, method and field of this assembly that the library's metadata names.
/// Used to list that surface, to record a summary of it in the DLL, and to check
/// at load time that a runtime provides all of it.
/// </summary>
public static class LibrarySurface
{
    public const string KeyRuntimeVersion = "dotcl-baked-runtime-version";
    public const string KeyRuntimeSource = "dotcl-baked-runtime-source";
    public const string KeySurface = "dotcl-runtime-surface";

    // Fixed-width stand-in for the surface value, patched once the image exists.
    internal static readonly string SurfacePlaceholder = "sha256:" + new string('x', 64) + " n=00000";

#if NET9_0_OR_GREATER
    private static string RuntimeAssemblyName => typeof(LibrarySurface).Assembly.GetName().Name!;

    private sealed class SigNames : ISignatureTypeProvider<string, object?>
    {
        private readonly MetadataReader _r;
        public SigNames(MetadataReader r) { _r = r; }
        public string GetPrimitiveType(PrimitiveTypeCode t) => t.ToString();
        public string GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle h, byte k)
        {
            var d = reader.GetTypeDefinition(h);
            return reader.GetString(d.Namespace) + "." + reader.GetString(d.Name);
        }
        public string GetTypeFromReference(MetadataReader reader, TypeReferenceHandle h, byte k) => TypeRefName(reader, h);
        public string GetTypeFromSpecification(MetadataReader reader, object? c, TypeSpecificationHandle h, byte k) =>
            reader.GetTypeSpecification(h).DecodeSignature(this, c);
        public string GetSZArrayType(string e) => e + "[]";
        public string GetArrayType(string e, ArrayShape s) => e + "[" + new string(',', s.Rank - 1) + "]";
        public string GetByReferenceType(string e) => e + "&";
        public string GetPointerType(string e) => e + "*";
        public string GetGenericInstantiation(string g, ImmutableArray<string> a) => g + "<" + string.Join(",", a) + ">";
        public string GetGenericMethodParameter(object? c, int i) => "!!" + i;
        public string GetGenericTypeParameter(object? c, int i) => "!" + i;
        public string GetFunctionPointerType(MethodSignature<string> s) => "fnptr";
        public string GetModifiedType(string m, string u, bool r) => u;
        public string GetPinnedType(string e) => e;
    }

    private static string TypeRefName(MetadataReader r, TypeReferenceHandle h)
    {
        var t = r.GetTypeReference(h);
        var name = r.GetString(t.Name);
        if (t.ResolutionScope.Kind == HandleKind.TypeReference)
            return TypeRefName(r, (TypeReferenceHandle)t.ResolutionScope) + "+" + name;
        var ns = r.GetString(t.Namespace);
        return ns.Length == 0 ? name : ns + "." + name;
    }

    private static bool InRuntime(MetadataReader r, TypeReferenceHandle h)
    {
        var scope = r.GetTypeReference(h).ResolutionScope;
        while (scope.Kind == HandleKind.TypeReference)
            scope = r.GetTypeReference((TypeReferenceHandle)scope).ResolutionScope;
        return scope.Kind == HandleKind.AssemblyReference
            && r.GetString(r.GetAssemblyReference((AssemblyReferenceHandle)scope).Name) == RuntimeAssemblyName;
    }

    /// <summary>The runtime members the metadata names, as (token, description):
    /// "T Type", "M Type::Name(args)->ret", "F Type::Name:type".</summary>
    private static List<(int Token, string Text)> Collect(MetadataReader r)
    {
        var sig = new SigNames(r);
        var list = new List<(int, string)>();
        foreach (var h in r.TypeReferences)
            if (InRuntime(r, h))
                list.Add((MetadataTokens.GetToken(h), "T " + TypeRefName(r, h)));
        foreach (var h in r.MemberReferences)
        {
            var m = r.GetMemberReference(h);
            if (m.Parent.Kind != HandleKind.TypeReference || !InRuntime(r, (TypeReferenceHandle)m.Parent)) continue;
            var owner = TypeRefName(r, (TypeReferenceHandle)m.Parent);
            var name = r.GetString(m.Name);
            string text;
            if (m.GetKind() == MemberReferenceKind.Field)
                text = "F " + owner + "::" + name + ":" + m.DecodeFieldSignature(sig, null);
            else
            {
                var s = m.DecodeMethodSignature(sig, null);
                text = "M " + owner + "::" + name + "(" + string.Join(",", s.ParameterTypes) + ")->" + s.ReturnType;
            }
            list.Add((MetadataTokens.GetToken(h), text));
        }
        return list;
    }

    private static MetadataReader ReaderOf(byte[] image, out PEReader pe)
    {
        pe = new PEReader(ImmutableArray.Create(image));
        return pe.GetMetadataReader();
    }

    /// <summary>The sorted surface of the library in IMAGE.</summary>
    public static List<string> Entries(byte[] image)
    {
        var r = ReaderOf(image, out var pe);
        using (pe)
            return Collect(r).Select(e => e.Text).Distinct().OrderBy(s => s, StringComparer.Ordinal).ToList();
    }

    private static readonly HashSet<string> s_boundaryTypes = new(StringComparer.Ordinal)
    {
        "DotCL.DotclHost", "DotCL.Emitter.DynamicClassBuilder", "DotCL.LibrarySurface", "DotCL.LispObject",
        "DotCL.Nil", "DotCL.T", "DotCL.Fixnum", "DotCL.DoubleFloat", "DotCL.SingleFloat", "DotCL.LispString",
        "DotCL.Symbol", "DotCL.LispFunction", "DotCL.Startup", "DotCL.Cons", "DotCL.LispChar",
    };

    private static readonly HashSet<string> s_boundaryMembers = new(StringComparer.Ordinal)
    {
        "DotCL.Nil::Instance", "DotCL.T::Instance", "DotCL.Fixnum::Make", "DotCL.Fixnum::get_Value",
        "DotCL.DoubleFloat::.ctor", "DotCL.SingleFloat::.ctor", "DotCL.LispString::.ctor",
        "DotCL.Startup::SymInPkg", "DotCL.Startup::Keyword", "DotCL.Cons::.ctor", "DotCL.LispChar::Make",
    };

    // What the code of CL's control and value operators (CATCH / THROW,
    // HANDLER-CASE, UNWIND-PROTECT, MULTIPLE-VALUE-BIND ...) calls.
    private static readonly HashSet<string> s_specialTypes = new(StringComparer.Ordinal)
    {
        "DotCL.MultipleValues", "DotCL.MvHeld", "DotCL.CatchTagStack", "DotCL.CatchThrowException",
        "DotCL.ControlFlowFilters", "DotCL.HandlerBinding", "DotCL.HandlerClusterStack", "DotCL.ThrowReturn",
        "DotCL.RestartClusterStack", "DotCL.DynamicBindings", "DotCL.BlockReturnException", "DotCL.GoException",
    };

    /// <summary>The group of a surface entry: "abi" (LibraryAbi), "boundary" (the
    /// object model and the entry points of the public wrappers), "primitive"
    /// (Runtime, one Common Lisp operation each), "special" (the support of the
    /// control and value operators) or "internal" (anything else).</summary>
    public static string Group(string entry)
    {
        var body = entry.Length > 2 ? entry.Substring(2) : entry;
        int sep = body.IndexOf("::", StringComparison.Ordinal);
        var type = sep < 0 ? body : body.Substring(0, sep);
        if (type == "DotCL.LibraryAbi" || type.StartsWith("DotCL.LibraryAbi+", StringComparison.Ordinal)) return "abi";
        if (type == "DotCL.Runtime") return "primitive";
        if (type == "DotCL.MultipleValues" && sep >= 0)
        {
            var name = body.Substring(sep + 2, body.IndexOf('(', sep) - sep - 2);
            if (name is "Reset" or "ResetOnExit" or "TakeMode" or "PassMode") return "internal";
        }
        if (s_specialTypes.Contains(type)) return "special";
        if (sep < 0) return s_boundaryTypes.Contains(type) ? "boundary" : "internal";
        if (type == "DotCL.DotclHost" || type == "DotCL.Emitter.DynamicClassBuilder" || type == "DotCL.LibrarySurface")
            return "boundary";
        int end = body.IndexOfAny(new[] { '(', ':' }, sep + 2);
        var member = end < 0 ? body : body.Substring(0, end);
        return s_boundaryMembers.Contains(member) ? "boundary" : "internal";
    }

    /// <summary>"sha256:HEX n=COUNT" of the entries, 5 digits of count.</summary>
    public static string Summary(List<string> entries)
    {
        var hash = SHA256.HashData(Encoding.UTF8.GetBytes(string.Join("\n", entries)));
        return "sha256:" + Convert.ToHexString(hash).ToLowerInvariant()
            + " n=" + Math.Min(entries.Count, 99999).ToString("D5");
    }

    /// <summary>Replace the placeholder surface value in IMAGE with the real one.</summary>
    internal static void StampImage(byte[] image)
    {
        var summary = Summary(Entries(image));
        var ph = Encoding.UTF8.GetBytes(SurfacePlaceholder);
        var val = Encoding.UTF8.GetBytes(summary);
        if (ph.Length != val.Length) return;
        for (int i = 0; i + ph.Length <= image.Length; i++)
        {
            int j = 0;
            while (j < ph.Length && image[i + j] == ph[j]) j++;
            if (j == ph.Length) { Array.Copy(val, 0, image, i, val.Length); return; }
        }
    }

    internal static string? Meta(Assembly a, string key)
    {
        foreach (var m in a.GetCustomAttributes<AssemblyMetadataAttribute>())
            if (m.Key == key) return m.Value;
        return null;
    }

    /// <summary>Called from the module initializer of a compiled-in library. Does
    /// nothing when this runtime was built from the same source as the one the
    /// library was built with; otherwise resolves every runtime member the library
    /// names and fails, naming the missing ones, if any is absent. A different
    /// version that still has everything is accepted silently.</summary>
    public static unsafe void Check(Assembly library)
    {
        try
        {
            var mine = Startup.RuntimeSource();
            var theirs = Meta(library, KeyRuntimeSource);
            if (mine != null && mine == theirs) return;
            if (!library.TryGetRawMetadata(out byte* blob, out int len)) return;
            var r = new MetadataReader(blob, len);
            var missing = new List<string>();
            var module = library.ManifestModule;
            foreach (var (token, text) in Collect(r))
            {
                try
                {
                    if (text.StartsWith("T ", StringComparison.Ordinal)) module.ResolveType(token);
                    else module.ResolveMember(token);
                }
                catch (Exception) { missing.Add(text); }
            }
            if (missing.Count == 0) return;
            var built = Meta(library, KeyRuntimeVersion) ?? "an unknown version";
            var msg = new StringBuilder();
            msg.Append("library ").Append(library.GetName().Name).Append(" was built with dotcl ")
               .Append(built).Append(" and needs ").Append(missing.Count)
               .Append(" member(s) that this DotCL.Runtime (")
               .Append(typeof(LibrarySurface).Assembly.GetName().Version).Append(") does not have: ");
            msg.Append(string.Join("; ", missing.Take(8)));
            if (missing.Count > 8) msg.Append("; ...");
            throw new InvalidOperationException(msg.ToString());
        }
        catch (InvalidOperationException) { throw; }
        catch (Exception) { /* a check that cannot run must not stop the library */ }
    }
#else
    public static void Check(Assembly library) { }
#endif
}
