using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;

namespace DotCL;

/// <summary>
/// The prose that ships with .NET: the summary, parameters, return value and
/// exceptions the compiler wrote into an XML file beside each assembly.
///
/// Two things have to be found. The file, which is not always beside the
/// assembly it documents -- the shared framework ships no XML at all, and its
/// types are documented by the reference pack, where StringBuilder is filed
/// under System.Runtime even though it runs out of System.Private.CoreLib. And
/// the entry, which is keyed by a documentation ID
/// (M:System.Text.StringBuilder.AppendLine(System.String)) built from the
/// member rather than from its name.
///
/// Both are done lazily and cached: a reader who never asks pays nothing, and
/// one who asks pays for the one file that answers.
/// </summary>
public static partial class Runtime
{
    private static readonly Dictionary<string, DocFile> _docFiles
        = new(StringComparer.OrdinalIgnoreCase);
    private static readonly object _docLock = new();

    /// <summary>One member's prose, as the XML file has it.</summary>
    internal sealed class MemberDoc
    {
        public string? Summary;
        public string? Returns;
        public List<(string Name, string Text)> Parameters = new();
        public List<(string Type, string Text)> Exceptions = new();
    }

    /// <summary>One file's entries, by documentation ID and by name.
    ///
    /// The second index is what makes a miss cheap. An ID this code cannot build
    /// exactly still has prose filed under the same name, but finding it by
    /// walking every entry costs the whole file per miss, and a completion list
    /// misses once per member it cannot place.</summary>
    internal sealed class DocFile
    {
        public readonly Dictionary<string, MemberDoc> ById = new(StringComparer.Ordinal);
        public readonly Dictionary<string, MemberDoc> ByName = new(StringComparer.Ordinal);

        public void Add(string id, MemberDoc doc)
        {
            ById[id] = doc;
            // M:System.Math.Sqrt(System.Double) is also filed under
            // M:System.Math.Sqrt, and the first overload wins.
            var cut = id.IndexOf('(');
            var generic = id.IndexOf("``", StringComparison.Ordinal);
            if (generic >= 0 && (cut < 0 || generic < cut)) cut = generic;
            if (cut < 0) return;
            var name = id.Substring(0, cut);
            if (!ByName.ContainsKey(name)) ByName[name] = doc;
        }
    }

    // ---- Finding the file ---------------------------------------------------

    private static string? _refPackDirectory;
    private static Dictionary<string, string>? _refPackTypeToFile;

    /// <summary>The reference pack documenting the shared framework this image runs
    /// on, or null when there is none (a self-contained or trimmed image).</summary>
    private static string? RefPackDirectory()
    {
        if (_refPackDirectory != null)
            return _refPackDirectory.Length == 0 ? null : _refPackDirectory;

        _refPackDirectory = "";
        var core = typeof(object).Assembly.Location;
        if (string.IsNullOrEmpty(core)) return null;

        // .../dotnet/shared/Microsoft.NETCore.App/10.0.10/System.Private.CoreLib.dll
        //     ^ root                                     -- three directories up.
        var root = System.IO.Path.GetDirectoryName(core);
        for (int i = 0; i < 3 && root != null; i++)
            root = System.IO.Path.GetDirectoryName(root);
        if (root == null) return null;

        var packs = System.IO.Path.Combine(root, "packs", "Microsoft.NETCore.App.Ref");
        if (!System.IO.Directory.Exists(packs)) return null;

        // Several versions can be installed; the newest describes the widest API
        // surface, and a member missing from it is one this image cannot call.
        string? best = null;
        foreach (var version in System.IO.Directory.GetDirectories(packs))
        {
            var refDir = System.IO.Path.Combine(version, "ref");
            if (!System.IO.Directory.Exists(refDir)) continue;
            foreach (var tfm in System.IO.Directory.GetDirectories(refDir))
                if (best == null || string.CompareOrdinal(tfm, best) > 0) best = tfm;
        }
        if (best == null) return null;
        _refPackDirectory = best;
        return best;
    }

#if !NETSTANDARD2_0
    /// <summary>Which reference assembly declares each type, so a type can find its
    /// XML file. Read from metadata rather than by loading: the reference
    /// assemblies have no code to run and must not enter the execution context.</summary>
    private static Dictionary<string, string> RefPackTypeMap()
    {
        if (_refPackTypeToFile != null) return _refPackTypeToFile;

        var map = new Dictionary<string, string>(StringComparer.Ordinal);
        var directory = RefPackDirectory();
        if (directory != null)
        {
            foreach (var dll in System.IO.Directory.GetFiles(directory, "*.dll"))
            {
                var xml = System.IO.Path.ChangeExtension(dll, ".xml");
                if (!System.IO.File.Exists(xml)) continue;
                try
                {
                    using var stream = System.IO.File.OpenRead(dll);
                    using var reader = new System.Reflection.PortableExecutable.PEReader(stream);
                    if (!reader.HasMetadata) continue;
                    var metadata = System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader(reader);
                    foreach (var handle in metadata.TypeDefinitions)
                    {
                        var definition = metadata.GetTypeDefinition(handle);
                        var space = metadata.GetString(definition.Namespace);
                        var name = metadata.GetString(definition.Name);
                        if (string.IsNullOrEmpty(name)) continue;
                        map[string.IsNullOrEmpty(space) ? name : space + "." + name] = xml;
                    }
                }
                catch { /* not a managed assembly, or unreadable: skip it */ }
            }
        }
        _refPackTypeToFile = map;
        return map;
    }
#else
    private static Dictionary<string, string> RefPackTypeMap() =>
        _refPackTypeToFile ??= new Dictionary<string, string>(StringComparer.Ordinal);
#endif

    private static readonly Dictionary<string, string> _noTypeMap = new(StringComparer.Ordinal);
    private static bool _refPackMapStarted;

    /// <summary>The type-to-file map when it is built, and otherwise nothing plus a
    /// background build. Reading the metadata of a hundred reference assemblies is
    /// tens of milliseconds, which is again not a keystroke's to pay.</summary>
    private static bool TryRefPackTypeMap(bool wait, Type? warmFor,
                                          out Dictionary<string, string> map)
    {
        bool start = false;
        lock (_docLock)
        {
            if (_refPackTypeToFile != null) { map = _refPackTypeToFile; return true; }
            map = _noTypeMap;
            if (!wait && !_refPackMapStarted) { _refPackMapStarted = true; start = true; }
        }
        if (wait) { map = RefPackTypeMap(); return true; }
        if (start)
            System.Threading.Tasks.Task.Run(() =>
            {
                RefPackTypeMap();
                // Whoever asked first is about to ask again, and the file their
                // type needs is only knowable once the map exists: reading it in
                // the same pass saves them a second round of asking.
                if (warmFor == null) return;
                var file = DocFileFor(warmFor, wait: true);
                if (file != null) DocsIn(file);
            });
        return false;
    }

    /// <summary>The XML file documenting TYPE, or null.</summary>
    private static string? DocFileFor(Type type, bool wait)
    {
        // A package brings its own XML along, which is the only place a
        // third-party library is documented at all.
        var location = type.Assembly.Location;
        if (!string.IsNullOrEmpty(location))
        {
            var beside = System.IO.Path.ChangeExtension(location, ".xml");
            if (System.IO.File.Exists(beside)) return beside;
        }

        // The framework's own types are documented by the reference pack, under
        // the contract assembly rather than the one they run out of.
        var top = type;
        while (top.IsNested && top.DeclaringType != null) top = top.DeclaringType;
        if (top.IsGenericType) top = top.GetGenericTypeDefinition();
        var full = top.Namespace == null ? top.Name : top.Namespace + "." + top.Name;
        TryRefPackTypeMap(wait, type, out var map);
        return map.TryGetValue(full, out var path) ? path : null;
    }

    // ---- Reading the file ---------------------------------------------------

    private static DocFile DocsIn(string path)
    {
        lock (_docLock)
        {
            if (_docFiles.TryGetValue(path, out var cached)) return cached;
        }
        var docs = new DocFile();
        try { ReadDocFile(path, docs); }
        catch { /* an unreadable or truncated file documents nothing */ }
        lock (_docLock)
        {
            if (_docFiles.TryGetValue(path, out var raced)) return raced;
            _docFiles[path] = docs;
            return docs;
        }
    }

    private static readonly DocFile _noDocs = new();
    private static readonly HashSet<string> _docFilesStarted = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>The entries of PATH when they are already in memory, and otherwise
    /// nothing plus a background read of the file.
    ///
    /// One file is several megabytes of XML and takes the better part of half a
    /// second to turn into entries, which is not a cost a keystroke can carry.
    /// Completion runs again on the next keystroke, so what this loses is the
    /// documentation on the first list and nothing after it.</summary>
    private static bool TryDocsIn(string path, out DocFile docs)
    {
        lock (_docLock)
        {
            if (_docFiles.TryGetValue(path, out var cached)) { docs = cached; return true; }
            docs = _noDocs;
            if (!_docFilesStarted.Add(path)) return false;
        }
        System.Threading.Tasks.Task.Run(() => DocsIn(path));
        return false;
    }

    private static void ReadDocFile(string path, DocFile into)
    {
        var settings = new System.Xml.XmlReaderSettings
        {
            IgnoreComments = true,
            IgnoreProcessingInstructions = true,
            DtdProcessing = System.Xml.DtdProcessing.Prohibit
        };
        using var reader = System.Xml.XmlReader.Create(path, settings);
        MemberDoc? current = null;
        while (reader.Read())
        {
            if (reader.NodeType != System.Xml.XmlNodeType.Element) continue;
            switch (reader.Name)
            {
                case "member":
                {
                    var name = reader.GetAttribute("name");
                    current = null;
                    if (!string.IsNullOrEmpty(name))
                    {
                        current = new MemberDoc();
                        into.Add(name!, current);
                    }
                    break;
                }
                case "summary" when current != null:
                    current.Summary = ReadDocText(reader);
                    break;
                case "returns" when current != null:
                    current.Returns = ReadDocText(reader);
                    break;
                case "param" when current != null:
                {
                    var name = reader.GetAttribute("name") ?? "";
                    current.Parameters.Add((name, ReadDocText(reader)));
                    break;
                }
                case "exception" when current != null:
                {
                    var thrown = ShortDocReference(reader.GetAttribute("cref") ?? "");
                    current.Exceptions.Add((thrown, ReadDocText(reader)));
                    break;
                }
            }
        }
    }

    /// <summary>The text of the element the reader is on, with cross-reference
    /// elements rendered as the names they point at.</summary>
    private static string ReadDocText(System.Xml.XmlReader reader)
    {
        if (reader.IsEmptyElement) return "";
        var depth = reader.Depth;
        var text = new StringBuilder();
        while (reader.Read())
        {
            if (reader.NodeType == System.Xml.XmlNodeType.EndElement && reader.Depth == depth)
                break;
            switch (reader.NodeType)
            {
                case System.Xml.XmlNodeType.Text:
                case System.Xml.XmlNodeType.CDATA:
                case System.Xml.XmlNodeType.SignificantWhitespace:
                case System.Xml.XmlNodeType.Whitespace:
                    text.Append(reader.Value);
                    break;
                case System.Xml.XmlNodeType.Element:
                    // The prose is written expecting the name to stand where the
                    // element is: <see cref="T:System.String" /> reads as String,
                    // and <paramref name="value" /> as value.
                    if (reader.Name == "see" || reader.Name == "seealso")
                        text.Append(ShortDocReference(reader.GetAttribute("cref")
                                                      ?? reader.GetAttribute("langword") ?? ""));
                    else if (reader.Name == "paramref" || reader.Name == "typeparamref")
                        text.Append(reader.GetAttribute("name") ?? "");
                    break;
            }
        }
        return CollapseWhitespace(text.ToString());
    }

    /// <summary>T:System.Text.StringBuilder as StringBuilder: the prose reads as a
    /// sentence, and a full name in the middle of one does not.</summary>
    private static string ShortDocReference(string cref)
    {
        if (cref.Length > 2 && cref[1] == ':') cref = cref.Substring(2);
        var tick = cref.IndexOf('`');
        if (tick >= 0) cref = cref.Substring(0, tick);
        var dot = cref.LastIndexOf('.');
        return dot >= 0 && dot + 1 < cref.Length ? cref.Substring(dot + 1) : cref;
    }

    private static string CollapseWhitespace(string text)
    {
        var result = new StringBuilder(text.Length);
        bool space = false;
        foreach (var c in text)
        {
            if (char.IsWhiteSpace(c)) { space = result.Length > 0; continue; }
            if (space) { result.Append(' '); space = false; }
            result.Append(c);
        }
        return result.ToString();
    }

    // ---- Naming the entry ---------------------------------------------------

    /// <summary>A type as a documentation ID spells it: nesting with a dot, a
    /// generic parameter by its position, a constructed generic in braces.</summary>
    private static void AppendDocTypeName(StringBuilder into, Type type, bool inParameter)
    {
        if (type.IsByRef)
        {
            AppendDocTypeName(into, type.GetElementType()!, inParameter);
            into.Append('@');
            return;
        }
        if (type.IsPointer)
        {
            AppendDocTypeName(into, type.GetElementType()!, inParameter);
            into.Append('*');
            return;
        }
        if (type.IsArray)
        {
            AppendDocTypeName(into, type.GetElementType()!, inParameter);
            var rank = type.GetArrayRank();
            if (rank == 1) into.Append("[]");
            else
            {
                into.Append('[');
                for (int i = 0; i < rank; i++) { if (i > 0) into.Append(','); into.Append("0:"); }
                into.Append(']');
            }
            return;
        }
        if (type.IsGenericParameter)
        {
            // `0 is the declaring type's first parameter, ``0 the method's.
            into.Append(type.DeclaringMethod != null ? "``" : "`")
                .Append(type.GenericParameterPosition);
            return;
        }

        var name = type.FullName
                   ?? (type.Namespace == null ? type.Name : type.Namespace + "." + type.Name);
        if (type.IsGenericType && !type.IsGenericTypeDefinition && inParameter)
        {
            var tick = name.IndexOf('`');
            if (tick >= 0) name = name.Substring(0, tick);
            into.Append(name.Replace('+', '.')).Append('{');
            var arguments = type.GetGenericArguments();
            for (int i = 0; i < arguments.Length; i++)
            {
                if (i > 0) into.Append(',');
                AppendDocTypeName(into, arguments[i], true);
            }
            into.Append('}');
            return;
        }
        // A FullName carries the assembly-qualified argument list of a closed
        // generic; a documentation ID never does.
        var bracket = name.IndexOf('[');
        if (bracket >= 0) name = name.Substring(0, bracket);
        into.Append(name.Replace('+', '.'));
    }

    private static string DocTypeName(Type type)
    {
        var text = new StringBuilder();
        AppendDocTypeName(text, type, false);
        return text.ToString();
    }

    private static void AppendDocParameters(StringBuilder into, ParameterInfo[] parameters)
    {
        if (parameters.Length == 0) return;
        into.Append('(');
        for (int i = 0; i < parameters.Length; i++)
        {
            if (i > 0) into.Append(',');
            AppendDocTypeName(into, parameters[i].ParameterType, true);
        }
        into.Append(')');
    }

    /// <summary>The documentation ID of a member, or null for one that has none.</summary>
    internal static string? DocIdFor(MemberInfo member)
    {
        if (member is Type asType) return "T:" + DocTypeName(asType);
        var declaring = member.DeclaringType;
        if (declaring == null) return null;
        var owner = declaring.IsGenericType ? declaring.GetGenericTypeDefinition() : declaring;
        var prefix = DocTypeName(owner) + ".";

        switch (member)
        {
            case ConstructorInfo ctor:
            {
                var text = new StringBuilder("M:").Append(prefix).Append("#ctor");
                AppendDocParameters(text, ctor.GetParameters());
                return text.ToString();
            }
            case MethodInfo method:
            {
                // An explicit interface implementation is spelled with # where the
                // reflected name has a dot.
                var text = new StringBuilder("M:").Append(prefix)
                                                  .Append(method.Name.Replace('.', '#'));
                if (method.IsGenericMethod)
                    text.Append("``").Append(method.GetGenericArguments().Length);
                AppendDocParameters(text, method.GetParameters());
                return text.ToString();
            }
            case PropertyInfo property:
            {
                var text = new StringBuilder("P:").Append(prefix).Append(property.Name);
                AppendDocParameters(text, property.GetIndexParameters());
                return text.ToString();
            }
            case FieldInfo field: return "F:" + prefix + field.Name;
            case EventInfo evt: return "E:" + prefix + evt.Name;
        }
        return null;
    }

    // ---- Looking one up -----------------------------------------------------

    /// <summary>The entry for a member declared on TYPE itself, or null.</summary>
    private static MemberDoc? DeclaredDocumentation(Type type, string memberName, bool wait)
    {
        var file = DocFileFor(type, wait);
        if (file == null) return null;
        DocFile docs;
        if (wait) docs = DocsIn(file);
        else if (!TryDocsIn(file, out docs)) return null;
        if (docs.ById.Count == 0) return null;

        var owner = type.IsGenericType ? type.GetGenericTypeDefinition() : type;

        // A property accessor is documented under the property: there is no
        // get_Length entry, there is a Length one.
        var wanted = memberName;
        if ((wanted.StartsWith("get_", StringComparison.Ordinal)
             || wanted.StartsWith("set_", StringComparison.Ordinal))
            && wanted.Length > 4
            && owner.GetProperty(wanted.Substring(4)) != null)
            wanted = wanted.Substring(4);

        foreach (var member in owner.GetMember(wanted,
                     BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static
                     | BindingFlags.DeclaredOnly))
        {
            var id = DocIdFor(member);
            if (id != null && docs.ById.TryGetValue(id, out var exact) && exact.Summary != null)
                return exact;
        }

        // An overload whose ID cannot be built exactly still shares its prose with
        // the others often enough that the first entry under the same name beats
        // saying nothing.
        var stem = DocTypeName(owner) + "." + wanted;
        foreach (var kind in new[] { "M:", "P:", "F:", "E:" })
        {
            // Both indexes: a parameterless member's ID is its own name, and one
            // with parameters is filed under its name as well.
            if (docs.ById.TryGetValue(kind + stem, out var plain)) return plain;
            if (docs.ByName.TryGetValue(kind + stem, out var named)) return named;
        }
        return null;
    }

    /// <summary>The documentation for TYPE, or for the member of TYPE called NAME.
    /// An inherited member is documented where it is declared, so the hierarchy is
    /// walked rather than only the receiver asked.</summary>
    internal static MemberDoc? DocumentationFor(Type type, string? memberName, bool wait)
    {
        if (string.IsNullOrEmpty(memberName))
        {
            var file = DocFileFor(type, wait);
            if (file == null) return null;
            DocFile docs;
            if (wait) docs = DocsIn(file);
            else if (!TryDocsIn(file, out docs)) return null;
            var owner = type.IsGenericType ? type.GetGenericTypeDefinition() : type;
            return docs.ById.TryGetValue("T:" + DocTypeName(owner), out var typeDoc) ? typeDoc : null;
        }

        for (var current = type; current != null; current = current.BaseType)
        {
            var found = DeclaredDocumentation(current, memberName!, wait);
            if (found != null) return found;
        }
        // An interface member reached through a class that documents nothing.
        foreach (var contract in type.GetInterfaces())
        {
            var found = DeclaredDocumentation(contract, memberName!, wait);
            if (found != null) return found;
        }
        return null;
    }

    /// <summary>Just the sentence, for a completion item to carry. Never waits for a
    /// file to be read: a list that is one keystroke behind is better than one that
    /// arrives late.</summary>
    internal static string? SummaryFor(Type type, string? memberName)
        => DocumentationFor(type, memberName, wait: false)?.Summary;

    /// <summary>DOTNET:DOCUMENTATION — the prose .NET ships for a type or one of its
    /// members, as (:summary :parameters :returns :exceptions), or NIL when neither
    /// an XML file beside the assembly nor the reference pack describes it.</summary>
    public static LispObject DotNetDocumentation(LispObject[] args)
    {
        if (args.Length < 1 || args.Length > 2)
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:DOCUMENTATION: expected 1 or 2 arguments, got {args.Length}"));

        Type type = args[0] is LispDotNetObject dno
            ? (dno.Value is Type asType ? asType : dno.Type)
            : ResolveElementTypeArg(args[0]);
        string? member = args.Length == 2 && args[1] != Nil.Instance ? NameArg(args[1]) : null;

        // Asked for outright, so this one waits for the file rather than
        // answering NIL while it is read.
        var doc = DocumentationFor(type, member, wait: true);
        if (doc == null) return Nil.Instance;

        LispObject parameters = Nil.Instance;
        for (int i = doc.Parameters.Count - 1; i >= 0; i--)
            parameters = new Cons(new Cons(new LispString(doc.Parameters[i].Name),
                                           new LispString(doc.Parameters[i].Text)),
                                  parameters);
        LispObject exceptions = Nil.Instance;
        for (int i = doc.Exceptions.Count - 1; i >= 0; i--)
            exceptions = new Cons(new Cons(new LispString(doc.Exceptions[i].Type),
                                           new LispString(doc.Exceptions[i].Text)),
                                  exceptions);

        LispObject result = Nil.Instance;
        if (exceptions != Nil.Instance)
            result = new Cons(Startup.Keyword("EXCEPTIONS"), new Cons(exceptions, result));
        if (!string.IsNullOrEmpty(doc.Returns))
            result = new Cons(Startup.Keyword("RETURNS"),
                              new Cons(new LispString(doc.Returns!), result));
        if (parameters != Nil.Instance)
            result = new Cons(Startup.Keyword("PARAMETERS"), new Cons(parameters, result));
        if (!string.IsNullOrEmpty(doc.Summary))
            result = new Cons(Startup.Keyword("SUMMARY"),
                              new Cons(new LispString(doc.Summary!), result));
        return result;
    }
}
